##@S Functions that extract and update the documentation


#' Extract all documentation
#' 
#' @param FD Object of class FilesDescription; See documentation to see how to describe a collection of files  
#' @param regexp_fxstart Regex to determine function starts; default should work
#' 
#' @return A list of two objects: A list of functions and information about each function, and then a data frame denoting ALL of the functions and information about each. 
#' 
#' @export
#' 
extract_full_docu = function(FD, regexp_fxstart = "(^[[:alnum:]_.]+) *(=|(<-)) *function") {
  ## Search for function headers
  MCB = search_code_matches(RE = Regex(base = regexp_fxstart), FD = FD, logged = "ROXY-TEMPLATES")
  
  fx_list = list()
  fx_df = NULL
  
  for (j in seq_along(MCB$code)) {
    fxinfo = zhdp_extractFxInfo(MCB$code[[j]], MCB$matchlines[[j]])
    temp = zhdp_extractDocu(MCB$code[[j]], MCB$matchlines[[j]], fxinfo)
    fx_list = c(fx_list, temp$list)
    fx_df = rbind(fx_df, cbind(fileID = j, filename = MCB$files[j], temp$df))
  }
  ## Add ID's
  fx_df = cbind(ID = seq_len(nrow(fx_df)), fx_df, want_docu = TRUE, want_export = TRUE)
  
  return(list(MCB = MCB, df = fx_df, list = fx_list))
}


#' Update/create roxygen templates
#' 
#' DO NOT RUN THIS WITHOUT VERSION CONTROL!
#' 
#' This function searches for existing documentation, and updates it to a specific format. (The format isn't really flexible at the moment; not sure if effort will be spent generalizing it)
#'     
#' This function assumes functions are of the format 
#' FUNCTION_NAME = function( .... ) \{
#'   content
#' \}
#' 
#' @param FD Object of class FilesDescription; See documentation to see how to describe a collection of files  
#' @param guess_emptyparam Should empty parameters be filled in by "default" value? 
#' @param regexp_noexport A vector of regular expressions that determine which functions that should not be exported
#' @param regexp_nodocu A vector of regular expressions that determine which functions should not receive documentation
#' @param regexp_fxstart Regex to determine function starts; default should work
#' @param test_run If TRUE: Won't write any changes to file. This is defaulted to TRUE for safety. 
#' 
#' @return message about success!
#' 
#' @export
#' 
update_fx_documentation = function(FD, guess_emptyparam = FALSE,
                                      regexp_noexport = NULL, regexp_nodocu = NULL,
                                      regexp_fxstart = "(^[[:alnum:]_.]+) *(=|(<-)) *function", 
                                      test_run = TRUE) {
  ## guess_emptyparam guess -> when empty/temp, replaces parameter documentation with most common documentation for specific parameter. --- want to add marker for when something is edited. ********* NOT IMPLEMENTED NOW ****************
  ## idea -- make this happen based on file or entire codebase (allow it as option) 
  
  if (FALSE) {
    FD = FilesDescription(mode = "R", dirlist = "codeProcessing/R/")
    regexp_fxstart = "(^[[:alnum:]_.]+) *(=|(<-)) *function"
    regexp_noexport = NULL
    regexp_nodocu = NULL
  }
  
  ## Setup
  temp = extract_full_docu(FD = FD, regexp_fxstart = regexp_fxstart)
  fx_df = temp$df
  fx_list = temp$list
  MCB = temp$MCB
  
  ## Check for undocumenting / unexporting
  for (reg in regexp_noexport) { fx_df$want_export[grep(reg, x = fx_df$fx_name)] = FALSE }
  for (reg in regexp_nodocu) { fx_df$want_docu[grep(reg, x = fx_df$fx_name)] = FALSE }
  
  ## Update all the documentation (store as $docu_new in the list)
  for (j in seq_along(fx_list)) {
    if (fx_df$want_docu[j]) {
      fx_list[[j]]$docu_new = reformat_documentation(fx_list[[j]]$docu_cur, params = fx_list[[j]]$params, to_export = fx_df$want_export[j])
    } else {
      fx_list[[j]]$docu_new = NULL
    }
  }
  
  ## Insert/replace existing documentation as necessary
  adjust_df = function(df, fx_no, to_add = FALSE, to_rm = FALSE, new_len = 0) {
    ids = which(df$fileID == df$fileID[fx_no])
    before_line = df$fx_start[fx_no] - 1 ## also adjust the doc_end also!
    if (to_rm) {
      adjust = (df$doc_end[fx_no] - df$doc_start[fx_no] + 1) * -1
      df$doc_end[fx_no] = NA; df$doc_start[fx_no] = NA
    } else if (to_add) {
      adjust = new_len
    } else {
      cur_len = (df$doc_end[fx_no] - df$doc_start[fx_no] + 1)
      adjust = new_len - cur_len
    }
    for(j in ids) {
      if (!is.na(df$doc_start[j]) && df$doc_start[j] >= before_line) { df$doc_start[j] = df$doc_start[j] + adjust }
      if (!is.na(df$doc_end[j]) && df$doc_end[j] >= before_line) { df$doc_end[j] = df$doc_end[j] + adjust }
      if (!is.na(df$fx_start[j]) && df$fx_start[j] >= before_line) { df$fx_start[j] = df$fx_start[j] + adjust }
    }
    if (to_add) {
      df$doc_start[fx_no] = df$fx_start[fx_no] - adjust ; df$doc_end[fx_no] = df$fx_start[fx_no] - 1
    } 
    return(df)
  }
  
  if (!test_run) {
    files_changed = rep(FALSE, max(fx_df$fileID))
    for (j in seq_along(fx_list)) {
      fileID = fx_df$fileID[j]; olds = fx_df$doc_start[j]; olde = fx_df$doc_end[j]
      todo = paste("## TODO: [Documentation-AUTO] Check/fix Roxygen2 Documentation (",fx_list[[j]]$fxname,")", sep = "")
      if (is.null(fx_list[[j]]$docu_new) & !is.null(fx_list[[j]]$docu_cur)) {
        # Then, clear old documentation
        MCB$code[[fileID]] = replace_codelines(MCB$code[[fileID]], newcode = NULL, 
                                          lines_start = olds, lines_end = olde)
        fx_df = adjust_df(df = fx_df, fx_no = j, to_rm = TRUE)
        files_changed[fileID] = TRUE
      } else if (is.null(fx_list[[j]]$docu_cur)) {
        # Insert documentation, since currently null
        MCB$code[[fileID]] = insert_codelines(basecode = MCB$code[[fileID]], newcode = c(todo, fx_list[[j]]$docu_new$Value), before_line = fx_df$fx_start[j])
        fx_df = adjust_df(fx_df, j, to_add = TRUE, new_len = nrow(fx_list[[j]]$docu_new)+1)
        files_changed[fileID] = TRUE
      } else if ( (nrow(fx_list[[j]]$docu_cur) != nrow(fx_list[[j]]$docu_new)) | 
                    (!all(fx_list[[j]]$docu_cur$Value == fx_list[[j]]$docu_new$Value)) ) {
        # Replace documentation
        MCB$code[[fileID]] = replace_codelines(MCB$code[[fileID]], newcode = c(todo, fx_list[[j]]$docu_new$Value), 
                                          lines_start = olds, lines_end = olde)
        fx_df = adjust_df(fx_df, j, new_len = nrow(fx_list[[j]]$docu_new)+1)
        files_changed[fileID] = TRUE
      } else {
        ## NO CHANGE
      }
    }
  }
  
  ## Write updated documentation as necessary
  if (!test_run) { write_MatchedCodebase(MCB, which(files_changed)) }
  
  return("Done! [Inserting/formatting documentation (roxygen) templates]")
}


