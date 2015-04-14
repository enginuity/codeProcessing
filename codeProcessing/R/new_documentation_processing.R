
## TODO: [Documentation-AUTO] Check/fix Roxygen2 Documentation (extract_full_docu)
#' <What does this function do>
#' 
#' @param FD temp
#' @param regexp_fxstart temp
#' 
#' @return temp
#' 
#' @export
#' 
extract_full_docu = function(FD, regexp_fxstart = "(^[[:alnum:]_.]+) *(=|(<-)) *function") {
  ### HELPER FUNCTIONS ###
  
  extractFxInfo = function(code, matchlines) {
    ## Check the function names extracted, and returns a list of matching functions and information for each function. 
    fn_name_regex = stringr::str_extract(code[matchlines], pattern = "[[:alnum:]_.]+") ## is character vector
    parsed_code = parse(text = code, keep.source = TRUE)
    
    fn_name_parse = lapply(parsed_code, function(x) {as.character(x[[2]])}) ## is list
    is_function = sapply(parsed_code, function(x) {x[[3]][[1]] == 'function'})
    res_list = list()
    for(i in which(is_function)) {
      if (fn_name_parse[[i]] %in% fn_name_regex) {
        fn_params = names(parsed_code[[i]][[3]][[2]])
        res_list[[i]] = list(fxname = fn_name_parse[[i]], params = fn_params, 
                             matchlineIND = which(fn_name_parse[[i]] == fn_name_regex))
      }
    }
    return(res_list)
  }
  
  extractDocu = function(code, all_matchlines, fxinfo) {
    ## For each codefile, extract all matches -- return a list of a data frame and an updated function information list. 
    reslist = list()
    resdf = data.frame(fx_name = sapply(fxinfo, function(x) {x$fxname}),
                       doc_exist = FALSE, doc_start = NA, doc_end = NA, fx_start = NA, fx_end = NA)
    
    for (j in seq_along(fxinfo)) {
      fx_start = all_matchlines[fxinfo[[j]]$matchlineIND]
      reslist[[j]] = list(fxname = fxinfo[[j]]$fxname, 
                          docu_cur = NULL, 
                          code = NULL, ## TODO: [Implement] this someday. 
                          params = fxinfo[[j]]$params)
      doclocs = extract_prev_headers(text = code, lineno = fx_start)
      if (length(doclocs) > 0) {
        resdf$doc_exist[j] = TRUE        
        resdf$doc_start[j] = min(doclocs)
        resdf$doc_end[j] = max(doclocs)
        
        reslist[[j]]$docu_cur = process_cur_docu(code, lines = doclocs)
      }
      resdf$fx_start[j] = fx_start
    }
    return(list(df = resdf, list = reslist))
  }
  
  ### Function starts here! ####################  
  
  ## Search for function headers
  MCB = search_code_matches(RE = Regex(base = regexp_fxstart), FD = FD, logged = "ROXY-TEMPLATES")
  
  fx_list = list()
  fx_df = NULL
  
  for (j in seq_along(MCB$code)) {
    fxinfo = extractFxInfo(MCB$code[[j]], MCB$matchlines[[j]])
    temp = extractDocu(MCB$code[[j]], MCB$matchlines[[j]], fxinfo)
    fx_list = c(fx_list, temp$list)
    fx_df = rbind(fx_df, cbind(fileID = j, filename = MCB$files[j], temp$df))
  }
  ## Add ID's
  fx_df = cbind(ID = seq_len(nrow(fx_df)), fx_df, want_docu = TRUE, want_export = TRUE)
  
  return(list(MCB = MCB, df = fx_df, list = fx_list))
}



## TODO: [Documentation-AUTO] Check/fix Roxygen2 Documentation (update_fx_documentation_v2)
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
#' @param regexp_noexport temp
#' @param regexp_nodocu temp
#' @param regexp_fxstart Regex to determine function starts; default should work
#' @param test_run If TRUE: Won't write any changes to file. This is defaulted to TRUE for safety. 
#' 
#' @return none
#' 
#' @export
#' 
update_fx_documentation_v2 = function(FD, guess_emptyparam = FALSE,
                                      regexp_noexport = NULL, regexp_nodocu = NULL,
                                      regexp_fxstart = "(^[[:alnum:]_.]+) *(=|(<-)) *function", 
                                      test_run = TRUE) {
  ## regexp_noexport/nodocu -> regular expressions that when applied to function names determines which functions that will not receive export or documentation respectively. This can be a vector of regular expressions. 
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
      fx_list[[j]]$docu_new = reformat_documentation_v2(fx_list[[j]]$docu_cur, params = fx_list[[j]]$params, to_export = fx_df$want_export[j])
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
  if (!test_run) { write_MatchedCodebase(MCB,which(files_changed)) }
  
  return("Done! [Inserting/formatting documentation (roxygen) templates]")
}


