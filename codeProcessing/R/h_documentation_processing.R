## Helper functions for processing documentation


#' [Helper] Extract metadata for each function
#' 
#' In addition, it checks for the funciton in two ways: first, based off regex matching, second, using the parse function. 
#' 
#' @param code character vector : Source code
#' @param matchlines numeric vector : Lines where functions start
#' 
#' @return A list of function information extracted for specific code file
#' 
#' @export
#' 
zhdp_extractFxInfo = function(code, matchlines) {
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
      if (length(res_list[[i]]$matchlineIND) > 1) { stop(paste("error--Function *",res_list[[i]]$fxname,"* has been defined more than once")) }
    }
  }
  res_list = res_list[is_function]
  return(res_list)
}


#' [Helper] Extracts existing (roxygen2-style) documentation
#' 
#' This function extends the function information extracted previously in fxinfo, and returns a list of an updated data.frame version of fxinfo, and a list of function information. 
#' 
#' @param code character vector -- source code
#' @param all_matchlines numeric vector -- Lines where functions potentially start
#' @param fxinfo Data frame containing information on functions in this file
#' 
#' @return List(df, list) : df is updated version of fxinfo, list contains information about each function
#' 
#' @export
#' 
zhdp_extractDocu = function(code, all_matchlines, fxinfo) {
  ## For each codefile, extract all matches -- return a list of a data frame and an updated function information list. 
  reslist = list()
  resdf = data.frame(fx_name = sapply(fxinfo, function(x) {x$fxname}),
                     doc_exist = FALSE, doc_start = NA, doc_end = NA, fx_start = NA, fx_end = NA, status = NA)
  ## status -- should eventually store what happened to this document: add docu, remove docu, no change, update docu
  
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


zhdp_updateDocu = function(fxdf, fxlist, MCB) {
  ## function that updates the documentation if necessary (comparing the existing docu to the 'standard' one.)  
  
  files_changed = rep(FALSE, max(fxdf$fileID))
  
  for (ID in seq_along(fxlist)) {
    fileID = fxdf$fileID[ID]
    todo = paste("## TODO: [Documentation-AUTO] Check/fix Roxygen2 Documentation (",fxlist[[ID]]$fxname,")", sep = "")
    
    ID_fxs = setdiff(which(fxdf$fileID == fileID), ID) ## Find all functions listed in this same file, ignoring current function
    line_before_fx = df$fx_start[ID] - 1
    
    ## Update documentation, and update fx_df. 
    ## Depending on the case -- fx_df needs to have doc_start, doc_end, fx_start, status updated. 
    ## Also, depending on the case, MCB has the code updated (replacing docu_cur with docu_new in fxlist)
    diff_nrow_0 = function(x,y) { return(ifelse(is.null(x), 0, nrow(x)) - ifelse(is.null(y), 0, nrow(y))) }
    lines_added = diff_nrow_0(fxlist[[ID]]$docu_new, fxlist[[ID]]$docu_cur)
    
    if (is.null(fxlist[[ID]]$docu_cur)) { # If no current documentation
      if (is.null(fxlist[[ID]]$docu_new)) { # Want no documentation -> do nothing
        fxdf$status[ID] = "no_change:no_docu"
        
      } else { # Add in new documentation
        fxdf$status[ID] = "add_docu"; files_changed[fileID] = TRUE
        fxdf$doc_start[ID] = fxdf$fx_start[ID] + 1; fxdf$doc_end[ID] = fxdf$fx_start[ID] - 1
        lines_added = lines_added + 1
        MCB$code[[fileID]] = insert_codelines(MCB$code[[fileID]], c(todo, fxlist[[ID]]$docu_new$Value), fxdf$fx_start[ID])
        
      }
    } else { # Else, current documentation exists
      if (is.null(fxlist[[ID]]$docu_new)) { # Want to remove documentation
        fxdf$status[ID] = "remove_docu"; files_changed[fileID] = TRUE
        fxdf$doc_end[ID] = NA; fxdf$doc_start[ID] = NA
        MCB$code[[fileID]] = replace_codelines(MCB$code[[fileID]], NULL, fxdf$doc_start[ID], fxdf$doc_end[ID])
        
      } else if (any(fxlist[[ID]]$docu_new$Value != fxlist[[ID]]$docu_cur$Value)) { # Documentation doesn't match
        fxdf$status[ID] = "update_docu"; files_changed[fileID] = TRUE
        fxdf$doc_start[ID] = fxdf$doc_start[ID] + 1
        lines_added = lines_added + 1
        MCB$code[[fileID]] = replace_codelines(MCB$code[[fileID]], c(todo, fxlist[[ID]]$docu_new$Value), fxdf$doc_start[ID], fxdf$doc_end[ID])
        
      } else {
        fxdf$status[ID] = "no_change:docu_exists"
      }
    }
    
    if (lines_added != 0) {
      ## Adjust locations for this specific function
      fxdf$doc_end[ID] = fxdf$doc_end[ID] + lines_added; fxdf$fx_start[ID] = fxdf$fx_start[ID] + lines_added
      
      ## Adjust locations for remaining functions in the file
      for(rs in ID_fxs) {
        for (cs in which(colnames(fxdf) %in% c("doc_start", "doc_end", "fx_start"))) {
          if (!is.na(fxdf[rs,cs]) && fxdf[rs,cs] >= line_before_fx) { fxdf[rs,cs] = fxdf[rs,cs] + lines_added }
        }
      }
    }  
  } 
  
  return(list(fx_df = fxdf, fx_list = fxlist, files_changed = files_changed, MCB = MCB))
}

#' Compute mode of text ignoring certain values
#' 
#' Ignores all values that are equal to "temp", and computes the mode of the remaining text values
#' (or ignores all values in 'ignore_type')
#' 
#' @param text Character vector whose mode is desired
#' @param ignore_type Character vector of elements to ignore. 
#' 
#' @return Mode of 'text'
#' 
#' @export
#' 
Mode_nontemp = function(text, ignore_type = "temp") {
  
  t = text[!(text %in% ignore_type)]
  if (length(t) == 0) { return(ignore_type[1]) }
  
  tab = table(t)
  return(names(tab)[which(tab == max(tab))[1]])
}


