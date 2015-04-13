
# parameters for base function -- fill_emptyparam = TRUE, guess_emptyparam = FALSE, test_run = TRUE
## fill_emptyparam = TRUE -> will give something other than empty for parameter documentation
## guess_emptyparam  only is exmained if fill_emptyparam is TRUE; guess -> when empty, replaces parameter documentation with most common documentation for specific parameter. --- want to add marker for when something is edited. 
## idea -- make this happen based on file or entire codebase (allow it as option) 





extract_fxs = function(FD, regexp_noexport = NULL, regexp_nodocu = NULL,
                       regexp_fxstart = "(^[[:alnum:]_.]+) *(=|(<-)) *function") {
  ## regexp_noexport/nodocu -> regular expressions that when applied to function names determines which functions that will not receive export or documentation respectively
  if (FALSE) {
    FD = FilesDescription(mode = "R", dirlist = "codeProcessing/R/")
    regexp_fxstart = "(^[[:alnum:]_.]+) *(=|(<-)) *function"
  }
  
  ## Search for function headers
  MCB = search_code_matches(RE = Regex(base = regexp_fxstart), FD = FD, logged = "ROXY-TEMPLATES")
  
  ## function -- check for function matches
  extractFxInfo = function(code, matchlines) {
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
    reslist = list()
    resdf = data.frame(fxname = sapply(fxinfo, function(x) {x$fxname}),
                       doc_exist = FALSE, doc_start = -1, doc_end = -1, fx_start = -1, fx_end = -1)
    
    for (j in seq_along(fxinfo)) {
      reslist[[j]] = list(fxname = fxinfo[[j]]$fxinfo, 
                          docu = find_all_prev_documentation(code, all_matchlines[fxinfo[[j]]$matchlineIND]), 
                          code = NULL, 
                          params = fxinfo[[j]]$params)
      if (is.null(reslist[[j]]$docu)) {
        
      }
    }
    return(list(df = resdf, list = reslist))
  }
  
  fxinfo = extractFxInfo(MCB$code[[1]], MCB$matchlines[[1]])
  
  extractDocu(MCB$code[[1]], MCB$matchlines[[1]], fxinfo)
  
  cur_doc = find_all_prev_documentation(text = MCB$code[[1]], MCB$matchlines[[1]][1])
  proper_doc = reformat_documentation(cur_doc, params = fxs[[1]]$params)
  
  return(NULL)
  
  #   
  #   
  #   ## for each file --
  #   get filename
  #   get function name
  #   get line numbers
  #   extract documentation and get line numbers
  #   store documentation and code in the list
  #   
  #   temp_list = lapply(seq_along(MCB$files), function(j) {
  #     
  #     data.frame(fileName = MCB$files[j], functionName = )
  #   })
  #   
  #   for(j in seq_along(MCB$files)) {
  #     txt = MCB$code[[j]]
  #     matchlines = MCB$matchlines[[j]]
  #     param_segments = find_all_enclosed(text = txt, startlocations = cbind(matchlines, 1))
  #   }
  #   
  #   
  #   
  #   
  #   
  #   
  #   
  #   
  #   ## Search for function headers
  #   MCB 
  #   ids_changed = NULL
  #   
  #   for(j in seq_along(MCB$files)) {
  #     txt = MCB$code[[j]]
  #     matchlines = MCB$matchlines[[j]]
  #     param_segments = find_all_enclosed(text = txt, startlocations = cbind(matchlines, 1))
  #     
  #     lines_to_clear = NULL
  #     for(k in seq_along(matchlines)) {
  #       params = find_current_params(param_segments[k])  
  #       
  #       cur_doc = find_all_prev_documentation(text = txt, lineno = matchlines[k])
  #       proper_doc = reformat_documentation(cur_doc = cur_doc, params = params)
  #       
  #       
  #       ## Only change documentation if format does not match completely
  #       if (class(cur_doc) != "data.frame" || length(proper_doc$Value) != length(cur_doc$Value) || !all(proper_doc$Value == cur_doc$Value)) {
  #         if (class(cur_doc) == "data.frame" ) { lines_to_clear = c(lines_to_clear, cur_doc$LineNo) }
  #         
  #         function_name = stringr::str_extract(txt[matchlines[k]], pattern = "[[:alnum:]_.]+")
  #         todo = paste("## TODO: [Documentation-AUTO] Check/fix Roxygen2 Documentation (",function_name,")", sep = "")
  #         doc = paste(proper_doc$Value, collapse = "\n")
  #         txt[matchlines[k]] = paste(todo, "\n", doc, "\n", txt[matchlines[k]], sep = "")
  #         
  #       }
  #     }
  #     
  #     if (!is.null(lines_to_clear)) { 
  #       txt = txt[-lines_to_clear] 
  #       ids_changed = c(ids_changed, j)
  #     }
  #     MCB$code[[j]] = txt
  #   }
}






extract_prev_headers = function(text, lineno, header="^#'") {
  ## locates all lines prior to lineno that match 'header'
  matchlines = grep(header, text) 
  closest_break = max(c(0,setdiff(seq_len(lineno-1), matchlines)))
  
  if (closest_break %in% c(0, lineno - 1)) {
    return(NULL)
  } else {
    return((closest_break+1):(lineno-1))
  }
}


