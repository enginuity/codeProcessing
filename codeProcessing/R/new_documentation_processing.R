
extract_fxs = function(FD, regexp_fxstart = "(^[[:alnum:]_.]+) *(=|(<-)) *function") {
  
  
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
                          docu = NULL, 
                          code = NULL, ## TODO: [Implement] this someday. 
                          params = fxinfo[[j]]$params)
      doclocs = extract_prev_headers(text = code, lineno = fx_start)
      if (length(doclocs) > 0) {
        resdf$doc_exist[j] = TRUE        
        resdf$doc_start[j] = min(doclocs)
        resdf$doc_end[j] = max(doclocs)
        
        reslist$docu = process_cur_docu(code, lines = doclocs)
      }
      resdf$fx_start[j] = fx_start
    }
    return(list(df = resdf, list = reslist))
  }
  
  ### Function starts here! ####################  
  
  if (FALSE) {
    FD = FilesDescription(mode = "R", dirlist = "codeProcessing/R/")
    regexp_fxstart = "(^[[:alnum:]_.]+) *(=|(<-)) *function"
  }
  
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
  
  return(list(df = fx_df, list = fx_list))
}


## Code to use in processing ... 

# parameters for base function -- fill_emptyparam = TRUE, guess_emptyparam = FALSE, test_run = TRUE
## fill_emptyparam = TRUE -> will give something other than empty for parameter documentation
## guess_emptyparam  only is exmained if fill_emptyparam is TRUE; guess -> when empty, replaces parameter documentation with most common documentation for specific parameter. --- want to add marker for when something is edited. 
## idea -- make this happen based on file or entire codebase (allow it as option) 


## regexp_noexport/nodocu -> regular expressions that when applied to function names determines which functions that will not receive export or documentation respectively. This can be a vector of regular expressions. 

# ## Check for undocumenting / unexporting
# for (reg in regexp_noexport) { fx_df$want_export[grep(reg, x = fx_df$fx_name)] = FALSE }
# for (reg in regexp_nodocu) { fx_df$want_docu[grep(reg, x = fx_df$fx_name)] = FALSE }
# 
# 
# #proper_doc = reformat_documentation(cur_doc, params = fxs[[1]]$params)
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





