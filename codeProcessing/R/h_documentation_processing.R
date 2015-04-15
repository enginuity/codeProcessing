## Helper functions for processing documentation


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
    }
  }
  return(res_list)
}


zhdp_extractDocu = function(code, all_matchlines, fxinfo) {
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


