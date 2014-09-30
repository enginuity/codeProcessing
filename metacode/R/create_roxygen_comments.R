##@S Functions to produce Roxygen2 comments

# Function to create Roxygen comments -------------------------------------

## TODO: [Documentation-AUTO] Check/fix Roxygen2 Documentation (update_fx_documentation)
#' Create roxygen templates (and fix/reorder them as necessary)
#'    
#' DO NOT DO THIS WITHOUT VERSION CONTROL!
#'     
#' Assumes functions are of the format 
#' FUNCTION_NAME = function( .... ) \{
#'   content
#' \}
#' 
#' @param FD Object of class FilesDescription; See documentation to see how to describe a collection of files  
#' @param fill_emptyparam temp
#' @param regexp_fxstart Regex to determine function starts; default should work
#' @param test_run Won't write any changes to file, unless test_run is FALSE
#' 
#' @return none
#' 
#' @export
#' 
update_fx_documentation = function(FD, fill_emptyparam = TRUE,
                                   regexp_fxstart = "(^[[:alnum:]_]+) += +function", test_run = FALSE) { 
  matchesL = search_code_matches(RE = Regex(base = regexp_fxstart), FD = FD, logged = "ROXY-TEMPLATES")
  
  for(j in seq_along(matchesL$files)) {
    txt = matchesL$code[[j]]
    matchlines = matchesL$matchlines[[j]]
    param_segments = find_all_enclosed(text = txt, startlocations = cbind(matchlines, 1))
    
    lines_to_clear = NULL
    for(k in seq_along(matchlines)) {
      params = find_current_params(param_segments[k])  
      cur_doc = find_all_prev_documentation(text = txt, lineno = matchlines[k])
      proper_doc = reformat_documentation(cur_doc, params, str_extract(txt[matchlines[k]], pattern = "[[:alnum:]_]+"))
      
      ## Only change documentation if format does not match completely
      if (class(cur_doc) != "data.frame" || length(proper_doc[-1]) != length(cur_doc$Value) || !all(proper_doc[-1] == cur_doc$Value)) {
        if (class(cur_doc) == "data.frame" ) { lines_to_clear = c(lines_to_clear, cur_doc$LineNo) }
        
        doc = paste(proper_doc, collapse = "\n")
        txt[matchlines[k]] = paste(doc, "\n", txt[matchlines[k]], sep = "")
      }
    }
    
    if (!is.null(lines_to_clear)) { txt = txt[-lines_to_clear] }
    matchesL$code[[j]] = txt
  }
  
  if (!test_run) { write_MatchedCodebase(matchesL) }
#|                 *********************
#|----##Rename write_matchlist --Tue Sep 30 14:38:43 2014--
  if (fill_emptyparam) {
    paramdf = extract_param_docu(FD = FD)
    for(s in unique(paramdf$paramname[which(sapply(strsplit(paramdf$paramval, " "), function(x) {x[4]}) == "temp")])) {
      update_param_docu(paramdf, param_name = s)
    }
  }
  return("Done! [Inserting/formatting documentation (roxygen) templates]")
}


#' Extract roxygen2 parameter documentation
#' 
#' @param FD Object of class FilesDescription; See documentation to see how to describe a collection of files  
#' @param regexp_fxstart Regex to determine function starts; default should work
#' 
#' @return Data frame of all parameter documentation
#' 
#' @export
#' 
extract_param_docu = function(FD, regexp_fxstart = "(^[[:alnum:]_]+) += +function") {
  
  matchesL = search_code_matches(RE = Regex(base = regexp_fxstart), FD, logged = "ROXY-param-matching")
  
  param_list = list()
  i = 1
  
  for(j in seq_along(matchesL$files)) {
    txt = matchesL$code[[j]]
    matchlines = matchesL$matchlines[[j]]
    param_segments = find_all_enclosed(text = txt, startlocations = cbind(matchlines, 1))
    
    for(k in seq_along(matchlines)) {
      params = find_current_params(param_segments[k])  
      cur_doc = find_all_prev_documentation(text = txt, lineno = matchlines[k])
      if (is.data.frame(cur_doc)) {
        fn_name = str_extract(txt[matchlines[k]], pattern = "[[:alnum:]_]+")
        
        param_list[[i]] = data.frame(filename = matchesL$files[j], funcname = fn_name, 
                                     paramname = params, paramval = cur_doc$Value[cur_doc$Mode == "@param"],
                                     lineno = cur_doc$LineNo[cur_doc$Mode == "@param"], stringsAsFactors = FALSE)
        i = i + 1
      }
    }
  }
  agg_params = do.call(rbind, param_list)
  return(agg_params)
}


#' Overwrite parameter documentation
#' 
#' @param locate_df Complete parameter data frame
#' @param param_name Parameter name to subset
#' @param replace_text If NULL: replaces with most frequent non(text/temp). Otherwise, replaces with replace_text.
#' @param replace_all If TRUE: replaces also lines that already have non-trivial parameter documentation
#' 
#' @return none
#' 
#' @export
#' 
update_param_docu = function(locate_df, param_name, replace_text = NULL, replace_all = FALSE) {
  
  inds = which(locate_df$paramname == param_name)
  if (is.null(replace_text)) { 
    replace_text = Mode_nontemp(locate_df$paramval[inds])
    if (replace_text == "temp") { return("[Parameter Documentation replacement NOT done: No temporary docs!]")}
  } else { replace_text = paste("#' @param", param_name, replace_text) }
  
  if (length(inds) == 0) { return("[No matching parameter names]") }
  
  ## The following code isn't particularly efficient (many read/writes of same file), but implemented much easier!
  for (i in inds) {
    if (replace_all || strsplit(locate_df$paramval[i], " ")[[1]][4] %in% c("temp")) {
      code = readLines(locate_df$filename[i])
      code[locate_df$lineno[i]] = replace_text
      writeLines(text = code, con = locate_df$filename[i])
    }
  }
  
  return("[Parameter Documentation replacement done!]")
}

