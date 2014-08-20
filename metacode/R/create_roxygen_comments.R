##@S Functions to produce Roxygen2 comments

# Function to create Roxygen comments -------------------------------------

#' Create roxygen templates (and fix/reorder them as necessary)
#'    
#' DO NOT DO THIS WITHOUT VERSION CONTROL!
#'     
#' Assumes functions are of the format 
#' FUNCTION_NAME = function( .... ) \{
#'   content
#' \}
#' 
#' @param dir Directory to search recursively for code files
#' @param file_regex If non-NULL: restrict to filenames that match this regex
#' @param regexp_fxstart Regex to determine function starts; default should work
#' @param test_run Won't write any changes to file, unless test_run is FALSE
#' 
#' @return none
#' 
#' @export
#' 
update_fx_documentation = function(dir=DIR, file_regex = NULL, regexp_fxstart = "(^[[:alnum:]_]+) += +function", test_run = FALSE) { 
#|***********************
#|----##Rename create_roxy_temp -> update_fx_doc --Tue Aug 19 21:36:55 2014--
  matchesL = search_code_matches(regexp = regexp_fxstart, regex_exact = FALSE, 
                                 dir = dir, mode = "R", file_regex = file_regex, logged = "ROXY-TEMPLATES")
  
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
  
  if (!test_run) { write_matchlist(matchesL) }
  return("Done! [Inserting/formatting documentation (roxygen) templates]")
}


#' Extract roxygen2 parameter documentation
#' 
#' @param dir Directory to search recursively for code files
#' @param file_regex If non-NULL: restrict to filenames that match this regex
#' @param regexp_fxstart Regex to determine function starts; default should work
#' 
#' @return Data frame of all parameter documentation
#' 
#' @export
#' 
extract_param_docu = function(dir,file_regex = NULL, regexp_fxstart = "(^[[:alnum:]_]+) += +function") {
#|******************
#|----##Rename create_roxy_temp -> update_fx_doc --Tue Aug 19 21:38:51 2014--
  matchesL = search_code_matches(regexp = regexp_fxstart, regex_exact = FALSE, 
                                 dir = dir, mode = "R", file_regex = file_regex, logged = "ROXY-param-matching")
  
  agg_params = data.frame(filename="", funcname="", paramname = "", paramval = "", lineno = 0, stringsAsFactors = FALSE)
  
  for(j in seq_along(matchesL$files)) {
    txt = matchesL$code[[j]]
    matchlines = matchesL$matchlines[[j]]
    param_segments = find_all_enclosed(text = txt, startlocations = cbind(matchlines, 1))
    
    for(k in seq_along(matchlines)) {
      params = find_current_params(param_segments[k])  
      cur_doc = find_all_prev_documentation(text = txt, lineno = matchlines[k])
      if (is.data.frame(cur_doc)) {
        fn_name = str_extract(txt[matchlines[k]], pattern = "[[:alnum:]_]+")
        agg_params = rbind(agg_params, 
                           data.frame(filename = matchesL$files[j], funcname = fn_name, 
                                      paramname = params, paramval = cur_doc$Value[cur_doc$Mode == "@param"],
                                      lineno = cur_doc$LineNo[cur_doc$Mode == "@param"], stringsAsFactors = FALSE))
      }
    }
  }
  return(agg_params[-1,])
}


## TODO: [Obselete] This function is kind of silly/simple.. is it necessary? Either upgrade / delete
#' Examine subset of roxygen parameters
#' 
#' @param locate_df Complete parameter data frame
#' @param param_name Parameter name to subset
#' 
#' @return Subset of parameter data frame, only containing certain parameter name
#' 
#' @export
#' 
roxyparam_subset = function(locate_df, param_name) {
  inds = which(locate_df$paramname == param_name)
  return(cbind(locate_df[inds,"funcname"], gsub(paste("#' @param ",param_name, " ", sep = ""), "", locate_df[inds, "paramval"])))
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
roxyparam_overwrite = function(locate_df, param_name, replace_text = NULL, replace_all = FALSE) {
  
  inds = which(locate_df$paramname == param_name)
  if (is.null(replace_text)) { 
    replace_text = Mode_nontemp(locate_df$paramval[inds])
    if (replace_text == "temp") { return("[Parameter Documentation replacement NOT done: No temporary docs!]")}
  } else { replace_text = paste("#' @param", param_name, replace_text) }
  
  if (length(inds) == 0) { return("[No matching parameter names]") }
  
  ## The following code isn't particularly efficient (many read/writes of same file), but implemented much easier!
  for (i in inds) {
    if (replace_all || strsplit(locate_df$paramval[i], " ")[[1]][4] %in% c("temp", "text")) {
      code = readLines(locate_df$filename[i])
      code[locate_df$lineno[i]] = replace_text
      writeLines(text = code, con = locate_df$filename[i])
    }
  }

  return("[Parameter Documentation replacement done!]")
}

