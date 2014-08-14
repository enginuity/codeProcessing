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
create_roxy_templates = function(dir=DIR, file_regex = NULL, regexp_fxstart = "(^[[:alnum:]_]+) += +function", test_run = FALSE) { 
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


## TODO: [Documentation-AUTO] Check/fix Roxygen2 Documentation (roxyparam_locate)
#' ********** WARNING -- INSERTED CODE **************
#' <<BasicInfo>> 
#' 
#' @param dir Directory to search recursively for code files
#' @param file_regex If non-NULL: restrict to filenames that match this regex
#' @param regexp_fxstart text
#' 
#' @return text
#' 
#' @export
#' 
roxyparam_locate = function(dir,file_regex = NULL, regexp_fxstart = "(^[[:alnum:]_]+) += +function") {
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


## TODO: [Documentation-AUTO] Check/fix Roxygen2 Documentation (roxyparam_subset)
#' ********** WARNING -- INSERTED CODE **************
#' <<BasicInfo>> 
#' 
#' @param locate_df text
#' @param param_name text
#' 
#' @return text
#' 
#' @export
#' 
roxyparam_subset = function(locate_df, param_name) {
  inds = which(locate_df$paramname == param_name)
  return(cbind(locate_df[inds,"funcname"], gsub(paste("#' @param ",param_name, " ", sep = ""), "", locate_df[inds, "paramval"])))
}



## TODO: [Documentation-AUTO] Check/fix Roxygen2 Documentation (roxyparam_overwrite)
#' ********** WARNING -- INSERTED CODE **************
#' <<BasicInfo>> 
#' 
#' @param locate_df text
#' @param param_name text
#' @param replace_text If NULL: replaces with most frequent non(text/temp). Otherwise, replaces with replace_text.
#' @param replace_all temp
#' 
#' @return text
#' 
#' @export
#' 
roxyparam_overwrite = function(locate_df, param_name, replace_text = NULL, replace_all = FALSE) {
  ## replace_all = FALSE: only replaces temp/text. otherwise, replaces all of them... 
  
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
  ## TODO: [Cleanup] Remove the following code as long as this function works. 
  #   
  #   files = unique(locate_df$filename)
  #   for(f in files) {
  #     lines = locate_df$lineno[intersect(inds, which(f == locate_df$filename))]
  #     code = readLines(f)
  #     for (l in lines) { if(replace_all || locate_df$paramval[] %in% c("temp", "test")) { code[l] = new_param_doc } }
  #     writeLines(text = code, con = f)
  #   }
  return("[Parameter Documentation replacement done!]")
}

