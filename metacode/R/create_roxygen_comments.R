##@S Functions to produce Roxygen2 comments

# Function to create Roxygen comments -------------------------------------

#' Create roxygen templates (and fix/reorder them as necessary)
#' DO NOT DO THIS WITHOUT VERSION CONTROL!
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
  ## Assumes functions are of the format 
  ## FUNCTION_NAME = function( .... ) {
  ##   content
  ## }
  
  matchesL = search_code_matches(regexp = regexp_fxstart, regex_exact = FALSE, dir = dir, mode = "R", file_regex = file_regex, logged = "ROXY-TEMPLATES")
  
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


#' Compute Mode of parameter documentation (ignoring certain types)
#' 
#' Ignores all values that are equal to 'test' or 'temp', and computes the mode of the remaining text values
#' 
#' @param text Character vector
#' 
#' @return Mode of 'text'
#' 
#' @export
#' 
Mode_nontemp = function(text) {
  
  ## TODO: Move function from this file? if necessary?
  t = text[text != "test" & text != "temp"]
  if (length(t) == 0) { return("temp") }
  
  tab = table(t)
  return(names(tab)[which(tab == max(tab))[1]])
}


## TODO: [Documentation-AUTO] Check/fix Roxygen2 Documentation (roxyparam_locate)
#' ********** WARNING -- INSERTED CODE **************
#' <<BasicInfo>> 
#' 
#' @param dir Directory to search recursively for code files
#' @param file_regex text
#' @param regexp_fxstart text
#' 
#' @return text
#' 
#' @export
#' 
roxyparam_locate = function(dir,file_regex = NULL, regexp_fxstart = "(^[[:alnum:]_]+) += +function") {
  matchesL = search_code_matches(regexp = regexp_fxstart, dir = dir, mode = "R", file_regex = file_regex, logged = "ROXY-param-matching")
#|----##This function has new parameter (regex_exact) added --Wed Aug 13 15:14:32 2014--
  
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


#' Takes old (or non-existent) roxygen documentation, and reformats it
#' 
#' @param cur_doc old documentation (as data frame output)
#' @param params function parameters
#' @param function_name function name
#' 
#' @return character vector: correct roxygen documentation
#' 
#' @export
#' 
reformat_documentation = function(cur_doc, params, function_name) {
  ## Wanted roxy output: 
  ## #` some title/description
  ## #` ...
  ## #` @param ... 
  ## #` @result ...
  ## #` @export
  
  head_doc = "#' <<BasicInfo>> "
  param_doc = paste("#' @param", params, "temp")
  return_doc = "#' @return temp"
  
  if (class(cur_doc) == "data.frame") {
    if (any(cur_doc$Mode == "Text")) { head_doc = cur_doc$Value[cur_doc$Mode == "Text"] }
    if (any(cur_doc$Mode == "@return")) { return_doc = cur_doc$Value[cur_doc$Mode == "@return"] }
    if (any(cur_doc$Mode == "@param")) {
      for(j in which(cur_doc$Mode == "@param")) {
        param_doc[which(params == cur_doc$Mode2[j])] = cur_doc$Value[j]
      }
    }
  }
  
  return(c(paste("## TODO: [Documentation-AUTO] Check/fix Roxygen2 Documentation (",function_name,")", sep = ""),
           head_doc, "#' ", param_doc, "#' ", return_doc, "#' ", "#' @export", "#' "))
}


#' Processes function parameters and attempts to extract out all parameter names
#' 
#' @param text Should be all the code 'here' in [function() { ..here.. }]
#' 
#' @return character vecctor of all parameter names
#' 
#' @export
#' 
find_current_params = function(text) {
  ## Get rid of stuff within parenthesis
  text = substr(text, start = 2, stop = nchar(text) - 1)
  text = gsub("\".*?\"","", text)
  text = gsub("[(].*?[)]", "", text)
  text = strsplit(text, ",")[[1]]
  text = gsub("=.*", "", text)
  text = gsub(" ", "", text)
  return(text)
}



#' Locates all previous lines that start with specific header type
#' 
#' @param text Source code
#' @param lineno Line number
#' @param header Documentation header
#' 
#' @return Vector of line numbers (can be NA)
#' 
#' @export
#' 
find_all_prev_headers = function(text, lineno, header="^#'") {
  z = grep(header, text) 
  breaks = c(0,setdiff(seq_along(text), z))
  closest_break = max(breaks[breaks < lineno])
  
  if (closest_break < 0) {
    return(NA)
  } else if (closest_break == (lineno - 1)) {
    return(NA)
  } else {
    return((closest_break+1):(lineno-1))
  }
}



#' Extracts documentation prior to a current line (function start line)
#' 
#' @param text Source code
#' @param lineno Line number
#' @param header Documentation header
#' 
#' @return Character vector of documentation lines
#' 
#' @export
#' 
find_all_prev_documentation = function(text, lineno, header = "^#'") {
  prev_lines = find_all_prev_headers(text, lineno, header)
  if (is.na(prev_lines[1])) {
    return(NA)
  }
  prev_docu = data.frame(LineNo=prev_lines, Mode="", Mode2 ="", Value = "", stringsAsFactors=FALSE)
  
  text_clean = gsub(header, "", text[prev_lines])
  ## Mode is either 'Empty', 'Text', or "@something"
  tsplit = strsplit(text_clean, " ")
  
  for(j in seq_along(prev_lines)) {
    t = tsplit[[j]]
    if (length(t) <= 1 | is.na(t[1])) {
      prev_docu$Mode[j] = 'Empty'
    } else {
      t = t[t != ""]
      if (length(t) == 0) {
        prev_docu$Mode[j] = "Text"
      } else {
        if (t[1] == "@param") {
          prev_docu$Mode[j] = "@param"
          prev_docu$Mode2[j] = t[2]
        } else if (t[1] == "@return") {
          prev_docu$Mode[j] = "@return"
        } else if (t[1] == "@export") {
          prev_docu$Mode[j] = "@export"
        } else {
          prev_docu$Mode[j] = "Text"
        }
      }
    }
    prev_docu$Value[j] = text[prev_lines[j]]
  }
  return(prev_docu)
}
