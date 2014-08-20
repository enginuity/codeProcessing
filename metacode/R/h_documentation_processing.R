## Hepler functions for processing documentation




#' Compute Mode of parameter documentation (ignoring certain types)
#'     
#' Ignores all values that are equal to 'temp', and computes the mode of the remaining text values
#' 
#' @param text Character vector
#' 
#' @return Mode of 'text'
#' 
#' @export
#' 
Mode_nontemp = function(text) {
  
  t = text[text != "temp"]
  if (length(t) == 0) { return("temp") }
  
  tab = table(t)
  return(names(tab)[which(tab == max(tab))[1]])
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

