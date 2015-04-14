

#' Locates chunk of previous lines that match a regex
#' 
#' Mainly used to figure out which previous lines correspond to the documentation segment (and check the regex specified by header)
#' 
#' @param text character vector -- should be code
#' @param lineno integer -- line number whose previous lines are searched
#' @param header character -- corresponds to regex to search for chunk
#' 
#' @return line numbers for the previous contiguous chunk (corresponding to indices of [text] that match [header])
#' 
#' @export
#' 
extract_prev_headers = function(text, lineno, header="^#'") {
  matchlines = grep(header, text) 
  closest_break = max(c(0,setdiff(seq_len(lineno-1), matchlines)))
  
  if (closest_break %in% c(0, lineno - 1)) {
    return(NULL)
  } else {
    return((closest_break+1):(lineno-1))
  }
}


#' Extract and process the current documentation
#' 
#' Creates a data frame storing information for the current documentation
#' 
#' @param code character vector -- should be source code
#' @param lines integer vector -- lines corresponding to 'code' that contain the documentation segment
#' @param header character -- corresponds to regex to search for chunk
#' 
#' @return A data frame containing information about current documentation (or NULL if none)
#' 
#' @export
#' 
process_cur_docu = function(code, lines, header = "^#'") {
  if (length(lines) <= 0) { return(NULL) }
  
  prev_docu = data.frame(LineNo = lines, Value = "", Type = "", ParamName = "", TypeOrder = 0, ParamOrder = 0, SubOrder = 0, stringsAsFactors=FALSE)
  
  ## Clean up text (clear extra spaces and stuff)
  text_clean = gsub(stringr::str_c(header, " +"), "", code[lines])
  tsplit = strsplit(text_clean, " ")
  
  ## Fill in Value, Type, ParamName
  for(j in seq_along(lines)) {
    t = tsplit[[j]]
    ## TODO: [Generalize] This imposes a very rigid instruction on the format of documentation. Could be relaxed. 
    t = t[t != ""]
    if (length(t) < 1 | is.na(t[1])) {
      prev_docu$Type[j] = 'Empty'
    } else if (t[1] == "@param") {
      prev_docu$Type[j] = "@param"
      prev_docu$ParamName[j] = t[2]
    } else if (length(t) > 0 && length(grep("@", x = t[1])) == 1) {
      ## If starts with @[...] other than 'param'
      prev_docu$Type[j] = t[1]
    } else {
      prev_docu$Type[j] = "Text"
    }
    prev_docu$Value[j] = code[lines[j]]
  }
  
  return(prev_docu)
}

