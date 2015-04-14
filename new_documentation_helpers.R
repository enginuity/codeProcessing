
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



process_cur_docu = function(code, lines, header = "^#'") {
  ## Processes the current documentation on lines [lines] of code in [code]
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

