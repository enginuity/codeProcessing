## Helper functions for processing documentation


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


## TODO: [General Documentation] Have to improve documentation for the data frame used here. 
#' Reorder parameters based on order in function. 
#' 
#' @param raw_docu_df Data frame describing the documentation for a specific function. 
#' @param correct_param_order Character vector: correct order for parameters
#' @param default_param_doc Data frame: columns ParamName and Doc (parameter name and documentation for parameter)
#' 
#' @return Data frame describing documentation for the specific function. 
#' 
#' @export
#' 
reform_params = function(raw_docu_df, correct_param_order, default_param_doc = NULL) {
  ## This assumes default documentation is one line. 
  ## TODO: [Idea] improve to assume that parameter documentation can be more than one line. 
  
  temp_order = raw_docu_df$ParamOrder
  
  ## Set current parameter ordering to a negative number. 
  temp_order = temp_order * -1
  correct_locations = match(correct_param_order, raw_docu_df$ParamName)
  
  params_toadd = NULL
  param_nums = NULL
  for(j in seq_along(correct_param_order)) {
    if (!is.na(correct_locations[j])) {
      temp_order[temp_order == temp_order[correct_locations[j]]] = j
    } else {
      ## Note to add parameter, since it doesn't exist. 
      params_toadd = c(params_toadd, correct_param_order[j])
      param_nums = c(param_nums, j)
    }
  }
  
  raw_docu_df$ParamOrder = temp_order
  
  if (length(params_toadd) > 0) {
    param_doc = rep("temp", times = length(params_toadd))
    param_doc[which(!is.na(match(params_toadd, default_param_doc$ParamName)))] = default_param_doc$Doc[match(params_toadd, default_param_doc$ParamName, nomatch = 0)]
    
    raw_docu_df = rbind(raw_docu_df, data.frame(LineNo=-1, Value = paste( "#' @param", params_toadd, param_doc), Type ="@param", ParamName = params_toadd, TypeOrder = 2, ParamOrder = param_nums, SubOrder = 1, stringsAsFactors=FALSE))
  }
  
  return(raw_docu_df)
}
## Test
# reform_params(raw_docu_df, correct_param_order = c("RE", "FD", "add_comment",  "blah","comment_heads"), data.frame(ParamName = c("blah", "test"), Doc = c("lalalla", "lalallala"), stringsAsFactors = FALSE))


#' Extract matching entries until certain match. SPECIFIC USE CASE
#' 
#' Given a character vector 'x' with different values, extract a section of 'x' (returning only the indices).
#' The section must start at 'start_line', and ends BEFORE the first match of 'end_marker' in 'x'. 
#' If 'keep_empty', this section is returned; otherwise, the section, minus all matches of 'Empty' is returned.
#' 
#' @param x Vector
#' @param start_line Index of match starting
#' @param end_marker Elements of x that mark end of the match
#' @param keep_empty If TRUE: then keep x's marked as "Empty", otherwise discard these
#' @param keep_one Not implemented. 
#' 
#' @return Vector identifying where the matches are
#' 
#' @export
#' 
find_type_section = function(x, start_line, end_marker, keep_empty = FALSE, keep_one = FALSE) {
  if (length(which(x %in% end_marker)) == 0) {
    end_line = length(x) 
  } else {
    end_line = min(which((x %in% end_marker) & (seq_along(x) > start_line))) - 1
  }
  
  if (start_line <= end_line) {
    res = start_line:end_line
    if (!keep_empty) { res = res[res != "Empty"] }
    return(res)
  } else {
    return(numeric(0))
  }
}

