##@S This file contains function related to reformatting a given extrated documentation (or generating it)

# Main Function -----------------------------------------------------------

## TODO: [Documentation-AUTO] Check/fix Roxygen2 Documentation (reformat_documentation_v2)
#' Takes old (or non-existent) roxygen documentation, and reformats it
#' Then outputs what it thinks the new format should be. Then, need to separately check whether it matches what it thinks it should be. 
#' 
#' @param cur_doc old documentation (as data frame output)
#' @param params function parameters
#' @param default_param_doc temp
#' @param to_export temp
#' 
#' @return character vector: correct roxygen documentation
#' 
#' @export
#' 
reformat_documentation = function(cur_doc, params, default_param_doc = NULL, to_export = TRUE) {
  
  reorder_rows = function(df) {
    return(df[order(df$TypeOrder, df$ParamOrder, df$SubOrder),])
  }
  
  ## General format of documentation: 
  ## 1. Title, Description, Details. These are separated by single empty lines. 
  ## 2. Param documentation. These lead by @param <paramname> <documentation> and can span multiple lines. 
  ##    NO empty lines, so these will be removed. 
  ## 3. Return documentation. This can span multiple lines. 
  ## 10. @export. This is followed by a single line, and then the function name. This does NOT have to exist, though. 
  ## The Numbering here is type numbering to be placed in TypeOrder. 
  ##
  ## General rules: 
  ## Empty lines have a single space after #'. (This won't be done in THIS function...)
  ## There should never be multiple empty lines in a row.   
  
  ## NEED TO CHECK FOR MISSING ITEMS. 
  ## IF cur_doc is NA -> need to fill out
  ## else if cur_doc is missing parts, need to fill out. 
  base_doc = data.frame(LineNo = -1, Value = c("#' <What does this function do>", "#' @return temp", "#' @export"), Type = c("Text", "@return", "@export"), ParamName = "", TypeOrder = 0, ParamOrder = 0, SubOrder = 0, stringsAsFactors=FALSE)
  
  if (is.null(cur_doc)) {
    new_doc = base_doc
  } else {
    new_doc = cur_doc
    rowstoadd = NULL
    if (!any(new_doc$Type == "Text")) { rowstoadd = c(rowstoadd, 1) }
    if (!any(new_doc$Type == "@return")) { rowstoadd = c(rowstoadd, 2) }
    if (!any(new_doc$Type == "@export")) { rowstoadd = c(rowstoadd, 3) }
    if (!is.null(rowstoadd)) { new_doc = rbind(new_doc, base_doc[rowstoadd,]) }
  }
  
  n = nrow(new_doc)  
  types = new_doc$Type
  
  ## Assign arrangement of regular sections
  ## TODO: [Refactor] This code is VERY ugly... Logic needs to be rethought... 
  matches = find_type_section(types, start_line = 1, end_marker = c("@param", "@return"), keep_empty = TRUE)
  new_doc$TypeOrder[matches] = 1
  new_doc$SubOrder[matches] = seq_along(matches)
  
  ## IF parameters exist: 
  if (any(types == "@param")) {
    matches = find_type_section(types, start_line = 1 + max(which(new_doc$TypeOrder > 0)), end_marker = c("@return"))
    new_doc$TypeOrder[matches] = 2
    new_doc$SubOrder[matches] = seq_along(matches)
    new_doc$TypeOrder[intersect(matches, which(types == "Empty"))] = 1000
  }
  
  matches = find_type_section(types, start_line = 1 + max(which(new_doc$TypeOrder > 0)), end_marker = c("@export"))
  new_doc$TypeOrder[matches] = 3
  new_doc$SubOrder[matches] = seq_along(matches)
  
  matches = find_type_section(types, start_line = 1 + max(which(new_doc$TypeOrder > 0 )), end_marker = c("endofdocu"))
  new_doc$TypeOrder[matches] = 10
  new_doc$SubOrder[matches] = seq_along(matches)
  
  ## Assign arrangement for parameters
  if (any(new_doc$TypeOrder == 2)) {
    for(j in seq_along(unique(new_doc$ParamName[new_doc$ParamName != ""]))) {
      matches = find_type_section(types, start_line = min(which((new_doc$TypeOrder == 2) & (new_doc$ParamOrder == 0))), end_marker = c("@param", "@return"), keep_one = TRUE)
      new_doc$ParamOrder[matches] = j
      new_doc$SubOrder[matches] = seq_along(matches)
    }
  }
  
  
  ## Add #'s for spaces: at positions 1.5, 2.5, 3.5, 10.5
  new_doc = rbind(new_doc, data.frame(LineNo=-1, Value = "#' ", Type ="Empty", ParamName = "", TypeOrder = c(1.5, 2.5, 3.5, 10.5), ParamOrder = 0, SubOrder = 1, stringsAsFactors=FALSE))
  
  ## Find correct parameter assignment
  new_doc = reform_params(raw_docu_df = new_doc, correct_param_order = params, default_param_doc = default_param_doc)
  
  ## Reorder rows using ordering
  new_doc = reorder_rows(new_doc)
  
  ## Check for multiple empty lines in a row. Set TypeOrder to 0
  double_empty_rows = which((new_doc$Type[-1] == "Empty") & (new_doc$Type[-1] == new_doc$Type[-length(new_doc$Type)]))
  if (length(double_empty_rows) > 0) { new_doc = new_doc[-double_empty_rows,] }
  
  
  ## Remove export code if appropriate
  if (!to_export) {
    new_doc = new_doc[new_doc$TypeOrder != 10,]
  }
  return(new_doc)
}


# Helper Functions --------------------------------------------------------

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
