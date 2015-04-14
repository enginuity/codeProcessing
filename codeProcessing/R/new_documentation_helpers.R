
## TODO: [Documentation-AUTO] Check/fix Roxygen2 Documentation (extract_prev_headers)
#' <What does this function do>
#' 
#' @param text temp
#' @param lineno temp
#' @param header temp
#' 
#' @return temp
#' 
#' @export
#' 
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


## TODO: [Documentation-AUTO] Check/fix Roxygen2 Documentation (process_cur_docu)
#' <What does this function do>
#' 
#' @param code temp
#' @param lines temp
#' @param header temp
#' 
#' @return temp
#' 
#' @export
#' 
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

