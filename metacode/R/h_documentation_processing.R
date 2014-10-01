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
  ## TODO: [Remove function] This should be rendered obselete somehow. 
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
find_all_prev_documentation_v2 = function(text, lineno, header = "^#'") {
  ## Testing vals
  if (FALSE) {
    text = readLines("metacode/R/search_code.R")
    lineno = grep("(^[[:alnum:]_]+) += +function", text)[1]
    header = "^#'"
  }
  
  prev_lines = find_all_prev_headers(text, lineno, header)
  if (is.na(prev_lines[1])) {
    return(NA)
  }
  prev_docu = data.frame(LineNo=prev_lines, Value = "", Type ="", ParamName = "", TypeOrder = 0, ParamOrder = 0, SubOrder = 0, stringsAsFactors=FALSE)
  
  text_clean = gsub(str_c(header, " +"), "", text[prev_lines])
  ## Mode is either 'Empty', 'Text', or "@something"
  tsplit = strsplit(text_clean, " ")
  
  ## Figure out what type each line is
  for(j in seq_along(prev_lines)) {
    t = tsplit[[j]]
    t = t[t != ""]
    if (length(t) < 1 | is.na(t[1])) {
      prev_docu$Type[j] = 'Empty'
    } else {
      if (t[1] == "@param") {
        prev_docu$Type[j] = "@param"
        prev_docu$ParamName[j] = t[2]
      } else if (t[1] == "@return") {
        prev_docu$Type[j] = "@return"
      } else if (t[1] == "@export") {
        prev_docu$Type[j] = "@export"
      } else {
        prev_docu$Type[j] = "Text"
      }
    }
    prev_docu$Value[j] = text[prev_lines[j]]
  }
  
  ## Process the types... 
  raw_docu_df = prev_docu
  prev_docu = process_documentation(prev_docu)
  return(prev_docu)
}



process_documentation = function(raw_docu_df) {
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
  
  
  n = nrow(raw_docu_df)
  types = raw_docu_df$Type
  
  ## TODO: [Refactor] This code is VERY ugly... Logic needs to be rethought... 
  matches = find_type_section(types, start_line = 1, end_marker = c("@param", "@return"), keep_empty = TRUE)
  raw_docu_df$TypeOrder[matches] = 1
  raw_docu_df$SubOrder[matches] = seq_along(matches)
  
  matches = find_type_section(types, start_line = 1 + max(which(raw_docu_df$TypeOrder > 0)), end_marker = c("@return"))
  raw_docu_df$TypeOrder[matches] = 2
  raw_docu_df$SubOrder[matches] = seq_along(matches)
  
  matches = find_type_section(types, start_line = 1 + max(which(raw_docu_df$TypeOrder > 0)), end_marker = c("@export"))
  raw_docu_df$TypeOrder[matches] = 3
  raw_docu_df$SubOrder[matches] = seq_along(matches)
  
  matches = find_type_section(types, start_line = 1 + max(which(raw_docu_df$TypeOrder > 0 )), end_marker = c("endofdocu"))
  raw_docu_df$TypeOrder[matches] = 10
  raw_docu_df$SubOrder[matches] = seq_along(matches)
  
  
  if (any(raw_docu_df$TypeOrder == 2)) {
    for(j in seq_along(unique(raw_docu_df$ParamName[raw_docu_df$ParamName != ""]))) {
      matches = find_type_section(types, start_line = min(which((raw_docu_df$TypeOrder == 2) & (raw_docu_df$ParamOrder == 0))), end_marker = c("@param", "@return"), keep_one = TRUE)
      raw_docu_df$ParamOrder[matches] = j
      raw_docu_df$SubOrder[matches] = seq_along(matches)
    }
  }
  
  ## Add #'s for spaces: at positions 1.5, 2.5, 3.5, 10.5
  raw_docu_df = rbind(raw_docu_df, data.frame(LineNo=-1, Value = "#' ", Type ="Empty", ParamName = "", TypeOrder = c(1.5, 2.5, 3.5, 10.5), ParamOrder = 0, SubOrder = 1, stringsAsFactors=FALSE))
  
  ## TODO: [Fix] Run reform_params
  
  ## Reorder rows
  raw_docu_df = raw_docu_df[order(raw_docu_df$TypeOrder, raw_docu_df$ParamOrder, raw_docu_df$SubOrder),]
  
  ## Check for multiple empty lines in a row. Set TypeOrder to 0
  double_empty_rows = which((raw_docu_df$Type[-1] == "Empty") & (raw_docu_df$Type[-1] == raw_docu_df$Type[-length(raw_docu_df$Type)]))
  if (length(double_empty_rows) > 0) { raw_docu_df = raw_docu_df[-double_empty_rows,] }
  
  return(raw_docu_df)
}



reform_params = function(raw_docu_df, correct_param_order, default_param_doc = NULL) {
  ## Given a vector of the correct parameter order, reorder raw_docu_df as necessary. Add missing parameters with 'temp' documentation as necessary. 
  ## default_param_doc should be a data.frame: $ParamName & $Doc
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
    print(param_doc)
    
    raw_docu_df = rbind(raw_docu_df, data.frame(LineNo=-1, Value = paste( "#' @param", param_doc), Type ="@param", ParamName = params_toadd, TypeOrder = 2, ParamOrder = param_nums, SubOrder = 1, stringsAsFactors=FALSE))
  }
  
  return(raw_docu_df)
}
## Test
# reform_params(raw_docu_df, correct_param_order = c("RE", "FD", "add_comment",  "blah","comment_heads"), data.frame(ParamName = c("blah", "test"), Doc = c("lalalla", "lalallala"), stringsAsFactors = FALSE))





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

