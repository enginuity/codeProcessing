## Helper functions for processing documentation


# Main Extraction Function (calls all these as sub-functions) -------------


#' Extract all documentation from source files
#' 
#' Only applies to R code (since searches for #'s and functions in R formatting)
#' 
#' @param FD [\code{\link{FilesDescription}}] :: A collection of source code files
#' @param regexp_fxstart [char] :: Regex to determine function starts; default should work
#' 
#' @return -- UPDATE THIS
#' 
#' @export
#' 
extract_all_docu = function(FD, regexp_fxstart = "(^[[:alnum:]_.]+) *(=|(<-)) *function") {
  ## Generate a 
  MCB = search_code_matches(RE = Regex(base = regexp_fxstart), FD = FD, logged = "ROXY-TEMPLATES")
  
  fx_list = list()
  fx_df = NULL
  
  for (j in seq_along(MCB$code)) {
    fx_info = zhdp_extractFxInfo(MCB$code[[j]], MCB$matchlines[[j]])
    temp = zhdp_extractDocu(MCB$code[[j]], MCB$matchlines[[j]], fx_info)
    fx_list = c(fx_list, temp$fx_list)
    fx_df = rbind(fx_df, cbind(fileID = j, filename = MCB$files[j], temp$fx_df))
  }
  ## Add ID's
  fx_df = cbind(ID = seq_len(nrow(fx_df)), fx_df, want_docu = TRUE, want_export = TRUE)
  
  return(list(MCB = MCB, fx_df = fx_df, fx_list = fx_list))
}



# Function to extract metadata from functions -----------------------------


#' [Helper] Extract metadata for each function
#' 
#' In addition, it checks for the funciton in two ways: first, based off regex matching, second, using the parse function. 
#' 
#' @param code [vector-char] :: Text (code)
#' @param matchlines [vector-int] :: All lines in the code which correspond to defining named functions
#' 
#' @return [list] :: Information about each function: 
#' \itemize{
#'   \item fxname -- [char] :: Function name
#'   \item params -- [vector-char] :: Parameter names in order of appearance
#'   \item matchlineIND -- [int] :: Index of input \code{matchlines} that corresponds to this function
#' }
#' 
#' @export
#' 
zhdp_extractFxInfo = function(code, matchlines) {
  ## Check the function names extracted, and returns a list of matching functions and information for each function. 
  fn_name_regex = stringr::str_extract(code[matchlines], pattern = "[[:alnum:]_.]+") ## is character vector
  parsed_code = parse(text = code, keep.source = TRUE)
  
  fn_name_parse = lapply(parsed_code, function(x) {as.character(x[[2]])}) ## is list
  is_function = sapply(parsed_code, function(x) {x[[3]][[1]] == 'function'})
  res_list = list()
  for(i in which(is_function)) {
    if (fn_name_parse[[i]] %in% fn_name_regex) {
      fn_params = names(parsed_code[[i]][[3]][[2]])
      res_list[[i]] = list(fxname = fn_name_parse[[i]], params = fn_params, 
                           matchlineIND = which(fn_name_parse[[i]] == fn_name_regex))
      if (length(res_list[[i]]$matchlineIND) > 1) { stop(paste("error--Function *",res_list[[i]]$fxname,"* has been defined more than once")) }
    }
  }
  res_list = res_list[is_function]
  return(res_list)
}


# Function (and two helpers) to extract existing documentation ------------


#' [Helper] Locates chunk of previous lines that match a regex
#' 
#' Mainly used to figure out which previous lines correspond to the documentation chunk (and check the regex specified by header). The input line \code{lineno} shouldn't match \code{header} in the intended usage case, but this is not checked. 
#' 
#' @param text [vector-char] :: Text (code) to search
#' @param lineno [int] :: Line number of which previous lines are checked against the header regular expression
#' @param header [char] :: A regex that is the header (first few characters) of all lines in the 'chunk'
#' 
#' @return [vector-int] :: Line numbers for the previous contiguous chunk (corresponding to indices of \code{text} that match \code{header})
#' 
#' @export
#' 
zhdp_extract_prev_headers = function(text, lineno, header="^#'") {
  if (!is.vector(lineno) || length(lineno) > 1) { stop("Invalid input 'lineno'")}
  matchlines = grep(header, text) 
  closest_break = max(c(0,setdiff(seq_len(lineno-1), matchlines)))
  
  if (closest_break %in% c(0, lineno - 1)) {
    return(NULL)
  } else {
    return((closest_break+1):(lineno-1))
  }
}


#' [Helper] Extract and process the current documentation for input code, for a given documentation segment
#' 
#' Creates a data frame storing information for the current documentation
#' 
#' @param code [vector-char] :: Text (code) to process
#' @param lines [vector-int] :: Lines in the code that correspond to a single documentation segment
#' @param header [char] :: A regex that is the header (first few characters) of all lines in the 'chunk'
#' 
#' @return [dataframe] :: NULL if no current documentation, otherwise: 
#' \itemize{
#'   \item LineNo -- [int] :: Line number of original code
#'   \item Value -- [char] :: Entire line in the original code
#'   \item Type -- [char] :: Type of documentation this line is. Choices are: (Empty, @@[<validtype>], Text)
#'   \item ParamName -- [char] :: If this is of type @@param, this contains the parameter name
#'   \item TypeOrder -- [int] :: **Zeroed out for now**
#'   \item ParamOrder -- [int] :: **Zeroed out for now**
#'   \item SubOrder -- [int] :: **Zeroed out for now**
#' }
#' 
#' @export
#' 
zhdp_process_cur_docu = function(code, lines, header = "^#'") {
  if (length(lines) <= 0) { return(NULL) }
  
  prev_docu = data.frame(LineNo = lines, Value = "", Type = "", ParamName = "", TypeOrder = 0, ParamOrder = 0, SubOrder = 0, stringsAsFactors=FALSE)
  
  ## Clean up text (clear extra spaces and stuff)
  text_clean = gsub(stringr::str_c(header, " +"), "", code[lines])
  tsplit = strsplit(text_clean, " ")
  
  ## Fill in Value, Type, ParamName
  for(j in seq_along(lines)) {
    t = tsplit[[j]]
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




#' [Helper] Extracts existing (roxygen2-style) documentation
#' 
#' This function extends the function information extracted previously in fxinfo, and returns a list of an updated data.frame version of fxinfo, and a list of function information. 
#' 
#' @param code [vector-char] :: Text (code) 
#' @param all_matchlines [vector-int] :: Lines where functions potentially start
#' @param fxinfo [list] :: Data frame containing information on functions in this file. This should be output from \code{\link{zhdp_extractFxInfo}}
#' 
#' @return [list] :: Two elements: \code{fx_df}, \code{fx_list}, where \cr
#' \code{fx_df} -- [dataframe] :: Contains information about location of the functions in the code
#' \itemize{
#'   \item fx_name -- [char] :: Function name
#'   \item doc_exist -- [logical] :: Is there existing documentation?
#'   \item doc_start -- [int] :: Line number for documentation to start
#'   \item doc_end -- [int] :: Line number for end of documentation
#'   \item fx_start -- [int] :: Line number for function declaration (function start)
#'   \item fx_end -- [int] :: **Zeroed out for now**
#'   \item status -- [char] :: **NA'd out for now**
#' } \cr
#' \code{fx_list} -- [list-list] :: Contains information about each function found as follows: 
#' \itemize{
#'   \item fxname -- [char] :: Function name
#'   \item docu_cur -- [dataframe] :: output from \code{\link{zhdp_extractFxInfo}}
#'   \item code -- [] :: **NOT IMPLEMENTED** -- intention is to store function's code. 
#'   \item params -- [vector-char] :: Character vector of parameter names (or length 0 if no parameters)
#' }
#' 
#' @export
#' 
zhdp_extractDocu = function(code, all_matchlines, fxinfo) {

  ## Setup storage containers
  reslist = list()
  resdf = data.frame(fx_name = sapply(fxinfo, function(x) {x$fxname}),
                     doc_exist = FALSE, doc_start = NA, doc_end = NA, fx_start = NA, fx_end = NA, status = NA)
  
  ## For each function, extract documentation information
  for (j in seq_along(fxinfo)) {
    fx_start = all_matchlines[fxinfo[[j]]$matchlineIND]
    reslist[[j]] = list(fxname = fxinfo[[j]]$fxname, 
                        docu_cur = NULL, 
                        code = NULL, ## TODO: [Implement] this someday. 
                        params = fxinfo[[j]]$params)
    doclocs = zhdp_extract_prev_headers(text = code, lineno = fx_start)
    if (length(doclocs) > 0) {
      resdf$doc_exist[j] = TRUE        
      resdf$doc_start[j] = min(doclocs)
      resdf$doc_end[j] = max(doclocs)
      
      reslist[[j]]$docu_cur = zhdp_process_cur_docu(code, lines = doclocs)
    }
    resdf$fx_start[j] = fx_start
  }
  return(list(fx_df = resdf, fx_list = reslist))
}


