## Collection of functions that write to disk. 


#' Writes a modified match list to the codefiles
#'   
#' If a match list was modified (ie the $code portions were modified), the changes could be written to file. 
#' This function does just that. 
#' 
#' @param MCB Regex match list : this is output of search-code-matches
#' @param ids Write only the files indexed by ids
#' 
#' @return none
#' 
#' @export
#' 
write_MatchedCodebase = function(MCB, ids = NULL) {
  if (is.null(ids)) { ids = seq_along(MCB$files) }
  for (j in ids) {
    print(MCB$files[j])
    writeLines(text = MCB$code[[j]], con = MCB$files[j])
  }
  invisible(0)
}



## TODO: [Documentation-AUTO] Check/fix Roxygen2 Documentation (replace_codelines)
#' <What does this function do>
#' 
#' @param basecode temp
#' @param newcode temp
#' @param lines_start temp
#' @param lines_end temp
#' 
#' @return temp
#' 
#' @export
#' 
replace_codelines = function(basecode, newcode, lines_start, lines_end) {
  ## basecode, newcode are character vectors corresponding to lines of a code file
  ## lines_start and lines_end denote the locations of where to start/end removing lines from basecode
  
  if (lines_start > lines_end) { stop("Error -- lines_start > lines_end") }
  N = length(basecode)
  base_start = NULL; base_end = NULL
  if (lines_start > 1) { base_start = basecode[1:(lines_start-1)] }
  if (lines_end < N) { base_end = basecode[(lines_end+1):N] }
  return(c(base_start, newcode, base_end))
}


## TODO: [Documentation-AUTO] Check/fix Roxygen2 Documentation (insert_codelines)
#' <What does this function do>
#' 
#' @param basecode temp
#' @param newcode temp
#' @param before_line temp
#' 
#' @return temp
#' 
#' @export
#' 
insert_codelines = function(basecode, newcode, before_line) {
  ## basecode is current code
  ## newcode is the code to insert before line [before_line]
  cat("before line = ", before_line, "** length = ", length(basecode), "\n")
  if (before_line > length(basecode)) { stop("Error -- before_line is outside of current code length") }
  N = length(basecode)
  return(c(basecode[seq_len(before_line - 1)], newcode, basecode[before_line:N]))
}



