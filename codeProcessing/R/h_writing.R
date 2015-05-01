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


#' Replaces lines of text
#' 
#' @param basecode base lines of code
#' @param newcode code that replaces old lines
#' @param lines_start start replacing from this line
#' @param lines_end end replacing at this line
#' 
#' @return updated text
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



#' Inserts lines of code into a script
#' 
#' @param basecode current text
#' @param newcode text to insert
#' @param before_line insert (newcode) before this line in (basecode)
#' 
#' @return updated text
#' 
#' @export
#' 
insert_codelines = function(basecode, newcode, before_line) {
  if (before_line > length(basecode)) { stop("Error -- before_line is outside of current code length") }
  N = length(basecode)
  return(c(basecode[seq_len(before_line - 1)], newcode, basecode[before_line:N]))
}



