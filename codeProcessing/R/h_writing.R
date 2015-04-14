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
  for(j in ifelse(test = is.null(ids), yes = seq_along(MCB$files), no = ids)) {
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
  base_start = ifelse(lines_start > 1, basecode[1:(lines_start-1)], NULL)
  base_end = ifelse(lines_end < N, basecode[(lines_end+1):N], NULL)
  return(c(base_start, newcode, base_end))
}




