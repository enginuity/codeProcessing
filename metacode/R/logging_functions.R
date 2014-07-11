##@S This file contains functions for logging


#' Add logline to logfile
#' 
#' @param ... Objects to log
#' @param file File to write to
#' @param header Should this line be treated as a header line? includes some spaces before this line in output
#' @param display Should this be printed to screen?
#' 
#' @return nothing this stores results in logfile and prints to screen
#' 
#' @export
log_result = function(..., file = log_file, header = TRUE, display = TRUE) {
  if (header) { 
    cat("\n---", date(), "---\n", sep = "", file = file, append = TRUE) 
  } else {    
    if (is.character(what) & (length(what) == 1)) {
      cat("---", date(), "--- ", ..., "\n", append = TRUE, file = file, sep = "")
    } else {
      cat(..., "\n", file = file, append = TRUE, sep = "")
    }
  }
  if (display) {
    cat(..., "\n")
  }
  invisible(0)
}


#' ********** WARNING -- INSERTED CODE **************
#' <<BasicInfo>> 
#' 
#' @param logtype text
#' @param query text
#' 
#' @return text
#' 
#' @export
logfile_namecreation = function(logtype, query) {
  res = paste("results/z", logtype, gsub("[^[:alnum:]]", "", query),
              "_", format(Sys.time(), "%Y%m%d-%H%M%S"), ".txt", 
               sep = "")
  return(res)
}
