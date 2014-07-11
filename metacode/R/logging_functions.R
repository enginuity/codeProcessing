##@S This file contains functions for logging


#' Add logline to logfile
#' 
#' @param ... Objects to log
#' @param file File to write to
#' @param header Should this line be treated as a header line? includes some spaces before this line in output
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
    print(what)
  }
  invisible(0)
}
