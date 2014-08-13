##@S This file contains functions for logging


#' Log object (characters usually)
#' 
#' Writes line to the logfile, and perhaps prints to screen
#' 
#' @param ... Objects to log
#' @param file File to write to
#' @param header Should this line be treated as a header line? Includes some spaces before this line in output
#' @param display Should this be printed to screen?
#' 
#' @return none
#' 
#' @export
#' 
log_result = function(..., file = log_file, header = FALSE, display = FALSE) {
  if (header) { 
    cat("\n---", date(), "---\n", sep = "", file = file, append = TRUE) 
  } else {    
    cat(..., "\n", file = file, append = TRUE, sep = "")
  }
  if (display) {
    cat(..., "\n", sep = "")
  }
  invisible(0)
}



#' Creates the logfile name (to conform to system standards)
#' 
#' @param logtype Type-identifier to be added to the logfile name
#' @param query Usually is regexp; this is added to the logfile name
#' 
#' @return A filename in under the folder results/
#' 
#' @export
#' 
logfile_namecreation = function(logtype, query) {
  res = paste("results/z", logtype, "_", gsub("[^[:alnum:]]", "", query),
              "_", format(Sys.time(), "%Y%m%d-%H%M%S"), ".txt", 
              sep = "")
  return(res)
}


## TODO: [Documentation-AUTO] Check/fix Roxygen2 Documentation (create)
#' Writes a logfile, for various search operations. 
#' 
#' @param logtype Type-identifier to be added to the logfile name
#' @param query regexp that was searched for, to be added to filename
#' @param m search result match list. 
#' 
#' @return Nothing
#' 
#' @export
#' 
create_search_log = function(logtype, query, m) {
  ## This function writes a search logfile (only does this; this shall be called by search_code)
  ## m = match list
  
  log_file <<- logfile_namecreation(logtype = logtype, query = query)
  log_result('Searching for "', query, '"', "\n", header = TRUE)
  
  ## Log actual matches. 
  for(j in seq_along(m$files)) {
    ## Insert file information
    log_result(str_pad("\n", 80, 'right', "*"),str_pad("\n", 80, 'right', "*"),"\n",
               "Matches found in '", m$files[j],"'", str_pad("\n", 80, 'right', "*"), "\n")
    
    ## Insert match info
    for(k in seq_along(m$matchlines[[j]])) {
      codeline = m$matchlines[[j]][k]
      log_result(str_pad(codeline, 4, 'right', " "), "||", m$code[[j]][codeline], "\n",
                 str_pad(" ", 4, 'right', " "), "||", mark_strlocate(m$matchlocs[[j]][[k]]), "\n")
    }
  }
  log_result("\n--- Search Done! ---\n", header = TRUE)
  
  invisible(0)
}



