##@S This file contains functions for logging



#' Log object (characters usually)
#'
#' Writes line to the logfile, and perhaps prints to screen
#'
#' @param ... Objects to log
#' @param file File to write to
#' @param header Should this line be treated as a header line? Includes some
#'   spaces before this line in output
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
#' @return A filename in under the folder codeProcessing_logs/
#'
#' @export
#'
logfile_namecreation = function(logtype, query) {
  querylog = gsub("\\[.*?\\]", "", query)
  querylog = gsub("[^[:alnum:]]", "", querylog)

  ## Create directory if storage directory doesn't exist.
  if (!file.exists("codeProcessing_logs")) { dir.create("codeProcessing_logs") }

  res = paste("codeProcessing_logs/", logtype, "_", querylog,
              "_", format(Sys.time(), "%Y%m%d-%H%M%S"), ".txt",
              sep = "")
  return(res)
}



#' Outputs a logfile for various search operations
#'
#' @param logtype Type-identifier to be added to the logfile name
#' @param MCB Object of class MatchedCodebase: contains output of matched
#'   location/pairs
#'
#' @return none
#'
#' @export
#'
create_search_log = function(logtype, MCB) {
  log_file <<- logfile_namecreation(logtype = logtype, query = MCB$REGEX$base)
  log_result('Searching for "', MCB$REGEX$base, '"', "\n", header = TRUE)

  ## Log actual matches.
  for(j in seq_along(MCB$files)) {
    ## Insert file information
    log_result(stringr::str_pad("\n", 80, 'right', "*"),
               stringr::str_pad("\n", 80, 'right', "*"),"\n",
               "Matches found in '", MCB$files[j],"'",
               stringr::str_pad("\n", 80, 'right', "*"), "\n")

    ## Insert match info
    for(k in seq_along(MCB$matchlines[[j]])) {
      codeline = MCB$matchlines[[j]][k]
      log_result(stringr::str_pad(codeline, 4, 'right', " "), "||",
                 MCB$code[[j]][codeline], "\n",
                 stringr::str_pad(" ", 4, 'right', " "), "||",
                 mark_strlocate(MCB$matchlocs[[j]][[k]]), "\n")
    }
  }
  log_result("\n--- Search Done! ---\n", header = TRUE)

  invisible(0)
}



