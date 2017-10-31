##@S Functions that extract and update the documentation


#' Update/create roxygen templates
#'
#' DO NOT RUN THIS WITHOUT VERSION CONTROL!
#'
#' This function searches for existing documentation, and updates it to a
#' specific format. (The format isn't really flexible at the moment; not sure
#' if effort will be spent generalizing it)
#'
#' This function assumes functions are of the format
#' FUNCTION_NAME = function( .... ) \{
#'   content
#' \}
#'
#' @param FD [\code{\link{FilesDescription}}] :: Information about which files
#'   to search for documentation
#' @param guess_emptyparam [char] :: Default value to fill in for empty
#'   parameter documentation. If NULL, then this is not done.
#'   ** NOT IMPLEMENTED**
#' @param regexp_noexport [vector-char] :: A vector of regular expressions that
#'   determine which functions that should not be exported
#' @param regexp_nodocu [vector-char] :: A vector of regular expressions that
#'   determine which functions should not receive documentation
#' @param regexp_fxstart [char] :: Regex to determine function starts; default
#'   should work
#' @param test_run [logical] :: If TRUE: Won't write any changes to file. This
#'   is defaulted to TRUE for safety.
#'
#' @return [\code{\link{FunctionDocu}}] :: Updated function documentation
#'
#' @export
#'
update_fx_documentation = function(
  FD, guess_emptyparam = FALSE, regexp_noexport = NULL, regexp_nodocu = NULL,
  regexp_fxstart = "(^[[:alnum:]_.]+) *(=|(<-)) *function",
  test_run = TRUE) {
  ## idea -- make guess_emptyparam do something based on entire codebase
  ##  (or specific file) this happen based on file or entire codebase
  ##   (allow it as option)

  if (FALSE) {
    FD = FilesDescription(mode = "R", dirlist = "codeProcessing/R/")
    regexp_fxstart = "(^[[:alnum:]_.]+) *(=|(<-)) *function"
    regexp_noexport = NULL
    regexp_nodocu = NULL
  }

  ## Setup
  fxdoc = extract_all_docu(FD = FD, regexp_fxstart = regexp_fxstart)

  ## Check for undocumenting / unexporting
  for (reg in regexp_noexport) {
    fxdoc$fxtable$want_export[grep(reg, x = fxdoc$fxtable$fx_name)] = FALSE
  }
  for (reg in regexp_nodocu) {
    fxdoc$fxtable$want_docu[grep(reg, x = fxdoc$fxtable$fx_name)] = FALSE
  }

  ## Update all the documentation (store as $docu_new in the list)
  for (j in seq_along(fxdoc$fxinfo)) {
    if (fxdoc$fxtable$want_docu[j]) {
      fxdoc$fxinfo[[j]]$docu_new = reformat_docu(
        fxdoc$fxinfo[[j]]$docu_cur, params = fxdoc$fxinfo[[j]]$params,
        to_export = fxdoc$fxtable$want_export[j])
    } else {
      fxdoc$fxinfo[[j]]$docu_new = NULL
    }
  }

  ## Insert/replace existing documentation as necessary
  fxdoc = update_stored_docu(fxdoc)

  ## Write updated documentation as necessary
  if (!test_run) {
    write_MatchedCodebase(fxdoc$MCB, which(fxdoc$files_changed))
  }
  print("Done! [Inserting/formatting documentation (roxygen) templates]")

  return(fxdoc)
}

