##@S Functions that extract and update the documentation


#' Update/create roxygen templates
#' 
#' DO NOT RUN THIS WITHOUT VERSION CONTROL!
#' 
#' This function searches for existing documentation, and updates it to a specific format. (The format isn't really flexible at the moment; not sure if effort will be spent generalizing it)
#'     
#' This function assumes functions are of the format 
#' FUNCTION_NAME = function( .... ) \{
#'   content
#' \}
#' 
#' @param FD Object of class FilesDescription; See documentation to see how to describe a collection of files  
#' @param guess_emptyparam Should empty parameters be filled in by "default" value? 
#' @param regexp_noexport A vector of regular expressions that determine which functions that should not be exported
#' @param regexp_nodocu A vector of regular expressions that determine which functions should not receive documentation
#' @param regexp_fxstart Regex to determine function starts; default should work
#' @param test_run If TRUE: Won't write any changes to file. This is defaulted to TRUE for safety. 
#' 
#' @return message about success!
#' 
#' @export
#' 
update_fx_documentation = function(FD, guess_emptyparam = FALSE,
                                   regexp_noexport = NULL, regexp_nodocu = NULL,
                                   regexp_fxstart = "(^[[:alnum:]_.]+) *(=|(<-)) *function", 
                                   test_run = TRUE) {
  ## guess_emptyparam guess -> when empty/temp, replaces parameter documentation with most common documentation for specific parameter. --- want to add marker for when something is edited. ********* NOT IMPLEMENTED NOW ****************
  ## idea -- make this happen based on file or entire codebase (allow it as option) 
  
  if (FALSE) {
    FD = FilesDescription(mode = "R", dirlist = "codeProcessing/R/")
    regexp_fxstart = "(^[[:alnum:]_.]+) *(=|(<-)) *function"
    regexp_noexport = NULL
    regexp_nodocu = NULL
  }
  
  ## Setup
  fxdoc = extract_all_docu(FD = FD, regexp_fxstart = regexp_fxstart)
  
  ## Check for undocumenting / unexporting
  for (reg in regexp_noexport) { fxdoc$fxtable$want_export[grep(reg, x = fxdoc$fxtable$fx_name)] = FALSE }
  for (reg in regexp_nodocu) { fxdoc$fxtable$want_docu[grep(reg, x = fxdoc$fxtable$fx_name)] = FALSE }
  
  ## Update all the documentation (store as $docu_new in the list)
  for (j in seq_along(fxdoc$fxinfo)) {
    if (fxdoc$fxtable$want_docu[j]) {
      fxdoc$fxinfo[[j]]$docu_new = reformat_docu(fxdoc$fxinfo[[j]]$docu_cur, params = fxdoc$fxinfo[[j]]$params, to_export = fxdoc$fxtable$want_export[j])
    } else {
      fxdoc$fxinfo[[j]]$docu_new = NULL
    }
  }
  
  ## Insert/replace existing documentation as necessary  
  fxdoc = update_stored_docu(fxdoc) 
  
  ## Write updated documentation as necessary
  if (!test_run) { write_MatchedCodebase(fxdoc$MCB, which(fxdoc$files_changed)) }
  print("Done! [Inserting/formatting documentation (roxygen) templates]")
  
  return(fxdoc)
}


