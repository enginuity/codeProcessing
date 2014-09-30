##@S This file contains code (and documentation) for the various classes used. 


#' Class Codebase
#' 
#' This stores a basic combination of filenames and the source code. 
#' Often used for further searching, or for applying some edit to the source code and then rewriting to disk. 
#' 
#' @param files A vector of filenames, accessible from the current working directory
#' @param code A list of source codes (each an individual character vector)
#' 
#' @return Object of class Codebase
#' 
#' @export
#' 
Codebase = function(files, code) {
  return(structure(list(files = files, code = code), class = "Codebase"))
}


#' Class MatchedCodebase 
#' 
#' Extends \alias{Codebase}
#' In addition to Codebase, this stores where 'regex' matchings occur in the source. 
#' 
#' @param Codebase Object of class Codebase
#' @param matchlines List of line-location of matches, as individual vectors
#' @param matchlocs List of str_locate_all output (for 'regex' on each file)
#' @param regex Regular expression to match
#' @param exact Is regex done using 'exact' parameter grep?
#' 
#' @return Object of class MatchedCodebase
#' 
#' @export
#' 
MatchedCodebase = function(Codebase, matchlines, matchlocs, regex, regex_exact) {
  if (!inherits(Codebase, what = "Codebase")) { stop("Input Codebase is not of the appropriate class") }
  
  res = append(Codebase, list(matchlines = matchlines, matchlocs = matchlocs, regex = regex, regex_exact = regex_exact))
  class(res) = append("MatchedCodebase", class(Codebase))
  
  return(res)
}

