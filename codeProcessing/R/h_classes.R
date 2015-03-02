##@S This file contains code (and documentation) for the various classes used. 

## TODO: [Make better documentation] -- Figure out how to do links in Roxygen2. 

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
#' Extends Codebase.
#' In addition to Codebase, this stores where 'regex' matchings occur in the source. 
#' 
#' @param CB Object of class Codebase
#' @param CB_subset If non-NULL, this specifies a subset of the files/code in CB to keep
#' @param matchlines List of line-location of matches, as individual vectors
#' @param matchlocs List of stringr::str_locate_all output (for 'regex' on each file)
#|                          ***********************
#|----##use stringr package call implicitly --Mon Mar 02 00:10:35 2015--
#' @param REGEX Object of class Regex: storing search request
#' 
#' @return Object of class MatchedCodebase
#' 
#' @export
#' 
MatchedCodebase = function(CB, CB_subset = NULL, matchlines, matchlocs, REGEX) {
  if (!inherits(CB, what = "Codebase")) { stop("Input Codebase is not of the appropriate class") }
  
  if (!is.null(CB_subset)) {
    CB$files = CB$files[CB_subset]
    CB$code = CB$code[CB_subset]
  }
  
  res = append(CB, list(matchlines = matchlines, matchlocs = matchlocs, REGEX = REGEX))
  class(res) = append("MatchedCodebase", class(CB))
  
  return(res)
}


#' Class FilesDescription
#'   
#' This represents a description for a collection of files. Function find_files will process members of this class and produce a vector of filenames that fit all the rules given in the description (ie. it will take all element of 'files' and process all appropriate elements of 'dirlist')
#' 
#' @param mode "R" or "C" -- looks for appropriate filename extensions. Defaults to "R".
#' @param dirlist Either a vector of directories, or a list of lists: Inner lists have $dir = some path; $file_regex = NULL or some regex. If input is instead a vector of 'dir's, then it will be converted into list form. Can be NULL. 
#' @param filelist temp
#' 
#' @return Object of class FilesDescription
#' 
#' @export
#' 
FilesDescription = function(mode = "R", dirlist = NULL, filelist = NULL) {
  if (!(mode %in% c("R", "C"))) { stop("mode value is not allowed")}
  if (is.vector(dirlist) & length(dirlist) > 0) { dirlist = lapply(dirlist, function(x) {list(dir = x, file_regex = NULL)}) }
  return(structure(list(mode = mode, dirlist = dirlist, filelist = filelist), class = "FilesDescription"))
}


#' Create object of class Regex
#'     
#' This should store all the information needed to do regex matching. Thus, there will be a consistent method to input parameters. 
#' 
#' @param base Base regex to search for
#' @param isexact Use 'exact = TRUE' in regex; might be outdated. 
#' @param isword Edit base regex to look for a 'word'
#' @param ignorecommentlines When doing regex; wish to ignore all lines with comments. 
#' 
#' @return Object of class Regex
#' 
#' @export
#' 
Regex = function(base, isexact = FALSE, isword = FALSE, ignorecommentlines = FALSE) {
  regex = base
  
  ## If want word-only matches, modify regex
  if (isword) { regex = paste("\\<", base, "\\>", sep = "") }
  
  return(structure(list(regex = regex, base = base, isexact = isexact, isword = isword, ignorecommentlines = ignorecommentlines), class = "Regex"))
}
