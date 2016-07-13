##@S This file contains code (and documentation) for the various classes used. 

#' Class Codebase
#'    
#' This stores a basic combination of filenames and the source code. 
#' Often used for further searching, or for applying some edit to the source code and then rewriting to disk. 
#' 
#' @param files [vector-char] :: A vector of filenames, accessible from the current working directory
#' @param code [list-vector-char] :: A list containing source code (each an individual character vector which are just line-by-line versions of source code)
#' 
#' @return [Codebase] :: A list (S3 class) with entries: 
#' \itemize{
#'   \item files -- [vector-char] :: A vector of filenames
#'   \item code -- [list-vector-char] :: A list of source code, each element is a character vector corresponding to source code. 
#' }
#' 
#' @export
#' 
Codebase = function(files, code) {
  return(structure(list(files = files, code = code), class = "Codebase"))
}


## TODO [Documentation] : Improve the parameter documentation to be more specific
#' Class MatchedCodebase 
#'    
#' Extends \code{\link{Codebase}}.
#' In addition to Codebase, this stores where 'regex' matchings occur in the source. 
#' 
#' @param CB [\code{\link{Codebase}}] :: Original input text collection
#' @param CB_subset [vector] :: If non-NULL, this is used as an index for files in the original codebase. This can be any indexing method (eg. numeric indices, negative numeric indices, logical vector, etc.). This allows for subsetting the original codebase (eg. to ignore certain files that are described in FD, or to ignore certain files that do not have any matches)
#' @param matchlines [list] :: Line locations of matches, as individual vectors in the list, corresponding to the order in the input CB. (This corresponds to subsetted remaining vector of appropriate)
#' @param matchlocs [list] :: stringr::str_locate_all output from running some regular expression on each file. 
#' @param REGEX [\code{\link{Regex}}] :: Allows for input of the search request for storage purposes
#' 
#' @return [MatchedCodebase] :: A list (S3 class) with entries:
#' \itemize{
#'   \item files -- [vector-char] :: A vector of filenames
#'   \item code -- [list-vector-char] :: A list of source code, each element is a character vector corresponding to source code. 
#'   \item matchlines -- [list-vector-int] :: A list storing line-locations of matches, as individual vectors. 
#'   \item matchlocs -- [list] :: A list storing match locations as in str_locate_all output format. 
#'   \item REGEX -- [\code{\link{Regex}}] :: Stores the regex requested
#' }
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
#' @param mode "R" or "C" -- looks for appropriate filename extensions. Defaults to "R". Also 'all' is allowed -> no subsetting of files, which is possibly not ideal. 
#' @param dirlist Either a vector of directories, or a list of lists: Inner lists have $dir = some path; $file_regex = NULL or some regex. If input is instead a vector of 'dir's, then it will be converted into list form. Can be NULL. 
#' @param filelist [list-char] :: List of individually specified files
#' 
#' @return [FilesDescription] :: A list (S3 class): 
#' \itemize{
#'    \item mode -- [char] :: "R" or "C" : looks for appropriate file extensions of this type
#'    \item dirlist -- [list-list] :: Inner lists have $dir to specify a path, and $file_regex to specify a regular expression to search. 
#'    \item filelist -- [list-char] :: List of individually specified files
#' }
#' 
#' @export
#' 
FilesDescription = function(mode = "R", dirlist = NULL, filelist = NULL) {
  if (!(mode %in% c("R", "C", "all"))) { stop("mode value is not allowed")}
  if (is.vector(dirlist) & length(dirlist) > 0) { dirlist = lapply(dirlist, function(x) {list(dir = x, file_regex = NULL)}) }
  return(structure(list(mode = mode, dirlist = dirlist, filelist = filelist), class = "FilesDescription"))
}


#' Create object of class Regex
#'     
#' This should store all the information needed to do regex matching. Thus, there will be a consistent method to input parameters. 
#' 
#' @param base [char] :: Base regex to search for
#' @param isexact [logical] :: If TRUE, Use 'exact = TRUE' in regex
#' @param isword [logical] :: If TRUE, edits the incoming regex to search for a single 'word' 
#' @param ignorecommentlines [logical] :: If TRUE, ignores all lines with comments
#' 
#' @return [Regex] :: A list (S3 class) with entries: 
#' \itemize{
#'   \item regex -- [char] :: Regex to actually search for
#'   \item base -- [char] :: Base regex (input by user)
#'   \item isexact -- [logical] :: If TRUE, Use 'exact = TRUE' in regex
#'   \item isword -- [logical] :: If TRUE, edits the incoming regex to search for a single 'word' 
#'   \item ignorecommentlines -- [logical] :: If TRUE, ignores all lines with comments
#' }
#' 
#' @export
#' 
Regex = function(base, isexact = FALSE, isword = FALSE, ignorecommentlines = FALSE) {
  regex = base
  
  ## If want word-only matches, modify regex
  if (isword) { regex = paste("\\<", base, "\\>", sep = "") }
  
  return(structure(list(regex = regex, base = base, isexact = isexact, isword = isword, ignorecommentlines = ignorecommentlines), class = "Regex"))
}


#' Create object of type FunctionDocu
#' 
#' This object stores all information about the documentation, given an input file. It's just a fancy container (list) that will get passed around... 
#' 
#' @param fxtable [dataframe] :: This is a list of dataframes, one for each input file (corresponding to the \code{MCB})
#' @param fxinfo [list-list] :: Contains information about each function
#' @param MCB [MatchedCodebase] :: Information about source code
#' @param files_changed [vector-logical; DEFAULT = NULL] :: T/F for each files in MCB identifying whether it has changed
#' 
#' @return [FunctionDocu] :: Stores all extracted information from the function documentation. This is a list (S3 class) with the following elements: fxtables, fxinfos, MCB, which are: \cr
#' \code{fxtable} -- [dataframe] :: This is a dataframe containing function information (and has information on the origin file)
#' \itemize{
#'   \item ID -- [int] :: Numeric index for function (to align with fxinfo)
#'   \item fileID -- [int] :: Index for file number in MCB
#'   \item filename -- [char] :: File name (to save to)
#'   \item fx_name -- [char] :: Function name
#'   \item doc_exist -- [logical] :: Is there existing documentation?
#'   \item doc_start -- [int] :: Line number for documentation to start
#'   \item doc_end -- [int] :: Line number for end of documentation
#'   \item fx_start -- [int] :: Line number for function declaration (function start)
#'   \item fx_end -- [int] :: **NA'd out for now, not implemented**
#'   \item status -- [char] :: **NA'd out for now**
#'   \item want_docu -- [logical] :: T/F - should this function be documented?
#'   \item want_export -- [logical] :: T/F - should this function be exported?
#' } \cr
#' \code{fxinfo} -- [list-list] :: Contains information about each function (the lists for each file are concatenated) found as follows: 
#' \itemize{
#'   \item fxname -- [char] :: Function name
#'   \item docu_cur -- [dataframe] :: output from \code{\link{zhdp_extractFxInfo}}
#'   \item code -- [] :: **NOT IMPLEMENTED** -- intention is to store function's code. 
#'   \item params -- [vector-char] :: Character vector of parameter names (or length 0 if no parameters)
#' } \cr
#' \code{MCB} -- [MatchedCodebase] :: Information about source code \cr
#' \code{files_changed} -- [vector-logical] :: T/F for each files in MCB identifying whether it has changed
#' 
#' @export
#' 
FunctionDocu = function(fxtable, fxinfo, MCB, files_changed = NULL) {
  return(structure(list(fxtable = fxtable, fxinfo = fxinfo, MCB = MCB, files_changed = files_changed), class = "FunctionDocu"))  
}

