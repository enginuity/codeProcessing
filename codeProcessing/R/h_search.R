## This file contains helper files for the searching code for regular expressions. 

## Documentation updated 7/7/2015 -- still needs some work!
## Code updated ././2015

## TODO: [WriteHTMLDocument] This info should be added to documentation somehow?

# Helper Functions --------------------------------------------------------

#' Locates regex matches in codebase
#' 
#' All the regular expressions stored in REGEX are searched for inside the set of files described by FD. This is a helper function that should be called by any function that utilizes regular expression searching.
#' 
#' @param REGEX [Regex] :: Set of regular expressions and options to search for in the text/code. 
#' @param FD [FilesDescription] :: Description of a collection of files to search the full text/code of. If this is NULL, uses current directory (by calling getwd())
#' @param logged [char] :: If non-NULL, then this is the logtype (to write in file name of the log file)
#' 
#' @return [MatchedCodebase] :: Object containing information describing all REGEX matches inside all files in FD. 
#' 
#' @export
#' 
search_code_matches = function(REGEX, FD, logged = NULL) {
  ## Ensure directory is correct: IF FD is null, default to current directory, and assume R code.  
  if (is.null(FD)) FD = FilesDescription(mode = "R", dirlist = ".") 
  
  ## Extract all the specified files (stored in Codebase object)
  all_code = extract_Codebase(FD = FD)
  
  ## Search all the texts for all the regular expressions given. 
  files_with_matches = which(sapply(all_code$code, function(code) {any(stringr::str_detect(code, REGEX$regex))}))
  
  ## Aggregate the search results
  matchline_list = list()
  matchloc_list = list()
  for(j in seq_along(files_with_matches)) {
    text = all_code$code[[files_with_matches[j]]]
    matchline_list[[j]] = which(stringr::str_detect(text, REGEX$regex))
    matchloc_list[[j]] = stringr::str_locate_all(text[matchline_list[[j]]], REGEX$regex)
  }
  
  ## Format the regular expression match results properly
  res = MatchedCodebase(CB = all_code, CB_subset = files_with_matches, matchlines = matchline_list, matchlocs = matchloc_list, REGEX = REGEX)
  
  ## Create a search log? (This outputs all the matches to a file.)
  if (!is.null(logged)) { create_search_log(logtype = logged, MCB = res) }
  
  return(res)
}


#' Inserts comments into code
#' 
#' This function can insert comments into code, and also write to (replace) the original code files as necessary. 
#' For example, this can be called when code replacement functions are called, if comments are desired to be left in the code to denote that things had been changed. 
#' 
#' @param MCB [] :: Regex match list : this is output of search-code-matches
#' @param add_comment [char] :: If non-NULL, this is the comment added to the next line in the source code
#' @param comment_heads [vector, char, length = 2] :: The first entry is a short comment header to add to comments. The second entry is a long comment header. (eg #| vs #|----## as comment headers)
#' @param mark [logical] :: If TRUE: Includes as a comment the location of replaced text 
#' @param mark_replace_len [numeric] :: If non-NULL, should be nchar of replacement (and the markers will be made in this length)
#' @param marker [char, "*"] :: Marker character
#' @param write [logical] :: Write comments to code? (FALSE is essentially for a test run, or to make modifications that are manually written later -- the output MCB will be updated, but not be written)
#' 
#' @return [] :: Modified version of the match list
#' 
#' @export
#' 
add_comment_matches = function(MCB, add_comment, comment_heads = c("#|", "#|----##"), 
                               mark = FALSE, mark_replace_len = NULL, marker = "*",
                               write = FALSE) {
  for(j in seq_along(MCB$files)) { 
    text = MCB$code[[j]][MCB$matchlines[[j]]]
    if (mark) {
      for(k in seq_along(MCB$matchlines[[j]])) {
        com = mark_strlocate(MCB$matchlocs[[j]][[k]])
        stringr::str_sub(com, 1, nchar(comment_heads)[1]) <- comment_heads[1]
        
        if (!is.null(mark_replace_len)) { 
          com = stringr::str_replace_all(com, pattern = paste("[",marker,"]+",sep=""), replacement = stringr::str_dup(marker, times = mark_replace_len)) 
        }
        
        text[k] = paste(text[k], "\n", com,sep = "")
      }
    }
    text = paste(text, "\n", comment_heads[2], add_comment, " --", date(), "--", sep = "")
    
    MCB$code[[j]][MCB$matchlines[[j]]] = text
  }
  
  if (write) { write_MatchedCodebase(MCB)}
  return(MCB)
}





