## This file contains helper files for the search code. 


## TODO: [WriteHTMLDocument] This info should be added to documentation somehow?
## mode: 'R' or 'C' depending on whether to look in R or C code.
## -- R code => looks at all .R files.
## -- C code => looks at all .c, .cc, .cpp, .h, .hh files.


# Helper Functions --------------------------------------------------------



#' Locates regex matches in codebase
#'    
#' This function searches the code in the 'dir'ectory (with 'file_regex' as needed, and of 'mode'). 
#' This should be a helper function called by any function that uses searching... 
#'    
#' The returned object is a list with the following elements:
#' \itemize{
#' \item $files -- A vector of filenames
#' \item $code -- A list of : source codes (individual character vectors)
#' \item $matchlines -- A list of : line-location of matches (individual numeric vectors)
#' \item $matchlocs -- A list of : str_locate_all output (for 'regex' on each file)
#' }
#' 
#' @param REGEX Object of class Regex. What to search for? 
#' @param FD Object of class FilesDescription; See documentation to see how to describe a collection of files  
#' @param logged If non-NULL, then this is the logtype (to write in filename)
#' 
#' @return A list of files, code, and match locations
#' 
#' @export
#' 
search_code_matches = function(REGEX, FD, logged = NULL) {
  
  ## Look for all files, that match the current mode and file_regex setting, and extract code. 
  all_code = extract_Codebase(FD = FD)
  
  ## Matching texts:
  files_with_matches = which(sapply(all_code$code, function(code) {any(str_detect(code, REGEX$regex))}))
  matchline_list = list()
  matchloc_list = list()
  for(j in seq_along(files_with_matches)) {
    text = all_code$code[[files_with_matches[j]]]
    matchline_list[[j]] = which(str_detect(text, REGEX$regex))
    matchloc_list[[j]] = str_locate_all(text[matchline_list[[j]]], REGEX$regex)
  }
  
  ## Format matched results properly
  res = MatchedCodebase(CB = all_code, CB_subset = files_with_matches, matchlines = matchline_list, matchlocs = matchloc_list, REGEX = REGEX)
  
  ## Log if necessary. Then return. 
  if (!is.null(logged)) { create_search_log(logtype = logged, MCB = res) }
  
  return(res)
}


#' Adds comments into the code, and write to file if necessary
#' 
#' @param matchesL Regex match list : this is output of search-code-matches
#' @param add_comment If non-NULL, this is added to the source code as a next-line comment
#' @param comment_heads Length 2 vector: A short and long comment header to add to comments
#' @param mark If TRUE: Includes as a comment the location of replaced text 
#' @param mark_replace_len If non-NULL, should be nchar of replacement (and the markers will be made in this length)
#' @param marker Marker character : defaults to "*"
#' @param write Write comments to code? (FALSE is essentially for a test run, or to make modifications that are manually written later)
#' 
#' @return Modified version of the match list
#' 
#' @export
#' 
add_comment_matches = function(matchesL, add_comment, comment_heads = c("#|", "#|----##"), 
                               mark = FALSE, mark_replace_len = NULL, marker = "*",
                               write = FALSE) {
  for(j in seq_along(matchesL$files)) { 
    text = matchesL$code[[j]][matchesL$matchlines[[j]]]
    if (mark) {
      for(k in seq_along(matchesL$matchlines[[j]])) {
        com = mark_strlocate(matchesL$matchlocs[[j]][[k]])
        str_sub(com, 1, nchar(comment_heads)[1]) <- comment_heads[1]
        
        if (!is.null(mark_replace_len)) { 
          com = str_replace_all(com, pattern = paste("[",marker,"]+",sep=""), replacement = str_dup(marker, times = mark_replace_len)) 
        }
        
        text[k] = paste(text[k], "\n", com,sep = "")
      }
    }
    text = paste(text, "\n", comment_heads[2], add_comment, " --", date(), "--", sep = "")
    
    matchesL$code[[j]][matchesL$matchlines[[j]]] = text
  }
  
  if (write) { write_MatchedCodebase(matchesL)}
  return(matchesL)
}



#' Writes a modified match list to the codefiles
#'   
#' If a match list was modified (ie the $code portions were modified), the changes could be written to file. 
#' This function does just that. 
#' 
#' @param matchesL Regex match list : this is output of search-code-matches
#' 
#' @return none
#' 
#' @export
#' 
write_MatchedCodebase = function(matchesL) {
  for(j in seq_along(matchesL$files)) {
    writeLines(text = matchesL$code[[j]], con = matchesL$files[j])
  }
  invisible(0)
}


