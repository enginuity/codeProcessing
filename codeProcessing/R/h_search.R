## This file contains helper files for the search code. 


## TODO: [WriteHTMLDocument] This info should be added to documentation somehow?


# Helper Functions --------------------------------------------------------



#' Locates regex matches in codebase
#'    
#' This function searches the code in the 'dir'ectory (with 'file_regex' as needed, and of 'mode'). 
#' This should be a helper function called by any function that uses searching... 
#' 
#' @param REGEX Object of class Regex. What to search for? 
#' @param FD Object of class FilesDescription; See documentation to see how to describe a collection of files -- if null -> uses current directory (getwd())
#' @param logged If non-NULL, then this is the logtype (to write in filename)
#' 
#' @return A MatchedCodebase object
#' 
#' @export
#' 
search_code_matches = function(REGEX, FD, logged = NULL) {
  
  ## Look for all files, that match the current mode and file_regex setting, and extract code. 
  if (is.null(FD)) FD = FilesDescription(mode = "R", dirlist = ".") ## IF null, default to current directory. 
  
  all_code = extract_Codebase(FD = FD)
  
  ## Matching texts:
  files_with_matches = which(sapply(all_code$code, function(code) {any(stringr::str_detect(code, REGEX$regex))}))
  matchline_list = list()
  matchloc_list = list()
  for(j in seq_along(files_with_matches)) {
    text = all_code$code[[files_with_matches[j]]]
    matchline_list[[j]] = which(stringr::str_detect(text, REGEX$regex))
    matchloc_list[[j]] = stringr::str_locate_all(text[matchline_list[[j]]], REGEX$regex)
  }
  
  ## Format matched results properly
  res = MatchedCodebase(CB = all_code, CB_subset = files_with_matches, matchlines = matchline_list, matchlocs = matchloc_list, REGEX = REGEX)
  
  ## Log if necessary. Then return. 
  if (!is.null(logged)) { create_search_log(logtype = logged, MCB = res) }
  
  return(res)
}


#' Adds comments into the code, and write to file if necessary
#' 
#' @param MCB Regex match list : this is output of search-code-matches
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



#' Writes a modified match list to the codefiles
#'   
#' If a match list was modified (ie the $code portions were modified), the changes could be written to file. 
#' This function does just that. 
#' 
#' @param MCB Regex match list : this is output of search-code-matches
#' @param ids Write only the files indexed by ids
#' 
#' @return none
#' 
#' @export
#' 
write_MatchedCodebase = function(MCB, ids = NULL) {
  for(j in ifelse(test = is.null(ids), yes = seq_along(MCB$files), no = ids)) {
    writeLines(text = MCB$code[[j]], con = MCB$files[j])
  }
  invisible(0)
}


