##@S Code for searching the entire codebase (all '.R' files) for certain text. 
##@S   Search results are output inside this directory (metadata)

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
#' @param regexp Regular Expression to search for
#' @param dir Directory to search recursively for code files
#' @param mode "R" or "C" -- looks for appropriate filename extensions
#' @param file_regex If non-NULL: restrict to filenames that match this regex
#' @param logged If non-NULL, then this is the logtype (to write in filename)
#' 
#' @return A list of files, code, and match locations
#' 
#' @export
#' 
search_code_matches = function(regexp = "Default Search", 
                               dir = DIR, mode = "R", file_regex = NULL, 
                               logged = NULL) {
  ## Look for all files, that match the current mode and file_regex setting, and extract code. 
  all_code = find_files(dir = dir, mode = mode, file_regex = file_regex)
  
  ## Matching texts:
  files_with_matches = which(sapply(all_code$code, function(code) {any(str_detect(code, regexp))}))
  matchline_list = list()
  matchloc_list = list()
  for(j in seq_along(files_with_matches)) {
    text = all_code$code[[files_with_matches[j]]]
    matchline_list[[j]] = which(str_detect(text, regexp))
    matchloc_list[[j]] = str_locate_all(text[matchline_list[[j]]], regexp)
  }
  
  res = list(files = all_code$files[files_with_matches], code = all_code$code[files_with_matches], 
             matchlines = matchline_list, matchlocs = matchloc_list)
  
  ## Log if necessary. Then return. 
  if (!is.null(logged)) { create_search_log(logtype = logged, query = regexp, m = res) }
  
  return(res)
}


## TODO: [Documentation-AUTO] Check/fix Roxygen2 Documentation (add)
#' Adds comments into the code, and writes if necessary
#' 
#' @param mats search match list
#' @param add_comment If non-NULL, this is added to the source code as a next-line comment.
#' @param comment_heads length 2 vector: short/long comment header
#' @param mark T/F: True includes location of replace match. 
#' @param mark_replace_len if non NULL, length of replacement (to modify search markers)
#' @param marker marker character, defaults to "*"
#' @param write write comments to code? (FALSE is essentially for a test run, or to make modifications that are manually written later)
#' 
#' @return modified mats list. 
#' 
#' @export
#' 
add_comment_matches = function(mats, add_comment, comment_heads = c("#|", "#|----##"), 
                               mark = FALSE, mark_replace_len = NULL, marker = "*",
                               write = FALSE) {
  for(j in seq_along(mats$files)) { 
    text = mats$code[[j]][mats$matchlines[[j]]]
    if (mark) {
      for(k in seq_along(mats$matchlines[[j]])) {
        com = mark_strlocate(mats$matchlocs[[j]][[k]])
        str_sub(com, 1, nchar(comment_heads)[1]) <- comment_heads[1]
        
        if (!is.null(mark_replace_len)) { 
          com = str_replace_all(com, pattern = paste("[",marker,"]+",sep=""), replacement = str_dup(marker, times = mark_replace_len)) 
        }
        
        text[k] = paste(text[k], "\n", com,sep = "")
      }
    }
    text = paste(text, "\n", comment_heads[2], add_comment, " --", date(), "--", sep = "")
    
    mats$code[[j]][mats$matchlines[[j]]] = text
  }
  
  if (write) { write_matchlist(mats)}
  return(mats)
}


## TODO: [Documentation-AUTO] Check/fix Roxygen2 Documentation (write)
#' ********** WARNING -- INSERTED CODE **************
#' <<BasicInfo>> 
#' 
#' @param mats text
#' 
#' @return text
#' 
#' @export
#' 
write_matchlist = function(mats) {
  for(j in seq_along(mats$files)) {
    writeLines(text = mats$code[[j]], con = mats$files[j])
  }
  invisible(0)
}


# Callable search functions -----------------------------------------------


## TODO: [Documentation-AUTO] Check/fix Roxygen2 Documentation (search)
#' Search code, and potentially add comments. 
#' 
#' @param regexp Regular Expression to search for
#' @param add_comment If non-NULL, this is added to the source code as a next-line comment.
#' @param dir Directory to search recursively for code files
#' @param mode "R" or "C" -- looks for appropriate filename extensions
#' @param file_regex If non-NULL: restrict to filenames that match this regex
#' 
#' @return text
#' 
#' @export
#' 
search_code = function(regexp = "Default Search...", add_comment = NULL, 
                       dir = DIR, mode = "R", file_regex = NULL) {
  
  mats = search_code_matches(regexp = regexp, dir = dir, mode = mode, file_regex = file_regex, logged = "SEARCH")
  
  if (!is.null(add_comment)) { add_comment_matches(mats, add_comment, write = TRUE) }
  return("Search is done!")
}


## TODO: [Documentation-AUTO] Check/fix Roxygen2 Documentation (replace)
#' ********** WARNING -- INSERTED CODE **************
#' <<BasicInfo>> 
#' 
#' @param regexp Regular Expression to search for
#' @param replace text
#' @param add_comment If non-NULL, this is added to the source code as a next-line comment.
#' @param comment_heads text
#' @param replace_mark text
#' @param dir Directory to search recursively for code files
#' @param mode "R" or "C" -- looks for appropriate filename extensions
#' @param file_regex If non-NULL: restrict to filenames that match this regex
#' 
#' @return text
#' 
#' @export
#' 
replace_code = function(regexp = "Default Search...", replace, 
                        add_comment, comment_heads = c("#|", "#|----##"), replace_mark = TRUE,
                        dir = DIR, mode = "R", file_regex = NULL) {
  ## replace_mark - true => place marks as like in search code as an additional line to just the comment_head line
  mats = search_code_matches(regexp = regexp, dir = dir, mode = mode, file_regex = file_regex, logged = "REPLACE")
  
  ## Do actual replacement: 
  for(j in seq_along(mats$files)) { mats$code[[j]] = str_replace_all(mats$code[[j]], regexp, replace) }
  
  mats = add_comment_matches(mats = mats, add_comment = add_comment, comment_heads = comment_heads, 
                             mark = TRUE, mark_replace_len = nchar(replace), write = TRUE)
  
  return("Replacements are done!")
}

## TODO: [Documentation-AUTO] Check/fix Roxygen2 Documentation (clear)
#' ********** WARNING -- INSERTED CODE **************
#' <<BasicInfo>> 
#' 
#' @param comment_regex text
#' @param dir Directory to search recursively for code files
#' @param mode "R" or "C" -- looks for appropriate filename extensions
#' @param file_regex If non-NULL: restrict to filenames that match this regex
#' 
#' @return text
#' 
#' @export
#' 
clear_comments = function(comment_regex = "^#[|]", dir = DIR, mode = "R", file_regex = NULL) {
  mats = search_code_matches(regexp = comment_regex, dir = dir, mode = mode, file_regex = file_regex, logged = "CLEAR-COMMENTS")
  
  ## Do actual comment clearing: 
  for (j in seq_along(mats$files)) { mats$code[[j]] = mats$code[[j]][-mats$matchlines[[j]]] }
  write_matchlist(mats)
  
  return("Comment clearing is done!")
}

