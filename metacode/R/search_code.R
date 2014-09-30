##@S Code for searching the entire codebase (all '.R' files) for certain text. 
##@S   Search results are output inside this directory (metadata)


## TODO: [Documentation-AUTO] Check/fix Roxygen2 Documentation (search_code)
#' Search code and potentially add comments
#' 
#' @param RE Object of class Regex, OR a simple regular expression. What to search for?
#' @param add_comment If non-NULL, this is added to the source code as a next-line comment
#' @param comment_heads Length 2 vector: A short and long comment header to add to comments
#' @param FD Object of class FilesDescription; See documentation to see how to describe a collection of files  
#' 
#' @return none
#' 
#' @export
#' 
search_code = function(RE, add_comment = NULL, comment_heads = c("#|", "#|----##"), FD = DEFAULT_FD) {
  
  ## If input is a regular expression instead of Regex object, use default settings. 
  if (class(RE) != "Regex") { RE = Regex(base = RE, isword = TRUE) }
  
  ## Find matches in MatchedCodebase format
  matchesL = search_code_matches(RE = RE, FD = FD, logged = "SEARCH")
  
  if (!is.null(add_comment)) { add_comment_matches(matchesL, add_comment, comment_heads = comment_heads, write = TRUE) }
  return("Search is done!")
}



#' Replace the regex of the code, comments added by default. 
#' 
#' @param RE Object of class Regex, OR a simple regular expression. What to search for?
#' @param replace What to replace 'regexp' with?
#' @param add_comment If non-NULL, this is added to the source code as a next-line comment
#' @param comment_heads Length 2 vector: A short and long comment header to add to comments
#' @param replace_mark IF TRUE: Adds additional line of comment denoting location of replacement
#' @param FD Object of class FilesDescription; See documentation to see how to describe a collection of files  
#' 
#' @return none
#' 
#' @export
#' 
replace_code = function(RE, replace, add_comment, 
                        comment_heads = c("#|", "#|----##"), replace_mark = TRUE, FD = DEFAULT_FD) {

  ## If input is a regular expression instead of Regex object, use default settings. 
  if (class(RE) != "Regex") { RE = Regex(base = RE, isword = TRUE) }
  
  matchesL = search_code_matches(RE = RE, FD = FD, logged = "REPLACE")
  
  ## Do actual replacement: 
  for(j in seq_along(matchesL$files)) { matchesL$code[[j]] = str_replace_all(matchesL$code[[j]], regexp, replace) }
  
  matchesL = add_comment_matches(matchesL = matchesL, add_comment = add_comment, comment_heads = comment_heads, 
                             mark = TRUE, mark_replace_len = nchar(replace), write = TRUE)
  
  return("Replacements are done!")
}


#' Clears automatically generated comments from source code
#' 
#' @param comment_regex Regex to detect comments (and all lines matching this will be deleted)
#' @param FD Object of class FilesDescription; See documentation to see how to describe a collection of files  
#' 
#' @return none
#' 
#' @export
#' 
clear_comments = function(comment_regex = "^#[|]", FD = DEFAULT_FD) {
  matchesL = search_code_matches(RE = Regex(base = comment_regex), FD = FD, logged = "CLEAR-COMMENTS")
  
  ## Do actual comment clearing: 
  for (j in seq_along(matchesL$files)) { matchesL$code[[j]] = matchesL$code[[j]][-matchesL$matchlines[[j]]] }
  write_matchlist(matchesL)
  
  return("Comment clearing is done!")
}

