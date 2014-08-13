##@S Code for searching the entire codebase (all '.R' files) for certain text. 
##@S   Search results are output inside this directory (metadata)


#' Search code and potentially add comments
#' 
#' @param regexp Regular Expression to search for
#' @param add_comment If non-NULL, this is added to the source code as a next-line comment
#' @param dir Directory to search recursively for code files
#' @param mode "R" or "C" -- looks for appropriate filename extensions
#' @param file_regex If non-NULL: restrict to filenames that match this regex
#' 
#' @return none
#' 
#' @export
#' 
search_code = function(regexp = "Default Search...", add_comment = NULL, regex_exact = TRUE,
                       dir = DIR, mode = "R", file_regex = NULL) {
  
  ## new feature: regex_exact => want to add something to regex to make the match word-wise the given regexp. 
  mats = search_code_matches(regexp = regexp, regex_exact = regex_exact, dir = dir, mode = mode, file_regex = file_regex, logged = "SEARCH")
#|----##This function has new parameter (regex_exact) added --Wed Aug 13 15:14:32 2014--
  
  if (!is.null(add_comment)) { add_comment_matches(mats, add_comment, write = TRUE) }
  return("Search is done!")
}


#' Replace the regex of the code, comments added by default. 
#' 
#' @param regexp Regular Expression to search for
#' @param replace What to replace 'regexp' with?
#' @param add_comment If non-NULL, this is added to the source code as a next-line comment
#' @param comment_heads Length 2 vector: A short and long comment header to add to comments
#' @param replace_mark IF TRUE: Adds additional line of comment denoting location of replacement
#' @param dir Directory to search recursively for code files
#' @param mode "R" or "C" -- looks for appropriate filename extensions
#' @param file_regex If non-NULL: restrict to filenames that match this regex
#' 
#' @return none
#' 
#' @export
#' 
replace_code = function(regexp = "Default Search...", replace, regex_exact = TRUE,
                        add_comment, comment_heads = c("#|", "#|----##"), replace_mark = TRUE,
                        dir = DIR, mode = "R", file_regex = NULL) {

  mats = search_code_matches(regexp = regexp, regex_exact = regex_exact, dir = dir, mode = mode, file_regex = file_regex, logged = "REPLACE")
#|----##This function has new parameter (regex_exact) added --Wed Aug 13 15:14:32 2014--
  
  ## Do actual replacement: 
  for(j in seq_along(mats$files)) { mats$code[[j]] = str_replace_all(mats$code[[j]], regexp, replace) }
  
  mats = add_comment_matches(mats = mats, add_comment = add_comment, comment_heads = comment_heads, 
                             mark = TRUE, mark_replace_len = nchar(replace), write = TRUE)
  
  return("Replacements are done!")
}


#' Clears automatically generated comments from source code
#' 
#' @param comment_regex Regex to detect comments (and all lines matching this will be deleted)
#' @param dir Directory to search recursively for code files
#' @param mode "R" or "C" -- looks for appropriate filename extensions
#' @param file_regex If non-NULL: restrict to filenames that match this regex
#' 
#' @return none
#' 
#' @export
#' 
clear_comments = function(comment_regex = "^#[|]", dir = DIR, mode = "R", file_regex = NULL) {
  mats = search_code_matches(regexp = comment_regex, dir = dir, mode = mode, file_regex = file_regex, logged = "CLEAR-COMMENTS")
#|----##This function has new parameter (regex_exact) added --Wed Aug 13 15:14:32 2014--
  
  ## Do actual comment clearing: 
  for (j in seq_along(mats$files)) { mats$code[[j]] = mats$code[[j]][-mats$matchlines[[j]]] }
  write_matchlist(mats)
  
  return("Comment clearing is done!")
}

