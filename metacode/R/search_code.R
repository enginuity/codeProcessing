##@S Code for searching the entire codebase (all '.R' files) for certain text. 
##@S   Search results are output inside this directory (metadata)

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

