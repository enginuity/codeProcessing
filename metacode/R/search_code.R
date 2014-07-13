##@S Code for searching the entire codebase (all '.R' files) for certain text. 
##@S   Search results are output inside this directory (metadata)

## TODO: Reorganize this file, rewrite functions?
## TODO: [Find code] Get my 'todo' finder...  

## mode: 'R' or 'C' depending on whether to look in R or C code.
## -- R code => looks at all .R files.
## -- C code => looks at all .c, .cc, .cpp, .h, .hh files.


# Helper Functions --------------------------------------------------------

#' Searches code for certain regular expressions. This is a helper function that should be called by 
#' any function that requires searching. 
#' 
#' @param regexp regex to search for
#' @param dir directory to search files under
#' @param mode appropraite file extensions
#' @param file_regex filler
#' @param logged if non_null, then this is the logtype
#' 
#' @return a list of search result matches. 
#' 
#' @export
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


# Callable search functions -----------------------------------------------









#' Search code, and potentially add comments. 
#' 
#' @param dir text
#' @param mode text
#' @param regexp text
#' @param add_comment adds a next line comment to original files where the regexp is found
#' @param file_regex a regular expression to restrict filenames to search/process
#' 
#' @return text
#' 
#' @export
search_code = function(regexp = "Default Search...", add_comment = NULL, 
                       dir = DIR, mode = "R", file_regex = NULL) {
  
  ## Look for all files, that match the current mode and file_regex setting, and extract code. 
  all_code = find_files(dir = dir, mode = mode, file_regex = file_regex)
  
  mats = search_code_matches(regexp = regexp, dir = dir, mode = mode, file_regex = file_regex, logged = "SEARCH")
  if (!is.null(add_comment)) { add_comment_matches(mats, add_comment, write = TRUE) }
  return("Search is done!")
}

#' ********** WARNING -- INSERTED CODE **************
#' <<BasicInfo>> 
#' 
#' @param m text
#' @param add_comment text
#' @param comment_heads text
#' @param mark text
#' @param mark_replace_len text
#' @param marker text
#' @param write text
#' 
#' @return text
#' 
#' @export
add_comment_matches = function(m, add_comment, comment_heads = c("#|", "#|----##"), 
                               mark = FALSE, mark_replace_len = NULL, marker = "*",
                               write = FALSE) {
  for(j in seq_along(m$files)) { 
    text = m$code[[j]][m$matchlines[[j]]]
    if (mark) {
      for(k in seq_along(m$matchlines[[j]])) {
        com = mark_strlocate(m$matchlocs[[j]][[k]])
        str_sub(com, 1, nchar(comment_heads)[1]) <- comment_heads[1]
        
        if (!is.null(mark_replace_len)) { 
          com = str_replace_all(com, pattern = paste("[",marker,"]+",sep=""), replacement = str_dup(marker, times = mark_replace_len)) 
        }
        
        text[k] = paste(text[k], "\n", com,sep = "")
      }
    }
    text = paste(text, "\n", comment_heads[2], add_comment, " --", date(), "--", sep = "")
    
    m$code[[j]][m$matchlines[[j]]] = text
  }
  
  if (write) { for(j in seq_along(m$files)) { writeLines(text = m$code[[j]], con = m$files[j]) } }
  return(m)
}

#' ********** WARNING -- INSERTED CODE **************
#' <<BasicInfo>> 
#' 
#' @param regexp text
#' @param replace text
#' @param add_comment text
#' @param comment_heads text
#' @param replace_mark text
#' @param mode text
#' @param file_regex text
#' 
#' @return text
#' 
#' @export
replace_code = function(regexp = "Default Search...", replace, 
                        add_comment, comment_heads = c("#|", "#|----##"), replace_mark = TRUE,
                        dir = DIR, mode = "R", file_regex = NULL) {
  ## replace_mark - true => place marks as like in search code as an additional line to just the comment_head line
  all_code = find_files(dir = dir, mode = mode, file_regex = file_regex)
  
  mats = search_code_matches(regexp = regexp, dir = dir, mode = mode, file_regex = file_regex, logged = "REPLACE")
  
  ## Do actual replacement: 
  for(j in )
  mats = add_comment_matches(m = mats, add_comment = add_comment, comment_heads = comment_heads, 
                             mark = TRUE, mark_replace_len = nchar(replace), write = TRUE)
  
  return("Replacements are done!")
}

#' ********** WARNING -- INSERTED CODE **************
#' <<BasicInfo>> 
#' 
#' @param dir text
#' @param mode text
#' @param "C" text
#' @param regexp text
#' @param file_regex text
#' 
#' @return text
#' 
#' @export
clear_comments = function(dir=DIR, mode = c("R", "C"), regexp = "^[#][|]", file_regex = NULL) {
  ## DO NOT DO THIS WITHOUT VERSION CONTROL!
  
  ## Look for all files, that match the current mode and file_regex setting
  all_code = find_files(dir = dir, mode = mode, file_regex = file_regex)
  #|----##*** Modify output: instead of list of sublists with two fields (filename, code), have list of two lists: files, code --Sat Jul 12 18:47:32 2014--
  
  ## Create savefile name
  sfile = paste("results/zCLEARCOMMENTS_", gsub("[^[:alnum:]]", "", regexp),"_", format(Sys.time(), "%Y%m%d-%H%M%S"), ".txt", sep = "")
  
  ## Search files, outputting relevant information to savefile. 
  cat('Searching for "', regexp, '"', "\n", date(), "\n\n", sep = "", file = sfile, append = TRUE)
  
  
  for(j in seq_along(all_code)) {
    gr = gregexpr(regexp, all_code[[j]]$code, fixed=fixed)
    if (length(gr) > 0) {
      match_lines = which(sapply(gr, function(x) {x[1] > 0}))
      
      if (length(match_lines) > 0) {
        any_match = TRUE
        cat("\n", file = sfile, append = TRUE)
        cat("**************************************************\n", file = sfile, append = TRUE)
        cat("**************************************************\n", file = sfile, append = TRUE)
        cat("Matches found in '", all_code[[j]]$filename,"' \n", sep = "", file = sfile, append = TRUE)
        
        for(k in match_lines) {
          cat(fix_length(t = k, len = 4), "||", 
              all_code[[j]]$code[k], 
              " \n", sep = "", file = sfile, append = TRUE)
          cat(fix_length(t = " ", len = 4), "||",
              mark_gregexpr_loc(gr = gr[[k]], len = nchar(all_code[[j]]$code[k])),
              "\n------\n\n", sep = "", file = sfile, append = TRUE)
        }
        replacement_code = all_code[[j]]$code
        writeLines(replacement_code[-match_lines], con = all_code[[j]]$filename)
      }    
    }
  }
  cat("\n--- Search Done! ---\n", sep = "", file = sfile, append = TRUE)
  return("Done! [Searching code for text]")
}  
