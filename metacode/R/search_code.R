##@S Code for searching the entire codebase (all '.R' files) for certain text. 
##@S   Search results are output inside this directory (metadata)

## TODO: Reorganize this file, rewrite functions?

##### Function that performs search. Examples of running this will come after. 



#' ********** WARNING -- INSERTED CODE **************
#' <<BasicInfo>> 
#' 
#' @param regexp text
#' 
#' @return text
#' 
#' @export
search_code_matches = function(regexp = "Default Search", 
                                dir = DIR, mode = "R", file_regex = NULL, 
                                logged = TRUE) {
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
  if (logged) { create_search_log(logtype = "SEARCH", query = regexp, m = res) }
  
  return(res)
}

#' ********** WARNING -- INSERTED CODE **************
#' <<BasicInfo>> 
#' 
#' @param logtype text
#' @param query text
#' @param m text
#' 
#' @return text
#' 
#' @export
create_search_log = function(logtype, query, m) {
  ## This function writes a search logfile (only does this; this shall be called by search_code)
  ## m = match list
  
  log_file <<- logfile_namecreation(logtype = logtype, query = query)
  log_result('Searching for "', query, '"', "\n", header = TRUE)
  
  ## Log actual matches. 
  for(j in seq_along(m$files)) {
    ## Insert file information
    log_result(str_pad("\n", 80, 'right', "*"),str_pad("\n", 80, 'right', "*"),"\n",
               "Matches found in '", m$files[j],"'", str_pad("\n", 80, 'right', "*"), "\n")
    
    ## Insert match info
    for(k in seq_along(m$matchlines[[j]])) {
      codeline = m$matchlines[[j]][k]
      log_result(str_pad(codeline, 4, 'right', " "), "||", m$code[[j]][codeline], "\n",
                 str_pad(" ", 4, 'right', " "), "||", mark_strlocate(m$matchlocs[[j]][[k]]), "\n")
    }
  }
  log_result("\n--- Search Done! ---\n", header = TRUE)
  
  invisible(0)
}



#   ## Add comments to original file if needed
#   if (!is.null(add_comment)) {
#     replacement_code[match_lines] = paste(replacement_code[match_lines], "\n", comment_head, add_comment," --",date(), "--", sep = "")
#     writeLines(replacement_code, con = all_code[[j]]$filename)
#     
#     

#   }
















#' ********** WARNING -- INSERTED CODE **************
#' <<BasicInfo>> 
#' 
#' @param dir text
#' @param mode text
#' @param regexp text
#' @param add_comment text
#' @param file_regex text
#' @param replace text
#' @param replace_mark text
#' @param comment_head text
#' @param fixed text
#' 
#' @return text
#' 
#' @export
search_code = function(dir = DIR, mode = "R", regexp = "Default Search...", add_comment = NULL, 
                       file_regex = NULL, replace = NULL, replace_mark = TRUE, comment_head = "#|----##", fixed = FALSE) {
  ## mode: 'R' or 'C' depending on whether to look in R or C code.
  ## -- R code => looks at all .R files.
  ## -- C code => looks at all .c, .cc, .cpp, .h, .hh files.
  
  ## file_regex: a regular expression to restrict filenames to search/process
  ## add_comment: adds a next line comment to original files where the regexp is found
  
  ## replace: adds functionality to replace line of code with something else. -- USE version control!
  
  ## TODO: [Document] this function
  ## TODO: [Find code] Get my 'todo' finder...
  
  ## TODO: Split off commenting, replacement into separate function. 
  
  ## Look for all files, that match the current mode and file_regex setting, and extract code. 
  all_code = find_files(dir = dir, mode = mode, file_regex = file_regex)
  #|----##*** Modify output: instead of list of sublists with two fields (filename, code), have list of two lists: files, code --Sat Jul 12 18:47:32 2014--
  
  ## Can't replace without add-comment
  if (!is.null(replace) & is.null(add_comment)) {add_comment = "Replaced code here... "}
  
  
  ## Create savefile name
  log_file = logfile_namecreation(logtype = "SEARCH", query = regexp)
  
  ## Search files, outputting relevant information to savefile. 
  cat('Searching for "', regexp, '"', "\n", date(), "\n\n", sep = "", file = sfile, append = TRUE)
  
  for(j in seq_along(all_code)) {
    gr = gregexpr(pattern=regexp, text=all_code[[j]]$code, fixed = fixed)
    if(length(gr) > 0) {
      match_lines = which(sapply(gr, function(x) {x[1] > 0}))
      
      replacement_code = all_code[[j]]$code
      
      if (length(match_lines) > 0) {
        any_match = TRUE
        cat("\n**************************************************\n**************************************************\n", file = sfile, append = TRUE)
        cat("Matches found in '", all_code[[j]]$filename,"' \n", sep = "", file = sfile, append = TRUE)
        
        ## NO replace case
        if (is.null(replace)){
          for(k in match_lines) {
            cat(fix_length(t = k, len = 4), "||", 
                all_code[[j]]$code[k], 
                " \n", sep = "", file = sfile, append = TRUE)
            cat(fix_length(t = " ", len = 4), "||",
                mark_gregexpr_loc(gr = gr[[k]], len = nchar(all_code[[j]]$code[k])),
                "\n------\n\n", sep = "", file = sfile, append = TRUE)
          }
        } else {
          for(k in match_lines) {
            cat(fix_length(t = k, len = 4), "||", 
                all_code[[j]]$code[k], 
                " \n", sep = "", file = sfile, append = TRUE)
            cat(fix_length(t = " ", len = 4), "||",
                mark_gregexpr_loc(gr = gr[[k]], len = nchar(all_code[[j]]$code[k])),
                "\n------\n\n", sep = "", file = sfile, append = TRUE)
            if (replace_mark) {
              mark = mark_gregexpr_loc(gr = gr[[k]], len = nchar(all_code[[j]]$code[k]))
              substr(mark,1,2) <- "#|"
            }
            
            replacement_code[k] = paste(gsub(replacement_code[k], pattern=regexp, replacement=replace, fixed = TRUE), "\n", mark, sep = "")
            
            
          }
        }
        
        ## Add comments to original file if needed
        if (!is.null(add_comment)) {
          replacement_code[match_lines] = paste(replacement_code[match_lines], "\n", comment_head, add_comment," --",date(), "--", sep = "")
          writeLines(replacement_code, con = all_code[[j]]$filename)
        }
      }
    }   
  }
  
  
  cat("\n--- Search Done! ---\n", sep = "", file = sfile, append = TRUE)
  return("Done! [Searching code for text]")
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
