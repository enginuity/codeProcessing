##@S Code for searching the entire codebase (all '.R' files) for certain text. 
##@S   Search results are output inside this directory (metadata)

## TODO: Reorganize this file, rewrite functions?

##### Function that performs search. Examples of running this will come after. 
#' ********** WARNING -- INSERTED CODE **************
#' <<BasicInfo>> 
#' 
#' @param gr text
#' @param len text
#' 
#' @return text
#' 
#' @export
mark_gregexpr_loc = function(gr, len) {
  # given gregexpr entry (gr is not a list; rather a vector with attributes)
  # outputs a line with " " where no match, and "-" where match. 
  # len = total length of string
  
  # TODO: [Move] function?
  # TODO: [Document] function
  res = create_space_chars(v = len)
  for(j in 1:length(gr)) {
    substr(res, start = gr[j], stop = (gr[j] + attr(gr, "match.length")[j] - 1)) <- create_space_chars(v = len, ch = "*")
  }
  return(res)
}

#' ********** WARNING -- INSERTED CODE **************
#' <<BasicInfo>> 
#' 
#' @param dir text
#' @param mode text
#' @param "C" text
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
search_code = function(dir = ".", mode = c("R", "C"), regexp = "Default Search...", add_comment = NULL, 
                       file_regex = NULL, replace = NULL, replace_mark = TRUE, comment_head = "#|----##", fixed = FALSE) {
  ## mode: 'R' or 'C' depending on whether to look in R or C code.
  ## -- R code => looks at all .R files.
  ## -- C code => looks at all .c, .cc, .cpp, .h, .hh files.
  
  ## file_regex: a regular expression to restrict filenames to search/process
  ## add_comment: adds a next line comment to original files where the regexp is found
  
  ## replace: adds functionality to replace line of code with something else. -- USE version control!
  
  ## TODO: [Document] this function
  ## TODO: [Find code] Get my 'todo' finder...
  
  ## Look for all files, that match the current mode and file_regex setting
  allfiles = find_files(dir = dir, mode = mode, file_regex = file_regex)
  
  ## Can't replace without add-comment
  if (!is.null(replace) & is.null(add_comment)) {add_comment = "Replaced code here... "}
  
  ## Load in all relevant code files
  all_code = list()
  for(j in seq_along(allfiles)) {
    all_code[[j]] = list(filename = allfiles[j],
                         code = readLines(allfiles[j])   )
  }
  
  ## Create savefile name
  sfile = paste("results/zSEARCH_", gsub("[^[:alnum:]]", "", regexp),"_", format(Sys.time(), "%Y%m%d-%H%M%S"), ".txt", sep = "")
  
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
  allfiles = find_files(dir = dir, mode = mode, file_regex = file_regex)
  
  ## Load in all relevant code files
  all_code = list()
  for(j in seq_along(allfiles)) {
    all_code[[j]] = list(filename = allfiles[j],
                         code = readLines(allfiles[j])   )
  }
  
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



##### Examples:

#search_code(mode = "R", regexp = "sim_one_set", add_comment = "Test comment")
##**##----- Test comment --Sun Apr 27 19:56:29 2014--
#search_code(mode = "C", regexp = "set_sample", add_comment = "test comment")
