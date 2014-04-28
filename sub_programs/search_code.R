##@S Code for searching the entire codebase (all '.R' files) for certain text. 
##@S   Search results are output inside this directory (metadata)


##### Function that performs search. Examples of running this will come after. 
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

search_code = function(dir = ".", mode = c("R", "C"), regexp = "Default Search...", add_comment = NULL, file_regex = NULL) {
  ## mode: 'R' or 'C' depending on whether to look in R or C code.
  ## -- R code => looks at all .R files.
  ## -- C code => looks at all .c, .cc, .cpp, .h, .hh files.
  
  ## file_regex: a regular expression to restrict filenames to search/process
  ## add_comment: adds a next line comment to original files where the regexp is found
  
  ## TODO: [Document] this function
  ## TODO: [Find code] Get my 'todo' finder...

  ## Look for all files, that match the current mode and file_regex setting
  allfiles = find_files(dir = dir, mode = mode, file_regex = file_regex)

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
    gr = gregexpr(regexp, all_code[[j]]$code)
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

      ## Add comments to original file if needed
      if (!is.null(add_comment)) {
        replacement_code = all_code[[j]]$code
        replacement_code[match_lines] = paste(replacement_code[match_lines], comment_head, add_comment," --",date(), "--", sep = "")
        writeLines(replacement_code, con = all_code[[j]]$filename)
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
