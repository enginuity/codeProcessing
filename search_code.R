#@S Code for searching the entire codebase (all '.R' files) for certain text. 
#@S   Search results are output inside this directory (metadata)

## Jump up to base directory
cur_dir = getwd()
setwd(gsub("/metacode.*$", "", cur_dir))


##### Preliminary code: Function dependencies copied:
create_space_chars = function(v, ch = " ") {
  #@F ----------------------------------------
  #@F Function 'create_space_chars'
  #@F Args (Input): v = numeric vector, ch = character. length 1. 
  #@F Purpose: Creates a character vector of a varying amount of 'ch' characters; this is 
  #@F   mainly used to space out text for pretty displaying (in printing dataframes)
  #@F Example: if v = c(5,1), returns c("     ", " ")
  #@F Output: character vector, length(v)
  #@F ----------------------------------------
  
  toreturn = as.character(v)
  for (j in 1:length(v)) {
    toreturn[j] = paste(rep(ch, times = v[j]), collapse = "")
  }
  return(toreturn)
}

fix_length = function(t, len, at_end = TRUE) {  
  #@F ----------------------------------------
  #@F Function 'fix_length'
  #@F Args (Input): t = character vector
  #@F   len = target length
  #@F   at_end = logical; modify entries at the beginning or at the end?
  #@F Purpose: For all elements of 't', its number of characters is adjusted to be 
  #@F   'len' either by removing characters or adding spaces, based on 'at_end'
  #@F Output: character singleton
  #@F ----------------------------------------

  fix_length_single = function(t_single, len, at_end = TRUE) {
    # This works for a single character singleton t
    t_len = nchar(t_single)
    if (t_len < len) {
      if (at_end) {
        return(paste(c(t_single, 
                       rep(" ", times = len - t_len)), 
                     collapse = "") )
      } else {
        return(paste(c(rep(" ", times = len - t_len), 
                       t_single), 
                     collapse = "") )
      }
    } else if (t_len > len) {
      if (at_end) {
        return(substr(t_single, start = 1, stop = len))
      } else {
        return(substr(t_single, start = t_len - len + 1, stop = t_len))
      }
    } else {
      return(as.character(t_single))
    }
  }
  return(vapply(t, FUN = fix_length_single, FUN.VALUE = 'character', len = len, at_end = at_end))
}


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

search_code = function(mode = c("R", "C"), regexp = "Default Search...", add_comment = NULL, file_regex = NULL) {
  ## mode: 'R' or 'C' depending on whether to look in R or C code.
  ## -- R code => looks at all .R files.
  ## -- C code => looks at all .c, .cc, .cpp, .h, .hh files.
  
  ## file_regex: a regular expression to restrict filenames to search/process
  ## add_comment: adds a next line comment to original files where the regexp is found
  
  ## TODO: [Document] this function
  ## TODO: [Find code] Get my 'todo' finder...
  

  ## Look for all files, that match the current mode
  allfiles = list.files(recursive = TRUE)

  if (mode == "R") {
    matches = grep("[.]R$", allfiles)
    comment_head = "\n  ##**##----- "
  } else if (mode == "C") {
    matches = grep("[.](c|cc|cpp|h|hh)$", allfiles)
    comment_head = "\n  //**##----- "
  } else {
    stop("Invalid mode (Not \"R\" or \"C\"")
  }
  
  ## If file_regex is set, searchthis regular expression in the remaining files.
  ## TODO: Code here is not efficient (searches all files, not just the previously matched. Fix? low priority. 
  if (!is.null(file_regex)) {
    matches = intersect(matches, grep(file_regex, allfiles))
  }

  ## Load in all relevant code files
  all_code = list()
  for(j in seq_along(matches)) {
    all_code[[j]] = list(filename = allfiles[matches[j]],
              code = readLines(allfiles[matches[j]])   )
  }
  
  ## Create savefile name
  sfile = paste("metacode/zSEARCH_", gsub("[^[:alnum:]]", "", regexp),"_", format(Sys.time(), "%Y%m%d-%H%M%S"), ".txt", sep = "")

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
  return("Done!")
}


##### Examples:

#search_code(mode = "R", regexp = "sim_one_set", add_comment = "Test comment")
  ##**##----- Test comment --Sun Apr 27 19:56:29 2014--
#search_code(mode = "C", regexp = "set_sample", add_comment = "test comment")
