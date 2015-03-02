## Helper functions for searching for enclosed text


#' Searches for enclosed text
#'    
#' Looks for the next open/close parethesis set, accounting for parenthesis embedding
#' Allows for arbitrary types of open/close parens. 
#' 
#' @param text Source code
#' @param startlocation A vector(lineno, charpos) denoting where to start searching
#' @param regex_s Regex to find 'open-paren'
#' @param regex_e Regex to find 'close-paren'
#' 
#' @return Content of next open/close paren set
#' 
#' @export
#' 
find_single_enclosed = function(text, startlocation, regex_s = "[(]", regex_e = "[)]") {
  ## text is a vector; this looks for things that span multiple entries. 
  ## 
  starts = stringr::str_locate_all(text, regex_s)
#|         ***********************
#|----##use stringr package call implicitly --Mon Mar 02 00:10:35 2015--
  ends = stringr::str_locate_all(text, regex_e)  
#|       ***********************
#|----##use stringr package call implicitly --Mon Mar 02 00:10:35 2015--
  
  lineno = startlocation[1]
  charno = startlocation[2]
  
  locs = find_next_location(starts, lineno, charno)
  loce = find_next_location(ends, lineno, charno)
  
  firstloc = find_next_location(starts, lineno, charno)
  lineno = firstloc[1]
  charno = firstloc[2] + 1
  embed = 1
  
  while (embed > 0) {
    locs = find_next_location(starts, lineno, charno)
    loce = find_next_location(ends, lineno, charno)
    if (compare_locations(locs, loce) == 1) {
      embed = embed + 1
      lineno = locs[1]
      charno = locs[2] + 1
    } else if (compare_locations(locs, loce) == 2) {
      embed = embed - 1
      lineno = loce[1]
      charno = loce[2] + 1
    } else {
      stop("Error in compare_locations: didn't return 1 or 2")
    }
  }
  endloc = c(lineno, charno - 1)
  
  return(gsub(" +", " ", extract_location_pair(text, start = firstloc, end = endloc)))
}


#' Extracts all text between the starting and ending vector pairs.
#' 
#' @param text Source code
#' @param start Starting vector(lineno, charno) pair
#' @param end Ending vector(lineno, charno) pair
#' 
#' @return All text (as character singleton) between starting/ending pairs
#' 
#' @export
#' 
extract_location_pair = function(text, start, end) {
  if (start[1] == end[1]) {
    return(substr(text[start[1]],start=start[2], stop = end[2]))
  } else if (start[1] + 1 == end[1]) {
    ## gap of 1
    return(paste(sep = "", 
                 substr(text[start[1]], start=start[2], stop = nchar(text[start[1]])),
                 substr(text[end[1]], start = 1, stop = end[2])
    ))
  } else {
    return(paste(sep = "", 
                 substr(text[start[1]], start=start[2], stop = nchar(text[start[1]])),
                 paste(text[(start[1]+1):(end[1]-1)], sep = "", collapse = ""),
                 substr(text[end[1]], start = 1, stop = end[2])
    ))
  }
}


#' Finds the next location in the list
#' 
#' @param loc_list List of possible locations
#' @param ln Line number
#' @param cn Character number
#' 
#' @return Location pair (Line,Character) which is after the given (ln,cn) pair
#' 
#' @export
#' 
find_next_location = function(loc_list, ln, cn) {
  same_line_match = which(loc_list[[ln]][,1] >= cn)
  if (length(same_line_match) > 0) {
    return(c(ln, loc_list[[ln]][min(same_line_match),1]))
  } else {
    ln_match = which(sapply(loc_list, length) > 0)
    ln_match = ln_match[ln_match > ln]
    if (length(ln_match) == 0) {
      return(NA)
    } else {
      return(c(ln_match[1], loc_list[[ln_match[1]]][1,1]))
    }
  }
}



#' Compares locations
#'   
#' Returns the earlier location (loc1 or loc2, represented by 1 or 2)
#' 
#' @param loc1 Location (Line, Character)
#' @param loc2 Location (Line, Character)
#' 
#' @return Either '1' or '2', whichever is earlier in the document
#' 
#' @export
#' 
compare_locations = function(loc1, loc2) {
  ## Returns the earlier location
  if (loc1[1] == loc2[1]) {
    if (loc1[2] > loc2[2]) {
      return(2)
    } else {
      return(1)
    }
  } else if (loc1[1] > loc2[1]) {
    return(2)
  } else {
    return(1)
  }
}


#' Extracts the text within the next 'regex_s -- regex_e' pair, for each startlocation
#' 
#' @param text Source code
#' @param startlocations Matrix of start positions (Line, Character)
#' @param regex_s Regex match for start of enclosed section
#' @param regex_e Regex match for end of enclosed section
#' 
#' @return Character singleton of enclosed text
#' 
#' @export
#' 
find_all_enclosed = function(text, startlocations, regex_s = "[(]", regex_e = "[)]") {
  N = nrow(startlocations)
  res = rep("", N)
  for(j in 1:N) {
    res[j] = find_single_enclosed(text=text, startlocation=startlocations[j,], regex_s=regex_s, regex_e = regex_e)
  }
  return(res)
}


