##@S This file contains generic text-processing functions

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
