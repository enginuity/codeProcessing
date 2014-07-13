##@S This file contains generic text-prettifying functions


#' Creates a character vector of a varying amount of 'ch' characters
#' This is mainly used to space out text for pretty displaying (in printing dataframes)
#' 
#' Example: If v = c(5,1), returns c("     ", " ")
#' 
#' @param v numeric vector
#' @param ch character to repeat
#' 
#' @return character vector of length(v)
#' 
#' @export
create_space_chars = function(v, ch = " ") {
  toreturn = as.character(v)
  for (j in 1:length(v)) {
    toreturn[j] = paste(rep(ch, times = v[j]), collapse = "")
  }
  return(toreturn)
}


#' For elements of \code{t}, adjust its number of characters by prepending/appending/removing
#' 
#' This is obseleted by 'str_pad' in stringr?
#' 
#' @param t character vector to extend or shorten
#' @param len target length
#' @param at_end TRUE/FALSE: modify entries at beginning or at the end
#' 
#' @return character vector, each with nchar equal to \code{len}
#' 
#' @export
fix_length = function(t, len, at_end = TRUE) {  
  fix_length_single = function(t_single, len, at_end = TRUE) {
    # Does fix_length for a singleton
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


#' Mark regex match locations
#' 
#' Given a gregexpr result, outputs a line, filling "*"s where it matches. 
#' 
#' @param gr NOT list; single gregexpr output
#' @param len total length of string corresponding to gregexpr output
#' @param fill_char single character to fill. 
#' 
#' @return character, denoting where the matches are. 
#' 
#' @export
mark_gregexpr_loc = function(gr, len, fill_char = "*") {
  res = create_space_chars(v = len)
  for(j in 1:length(gr)) {
    substr(res, start = gr[j], stop = (gr[j] + attr(gr, "match.length")[j] - 1)) <- create_space_chars(v = len, ch = fill_char)
  }
  return(res)
}

#' Like mark_gregexpr_loc, except for str_locate output. 
#' 
#' @param j single entry str_locate output (corresponding to one vector, so a matrix with two columns, but any # rows
#' @param fill what to use as indicator text
#' 
#' @return character singleton that denotes where in the code the matches were. 
#' 
#' @export
mark_strlocate = function(j, fill = "*") {
  res = str_dup(" ", max(j[,2]))
  for(k in nrow(j)) {
    str_sub(res, j[k,1], j[k,2]) <- str_dup(fill, j[k,2] - j[k,1]+1)
  }
  return(res)
}
