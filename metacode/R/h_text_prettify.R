##@S This file contains generic text-prettifying functions



#' Creates a character vector of a varying amount of 'ch' characters
#'   
#' This is mainly used to space out text for pretty displaying (in printing dataframes)
#' Example: If v = c(5,1), returns c("     ", " ")
#' 
#' @param v Numeric vector
#' @param ch Character to repeat
#' 
#' @return Character vector of length(v)
#' 
#' @export
#' 
create_space_chars = function(v, ch = " ") {
  toreturn = as.character(v)
  for (j in 1:length(v)) {
    toreturn[j] = paste(rep(ch, times = v[j]), collapse = "")
  }
  return(toreturn)
}




## TODO: [Documentation-AUTO] Check/fix Roxygen2 Documentation (mark)
#' Like mark_gregexpr_loc, except for str_locate output. 
#' 
#' @param s single entry str_locate output (corresponding to one vector, so a matrix with two columns, but any # rows
#' @param fill what to use as indicator text
#' 
#' @return character singleton that denotes where in the code the matches were. 
#' 
#' @export
#' 
mark_strlocate = function(s, fill = "*") {
  res = str_dup(" ", max(s[,2]))
  for(k in nrow(s)) {
    str_sub(res, s[k,1], s[k,2]) <- str_dup(fill, s[k,2] - s[k,1]+1)
  }
  return(res)
}

