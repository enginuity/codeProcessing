## Helper functions for processing documentation


#' Compute mode of text ignoring certain values
#' 
#' Ignores all values that are equal to "temp", and computes the mode of the remaining text values
#' (or ignores all values in 'ignore_type')
#' 
#' @param text Character vector whose mode is desired
#' @param ignore_type Character vector of elements to ignore. 
#' 
#' @return Mode of 'text'
#' 
#' @export
#' 
Mode_nontemp = function(text, ignore_type = "temp") {
  
  t = text[!(text %in% ignore_type)]
  if (length(t) == 0) { return(ignore_type[1]) }
  
  tab = table(t)
  return(names(tab)[which(tab == max(tab))[1]])
}


