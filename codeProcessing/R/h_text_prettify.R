##@S This file contains generic text-prettifying functions


## TODO: [Update] This function probably needs update to handle stringr::str_locate_all? is there a bug with this?
#' Marks the location of stringr::str_locate output 
#' 
#' @param strLoc Single entry stringr::str_locate output (corresponding to one vector, so a matrix with two columns, but any # rows)
#' @param marker What to use as indicator text
#' 
#' @return String with mostly spaces, except with matching locations marked
#' 
#' @export
#' 
mark_strlocate = function(strLoc, marker = "*") {
  res = stringr::str_dup(" ", max(strLoc[,2]))
  for(k in nrow(strLoc)) {
    stringr::str_sub(res, strLoc[k,1], strLoc[k,2]) <- stringr::str_dup(marker, strLoc[k,2] - strLoc[k,1]+1)
  }
  return(res)
}

