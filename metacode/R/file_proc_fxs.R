##@S This file contains function to aid with repeating file processing tasks (file-name processing, etc.)

#' ********** WARNING -- INSERTED CODE **************
#' <<BasicInfo>> 
#' 
#' @param dir text
#' @param mode text
#' @param "C" text
#' @param file_regex text
#' 
#' @return text
#' 
#' @export
find_files = function(dir = ".", mode = c("R","C"), file_regex = NULL) {
  ## TODO: [Document] this function
  res = list.files(path = dir, recursive = TRUE, full.names = TRUE)

  ## Find files with correct filename extension
  if (mode == "R") {
    matches = grep("[.]R$", res)
    comment_head = "\n  ##**##----- "
  } else if (mode == "C") {
    matches = grep("[.](c|cc|cpp|h|hh)$", res)
    comment_head = "\n  //**##----- "
  } else {
    stop("Invalid mode (Not \"R\" or \"C\"")
  }
  res = res[matches]

  ## Apply regex of file_regex when appropriate
  ## TODO: This only has the functionality ot look at the entire filename. 
  if (!is.null(file_regex)) {
    matches = grep(file_regex, res)
    res = res[matches]
  }

  return(res)
}
