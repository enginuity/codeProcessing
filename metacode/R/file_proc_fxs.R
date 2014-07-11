##@S This file contains function to aid with repeating file processing tasks (file-name processing, etc.)


#' Find all files with appropriate file extensions
#' 
#' @param dir what directory to search for (and all sub-directories)
#' @param mode "R" or "C" -- looks for appropriate filename extensions
#' @param file_regex if non-NULL, applies this regular expression to the filenames
#' 
#' @return vector of all appropriate filenames 
#' 
#' @export
find_files = function(dir = ".", mode = c("R","C"), file_regex = NULL) {
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
  if (!is.null(file_regex)) {
    ## TODO: This only has the functionality ot look at the entire filename.
    ## TODO: -- Should this be edited to only look at the last portion of the filename?
    matches = grep(file_regex, res)
    res = res[matches]
  }

  return(res)
}


extract_code = function(files) {
  res = list()
  for(j in seq_along(files)) {
    res[[j]] = list(filename = files[j], code = readLines(files[j]))
  }
  return(res)
}

