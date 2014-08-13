##@S This file contains function to aid with repeating file processing tasks (file-name processing, etc.)


#' Find all files with appropriate file extensions and extract code
#' 
#' @param dir Directory to search recursively for code files
#' @param mode "R" or "C" -- looks for appropriate filename extensions
#' @param file_regex If non-NULL: restrict to filenames that match this regex
#' 
#' @return list : $files, $code
#' 
#' @export
#' 
extract_all_code = function(dir = ".", mode = c("R","C"), file_regex = NULL) {
  files = find_files(dir = dir, mode = mode, file_regex = file_regex) 
  code = extract_code(res)
  return(list(files = files, code = code))
}


#' Find all files with appropriate file extensions
#' 
#' @param dir Directory to search recursively for code files
#' @param mode "R" or "C" -- looks for appropriate filename extensions
#' @param file_regex If non-NULL: restrict to filenames that match this regex
#' 
#' @return Character vector of filenames
#' 
#' @export
#' 
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
    ## TODO: [Idea] This only has the functionality ot look at the entire filename.
    ## TODO: -- Should this be edited to only look at the last portion of the filename?
    matches = grep(file_regex, res)
    res = res[matches]
  }

  return(res)
}


#' Extracts code for each file input
#' 
#' @param files Vector of filenames
#' 
#' @return List of code files
#' 
#' @export
#' 
extract_code = function(files) {
  res = list()
  for(j in seq_along(files)) {
    res[[j]] = readLines(files[j])
  }
  return(res)
}

