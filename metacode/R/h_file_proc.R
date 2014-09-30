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
extract_Codebase = function(FilesDescription) {
  files = find_files(FilesDescription = FilesDescription) 
  code = extract_code(files)
  return(Codebase(files = files, code = code))
}


#' Find all files
#' 
#' See documentation on class FilesDescription to see how to describe a file. 
#' 
#' @param FilesDescription Object of class FilesDescription; describes files to look for
#' 
#' @return Character vector of filenames
#' 
#' @export 
#' 
find_files = function(FilesDescription) {
  if (!inherits(x = FilesDescription, "FilesDescription")) {stop("Input class is not of class FilesDescription")} 

  ## Find appropriate filename extension
  if (FilesDescription$mode == "R") {
    ext_regex = "[.]R$"
  } else if (FilesDescription$mode == "C") {
    ext_regex = "[.](c|cc|cpp|h|hh)$"
  }
  
  ## Start with exact files if any
  allfiles = FilesDescription$files
  
  ## Check file directories if any
  for(j in seq_along(FilesDescription$dirlist)) {
    temp = list.files(path = FilesDescription$dirlist[[j]]$dir, recursive = TRUE, full.names = TRUE)
    
    ## Find files with correct filename extension
    temp = temp[grep(ext_regex, temp)]
    
    ## Apply file_regex as appropriate 
    if (!is.null(FilesDescription$dirlist[[j]]$file_regex)) {
      temp = temp[grep(file_regex, temp)]
    }

    allfiles = c(allfiles, temp)
  }
  return(allfiles)
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

