##@S This file contains function to aid with repeating file processing tasks (file-name processing, etc.)


#' Find all files with appropriate file extensions and extract code
#' 
#' @param FD Object of class FilesDescription; See documentation to see how to describe a collection of files  
#' 
#' @return Object of class Codebase
#' 
#' @export
#' 
extract_Codebase = function(FD) {
  files = find_files(FD = FD) 
  code = extract_code(files)
  return(Codebase(files = files, code = code))
}


#' Find all files
#' 
#' See documentation on class FilesDescription to see how to describe a file. 
#' 
#' @param FD Object of class FilesDescription; See documentation to see how to describe a collection of files
#' 
#' @return Character vector of filenames
#' 
#' @export 
#' 
find_files = function(FD) {
  if (!inherits(x = FD, "FilesDescription")) {stop("Input class is not of class FilesDescription")} 

  ## Find appropriate filename extension
  if (FD$mode == "R") {
    ext_regex = "[.]R$"
  } else if (FD$mode == "C") {
    ext_regex = "[.](c|cc|cpp|h|hh)$"
  }
  
  ## Start with exact files if any
  allfiles = FD$files
  
  ## Check file directories if any
  for(j in seq_along(FD$dirlist)) {
    temp = list.files(path = FD$dirlist[[j]]$dir, recursive = TRUE, full.names = TRUE)
    
    ## Find files with correct filename extension
    temp = temp[grep(ext_regex, temp)]
    
    ## Apply file_regex as appropriate 
    if (!is.null(FD$dirlist[[j]]$file_regex)) {
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

