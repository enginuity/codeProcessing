##@S This file contains function to aid with repeating file processing tasks (file-name processing, etc.)

find_files = function(dir = ".", mode = c("R","C")) {
  ## TODO: [Document] this function
  res = list.files(path = dir, recursive = TRUE, full.names = TRUE)

  if (mode == "R") {
    matches = grep("[.]R$", res)
    comment_head = "\n  ##**##----- "
  } else if (mode == "C") {
    matches = grep("[.](c|cc|cpp|h|hh)$", res)
    comment_head = "\n  //**##----- "
  } else {
    stop("Invalid mode (Not \"R\" or \"C\"")
  }

  return(res[matches])
}
