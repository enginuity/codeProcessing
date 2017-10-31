##@S Functions to perform filename processing -- extract common proportions of
##  filenames


#' Shorten filenames (that contain paths)
#'
#' @param filenames Vector of filenames to shorten
#' @param maxclass Maximum number of classes of files (not implemented for
#'   maxclass != 1)
#' @param drop Indices of filenames to ignore when applying this function.
#'
#' @return A list with $shortfn = shortened function names, $classes = data
#'   frame with $path and $shortname.
#'
#' @export
#'
process_common_paths = function(filenames, maxclass = 1, drop = NULL) {
  ## TODO: [Idea] Extend functionality to allow for multiple classes of files

  if(length(filenames) == 1) {
    return(list(shortfn = filenames, classes = data.frame(
      path = "", shortname = "nochange", stringsAsFactors = FALSE)))
  }

  fnsplit = strsplit(filenames, split = "/")
  tokeep = min(sapply(seq_along(fnsplit)[-c(1, drop)],
                      function(x) {max(which(fnsplit[[1]] == fnsplit[[x]]))}))

  if (tokeep > 0) {
    common = paste(fnsplit[[1]][1:tokeep], collapse = "/")
    shortfn = gsub(pattern = common, replacement = "--", x = filenames,
                   fixed = TRUE)
  } else {
    common = ""
    shortfn = filenames
  }

  return(list(shortfn = shortfn,
              classes = data.frame(path = common, shortname = "--",
                                   stringsAsFactors = FALSE)))
}

