##@S Functions to perform filename processing -- extract common proportions of filenames

## TODO: [Documentation-AUTO] Check/fix Roxygen2 Documentation (process_common_paths)
## TODO: [Documentation-AUTO] Check/fix Roxygen2 Documentation (process_common_paths)
#' <<BasicInfo>> 
#' 
#' @param filenames temp
#' @param maxclass temp
#' @param drop temp
#' 
#' @return temp
#' 
#' @export
#' 
process_common_paths = function(filenames, maxclass = 1, drop = NULL) {
  ## Returns a list: 
  #  $shortfn = shortened fn's
  #  $classes = data frame with $path & $shortname
  ## drop = drop indices of filenames as necessary
  
  ## TODO: [Idea] Extend functionality to allow for multiple classes of files
  
  if(length(filenames) == 1) {
    return(list(shortfn = filenames, classes = data.frame(path = "", shortname = "nochange", stringsAsFactors = FALSE)))
  }
  
  fnsplit = strsplit(filenames, split = "/")
  tokeep = min(sapply(seq_along(fnsplit)[-c(1, drop)], function(x) {max(which(fnsplit[[1]] == fnsplit[[x]]))}))
  
  if (tokeep > 0) {
    common = paste(fnsplit[[1]][1:tokeep], collapse = "/")
    shortfn = gsub(pattern = common, replacement = "--", x = filenames, fixed = TRUE)
  } else {
    common = ""
    shortfn = filenames
  }
  
  return(list(shortfn = shortfn, 
         classes = data.frame(path = common, shortname = "--", stringsAsFactors = FALSE)))
}

