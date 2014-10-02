##@S Contains code for general launcher; this makes it easier than to remember all the individual functions? 


process_code = function(dir = NULL, mode, out_file = "test.pdf", regex = NULL, add_comment = NULL, replace = NULL) {
  
  modelist = c("dependencyplot", "todolist", "searchcode", "replacecode", "clearcomments", "documentationupdate")
  match = grep(mode, modelist)
  if (length(match) > 1) {
    stop("mode matches multiple possible modes")
  } else if (length(match) == 0) {
    stop("mode does not match any possible modes")
  }
  
  ## TODO: [Generalize] make this more extensible. 
  if (is.vector(dir)) {
    FD = FilesDescription(dirlist = dir)
  } else if (class(dir) == "FilesDescription") {
    FD = dir
  } else {
    stop ("Invalid choice for dir")
  }
  
  if (match == 1) {
    return(plot_dependency(FD = FD, out_file = out_file))
  } else if (match == 2) {
    return(generate_todolist(FD = FD))
  } else if (match == 3) {
    if (is.null(regex)) stop("regex cannot be null")
    return(search_code(RE = Regex(base = regex, isword = TRUE), add_comment = add_comment, FD = FD))
  } else if (match == 4) {
    if (is.null(regex)) stop("regex cannot be null")
    if (is.null(replace)) stop("replace cannot be null")
    return(replace_code(RE = Regex(base = regex, isword = TRUE), replace = replace, add_comment = add_comment, FD = FD))
  } else if (match == 5) {
    return(clear_comments(FD = FD))
  } else if (match == 6) {
    return(update_fx_documentation(FD = FD))
  }
}


