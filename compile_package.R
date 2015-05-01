##@S Contains code to compile the codeProcessing package (to be run from R)

# Update Documentation (if needed/possible) -------------------------------

## This section only needs to be run if the source code is not necessarily up to date (ie new functions were written, or if parameters were added). It will also only be run if the package actually exists. 
if (require(codeProcessing)) {
  ## This is a package I've written to speed up my own coding efficiency to use when rewriting code / package writing
  
  ## Update documentation -- this looks for new parameters and creates documentation lines for it
  update_fx_documentation(FD = FilesDescription(dirlist = "codeProcessing/R/"), test_run = FALSE)
  
  if (FALSE) { ## Compile without exporting everything?
    update_fx_documentation(FD = FilesDescription(dirlist = "codeProcessing/R/"), test_run = FALSE, regexp_noexport = c("zhdp"))
  }
  detach(package:codeProcessing)
}

# Compile Package ---------------------------------------------------------

if (require(roxygen2)) {
  ## Generate the documentation -- THIS MUST be run before building packages (since the documentation files are not version-controlled, as the version-controlled version is in the raw source code)
  roxygenise("codeProcessing/", clean = TRUE)
  
  ## Install the package
  system("R CMD INSTALL codeProcessing")
} else {
  stop("Package 'roxygen2' is not installed: the codeProcessing package is not compilable.")
}


## This line is here for easy calling of the library. 
if (FALSE) {
  library(codeProcessing)
}
