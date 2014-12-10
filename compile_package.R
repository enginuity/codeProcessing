##@S Contains code to compile the netcomp_sbm package (to be run from R)


# Optional Steps ----------------------------------------------------------
## These only need to be run, if the source code was edited. 

## Load up needed libraries

if ("codeProcessing" %in% installed.packages()) {
  source("load_library.R") ## Loads this package and loads stringr
} else {
  source("load_source.R")
}

## Update documentation -- this looks for new parameters & 
update_fx_documentation(FD = FilesDescription(dirlist = "codeProcessing/R/"), fill_emptyparam = FALSE)
detach(package:codeProcessing)

# Compile Package ---------------------------------------------------------
## Both these steps need to be run in order to build the package properly

## Generate the documentation -- THIS MUST be run before building packages (since the documentation files are not version-controlled, as the version-controlled version is in the raw source code)
require(roxygen2)
roxygenise("codeProcessing/", clean = TRUE)

## Install the package
system("R CMD INSTALL codeProcessing")

