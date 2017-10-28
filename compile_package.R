## Scripts to install or load the package in various forms.

# Source Package ----------------------------------------------------------

# Loads needed libraries and functions. This loads all functions into the global
# environment.
require(stringr, quietly = TRUE)
for (s in list.files("codeProcessing/R/", full.names = TRUE)) { source(s) }


# Load Package as Library -------------------------------------------------
require(codeProcessing)



# Update Documentation ----------------------------------------------------
# Run this code to update the documentation files. It relies on the package, so
# it must already be built.

if (require(codeProcessing)) {
  update_fx_documentation(FD = FilesDescription(dirlist = "codeProcessing/R/"),
                          test_run = FALSE)

  # Detach the package, since the documentation is updated.
  detach(package:codeProcessing)
}


# Compile Package ---------------------------------------------------------
# Compiles the package. roxygen2 is required.

if (require(roxygen2)) {
  # Generate the documentation.
  roxygenise("codeProcessing/", clean = TRUE)

  # Install the package
  system("R CMD INSTALL codeProcessing")
} else {
  stop("Package 'roxygen2' is not installed.")
}
