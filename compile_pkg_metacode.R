## TODO: [Generalize] This code here is intended to rebuild the package, assuming the package already exists. This should probably be more documented. 


library(metacode)
library(stringr)

update_fx_documentation(FD = FilesDescription(dirlist = "metacode/"))

detach(package:metacode)

library(roxygen2)
roxygenise("metacode/", clean = TRUE)

#system("R CMD build metacode")

## For windows 
#system("R CMD check metacode_1.0.tar.gz")
system("R CMD INSTALL metacode")

## For unix
## install.packages(pkgs="metacode_1.0.tar.gz")

library(metacode)
library(stringr)


