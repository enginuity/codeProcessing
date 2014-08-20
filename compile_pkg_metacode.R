library(metacode)
library(stringr)
update_fx_documentation(dir = "metacode/")
#|***********************
#|----##Rename create_roxy_temp -> update_fx_doc --Tue Aug 19 21:36:55 2014--

detach(package:metacode)

library(roxygen2)
roxygenise("metacode/", clean = TRUE)

#system("R CMD build metacode")

## For windows 
#system("R CMD check metacode_1.0.tar.gz")
system("R CMD INSTALL metacode")

## For unix
#  install.packages(pkgs="metacode_1.0.tar.gz")

library(metacode)
library(stringr)


