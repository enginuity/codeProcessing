library(metacode)
library(stringr)
create_roxy_templates(dir = "metacode/")

detach(package:metacode)

library(roxygen2)
roxygenise("metacode/", clean = TRUE)

system("R CMD build metacode")

## For windows 
#system("R CMD check metacode_1.0.tar.gz")
system("R CMD INSTALL metacode")

## For unix
#  install.packages(pkgs="metacode_1.0.tar.gz")

library(metacode)
library(stringr)


