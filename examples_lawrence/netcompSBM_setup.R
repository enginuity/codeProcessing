##@S This is examples / testing for the netcompSBM directory. 

## Load all available programs
library(codeProcessing)
library(stringr)

DIR = FilesDescription(dirlist = "../netcompSBM/")

search_code(RE = "fit_SBM", FD = DIR, add_comment = "--Reparameterizing this function")
search_code(RE = "hide_edges", FD = DIR, add_comment = "--Reparameterizing this function")
