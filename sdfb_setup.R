##@S This is examples of how to run this for the netcomp project.

DIR = "../sdfb_network/"

## Load all available programs
library(stringr)
library(metacode)



replace_code(regexp = "(../../)*private_data/odnb_data_proc/ODNB_metadata.Rdata", replace = "data/ODNB_raw/ODNB_metadata20140404.Rdata", add_comment = "Fix old directory structure")

clear_comments()
