##@S This is examples / testing for the metacode directories. 

## Load all available programs
library(metacode)
library(stringr)

DIR = "."
log_file = "results/test.txt"
# For fixing templates
# DIR = "metacode/"
# create_roxy_templates(dir = DIR)

search_code(mode = "R", regexp = "find_files", 
            add_comment = "*** Modify output: instead of list of sublists with two fields (filename, code), have list of two lists: files, code")

search_code_matches(regexp = "find_files") -> m

create_roxy_templates(dir = "metacode/", test_run = TRUE)


