##@S This is examples / testing for the metacode directories. 

## Load all available programs
library(metacode)
library(stringr)

DIR = "."
log_file = "results/test.txt"
# For fixing templates
# DIR = "metacode/"
# create_roxy_templates(dir = DIR)

search_code(regexp = "find_files")

search_code(regexp = "search_code", add_comment = "this function has been split (order of args changed)")

#Example -- 
#replace_code(regexp = "find_files", replace = "testtest", add_comment = "TESTING!")

search_code(regexp = "clear_comments", add_comment = "This function has been rewritten")
clear_comments()











