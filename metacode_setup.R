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
search_code(regexp = "is_roxy")

search_code(regexp = "search_code", add_comment = "this function has been split (order of args changed)")

#Example -- 
#replace_code(regexp = "find_files", replace = "testtest", add_comment = "TESTING!")

search_code(regexp = "clear_comments", add_comment = "This function has been rewritten")
clear_comments()




test = roxyparam_locate(dir = "metacode/")
roxyparam_subset(test, "dir")
roxyparam_overwrite(test, "dir")
roxyparam_overwrite(test, "dir", "Directory to search recursively for code files")
roxyparam_overwrite(test, "file_regex", "If non-NULL: restrict to filenames that match this regex")


## TODO: [Idea] Write function to compare order of parameters (to compare for consistency across functions)





