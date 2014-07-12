##@S This is examples / testing for the metacode directories. 

## Load all available programs
library(metacode)
library(stringr)

# For fixing templates
# DIR = "metacode/"
# create_roxy_templates(dir = DIR)

search_code(mode = "R", regexp = "extract_code", 
#|----##*** Modify output: instead of list of sublists with two fields (filename, code), have list of two lists: files, code --Sat Jul 12 18:46:57 2014--
            add_comment = "*** Modify output: instead of list of sublists with two fields (filename, code), have list of two lists: files, code")
#|----##*** This function is modified to return what used to be 'allcode', so all functions need to be edited.  --Fri Jul 11 02:42:44 2014--


