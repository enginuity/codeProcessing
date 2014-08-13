##@S This is examples / testing for the metacode directories. 

## Load all available programs
library(metacode)
library(stringr)

DIR = "."
log_file = "results/test.txt"

replace_code(regexp="mats", replace = "matchesL", add_comment = "Rename mats -> matchesL")


# For fixing templates
# DIR = "metacode/"
# create_roxy_templates(dir = DIR)

search_code(regexp = "search_code_matches", add_comment = "This function has new parameter (regex_exact) added")


search_code(regexp = "find_files")
search_code(regexp = "is_roxy")

search_code(regexp = "search_code", add_comment = "this function has been split (order of args changed)")

#Example -- 
#replace_code(regexp = "find_files", replace = "testtest", add_comment = "TESTING!")

search_code(regexp = "clear_comments", add_comment = "This function has been rewritten")
clear_comments()




test = roxyparam_locate(dir = "metacode/")
test$paramvalshort = substr(test$paramval, start = 1, stop = 40)
test[order(test$paramname),-which(names(test) == "paramval")]


roxyparam_subset(test, "mode")
# roxyparam_overwrite(test, "out_file", replace_text = "File to plot the dependency tree to")
# roxyparam_overwrite(test, "regex_exact", replace_text = "If TRUE: Adjusts regexp so that matches must have non-word characters before and after")
# roxyparam_overwrite(test, "comment_heads")
# roxyparam_overwrite(test, "add_comment", replace_text = "If non-NULL, this is added to the source code as a next-line comment", replace_all = TRUE)
# roxyparam_overwrite(test, "mode", replace_text = "\"R\" or \"C\" -- looks for appropriate filename extensions", replace_all = TRUE)
# roxyparam_overwrite(test, "file_regex", "If non-NULL: restrict to filenames that match this regex")
 

## TODO: [Idea] Write function to compare order of parameters (to compare for consistency across functions)



gen_depend_R(".") -> test
plot_dependency(".", mode = "R", out_file = "results/ztestplot.pdf")

plot_dependency("../27may_user_engagement/source/fxForUserchurn/R/", mode = "R", out_file = "results/znewproj.pdf")



generate_todolist(dir = "metacode/")


