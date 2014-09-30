##@S This is examples / testing for the metacode directories. 

## Load all available programs
library(metacode)
library(stringr)

package_FD = FilesDescription(mode = "R", dirlist = "metacode/R/")
examples_FD = FilesDescription(mode = "R", dirlist = c("examples/", "old_programs/"))
find_files(examples_FD)

replace_code(RE = "write_MatchedCodebase", replace = "write_MatchedCodebase", add_comment = "Rename write_MatchedCodebase")
clear_comments()

search_code(Regex("search_code_matches"))
search_code("create_search_log")

search_code("extract_param_docu")


search_code("log_result")
replace_code("MCB", replace = "MCB", add_comment = "Rename MCB -> MCB")

replace_code(regexp="mats", replace = "MCB", add_comment = "Rename mats -> MCB")





test$paramvalshort = substr(test$paramval, start = 1, stop = 40)
test[order(test$paramname),-which(names(test) == "paramval")]



update_param_docu(test, "regexp_fxstart")

update_param_docu(test, "logtype", "Type-identifier to be added to the logfile name", replace_all = TRUE)
update_param_docu(test, "logtype", "Type-identifier to be added to the logfile name", replace_all = TRUE)
update_param_docu(test, "MCB", "Regex match list : this is output of search-code-matches", replace_all = TRUE)


## TODO: [Idea] Write function to compare order of parameters (to compare for consistency across functions)


search_code(regexp = "search_code")
gen_depend_R(".") -> test

plot_dependency(".", mode = "R", out_file = "results/ztestplot.pdf")

plot_dependency("../27may_user_engagement/source/fxForUserchurn/R/", mode = "R", out_file = "results/znewproj.pdf")



generate_todolist(dir = "metacode/")


