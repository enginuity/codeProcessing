##@S This is examples / testing for the metacode directories. 

## Load all available programs
library(metacode)
library(stringr)

package_FD = FilesDescription(mode = "R", dirlist = "metacode/R/")
examples_FD = FilesDescription(mode = "R", dirlist = c("examples/", "old_programs/"))
find_files(examples_FD)

one_FD = FilesDescription(mode = "R", filelist = list("metacode/R/h_documentation_processing.R"))


update_fx_documentation(one_FD, test_run = FALSE)

search_code("process_documentation")

search_code("update_fx_documentation")

replace_code("find_all_prev_documentation_v2", replace = "find_all_prev_documentation", add_comment = "Remove _v2 from name", FD = package_FD)
replace_code("reformat_documentation_v2", replace = "reformat_documentation", add_comment = "Remove _v2 from name", FD = package_FD)

replace_code("update_fx_documentation_v2", replace = "update_fx_documentation", add_comment = "Remove _v2 from name", FD = package_FD)
clear_comments(FD = package_FD)

search_code("test")




test$paramvalshort = substr(test$paramval, start = 1, stop = 40)
test[order(test$paramname),-which(names(test) == "paramval")]



update_param_docu(test, "regexp_fxstart")

update_param_docu(test, "logtype", "Type-identifier to be added to the logfile name", replace_all = TRUE)
update_param_docu(test, "logtype", "Type-identifier to be added to the logfile name", replace_all = TRUE)
update_param_docu(test, "MCB", "Regex match list : this is output of search-code-matches", replace_all = TRUE)


## TODO: [Idea] Write function to compare order of parameters (to compare for consistency across functions)


search_code(regexp = "search_code")
gen_depend_R(".") -> test

plot_dependency(FD = package_FD, out_file = "test.pdf")

generate_todolist(FD = package_FD)


