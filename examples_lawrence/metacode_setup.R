##@S This is examples / testing for the codeProcessing directories. 

## Load all available programs
library(codeProcessing)
package_FD = FilesDescription(mode = "R", dirlist = "codeProcessing/R/")

generate_todolist(FD = package_FD)
search_code(RE = Regex(base = "write", isexact = FALSE, isword = FALSE), FD = package_FD)



search_code("extract_param_docu", FD = package_FD)
# 
# examples_FD = FilesDescription(mode = "R", dirlist = c("examples/", "old_programs/"))
# find_files(examples_FD)
# 
# plot_dependency(FD = package_FD, out_file = "test.pdf")
# generate_todolist(FD = package_FD)

# 
# one_FD = FilesDescription(mode = "R", filelist = list("codeProcessing/R/h_documentation_processing.R"))
# 
# FD = package_FD
# 
# update_fx_documentation(one_FD, test_run = FALSE)

search_code("DEFAULT_FD", FD = package_FD)

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



search_code("reform_params")
gen_depend_R(".") -> test




