##@S This is examples / testing for the codeProcessing directories. 

## Load all available programs
library(codeProcessing)
package_FD = FilesDescription(mode = "R", dirlist = "codeProcessing/R/")

update_fx_documentation(FD = package_FD, test_run = FALSE)



## Functions to remove
search_code("find_current_params", FD = package_FD)
search_code("find_all_prev_headers", FD = package_FD)
search_code("find_all_prev_documentation", FD = package_FD)
search_code("reformat_documentation", FD = package_FD)





plot_dependency(FD = package_FD, out_file = "test.pdf")
generate_todolist(FD = package_FD)



## Examples
search_code(RE = Regex(base = "write", isexact = FALSE, isword = FALSE), FD = package_FD)



search_code("extract_param_docu", FD = package_FD)



# examples_FD = FilesDescription(mode = "R", dirlist = c("examples/", "old_programs/"))
# find_files(examples_FD)
# 
# 
# update_param_docu(test, "MCB", "Regex match list : this is output of search-code-matches", replace_all = TRUE)



