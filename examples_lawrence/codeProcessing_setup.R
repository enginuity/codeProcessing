##@S This is examples / testing for the codeProcessing directories. 


# Setup -------------------------------------------------------------------
library(codeProcessing)
package_FD = FilesDescription(mode = "R", dirlist = "codeProcessing/R/")



# Default stuff to run ----------------------------------------------------
update_fx_documentation(FD = package_FD, test_run = FALSE)

plot_dependency(FD = package_FD, out_file = "test.pdf")
generate_todolist(FD = package_FD)



# Misc Examples -----------------------------------------------------------

search_code(RE = Regex(base = "write", isexact = FALSE, isword = FALSE), FD = package_FD)
search_code("extract_param_docu", FD = package_FD)

search_code("zhdp_extractDocu", FD = package_FD)


