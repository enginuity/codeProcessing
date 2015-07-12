##@S This is examples / testing for the codeProcessing directories. 


# Setup -------------------------------------------------------------------
library(codeProcessing)
package_FD = FilesDescription(mode = "R", dirlist = "codeProcessing/R/")



# Default stuff to run ----------------------------------------------------
fxdoc = update_fx_documentation(FD = package_FD, test_run = FALSE)

plot_dependency(FD = package_FD, out_file = "test.pdf")
generate_todolist(FD = package_FD)



# Misc Examples -----------------------------------------------------------

search_code(RE = Regex(base = "write", isexact = FALSE, isword = FALSE), FD = package_FD)
search_code("update_stored_docu", FD = package_FD)

search_code("(default_param_doc)|(reformat_documentation)", FD = package_FD)


replace_code("write_updated_docu", replace = "update_stored_docu", add_comment = "Rename function", FD = package_FD)

