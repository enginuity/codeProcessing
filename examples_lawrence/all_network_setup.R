##@S This are examples and usage for all network comparison folders (on my new laptop)

## Load all available programs
library(codeProcessing)

DIR = FilesDescription(dirlist = c("../smoothingGraphs/", "../netcompLib/", "../netcompSBM", "../../network-comparison/netcomp-project/"), mode = "R")

search_code(Regex("smoother", isexact = FALSE), FD = DIR)
search_code("extractStruct", FD = DIR, add_comment = "Issue #26 -- destroy this function; replace with call to base NetworkStruct function.")

search_code("recycle_fitstructs", FD = DIR, add_comment = "In set_sim_param, recycle_fitstructs is renamed into fitstruct_method")

search_code("hide_edges", FD = DIR, add_comment = "Check parameter usage of invert_template. It's been converted into a logical argument, relying on input of template")

search_code("NetworkModelPair", FD = DIR, add_comment = "Parameters changed")


plot_dependency(FD = FilesDescription(dirlist = c("../netcompLib/")), out_file = "test.pdf")


search_code("extract_result_list", FD = DIR)




## Generate TODOlist for base network comparison
baseFD = FilesDescription(dirlist = c("../../network-comparison/netcomp-project/code/"), mode = "R")
generate_todolist(FD = baseFD)
