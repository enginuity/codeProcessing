##@S This are examples and usage for all network comparison folders (on my new laptop)

## Load all available programs
library(codeProcessing)

DIR = FilesDescription(dirlist = c("../smoothingGraphs/", "../netcompLib/", "../netcompSBM", "../../network-comparison/netcomp-project/", "../../papers/code/"), mode = "R")

replace_code("mult_pearson", replace = "mult_fisher", add_comment = "rename mult_pearson", FD = DIR)

search_code("computePval", FD = DIR, add_comment = "Change available modes (default vs default-slow)")

  




plot_dependency(FD = FilesDescription(dirlist = c("../netcompLib/")), out_file = "test.pdf")

search_code("extract_result_list", FD = DIR)




## Generate TODOlist for base network comparison
baseFD = FilesDescription(dirlist = c("../../network-comparison/netcomp-project/code/"), mode = "R")
generate_todolist(FD = baseFD)
