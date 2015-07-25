##@S This are examples and usage for all network comparison folders (on my new laptop)

## Load all available programs
library(codeProcessing)

DIR = FilesDescription(dirlist = c("../smoothingGraphs/", "../netcompLib/", "../netcompSBM", "../../network-comparison/netcomp-project/"), mode = "R")

search_code("fast_compute_pval", FD = DIR, add_comment = "Function obseleted. These simulations should use newer code (computePval/sim_hyptest) instead...")

search_code("compute_pval_multtrees", FD = DIR, add_comment = "Function deleted. These simulations should be replaced with newer code...")

search_code("compute_pval", FD = DIR, add_comment = "Function obseleted. These simulations should use newer code (computePval/sim_hyptest) instead...")

search_code("computeDfAdj", FD = DIR, add_comment = "** MUST EDIT -- TODO -- hidden_nodes parameter renamed to hidden_edges")


plot_dependency(FD = FilesDescription(dirlist = c("../netcompLib/")), out_file = "test.pdf")


search_code("extract_result_list", FD = DIR)

## TODO -- in netowrkModel & networkstruct calls, rely on Nnodes and type from set_model_params, not from separate input. does not seem like a good reason to keep it still allowed... 






