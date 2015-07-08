##@S This are examples and usage for all network comparison folders (on my new laptop)

## Load all available programs
library(codeProcessing)

DIR = FilesDescription(dirlist = c("../smoothingGraphs/", "../netcompLib/", "../netcompSBM", "../../network-comparison/netcomp-project/"), mode = "R")

search_code("fit_SBM_v2", FD = DIR, add_comment = "Renamed to fit_SBM")

search_code("fit_SBM_v2", FD = DIR)
search_code("obtain_contribs", FD = DIR)


## TODO -- in netowrkModel & networkstruct calls, rely on Nnodes and type from set_model_params, not from separate input. does not seem like a good reason to keep it still allowed... 






