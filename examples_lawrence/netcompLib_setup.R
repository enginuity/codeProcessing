##@S This is examples / testing for the netcompSBM directory. 

## Load all available programs
library(codeProcessing)

DIR = FilesDescription(dirlist = "../netcompLib/netcompLib/R/", mode = "R")

plot_dependency(FD = DIR, out_file = "test.pdf")
generate_todolist(DIR)




search_code(Regex("NetworkModel[A-Za-z]*"), FD = DIR)


test = extract_param_docu(FD = DIR)
