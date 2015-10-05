##@S This is examples / testing for the netcompSBM directory. 

## Load all available programs
library(codeProcessing)

DIR = FilesDescription(dirlist = "../netcompLib/netcompLib/R/", mode = "R")




z = plot_dependency(FD = DIR, out_file = "test.pdf")
generate_todolist(DIR)


clean_function_output = function(out, filename = "fxlistout.csv") {
  ## out is from plot_dependency
  res = data.frame(FxName = out$function_table$name, File = out$function_table$filename, stringsAsFactors = FALSE)
  res = res[-nrow(res),]
  
  splittext = strsplit(res$File, split = "/")
  maxcommon = min(sapply(splittext[-1], function(x) { max(which(x == splittext[[1]])) }))
  res$File = sapply(splittext, function(x) { x[-(1:maxcommon)]})
  write.csv(res, file = filename)
}
clean_function_output(z)

search_code(Regex("NetworkModel[A-Za-z]*"), FD = DIR)

search_code("(abind)|(logit)|(ilogit)", FD = DIR)


test = extract_param_docu(FD = DIR)
