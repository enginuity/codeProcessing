## Try using existing functions to extract latex information

library(codeProcessing)

test = search_code(RE = Regex("label[{].*[}]"), 
                   FD = FilesDescription(mode = "all", filelist = "../papers/pnas_edgepartition/netcomp_edgepart_SI.tex"))

# search_code needs a way to have outputs savable for further processing... 

## Things desired 
# a way to extract all labels and where they are referenced
# a way to extract all new commands and where they are used
# -> if these can be generated into a latex file, all the better... 


