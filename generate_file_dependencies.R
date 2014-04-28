#@S This file is a script that tries to extract all file dependencies in the code
#@S  and outputs the entries --> 'zGEN_filedep.txt'
#@L It extracts from each R-script file: specifically all <.R> files. 

# TODO: [somewhathighpriority] Write this script 
# TODO: Remove irrelevant code in this body

allfiles = list.files(recursive = TRUE)
matches = grep("[.]R$",allfiles)

sourcefiles = allfiles[matches]
dir_dep_matrix = matrix(0, nrow = length(sourcefiles), ncol = length(sourcefiles))
# entry[row_i, col_j] = 1 iff jth script is needed for ith script (ie ith script 'sources' jth)

# stopped here
# NOTE can use / borrow code from search_code.R


shortdoc = NULL
longdoc = NULL
functiondoc = NULL

for(j in matches) {
  addition = c("**", "**", "**", "**",
               paste("--For file:", allfiles[j]), 
               "________________________________________")
  f = readLines(allfiles[j])
  
  # Grab all TODO entries, that start a line
  match_short = grep("^ *#@S", f) # short doc
  match_long = grep("^ *#@(S|L)", f) # long doc
  match_function = grep("^ *#@F",f) # within-function documentation
  
  if (length(match_short) > 0) {
    shortdoc = c(shortdoc, addition, gsub("^ *#@S", "", f[match_short]))
  } else {
    shortdoc = c(shortdoc, addition, "***This file has no documentation.***")
  }
  
  if (length(match_long) > 0) {
    longdoc = c(longdoc, addition, gsub("^ *#@(L|S)", "", f[match_long]))
  } else {
    longdoc = c(longdoc, addition, "***This file has no documentation.***")
  }
  
  if (length(match_function) > 0) {
    functiondoc = c(functiondoc, addition, gsub("^ *#@F", "", f[match_function]))
  }
}

# Output to file
writeLines(shortdoc, con = "zGEN_shortdoc.txt")
writeLines(longdoc, con = "zGEN_longdoc.txt")
writeLines(functiondoc, con = "zGEN_funcdoc.txt")

