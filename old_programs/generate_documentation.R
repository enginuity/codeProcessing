##@S This file is a script that pulls all '#@' sections in the code
##@S  and outputs the entries:
##@S Short documentation         --> 'zGEN_shortdoc.txt'
##@S More in-depth documentation --> 'zGEN_longdoc.txt'
##@S Function documentation      --> 'zGEN_funcdoc.txt'
##@L It generates a documentation for each R-script file, specifically all <.R> files. 


allfiles = list.files(recursive = TRUE)
matches = grep("[.]R$",allfiles)

shortdoc = NULL
longdoc = NULL
functiondoc = NULL

for(j in matches) {
  addition = c("**", "**", "**", "**",
               paste("--For file:", allfiles[j]), 
               "________________________________________")
  f = readLines(allfiles[j])
  
  # Grab all TODO entries, that start a line
  match_short = grep("^ *#{1,2}@S", f) # short doc
  match_long = grep("^ *#{1,2}@(S|L)", f) # long doc
  match_function = grep("^ *#{1,2}@F",f) # within-function documentation
  
  if (length(match_short) > 0) {
    shortdoc = c(shortdoc, addition, gsub("^ *#{1,2}@S", "", f[match_short]))
  } else {
    shortdoc = c(shortdoc, addition, "***This file has no documentation.***")
  }
  
  if (length(match_long) > 0) {
    longdoc = c(longdoc, addition, gsub("^ *#{1,2}@(L|S)", "", f[match_long]))
  } else {
    longdoc = c(longdoc, addition, "***This file has no documentation.***")
  }
  
  if (length(match_function) > 0) {
    functiondoc = c(functiondoc, addition, gsub("^ *#@{1,2}F", "", f[match_function]))
  }
}

# Output to file
writeLines(shortdoc, con = "codeProcessing/zGEN_shortdoc.txt")
#|                          **************
#|----##change package name --Wed Dec 10 01:19:47 2014--
writeLines(longdoc, con = "codeProcessing/zGEN_longdoc.txt")
#|                         **************
#|----##change package name --Wed Dec 10 01:19:47 2014--
writeLines(functiondoc, con = "codeProcessing/zGEN_funcdoc.txt")
#|                             **************
#|----##change package name --Wed Dec 10 01:19:47 2014--

