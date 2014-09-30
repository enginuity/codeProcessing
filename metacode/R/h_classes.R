##@S This file contains code (and documentation) for the various classes used. 

Codebase = function(files, code) {
  return(structure(list(files = files, code = code), class = "Codebase"))
}

MatchedCodebase = function(Codebase, matchlines, matchlocs, regex, regex_exact) {
  if (!inherits(Codebase, what = "Codebase")) { stop("Input Codebase is not of the appropriate class") }
  
  res = append(Codebase, list(matchlines = matchlines, matchlocs = matchlocs, regex = regex, regex_exact = regex_exact))
  class(res) = append("MatchedCodebase", class(Codebase))
  
  return(res)
}

