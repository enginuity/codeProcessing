## TODO: [Testing] -- need to do more testing of functions/features here... 
## TODO: Allow function to reorder the params when the order of input is changed. 

## TODO: [Idea] integrate dependency tree with documentation; 
## TODO: -- allow clicking on dependency tree to load documentation

# Function to create Roxygen comments -------------------------------------

#' Create roxygen templates (and fix/reorder them as necessary)
#' 
#' @param dir directory to search under
#' @param file_regex If non-null, apply this file_regex. 
#' 
#' @return no output
#' 
#' @export
create_roxy_templates = function(dir=DIR, file_regex = NULL, regexp_fxstart = "(^[[:alnum:]_]+) += +function",
                                 mode = c("R", "C")) { 
  ## Assumes functions are of the format 
  ## FUNCTION_NAME = function( .... ) {
  ##   content
  ## }
  
  ## DO NOT DO THIS WITHOUT VERSION CONTROL!

  log_file = logfile_namecreation(logtype = "create_roxy", query = regexp_fxstart)
  log_result("Searching for functions to add/fix roxygen2 template", file = log_file)
  ## Find files, extract code
  all_code = find_files(dir = dir, mode = mode, file_regex = file_regex)
#|----##*** Modify output: instead of list of sublists with two fields (filename, code), have list of two lists: files, code --Sat Jul 12 18:47:32 2014--

  ## Wanted roxy output: 
  ## #` some title/description
  ## #` ...
  ## #` @param ... 
  ## #` @result ...
  ## #` @export
  
  ## Search through code and make changes as necessary
  for(j in seq_along(all_code$files)) {
    txt = all_code$code[[j]]
    
    gr = str_match_all(pattern=regexp_fxstart, txt)
    heads = which(sapply(gr, length) > 0)
    
    if(length(heads) > 0) {
      param_segments = find_all_enclosed(text=txt, startpositions=cbind(heads, 1))
      
      replacement_code = all_code$code[[j]]
      for(k in seq_along(heads)) {
        params = find_current_params(param_segments[k])
        cur_doc = find_all_prev_documentation(text=txt, lineno = heads[k])
        
        ## TODO: Add in default info, for parameters? forget this for now. 
        ## TODO: Actually copy info, rather than only checking to see if things are missing. 
        ## TODO: Figure out if 'everything' should be @exported; this is currently done. 
        good_format = TRUE
        if (class(cur_doc) != "data.frame") {
          good_format = FALSE
        } else {
          if (!(any(cur_doc$Mode == "@return")) | !(any(cur_doc$Mode =="@export"))) {
            good_format = FALSE
          } else if (any(is.na(match(params, cur_doc$Mode2)))) {
            good_format = FALSE
          }
        }
        if(!good_format) {
          ins = c("#' ********** WARNING -- INSERTED CODE **************", "#' <<BasicInfo>> ", "#' ", paste("#' @param", params,"text"), "#' ", "#' @return text", "#' ", "#' @export")
          doc = paste(ins, collapse = "\n")
          lineno = heads[k]
          replacement_code[lineno] = paste(doc, "\n", replacement_code[lineno], sep = "")
        }

      }
    }
    
    writeLines(replacement_code, con = all_code$files[[j]])
  }
  return("Done! [Inserting documentation]")
}


#' ********** WARNING -- INSERTED CODE **************
#' <<BasicInfo>> 
#' 
#' @param text text
#' 
#' @return text
#' 
#' @export
find_current_params = function(text) {
  ## Get rid of stuff within parenthesis
  text = substr(text, start = 2, stop = nchar(text) - 1)
  text = gsub("[(].*?[)]", "", text)
  text = gsub("\".*?\"","", text)
  text = strsplit(text, ",")[[1]]
  text = gsub("=.*", "", text)
  text = gsub(" ", "", text)
  return(text)
}


#' ********** WARNING -- INSERTED CODE **************
#' <<BasicInfo>> 
#' 
#' @param text text
#' @param startposition text
#' @param regex_s text
#' @param regex_e text
#' 
#' @return text
#' 
#' @export
find_single_enclosed = function(text, startposition, regex_s = "[(]", regex_e = "[)]") {
  ## text is a vector; this looks for things that span multiple entries. 
  ## startposition is a vector(lineno, charpos)
  starts = str_locate_all(text, regex_s)
  ends = str_locate_all(text, regex_e)  
  
  lineno = startposition[1]
  charno = startposition[2]
  
  locs = find_next_location(starts, lineno, charno)
  loce = find_next_location(ends, lineno, charno)
  
  firstloc = find_next_location(starts, lineno, charno)
  lineno = firstloc[1]
  charno = firstloc[2] + 1
  embed = 1
  
  while (embed > 0) {
    locs = find_next_location(starts, lineno, charno)
    loce = find_next_location(ends, lineno, charno)
    if (compare_locations(locs, loce) == 1) {
      embed = embed + 1
      lineno = locs[1]
      charno = locs[2] + 1
    } else if (compare_locations(locs, loce) == 2) {
      embed = embed - 1
      lineno = loce[1]
      charno = loce[2] + 1
    } else {
      stop("Error in compare_locations: didn't return 1 or 2")
    }
  }
  endloc = c(lineno, charno - 1)
  
  return(gsub(" +", " ", extract_location_pair(text, start = firstloc, end = endloc)))
}

#' ********** WARNING -- INSERTED CODE **************
#' <<BasicInfo>> 
#' 
#' @param text text
#' @param start text
#' @param end text
#' 
#' @return text
#' 
#' @export
extract_location_pair = function(text, start, end) {
  if (start[1] == end[1]) {
    return(substr(text[start[1]],start=start[2], stop = end[2]))
  } else if (start[1] + 1 == end[1]) {
    ## gap of 1
    return(paste(sep = "", 
                 substr(text[start[1]], start=start[2], stop = nchar(text[start[1]])),
                 substr(text[end[1]], start = 1, stop = end[2])
    ))
  } else {
    return(paste(sep = "", 
                 substr(text[start[1]], start=start[2], stop = nchar(text[start[1]])),
                 paste(text[start[1]:end[1]], sep = "", collapse = ""),
                 substr(text[end[1]], start = 1, stop = end[2])
    ))
  }
}

#' ********** WARNING -- INSERTED CODE **************
#' <<BasicInfo>> 
#' 
#' @param loc_list text
#' @param ln text
#' @param cn text
#' 
#' @return text
#' 
#' @export
find_next_location = function(loc_list, ln, cn) {
  same_line_match = which(loc_list[[ln]][,1] >= cn)
  if (length(same_line_match) > 0) {
    return(c(ln, loc_list[[ln]][min(same_line_match),1]))
  } else {
    ln_match = which(sapply(loc_list, length) > 0)
    ln_match = ln_match[ln_match > ln]
    if (length(ln_match) == 0) {
      return(NA)
    } else {
      return(c(ln_match[1], loc_list[[ln_match[1]]][1,1]))
    }
  }
}

#' ********** WARNING -- INSERTED CODE **************
#' <<BasicInfo>> 
#' 
#' @param loc1 text
#' @param loc2 text
#' 
#' @return text
#' 
#' @export
compare_locations = function(loc1, loc2) {
  ## Returns the earlier location
  if (loc1[1] == loc2[1]) {
    if (loc1[2] > loc2[2]) {
      return(2)
    } else {
      return(1)
    }
  } else if (loc1[1] > loc2[1]) {
    return(2)
  } else {
    return(1)
  }
}

#' ********** WARNING -- INSERTED CODE **************
#' <<BasicInfo>> 
#' 
#' @param text text
#' @param startpositions text
#' @param regex_s text
#' @param regex_e text
#' 
#' @return text
#' 
#' @export
find_all_enclosed = function(text, startpositions, regex_s = "[(]", regex_e = "[)]") {
  N = nrow(startpositions)
  res = rep("", N)
  for(j in 1:N) {
    res[j] = find_single_enclosed(text=text, startposition=startpositions[j,], regex_s=regex_s, regex_e = regex_e)
  }
  return(res)
}

#' ********** WARNING -- INSERTED CODE **************
#' <<BasicInfo>> 
#' 
#' @param text text
#' @param lineno text
#' @param header text
#' 
#' @return text
#' 
#' @export
find_all_prev_headers = function(text, lineno, header="^#'") {
  ## Finds all previous lines (compared to lineno) that start with the specific header type
  ## reutnrs linenumbres
  z = grep(header, text) 
  breaks = setdiff(seq_along(text), z)
  closest_break = max(breaks[breaks < lineno])
  ## TODO: This line (closest_break = .. need to do something when breaks is empty)
  
  if (closest_break < 0) {
    return(NA)
  } else if (closest_break == (lineno - 1)) {
    return(NA)
  } else {
    return((closest_break+1):(lineno-1))
  }
}

#' ********** WARNING -- INSERTED CODE **************
#' <<BasicInfo>> 
#' 
#' @param text text
#' @param lineno text
#' @param header text
#' 
#' @return text
#' 
#' @export
find_all_prev_documentation = function(text, lineno, header = "^#'") {
  prev_lines = find_all_prev_headers(text, lineno, header)
  if (is.na(prev_lines[1])) {
    return(NA)
  }
  prev_docu = data.frame(LineNo=prev_lines, Mode="", Mode2 ="", Value = "", stringsAsFactors=FALSE)
  
  text = gsub(header, "", text[prev_lines])
  ## Mode is either 'Empty', 'Text', or "@something"
  tsplit = strsplit(text, " ")
  
  for(j in seq_along(prev_lines)) {
    t = tsplit[[j]]
    t = t[t != ""]
    if (length(t) == 0 | is.na(t[1]) | t[1] == "") {
      prev_docu$Mode[j] = 'Empty'
    } else {
      if (t[1] == "@param") {
        prev_docu$Mode[j] = "@param"
        prev_docu$Mode2[j] = t[2]
      } else if (t[1] == "@return") {
        prev_docu$Mode[j] = "@return"
      } else if (t[1] == "@export") {
        prev_docu$Mode[j] = "@export"
      } else {
        prev_docu$Mode[j] = "Text"
      }
    }
    prev_docu$Value[j] = text[j]
  }
  return(prev_docu)
}
