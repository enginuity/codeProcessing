
# Function to create Roxygen comments -------------------------------------

create_roxy_templates = function(dir=DIR, file_regex = NULL) { 
  ## Assumes functions are of the format 
  ## FUNCTION_NAME = function( .... ) {
  ##   content
  ## }
  
  ## It should also be okay if the arguments are split across multiple lines. 
  
  ## DO NOT DO THIS WITHOUT VERSION CONTROL!
  
  ## Look for all files, that match the current mode and file_regex setting
  allfiles = find_files(dir = dir, mode = "R", file_regex = file_regex)
  
  ## Load in all relevant code files
  all_code = list()
  for(j in seq_along(allfiles)) {
    all_code[[j]] = list(filename = allfiles[j],
                         code = readLines(allfiles[j])   )
  }
  
  ## Create savefile name
  sfile = paste("results/zTEMPLATE_ROXY_COMMENTS_", gsub("[^[:alnum:]]", "", regexp),"_", format(Sys.time(), "%Y%m%d-%H%M%S"), ".txt", sep = "")
  
  ## Search files, outputting relevant information to savefile. 
  cat('Searching for "', regexp, '"', "\n", date(), "\n\n", sep = "", file = sfile, append = TRUE)
  
  
  ## Wanted roxy output: 
  ## #` some title/description
  ## #` ...
  ## #` @param ... 
  ## #` @result ...
  ## #` @export
  
  for(j in seq_along(all_code)) {
    txt = all_code[[j]]$code
    
    gr = str_match_all(pattern=regexp_fxstart, txt)
    heads = which(sapply(gr, length) > 0)
    
    if(length(heads) > 0) {
      param_segments = find_all_enclosed(text=txt, startpositions=cbind(heads, 1))
      
      replacement_code = all_code[[j]]$code
      for(k in seq_along(heads)) {
        params = gsub(" ", "", gsub("=.*", "", strsplit(gsub("[()]", "", param_segments[k]), split = ",")[[1]]))
        
        cur_doc = find_all_prev_documentation(text=txt, lineno = heads[1])
        
        ## TODO: Add in default info, for parameters? forget this for now. 
        ## TODO: Actually copy info, rather than only checking to see if things are missing. 
        ## TODO: Figure out if 'everything' should be @exported; this is currently done. 
        good_format = TRUE
        if (is.na(cur_doc)) {
          good_format = FALSE
        } else {
          if (!(any(cur_doc$Mode == "@return")) | !(any(cur_doc$Mode =="@export"))) {
            good_format = FALSE
          } else if (any(is.na(match(params, cur_doc$Mode2)))) {
            good_format = FALSE
          }
        }
        
        if(!good_format) {
          ins = c("#' <<BasicInfo>> ", "#' ", paste("#' @params text", params), "#' ", "#' @return text", "#' ", "#' @export")
          doc = paste(ins, sep = "\n")
        }
        lineno = heads[k]
        replacement_code[lineno] = paste(doc, "\n", replacement_code[lineno], sep = "")
      }
    }
    
    writeLines(replacement_code, con = all_code[[j]]$filename)
  }
  return("Done! [Inserting documentation]")
}


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

extract_location_pair = function(text, start, end) {
  if (start[1] == end[1]) {
    return(substr(text[start[1]],start=start[2], stop = end[2]))
  } else if (start[1] + 1 == end[1]) {
    ## gap of 1
    return(paste(sep = "", 
                 substr(text[start[1]], start=start[2], stop = nchar(text[start[1]])),
                 substr(text[end[1]], start = 1, stop = nchar(text[end[1]]))
    ))
  } else {
    return(paste(sep = "", 
                 substr(text[start[1]], start=start[2], stop = nchar(text[start[1]])),
                 paste(text[start[1]:end[1]], sep = "", collapse = ""),
                 substr(text[end[1]], start = 1, stop = nchar(text[end[1]]))
    ))
  }
}

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

find_all_enclosed = function(text, startpositions, regex_s = "[(]", regex_e = "[)]") {
  N = nrow(startpositions)
  res = rep("", N)
  for(j in 1:N) {
    res[j] = find_single_enclosed(text=text, startposition=startpositions[j,], regex_s=regex_s, regex_e = regex_e)
  }
  return(res)
}

find_all_prev_headers = function(text, lineno, header="^#'") {
  ## Finds all previous lines (compared to lineno) that start with the specific header type
  ## reutnrs linenumbres
  z = grep(header, text) 
  breaks = setdiff(seq_along(text), z)
  closest_break = max(breaks[breaks < lineno])
  
  if (closest_break < 0) {
    return(NA)
  } else if (closest_break == (lineno - 1)) {
    return(NA)
  } else {
    return((closest_break+1):(lineno-1))
  }
}

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
