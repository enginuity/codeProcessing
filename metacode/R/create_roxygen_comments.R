##@S Functions to produce Roxygen2 comments

## TODO: [Testing] -- need to do more testing of functions/features here... 

## TODO: [Idea] integrate dependency tree with documentation; 
## TODO: -- allow clicking on dependency tree to load documentation

## TODO: [Idea] allow for matching/checking of documentation for parameters to be the same... 

## TODO: [Low-Priority] Figure out if 'everything' should be @exported; this is currently done. 

## TODO: For new documentation, can use most frequent old documentation as default (instead of 'temp' or 'test')

# Function to create Roxygen comments -------------------------------------

#' Create roxygen templates (and fix/reorder them as necessary)
#' DO NOT DO THIS WITHOUT VERSION CONTROL!
#' 
#' @param dir Directory to search recursively for code files
#' @param file_regex If non-NULL: restrict to filenames that match this regex
#' @param regexp_fxstart Regex to determine function starts; default should work
#' @param test_run Won't write any changes to file, unless test_run is FALSE. 
#' 
#' @return none
#' 
#' @export
#' 
create_roxy_templates = function(dir=DIR, file_regex = NULL, regexp_fxstart = "(^[[:alnum:]_]+) += +function", test_run = FALSE) { 
  ## Assumes functions are of the format 
  ## FUNCTION_NAME = function( .... ) {
  ##   content
  ## }
  
  mats = search_code_matches(regexp = regexp_fxstart, dir = dir, mode = "R", file_regex = file_regex, logged = "ROXY-TEMPLATES")
  
  for(j in seq_along(mats$files)) {
    txt = mats$code[[j]]
    matchlines = mats$matchlines[[j]]
    param_segments = find_all_enclosed(text = txt, startpositions = cbind(matchlines, 1))
    
    lines_to_clear = NULL
    for(k in seq_along(matchlines)) {
      params = find_current_params(param_segments[k])  
      cur_doc = find_all_prev_documentation(text = txt, lineno = matchlines[k])
      proper_doc = reformat_documentation(cur_doc, params, str_extract(txt[matchlines[k]], pattern = "[[:alnum:]_]+"))
      
      ## Only change documentation if format does not match completely
      if (class(cur_doc) != "data.frame" || !all(proper_doc[-1] == cur_doc$Value)) {
        if (class(cur_doc) == "data.frame" ) { lines_to_clear = c(lines_to_clear, cur_doc$LineNo) }
        
        doc = paste(proper_doc, collapse = "\n")
        txt[matchlines[k]] = paste(doc, "\n", txt[matchlines[k]], sep = "")
      }
    }
    
    if (!is.null(lines_to_clear)) { txt = txt[-lines_to_clear] }
    mats$code[[j]] = txt
  }
  
  if (!test_run) { write_matchlist(mats) }
  return("Done! [Inserting/formatting documentation (roxygen) templates]")
}

## TODO: [Documentation-AUTO] Check/fix Roxygen2 Documentation (roxyparam_subset)
#' ********** WARNING -- INSERTED CODE **************
#' <<BasicInfo>> 
#' 
#' @param locate_df text
#' @param param_name text
#' 
#' @return text
#' 
#' @export
#' 
roxyparam_subset = function(locate_df, param_name) {
  inds = which(locate_df$paramname == param_name)
  return(cbind(locate_df[inds,"funcname"], gsub(paste("#' @param ",param_name, " ", sep = ""), "", locate_df[inds, "paramval"])))
}



## TODO: [Documentation-AUTO] Check/fix Roxygen2 Documentation (roxyparam_overwrite)
#' ********** WARNING -- INSERTED CODE **************
#' <<BasicInfo>> 
#' 
#' @param locate_df text
#' @param param_name text
#' @param replace_text If NULL: replaces with most frequent non(text/temp). Otherwise, replaces with replace_text.
#' 
#' @return text
#' 
#' @export
#' 
roxyparam_overwrite = function(locate_df, param_name, replace_text = NULL) {
  inds = which(locate_df$paramname == param_name)
  if (is.null(replace_text)) { 
    replace_text = Mode_nontemp(inds)
    if (replace_text == "temp") { return("[Parameter Documentation replacement NOT done: No temporary docs!]")}
  }
  
  if (length(inds) == 0) { return("[No matching parameter names]") }
  
  new_param_doc = paste("#' @param", param_name, replace_text)
  files = unique(locate_df$filename)
  for(f in files) {
    lines = locate_df$lineno[intersect(inds, which(f == locate_df$filename))]
    code = readLines(f)
    for (l in lines) { code[l] = new_param_doc }
    writeLines(text = code, con = f)
  }
  return("[Parameter Documentation replacement done!]")
}


## TODO: [Documentation-AUTO] Check/fix Roxygen2 Documentation (Mode_nontemp)
#' <<BasicInfo>> 
#' 
#' @param text temp
#' 
#' @return temp
#' 
#' @export
#' 
Mode_nontemp = function(text) {
  ## Returns "mode" of text, ignoring values that are equal to "test" or "temp"
  ## TODO: Move function from this file? if necessary?
  t = text[text != "test" & text != "temp"]
  if (length(t) == 0) { return("temp") }
  
  tab = table(t)
  return(names(tab)[which(tab == max(tab))])
}


## TODO: [Documentation-AUTO] Check/fix Roxygen2 Documentation (roxyparam_locate)
#' ********** WARNING -- INSERTED CODE **************
#' <<BasicInfo>> 
#' 
#' @param dir text
#' @param file_regex text
#' @param regexp_fxstart text
#' 
#' @return text
#' 
#' @export
#' 
roxyparam_locate = function(dir,file_regex = NULL, regexp_fxstart = "(^[[:alnum:]_]+) += +function") {
  mats = search_code_matches(regexp = regexp_fxstart, dir = dir, mode = "R", file_regex = file_regex, logged = "ROXY-param-matching")
  
  agg_params = data.frame(filename="", funcname="", paramname = "", paramval = "", lineno = 0, stringsAsFactors = FALSE)
  
  for(j in seq_along(mats$files)) {
    txt = mats$code[[j]]
    matchlines = mats$matchlines[[j]]
    param_segments = find_all_enclosed(text = txt, startpositions = cbind(matchlines, 1))
    
    for(k in seq_along(matchlines)) {
      params = find_current_params(param_segments[k])  
      cur_doc = find_all_prev_documentation(text = txt, lineno = matchlines[k])
      if (is.data.frame(cur_doc)) {
        fn_name = str_extract(txt[matchlines[k]], pattern = "[[:alnum:]_]+")
        agg_params = rbind(agg_params, 
                           data.frame(filename = mats$files[j], funcname = fn_name, 
                                      paramname = params, paramval = cur_doc$Value[cur_doc$Mode == "@param"],
                                      lineno = cur_doc$LineNo[cur_doc$Mode == "@param"], stringsAsFactors = FALSE))
      }
    }
  }
  return(agg_params[-1,])
}


#' Takes old (or non-existent) roxygen documentation, and reformats it
#' 
#' @param cur_doc old documentation (as data frame output)
#' @param params function parameters
#' @param function_name function name
#' 
#' @return character vector: correct roxygen documentation
#' 
#' @export
#' 
reformat_documentation = function(cur_doc, params, function_name) {
  ## Wanted roxy output: 
  ## #` some title/description
  ## #` ...
  ## #` @param ... 
  ## #` @result ...
  ## #` @export
  
  head_doc = "#' <<BasicInfo>> "
  param_doc = paste("#' @param", params, "temp")
  return_doc = "#' @return temp"
  
  if (class(cur_doc) == "data.frame") {
    if (any(cur_doc$Mode == "Text")) { head_doc = cur_doc$Value[cur_doc$Mode == "Text"] }
    if (any(cur_doc$Mode == "@return")) { return_doc = cur_doc$Value[cur_doc$Mode == "@return"] }
    if (any(cur_doc$Mode == "@param")) {
      for(j in which(cur_doc$Mode == "@param")) {
        param_doc[which(params == cur_doc$Mode2[j])] = cur_doc$Value[j]
      }
    }
  }
  
  return(c(paste("## TODO: [Documentation-AUTO] Check/fix Roxygen2 Documentation (",function_name,")", sep = ""),
           head_doc, "#' ", param_doc, "#' ", return_doc, "#' ", "#' @export", "#' "))
}


#' Processes function parameters and attempts to extract out all parameter names
#' 
#' @param text Should be all the code 'here' in [function() { ..here.. }]
#' 
#' @return character vecctor of all parameter names
#' 
#' @export
#' 
find_current_params = function(text) {
  ## Get rid of stuff within parenthesis
  text = substr(text, start = 2, stop = nchar(text) - 1)
  text = gsub("\".*?\"","", text)
  text = gsub("[(].*?[)]", "", text)
  text = strsplit(text, ",")[[1]]
  text = gsub("=.*", "", text)
  text = gsub(" ", "", text)
  return(text)
}


#' Looks for the next open/close parethesis set, accounting for parenthesis embedding
#' Allows for arbitrary types of open/close parens. 
#' 
#' @param text code to process
#' @param startposition a vector(lineno, charpos) denoting where to find 
#' @param regex_s regex to find 'open-paren'
#' @param regex_e regex to find 'close-paren'
#' 
#' @return contents of next open/close paren set
#' 
#' @export
#' 
find_single_enclosed = function(text, startposition, regex_s = "[(]", regex_e = "[)]") {
  ## text is a vector; this looks for things that span multiple entries. 
  ## 
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


#' Extracts all text between the starting and ending vector pairs.
#' 
#' @param text text
#' @param start starting vector(lineno, charno) pair
#' @param end ending vector(lineno, charno) pair
#' 
#' @return All text (as character singleton) between starting/ending pairs
#' 
#' @export
#' 
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
                 paste(text[(start[1]+1):(end[1]-1)], sep = "", collapse = ""),
                 substr(text[end[1]], start = 1, stop = end[2])
    ))
  }
}

## TODO: [Documentation-AUTO] Check/fix Roxygen2 Documentation (find)
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
#' 
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

## TODO: [Documentation-AUTO] Check/fix Roxygen2 Documentation (compare)
#' ********** WARNING -- INSERTED CODE **************
#' <<BasicInfo>> 
#' 
#' @param loc1 text
#' @param loc2 text
#' 
#' @return text
#' 
#' @export
#' 
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

## TODO: [Documentation-AUTO] Check/fix Roxygen2 Documentation (find)
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
#' 
find_all_enclosed = function(text, startpositions, regex_s = "[(]", regex_e = "[)]") {
  N = nrow(startpositions)
  res = rep("", N)
  for(j in 1:N) {
    res[j] = find_single_enclosed(text=text, startposition=startpositions[j,], regex_s=regex_s, regex_e = regex_e)
  }
  return(res)
}

## TODO: [Documentation-AUTO] Check/fix Roxygen2 Documentation (find)
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
#' 
find_all_prev_headers = function(text, lineno, header="^#'") {
  ## Finds all previous lines (compared to lineno) that start with the specific header type
  ## reutnrs linenumbres
  z = grep(header, text) 
  breaks = c(0,setdiff(seq_along(text), z))
  closest_break = max(breaks[breaks < lineno])
  
  if (closest_break < 0) {
    return(NA)
  } else if (closest_break == (lineno - 1)) {
    return(NA)
  } else {
    return((closest_break+1):(lineno-1))
  }
}

## TODO: [Documentation-AUTO] Check/fix Roxygen2 Documentation (find)
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
#' 
find_all_prev_documentation = function(text, lineno, header = "^#'") {
  prev_lines = find_all_prev_headers(text, lineno, header)
  if (is.na(prev_lines[1])) {
    return(NA)
  }
  prev_docu = data.frame(LineNo=prev_lines, Mode="", Mode2 ="", Value = "", stringsAsFactors=FALSE)
  
  text_clean = gsub(header, "", text[prev_lines])
  ## Mode is either 'Empty', 'Text', or "@something"
  tsplit = strsplit(text_clean, " ")
  
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
    prev_docu$Value[j] = text[prev_lines[j]]
  }
  return(prev_docu)
}
