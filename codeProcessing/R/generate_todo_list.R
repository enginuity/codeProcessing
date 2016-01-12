##@S This file is a script that pulls all '# TODO:' (or ## TODO:) sections in the code and outputs the entries into 'zGEN_todolist.txt'
##@L Goal: Produce a project-wide todo list

## TODO: [C-functionality Extension] add C processing also to this. 

#' Generates TODO list
#'    
#' This function generates a TODO list by looking for all #/## TODO: in the code base. 
#' Of course, this only works for R code. 
#' 
#' @param FD Object of class FilesDescription; See documentation to see how to describe a collection of files  
#' @param outFile Name of file to output text
#' 
#' @return none
#' 
#' @export
#' 
generate_todolist = function(FD, outFile = "GEN_todolist.txt") {
  
  ## Find files
  all_code = extract_Codebase(FD = FD)
  
  todo_type = NULL
  todolist = NULL
  
  for(j in seq_along(all_code$files)) {
    f = all_code$code[[j]]
    
    ## Grab line numbers for complete header documentation
    match_long = grep("^ *#{1,2}@(S|L)", f)
    
    ## Grab line number matches for TODOs. (only grabs leading TODOs) 
    linematches = grep("^ *#+ TODO:", f)
    
    # Temporarily broken
    ## TODO: Fix file extraction somehow? bugs out?
    #     ## Grab line numbers and function names
    #     temp = extract_all_docu(FD)
    
    ## Make a table for TODOs and functions
    if (length(linematches) > 0) { 
      todotable = data.frame(lines = linematches, text = f[linematches])
    } else {
      todotable = NULL
    }
    fxtable = NULL
    #     if (length(temp$fxtables$fx_name) > 0) { 
    #       fxtable = data.frame(lines = temp$fxtables$fx_start, text = paste("***FunctionStart***", temp$fxtables$fx_name))
    #     } else {
    #       fxtable = NULL
    #     }
    
    
    ## Count todo TYPE
    if (length(linematches) > 0) {
      todo_type = c(todo_type, gsub("^ *#+ TODO: ", "", f[linematches]))
    }
    
    ## TODO: [Refactor] Generalize this sort of printing, or use the printing mechanisms used in search_code and others. 
    tab = rbind(todotable, fxtable)
    
    todolist = c(todolist, 
                 paste("--In file:", all_code$files[[j]]), ## Identify File
                 "Description---", f[match_long], ## Add description
                 paste(stringr::str_pad(string = tab$lines, width = 5, side = "right"), 
                       tab$text, sep = " ::: "), ## Add todo/function
                 "##########", "##########", "##########", "") ## Add separator
  }
  
  ## Add TODO analysis
  ## Output alternate note, if no TODO's. 
  if (length(todolist) == 0) {
    todolist = "No outstanding '# TODO:' 's"
  } else {
    first_word = sapply(strsplit(todo_type, split = " "), function(x) { x[1]})
    specific_typed = grep("(\\[.*\\])|(--)", first_word)
    table_type = table(first_word[specific_typed])
    todolist = c(todolist, 
                 paste("----- There are", length(first_word) ,"outstanding TODO lines"),
                 paste("----- There are", length(first_word) - length(specific_typed) ,"unsorted TODO lines"),
                 "-----")
    max_len = max(nchar(names(table_type))) + 1
    if (length(table_type) > 0) {
      for(j in 1:length(table_type)) {
        todolist = c(todolist, 
                     paste("-----", 
                           stringr::str_pad(names(table_type)[j], width = max_len, side = 'right'),
                           "**", stringr::str_pad(table_type[j], width = 3, side = 'right'), sep = " "))
      }
    }
  }
  
  ## Output to file
  writeLines(todolist, con = outFile)
  
  return("Done! [Todo-list generation]")
}



