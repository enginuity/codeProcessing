##@S This file is a script that pulls all '# TODO:' (or ## TODO:) sections in the code and outputs the entries into 'zGEN_todolist.txt'
##@L Goal: Produce a project-wide todo list

## TODO: [C-functionality] add C processing also

#' Generates TODO list
#'    
#' This function generates a TODO list by looking for all #/## TODO: in the code base. 
#' Of course, this only works for R code... 
#' 
#' 
#' @param FD Object of class FilesDescription; See documentation to see how to describe a collection of files  
#' 
#' @return none
#' 
#' @export
#' 
generate_todolist = function(FD) {
  
  ## Find files
  all_code = extract_Codebase(FD = FD)

  todo_type = NULL
  todolist = NULL

  for(j in seq_along(all_code$files)) {
    addition = paste("--In file:", gsub(dir, "", all_code$files[[j]]))
    f = all_code$code[[j]]
    ## Grab all TODO entries, that start a line
    linematches = grep("^ *#+ TODO:", f)      
    if (length(linematches) > 0) {
        todolist = c(todolist, addition, 
          paste(str_pad(string = linematches, width = 5, side = "right"),
              gsub("^ *", "", f[linematches]), sep = " ::: "), 
        "##########", "##########", "##########")
      todo_type = c(todo_type, gsub("^ *#+ TODO: ", "", f[linematches]))
    }
  }
  
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
    for(j in 1:length(table_type)) {
      todolist = c(todolist, 
        paste("-----", 
              str_pad(names(table_type)[j], width = max_len, side = 'right'),
              "**", str_pad(table_type[j], width = 3, side = 'right'), sep = " "))
    }
  }
  
  ## Output to file
  writeLines(todolist, con = "GEN_todolist.txt")
  
  return("Done! [Todo-list generation]")
}



