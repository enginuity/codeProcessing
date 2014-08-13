##@S This file is a script that pulls all '# TODO:' (or ## TODO:) sections in the code and outputs the entries into 'zGEN_todolist.txt'
##@L Goal: Produce a project-wide todo list

## TODO: [C-functionality] add C processing also

#' Generates TODO list
#'    
#' This function generates a TODO list by looking for all #/## TODO: in the code base. 
#' Of course, this only works for R code... 
#' 
#' @param dir Directory to search recursively for code files
#' 
#' @return none
#' 
#' @export
#' 
generate_todolist = function(dir = ".") {
  
  ## Find files
  all_code = extract_all_code(dir = dir, mode = "R", file_regex = NULL)
#|          ****************
#|----##Rename find_files -> extract_all_code --Wed Aug 13 16:08:27 2014--

  todo_type = NULL
  todolist = NULL

  for(j in seq_along(all_code$files)) {
    addition = paste("--In file:", gsub(dir, "", all_code$files[[j]]))
    f = all_code$code[[j]]
    ## Grab all TODO entries, that start a line
    linematches = grep("^ *#+ TODO:", f)      
    if (length(linematches) > 0) {
        todolist = c(todolist, addition, 
          paste(fix_length(t=linematches, len = 5), 
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
              fix_length(names(table_type)[j], len = max_len),
              "**", fix_length(table_type[j], len = 3), sep = " "))
    }
  }
  
  ## Output to file
  writeLines(todolist, con = "results/zGEN_todolist.txt")
  
  return("Done! [Todo-list generation]")
}



