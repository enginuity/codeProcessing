##@S This file is a script that pulls all '# TODO:' sections in the code
##@S  and outputs the entries into 'zGEN_todolist.txt
##@L The purpose is to extract an overall TODO list. 

source("general_help_functions/textOutputFormatting.R")

allfiles = list.files(recursive = TRUE)
matches = grep("[.]R$",allfiles)

todo_type = NULL
todolist = NULL
for(j in matches) {
  addition = paste("--In file:", allfiles[j])
  f = readLines(allfiles[j])
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
writeLines(todolist, con = "metacode/zGEN_todolist.txt")
