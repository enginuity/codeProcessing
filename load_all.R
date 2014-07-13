##@S This loads all the functions (so running this script would load all the written functions...

## Load helper functions
source("sub_programs/text_proc_fxs.R")
source("sub_programs/file_proc_fxs.R")

## Load code-searching script
source("sub_programs/search_code.R")
#|----##this function has been split (order of args changed) --Sun Jul 13 00:43:19 2014--

## Load todo-list generation script
source("sub_programs/generate_todo_list.R")

library(stringr)

source("sub_programs/create_roxygen_comments.R")
