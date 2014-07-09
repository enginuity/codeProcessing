##@S This is examples of how to run this for the netcomp project.

DIR = "../27may_user_engagement/source/R/"

## Load all available programs
source("load_all.R")


## Search code (and add comments)
#search_code(dir = DIR, mode = "R", regexp = "sim_one", add_comment = "Test this..")
clear_comments(dir = DIR, mode = "R", file_regex = "script_agg")

search_code(dir=DIR, mode="R", add_comment="relabeled", file_regex="script_agg", regexp="dds", replace="tenure", fixed = TRUE)





search_code(dir = DIR, mode = "R", regexp = "Create[_]features", fixed = FALSE, add_comment = "TODO: Create_features -- change usage of 'change' parameter")



search_code(dir = DIR, mode = "R", regexp = "square")
search_code(dir = DIR, mode = "R", regexp = "agg_fx")

search_code(dir = DIR, mode = "R", regexp = "sim_one")
search_code(dir = DIR, mode = "R", regexp = "HP_user")

search_code(dir = DIR, mode = "R", regexp="agg_fx", add_comment="need-rename")




search_code(dir=DIR, mode="R", regexp="file *= *f, *", add_comment="remove this", file_regex="script", replace="")



## Generate todo list
generate_todolist(dir = DIR)









