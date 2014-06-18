##@S This is examples of how to run this for the netcomp project.

DIR = "../27may_user_engagement/source/R/"

## Load all available programs
source("load_all.R")


## Search code (and add comments)
#search_code(dir = DIR, mode = "R", regexp = "sim_one", add_comment = "Test this..")
search_code(dir = DIR, mode = "R", regexp = "sim_one")
search_code(dir = DIR, mode = "R", regexp = "HP_user")

search_code(dir = DIR, mode = "R", regexp=, add_comment="Change directory structure")


## Generate todo list
generate_todolist(dir = DIR)









