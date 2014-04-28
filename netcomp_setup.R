##@S This is examples of how to run this for the netcomp project.

DIR = "../network-comparison/netcomp-project/"

## Load all available programs
source("load_all.R")


## Search code (and add comments)
#search_code(dir = DIR, mode = "R", regexp = "sim_one", add_comment = "Test this..")
search_code(dir = DIR, mode = "R", regexp = "sim_one")

## Generate todo list
generate_todolist(dir = DIR)
