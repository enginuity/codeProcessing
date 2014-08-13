##@S This is examples of how to run this for the netcomp project.

DIR = "../network-comparison/netcomp-project/"

## Load all available programs
source("load_all.R")


# ## Search code (and add comments)
# #search_code(dir = DIR, mode = "R", regexp = "sim_one", add_comment = "Test this..")
# #|----##this function has been split (order of args changed) --Sun Jul 13 00:43:19 2014--
# search_code(dir = DIR, mode = "R", regexp = "sim_one")
# #|----##this function has been split (order of args changed) --Sun Jul 13 00:43:19 2014--

## Generate todo list
generate_todolist(dir = DIR)
