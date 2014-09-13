##@S This is examples of how to run this for the netcomp project.

library(stringr)
library(metacode)


DIR = "../network-comparison/netcomp-project/"


# ## Search code (and add comments)
# #search_code(dir = DIR, mode = "R", regexp = "sim_one", add_comment = "Test this..")
# #|----##this function has been split (order of args changed) --Sun Jul 13 00:43:19 2014--
# search_code(dir = DIR, mode = "R", regexp = "sim_one")
# #|----##this function has been split (order of args changed) --Sun Jul 13 00:43:19 2014--

## Generate todo list
# generate_todolist(dir = DIR)
search_code("sim_results")
search_code("single.procedure.test")
#replace_code("depth[.]from[.]parents", "depth_from_parents", "replace . with _")
#replace_code("display[.]tree", "display_tree", "replace . with _")





clear_comments()
