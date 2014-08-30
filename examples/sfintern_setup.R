##@S This is examples of how to run this for the netcomp project.

DIR = "../27may_user_engagement/source/"

## Load all available programs
source("load_all.R")


## Search code (and add comments)
#search_code(dir = DIR, mode = "R", regexp = "sim_one", add_comment = "Test this..")

create_roxy_templates(dir="../27may_user_engagement/source/fxForUserchurn/R/")


clear_comments(dir = DIR, mode = "R")

# search_code(dir=DIR, mode="R", add_comment="relabeled", file_regex="script_agg", regexp="dds", replace="tenure", fixed = TRUE)
# #|----##this function has been split (order of args changed) --Sun Jul 13 00:43:19 2014--





# search_code(dir = DIR, mode = "R", regexp = "Create[_]features", fixed = FALSE, add_comment = "TODO: Create_features -- change usage of 'change' parameter")
# #|----##this function has been split (order of args changed) --Sun Jul 13 00:43:19 2014--
# 
# 
# 
# search_code(dir = DIR, mode = "R", regexp = "square")
# #|----##this function has been split (order of args changed) --Sun Jul 13 00:43:19 2014--
# search_code(dir = DIR, mode = "R", regexp = "agg_fx")
# #|----##this function has been split (order of args changed) --Sun Jul 13 00:43:19 2014--
# 
# search_code(dir = DIR, mode = "R", regexp = "sim_one")
# #|----##this function has been split (order of args changed) --Sun Jul 13 00:43:19 2014--
# search_code(dir = DIR, mode = "R", regexp = "HP_user")
# #|----##this function has been split (order of args changed) --Sun Jul 13 00:43:19 2014--
# 
# search_code(dir = DIR, mode = "R", regexp="agg_fx", add_comment="need-rename")
# #|----##this function has been split (order of args changed) --Sun Jul 13 00:43:19 2014--
# 
# 
# 
# 
# search_code(dir=DIR, mode="R", regexp="file *= *f, *", add_comment="remove this", file_regex="script", replace="")
# #|----##this function has been split (order of args changed) --Sun Jul 13 00:43:19 2014--



## Generate todo list
generate_todolist(dir = DIR)









