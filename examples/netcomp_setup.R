##@S This is examples of how to run this for the netcomp project.

library(stringr)
library(metacode)

## yoga ubuntu
DIR = "../network-comparison/netcomp-project/"
plot_dependency(dir = "../network-comparison/netcomp-project/netcomplib/", mode = "R", out_file = "results/test.pdf")

## Desktop
DIR = "../netcomp-project/"
plot_dependency(dir = "../netcomp-project/netcomplib/", mode = "R", out_file = "results/test.pdf")
search_code("heat", regex_exact = FALSE)


# ## Search code (and add comments)
# #search_code(dir = DIR, mode = "R", regexp = "sim_one", add_comment = "Test this..")
# #|----##this function has been split (order of args changed) --Sun Jul 13 00:43:19 2014--
# search_code(dir = DIR, mode = "R", regexp = "sim_one")
# #|----##this function has been split (order of args changed) --Sun Jul 13 00:43:19 2014--

## Generate todo list
search_code("compute_likelihood")
search_code("update_probabilities")


# generate_todolist(dir = DIR)
search_code("sim_results")
search_code("single.procedure.test")
#replace_code("depth[.]from[.]parents", "depth_from_parents", "replace . with _")
#replace_code("display[.]tree", "display_tree", "replace . with _")
search_code("cmn_mcmc", add_comment = "Updated this function (see model_mcmc.R), so need to update usages as necessary")

search_code("mle_loglik", add_comment = "Need to Use updated version of mle_loglik in lrt_functions.R; more parameters to pass in")

search_code("cmn_network")

search_code("gen_tree", add_comment = "Check usage; this function updated to having 'random_plimit' as a parameter 9/15/2014", )

## clear_comments()
