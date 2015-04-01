##@S This is examples of how to run this for the netcomp project.

library(stringr)
library(codeProcessing)

## yoga ubuntu
DIR = FilesDescription(dirlist = "../network-comparison/netcomp-project/")
plot_dependency(FD = FilesDescription(dirlist = "../network-comparison/netcomp-project/netcomplib/"), out_file = "test.pdf")

## Desktop
DIR = "../netcomp-project/"
plot_dependency(dir = "../netcomp-project/netcomplib/", mode = "R", out_file = "results/test.pdf")

search_code("sim_one_set", FD = DIR)

search_code("gen_network_pair", FD = DIR, add_comment = "Edited parameters! ")

search_code("gen_model_fx", FD = DIR, add_comment = "Edited output list (added values; should not break anything)!")

search_code("block", FD = DIR)



DIR = FilesDescription(dirlist = "../network-comparison/netcomp-project/")
search_code("generate_fitting_models", add_comment = "Renamed parameters", FD = DIR)
search_code("compute_loglik_fromPC", FD = DIR)
#|----##Renamed parameters --Thu Oct  9 15:10:53 2014--

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
search_code("compute_df_adjustment2", add_comment = "Removed the first parameter (mle). Also, function is moved to the library")
search_code("mle_loglik", add_comment = "Need to Use updated version of mle_loglik in lrt_functions.R; more parameters to pass in")

search_code("compute_pval_multtrees", add_comment = "Check that the parameterization is correct; updated version of this had been written")

## clear_comments()
