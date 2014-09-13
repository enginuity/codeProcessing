##@S This is examples of how to run this for the netcomp project.

DIR = "../sdfb_network/"

## Load all available programs
library(stringr)
library(metacode)

create_roxy_templates(dir = "../sdfb_network/code/ODNB/text_processing/helper/")


# replace_code(regexp = "(../../)*private_data/odnb_data_proc/ODNB_metadata.Rdata", replace = "data/ODNB_raw/ODNB_metadata20140404.Rdata", add_comment = "Fix old directory structure")


search_code(regexp = "\"data/ODNB_intermediate/preNER/ODNB_splitcosub20140228.Rdata\"")

replace_code(regexp = "\"data/ODNB_intermediate/NER/ODNB_combtags20140404.Rdata\"", replace = "zzfile_textproc_ner_combtags", add_comment = "Use variable instead of filename")


search_code("good_docs_dates")

clear_comments()



# clear_comments()
search_code(regexp = "proc.tagtext")
replace_code(regexp = "proc[.]tagtext", replace = "proc_tagtext", add_comment = 'replace period with _')

