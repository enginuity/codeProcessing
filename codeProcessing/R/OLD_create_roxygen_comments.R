##@S Functions to produce Roxygen2 comments

# TODO: Extract out the exact regex for functions into some default file? or make more explicit? (should have no manually written regexs inside the files)

# #' Extract roxygen2 parameter documentation
# #' 
# #' @param FD Object of class FilesDescription; See documentation to see how to describe a collection of files  
# #' @param regexp_fxstart Regex to determine function starts; default should work
# #' 
# #' @return Data frame of all parameter documentation
# #' 
# #' @export
# #' 
# extract_param_docu = function(FD, regexp_fxstart = "(^[[:alnum:]_.]+) += +function") {
#   
#   MCB = search_code_matches(RE = Regex(base = regexp_fxstart), FD, logged = "ROXY-param-matching")
#   
#   param_list = list()
#   i = 1
#   
#   for(j in seq_along(MCB$files)) {
#     txt = MCB$code[[j]]
#     matchlines = MCB$matchlines[[j]]
#     param_segments = find_all_enclosed(text = txt, startlocations = cbind(matchlines, 1))
#     
#     for(k in seq_along(matchlines)) {
#       params = find_current_params(param_segments[k]) ########### DESTROYED. there is a better way to find parameters
#       cur_doc = find_all_prev_documentation(text = txt, lineno = matchlines[k]) ########## new becomes process_cur_docu
#       if (is.data.frame(cur_doc) & (length(params) > 0)) {
#         fn_name = stringr::str_extract(txt[matchlines[k]], pattern = "[[:alnum:]_.]+")
#         
#         paramvals = gsub("^#' @param [[:alnum:]._]+", "", cur_doc$Value[cur_doc$Type == "@param"])
#         param_list[[i]] = data.frame(filename = MCB$files[j], funcname = fn_name, 
#                                      paramname = params, paramval = paramvals,
#                                      lineno = cur_doc$LineNo[cur_doc$Type == "@param"], stringsAsFactors = FALSE)
#         i = i + 1
#       }
#     }
#   }
#   agg_params = do.call(rbind, param_list)
#   return(agg_params)
# }
# 
# 
# #' Overwrite parameter documentation
# #' 
# #' @param locate_df Complete parameter data frame
# #' @param param_name Parameter name to subset
# #' @param replace_text If NULL: replaces with most frequent non(text/temp). Otherwise, replaces with replace_text.
# #' @param replace_all If TRUE: replaces also lines that already have non-trivial parameter documentation
# #' 
# #' @return none
# #' 
# #' @export
# #' 
# update_param_docu = function(locate_df, param_name, replace_text = NULL, replace_all = FALSE) {
#   ## TODO: [Refactor] The whole of roxy processing probably needs a re-thinking. Works as is, but probably gets increasingly difficult to update. 
#   
#   inds = which(locate_df$paramname == param_name)
#   if (is.null(replace_text)) { 
#     replace_text = Mode_nontemp(locate_df$paramval[inds])
#     if (replace_text == "temp") { return("[Parameter Documentation replacement NOT done: No temporary docs!]")}
#   } else { replace_text = paste("#' @param", param_name, replace_text) }
#   
#   if (length(inds) == 0) { return("[No matching parameter names]") }
#   
#   ## The following code isn't particularly efficient (many read/writes of same file), but implemented much easier!
#   for (i in inds) {
#     if (replace_all || strsplit(locate_df$paramval[i], " ")[[1]][4] %in% c("temp")) {
#       code = readLines(locate_df$filename[i])
#       code[locate_df$lineno[i]] = replace_text
#       writeLines(text = code, con = locate_df$filename[i])
#     }
#   }
#   
#   return("[Parameter Documentation replacement done!]")
# }
# 
# 
