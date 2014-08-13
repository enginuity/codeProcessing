## Reads code and plots dependency chart for well-formatted code

# library(Rgraphviz)

## TODO: Need to write a code processor (in R, or find one..), to treat it as 'XML'... 

## TODO: [Documentation-AUTO] Check/fix Roxygen2 Documentation (gen_depend_R)
#' <<BasicInfo>> 
#' 
#' @param dir Directory to search recursively for code files
#' 
#' @return temp
#' 
#' @export
#' 
gen_depend_R = function(dir) {
  ## Start over again! Use new functions written... 
  ## Only accepts well-formatted functions (and no non-globally defined functions)
  ##   [well-formatted => starts at first character, and ends with a lone left-justified }. 
  
  fs = extract_all_code(dir, mode = "R")
  files = fs$files
  
  # Find functions within the code
  
  fxlist = list()
  for(k in seq_along(files)) {
    code = fs$code[[k]]
    
    ## TODO: Generalize this section? can ideas be pulled from other areas
    poss_function_starts = grep("^[A-Za-z]+.*?= *function", code)
    poss_function_ends = grep("^[}]", code)
    
    if (length(poss_function_starts) > 0) {
      ## If there are actually any functions, then:
      
      temp = strsplit(code[poss_function_starts], " ")
      fxlist[[length(fxlist) + 1]] = data.frame(name = sapply(temp, function(x) {x[1]}),
                                                start = poss_function_starts,
                                                end = sapply(poss_function_starts, function(x) {
                                                  return(min(poss_function_ends[poss_function_ends > x])) }),
                                                fileID = k,
                                                stringsAsFactors = FALSE)
    }
  } 
  fxtable = do.call(rbind, fxlist)
  
  
  # Find function calls within the code
  call_list = list()
  
  for(i in seq_along(fxtable$name)) { 
    reg = paste("(^|([^_[:alnum:].]))", fxtable$name[i], "($|([^_[:alnum:].]))", sep = "")
    ## TODO: [Idea] write this into a function? this is generally useful? checks for words...
    
    for(j in seq_along(fxtable$name)) {  
      if (length(grep(reg, fs$code[[fxtable$fileID[j]]][(fxtable$start[j]+1):(fxtable$end[j]-1)])) > 0) {
        ## TODO: [Idea] Incorporate the call line numbers? does this matter?
        call_list[[length(call_list) + 1]] = data.frame(caller = j, called = i)
      }
    }
  }
  all_calls = do.call(rbind, call_list)
  fxtable$filename = fs$files[fxtable$fileID]
  
  # Identify uncalled functions
  uncalled = setdiff(seq_along(fxtable$name), all_calls$called)
  if (length(uncalled) > 0) {
    all_calls = rbind(all_calls, cbind(caller = nrow(fxtable) + 1, called = uncalled))
    fxtable = rbind(fxtable, data.frame(name = "OUTSIDE_CALLER", start = 0, end = 0, fileID = 0, filename = "whoknows.R"))
  }
  
  return(list(function_table = fxtable, calls = all_calls))
}









## TODO: [Documentation-AUTO] Check/fix Roxygen2 Documentation (plot)
## TODO: [Documentation-AUTO] Check/fix Roxygen2 Documentation (plot_depend)
#' ********** WARNING -- INSERTED CODE **************
#' <<BasicInfo>> 
#' 
#' @param fxs text
#' @param calls temp
#' @param out_file File to plot the dependency tree to
#' 
#' @return text
#' 
#' @export
#' 
plot_depend = function(fxs, calls, out_file = "test_depend.pdf") {
  require(Rgraphviz)
  pdf(out_file, width = 12, height = 12)
  NN = nrow(fxs)
  
  g = ftM2graphNEL(as.matrix(calls))
  
  
  ## Setup graph attributes
  graphAttr = list(rankdir = "LR")
  
  ## Setup node attributes
  label_vec = paste(fxs$name, "\\\n", fxs$filename, "\\\n",
                    "lines (",fxs$start,"-",fxs$end,")", sep = "")
  names(label_vec) = 1:NN
  
  shape_vec = rep("rectangle", times = NN)
  names(shape_vec) = 1:NN
  
  unique_IDS = sort(unique(fxs$fileID))
  cs = unique_IDS - 1
  colors = c("#AAAAAA", rgb(runif(cs, min = 0, max = 0.3),runif(cs, min = 0, max = 0.3), runif(cs, min = 0, max = 0.3),alpha = 0.4))
  fillcolor_vec = colors[match(fxs$fileID, unique_IDS)]
  names(fillcolor_vec) = 1:NN
  
  font_size = rep("16", times = NN)
  names(font_size) = 1:NN
  
  height_vec = rep(1, times = NN)
  width_vec = rep(3, times = NN)
  names(height_vec) = 1:NN
  names(width_vec) = 1:NN
  
  fixed_size = rep("false", times = NN)
  names(fixed_size) = 1:NN
  
  nodeAttr = list(label = label_vec, shape = shape_vec, fillcolor = fillcolor_vec,
                  fontsize = font_size, height = height_vec, width = width_vec,
                  fixedsize = fixed_size)
  
  ## Set up edge attributes
  edge_col = rep("darkgreen", times = nrow(calls))
  names(edge_col) = paste(calls[,1], "~", calls[,2], sep = "")
  
  edge_arrowhead = rep("none", times = nrow(calls))
  names(edge_arrowhead) = paste(calls[,1], "~", calls[,2], sep = "")
  
  edgeAttr = list(
    color = edge_col,
    arrowhead = edge_arrowhead
  )
  
  ## Plot / output
  plot(g, "dot", attrs = list(graph=graphAttr), nodeAttrs = nodeAttr,
       edgeAttrs = edgeAttr)
  dev.off()
}

## TODO: [Documentation-AUTO] Check/fix Roxygen2 Documentation (plot)
## TODO: [Documentation-AUTO] Check/fix Roxygen2 Documentation (plot_dependency)
#' ********** WARNING -- INSERTED CODE **************
#' <<BasicInfo>> 
#' 
#' @param dir Directory to search recursively for code files
#' @param mode "R" or "C" -- looks for appropriate filename extensions
#' @param out_file File to plot the dependency tree to
#' @param leading_spaces text
#' 
#' @return text
#' 
#' @export
#' 
plot_dependency = function(dir, mode = c("R", "C"),
                           out_file = "depend_out.pdf", leading_spaces = NULL) {
  if (mode == "C") {
    stop("Does this still work? probably needs fixing?")
    LS = 2
    if (!is.null(leading_spaces)) {
      LS = leading_spaces
    }
    temp = gen_depend_C(file = codefile, leading_spaces = LS)
  } else if (mode == "R") {
    temp = gen_depend_R(dir)
  } else {
    stop("Unallowed mode")
  }
  
  plot_depend(fxs = temp$function_table, calls = temp$calls, out_file = out_file)
  invisible(temp)
}

# ## For now, this will look for all functions, and look at all function calls in C++ code.
# setwd("../")
# plot_dependency("src_cpp/allcode.cpp", mode = "C")
# plot_dependency("src_r/distance_distrib_samp.R", mode = "R")
# 
# 
# plot_dependency("src_r/allcode-general.R", mode = "R")
# ## errors
# plot_dependency("src_r/distance_distrib_samp.R", mode = "R")
# 
# plot_dependency("src_r/distance_distrib_samp.R", mode = "R")
# 
# plot_dependency("src_r/simscripts/alt_dist_parallel_fx.R", mode = "R")







