## Reads code and plots dependency chart for well-formatted code

library(Rgraphviz)


gen_depend_R_new = function(file) {
  ## New attempt at writing this function
  ## Only accepts well-formatted functions (and no non-globally defined functions) [well-formatted => starts at first character, and ends with a lone left-justified }. 

  ## file = c("src_r/smooth_fit.R", "src_r/parallel_helper.R")
  ## file can be a vector of filenames.
  fxs = NULL
  ft_mat = matrix(0, nrow = 1, ncol = 2)

  ## Find all functions first. 
  for(k in seq_along(file)) {
    ## Find possible function starts / ends.
    
    code = readLines(file[k])
    poss_function_starts = grep("^[A-Za-z]+.*?= *function", code)
    poss_function_ends = grep("^[}]", code)

    if (length(poss_function_starts) > 0) {
      ## If there are actually any functions, then:
      
      temp = strsplit(code[poss_function_starts], " ")
      fxs = rbind(fxs, data.frame(name = sapply(temp, function(x) {x[1]}),
        start = poss_function_starts,
        end = sapply(poss_function_starts, function(x) {
          return(min(poss_function_ends[poss_function_ends > x])) }),
        fileID = k,
        stringsAsFactors = FALSE) )
    }
  }

  

  ## FINISH WRITING FUNCTION
  
  NN = nrow(fxs)
  ## dep_mat[i,j] = 1 => function i calls function j.
  ft_mat = matrix(0, nrow = 1, ncol = 2)

  for(j in 1:NN) {
    for(i in 1:NN) {
      temp = grep(fxs$name[j], code[(fxs$start[i] + 1):(fxs$end[i] - 1)])
      if (length(temp) > 0) {
        ft_mat = rbind(ft_mat, cbind(i,j))
      }
    }
  }

  ## Check for uncalled functions
  exist.uncalled = FALSE
  for(j in 1:NN) {
    if(sum(ft_mat == j) == 0) {
      ft_mat = rbind(ft_mat, cbind(NN+1,j))
      exist.uncalled = TRUE
    }
  }

  ft_mat = ft_mat[-1,]
  if (exist.uncalled) {
    fxs = rbind(fxs,data.frame(name="NULL_FUNCTION",
      start = -1, end = -1))
  }
  return(list(fxs, ft_mat))
  
  
  

  
  
}

gen_depend_R = function(file) {
  ## To be replaced by gen..R_new. 
  LS = 0
  
  code = readLines(file)
  
  poss_function_starts = grep(
    paste(c("^", rep(" ", times = LS), "[A-Za-z]+.*?= *function"), collapse = ""), code)
  poss_function_ends = grep(paste(c("^", rep(" ", times = LS),"([A-za-z]+.*[}]|[}])"), collapse = ""), code)

  temp = strsplit(code[poss_function_starts], " ")
  fxs = data.frame(name = sapply(temp, function(x) {x[1]}),
    start = poss_function_starts,
    end = sapply(poss_function_starts, function(x) {
      return(min(poss_function_ends[poss_function_ends > x])) }),
    stringsAsFactors = FALSE
    )
  
  fxs = fxs[fxs$name != "{",]
  fxs$name = gsub("[(].*$", "", fxs$name)

  temp = grep("^[*]", fxs$name)
  fxs$name = gsub("^[*]", "", fxs$name)
  
  NN = nrow(fxs)
  ## dep_mat[i,j] = 1 => function i calls function j.
  ft_mat = matrix(0, nrow = 1, ncol = 2)

  for(j in 1:NN) {
    for(i in 1:NN) {
      temp = grep(fxs$name[j], code[(fxs$start[i] + 1):(fxs$end[i] - 1)])
      if (length(temp) > 0) {
        ft_mat = rbind(ft_mat, cbind(i,j))
      }
    }
  }

  ## Check for uncalled functions
  exist.uncalled = FALSE
  for(j in 1:NN) {
    if(sum(ft_mat == j) == 0) {
      ft_mat = rbind(ft_mat, cbind(NN+1,j))
      exist.uncalled = TRUE
    }
  }

  ft_mat = ft_mat[-1,]
  if (exist.uncalled) {
    fxs = rbind(fxs,data.frame(name="NULL_FUNCTION",
      start = -1, end = -1))
  }
  return(list(fxs, ft_mat))
}









gen_depend_C = function(file, leading_spaces = 0) {

  LS = leading_spaces
  ## Function is not very efficient. doesnt matter?
  
  code = readLines(file)
  
  poss_function_starts = grep(
    paste(c("^", rep(" ", times = leading_spaces), "[A-za-z]+.*[{]"), collapse = ""),
    code)
  poss_function_ends = grep(paste(c("^", rep(" ", times = leading_spaces),"([A-za-z]+.*[}]|[}])"), collapse = ""), code)

  temp = strsplit(code[poss_function_starts], " ")
  fxs = data.frame(
    type = sapply(temp,
      function(x) {
        if (x[1+LS] == "struct") {
          return(x[2+LS])
        } else {
          return(x[1+LS])
        }
      }),
    name = sapply(temp,
      function(x) {
        if (x[1+LS] == "struct") {
          return(x[3+LS])
        } else {
          return(x[2+LS])
        } 
      }),
    start = poss_function_starts,
    end = sapply(poss_function_starts, function(x) {
      return(min(poss_function_ends[poss_function_ends > x])) }),
    stringsAsFactors = FALSE
    )
  
  fxs = fxs[fxs$name != "{",]
  fxs$name = gsub("[(].*$", "", fxs$name)

  temp = grep("^[*]", fxs$name)
  fxs$name = gsub("^[*]", "", fxs$name)
  fxs$type[temp] = paste(fxs$type[temp], "*", sep = " ")

  NN = nrow(fxs)
  ## dep_mat[i,j] = 1 => function i calls function j.
  ft_mat = matrix(0, nrow = 1, ncol = 2)

  for(j in 1:NN) {
    for(i in 1:NN) {
      temp = grep(fxs$name[j], code[(fxs$start[i] + 1):(fxs$end[i] - 1)])
      if (length(temp) > 0) {
        ft_mat = rbind(ft_mat, cbind(i,j))
      }
    }
  }

  ## Check for uncalled functions
  exist.uncalled = FALSE
  for(j in 1:NN) {
    if(sum(ft_mat == j) == 0) {
      ft_mat = rbind(ft_mat, cbind(NN+1,j))
      exist.uncalled = TRUE
    }
  }

  ft_mat = ft_mat[-1,]
  if (exist.uncalled) {
    fxs = rbind(fxs,data.frame(type="NULL", name="NULL_FUNCTION",
      start = -1, end = -1))
  }
  return(list(fxs, ft_mat))
}

plot_depend = function(fxs, mat, out_file = "test_depend.pdf") {
  pdf(out_file, width = 12, height = 12)
  NN = nrow(fxs)

  g = ftM2graphNEL(mat)


  ## Setup graph attributes
  graphAttr = list(rankdir = "LR")
  
  ## Setup node attributes
  label_vec = paste(fxs$name, "\\\n", fxs$type, "\\\n",
    "lines (",fxs$start,"-",fxs$end,")", sep = "")
  names(label_vec) = 1:NN

  shape_vec = rep("rectangle", times = NN)
  names(shape_vec) = 1:NN

  font_size = rep("16", times = NN)
  names(font_size) = 1:NN

  height_vec = rep(1, times = NN)
  width_vec = rep(3, times = NN)
  names(height_vec) = 1:NN
  names(width_vec) = 1:NN

  fixed_size = rep("false", times = NN)
  names(fixed_size) = 1:NN
  
  nodeAttr = list(label = label_vec, shape = shape_vec,
    fontsize = font_size, height = height_vec, width = width_vec,
    fixedsize = fixed_size)

  ## Set up edge attributes
  edge_col = rep("darkgreen", times = nrow(mat))
  names(edge_col) = paste(mat[,1], "~", mat[,2], sep = "")

  edge_arrowhead = rep("none", times = nrow(mat))
  names(edge_arrowhead) = paste(mat[,1], "~", mat[,2], sep = "")
  
  edgeAttr = list(
    color = edge_col,
    arrowhead = edge_arrowhead
    )
  
  ## Plot / output
  plot(g, "dot", attrs = list(graph=graphAttr), nodeAttrs = nodeAttr,
       edgeAttrs = edgeAttr)
  dev.off()
}

plot_dependency = function(codefile, mode = c("R", "C"),
  out_file = "depend_out.pdf", leading_spaces = NULL) {
  if (mode == "C") {
    LS = 2
    if (!is.null(leading_spaces)) {
      LS = leading_spaces
    }
    temp = gen_depend_C(file = codefile, leading_spaces = LS)
  } else if (mode == "R") {
    temp = gen_depend_R(file = codefile)
  } else {
    stop("Unallowed mode")
  }
  
  plot_depend(temp[[1]], temp[[2]], out_file = out_file)
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







