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
