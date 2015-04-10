exp1 = parse(text = "f1 = function(x = 3, y) { return(1) }")


exp2 = parse(text = c("f1 = function(x = 3, y) { return(1) }", "tempvar = 1"))


exp3 = parse(file = "codeProcessing/R/h_search.R", keep.source = TRUE)
deparse(expr = exp3[[2]])

sapply(exp3, function(x) {x[[1]]})
sapply(exp3, function(x) {x[[3]][[1]]})
sapply(exp3, function(x) {x[[3]][[2]]})


exp4 = parse(text = ("3 -> assignright"))
exp4
exp4[[1]][[3]]
