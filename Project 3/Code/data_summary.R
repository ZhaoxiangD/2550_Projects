library(data.table)
library(parallel)
setwd("/Users/zhaoxiangding/Documents/GitHub/2550_Projects/Project 3/Data")

read_data <- function(m){
  file_name <- paste0("../Data/data_", m, ".csv")
  data <- fread(file_name)
  return(data)
}

estimation_fun <- function(data_sub){
  model <- lm(Y ~ X, data = data_sub)
  return(coef(model)[2])
}


main <- function(m){
  data <- read_data(m)
  res <- data[, estimation_fun(.SD), by = .(gamma, sigma, scenario, p, ratio)]
  res$m <- m
  return(res)
}

est_res <- mclapply(1:100, main, mc.cores = 7)
est_res <- do.call(rbind, est_res)
write.csv(est_res, "estimation_summary.csv", row.names = FALSE)