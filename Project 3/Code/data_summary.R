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

confint_fun <- function(data_sub){
  model <- lm(Y ~ X, data = data_sub)
  return(confint(model)[2,])
}

coverage_fun <- function(data_sub){
  res <- data_sub[, .(cov = (upper >= 10 & lower <= 10) == 1)]
  return(mean(res$cov, na.rm = T)*100)
}

main <- function(m){
  data <- read_data(m)
  res <- data[, .(beta_hat = estimation_fun(.SD), lower = confint_fun(.SD)[1], upper = confint_fun(.SD)[2]), 
              by = .(gamma, sigma, scenario, p, ratio)]
  res$m <- m
  return(res)
}

est_res <- mclapply(1:100, main, mc.cores = 7)
est_res <- do.call(rbind, est_res)
write.csv(est_res, "../Result/estimation_result.csv", row.names = FALSE)

est_summary <- est_res[,.(Bias = mean(beta_hat-10, na.rm = T), MSE = mean((beta_hat - 10)^2, na.rm = T), 
           Var = var(beta_hat, na.rm = T), Coverage = coverage_fun(.SD)), 
        by = .(gamma, sigma, scenario, p, ratio)]
write.csv(est_summary, "../Result/estimation_summary.csv", row.names = FALSE)
