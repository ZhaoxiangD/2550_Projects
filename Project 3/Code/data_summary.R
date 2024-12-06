library(data.table)
library(parallel)
library(lme4)
library(dplyr)
setwd("/Users/zhaoxiangding/Documents/GitHub/2550_Projects/Project 3/Data")

read_data <- function(m){
  #' Read data from csv file
  #' @param m: index of data file
  #' @return: data table

  file_name <- paste0("../Data/data_", m, ".csv")
  data <- fread(file_name)
  return(data)
}

estimation_fun_normal <- function(data_sub){
  #' Estimate treatment effect using linear mixed effect model
  #' @param data_sub: data table
  #' @return: estimated treatment effect

  if (length(unique(data_sub$X)) == 1){
    return(NA)
  } else if (data_sub$R[1] == 1) { # only one sample in each cluster
    res <- tryCatch({
      model <- lm(Y_N ~ X, data = data_sub)
      coef(model)[2]
    }, error = function(e) {
      NA
    })
    return(res)
  } else {
    res <- tryCatch({
      model <- lmer(Y_N ~ X + (1|cluster), data = data_sub)
      coef(model)[[1]][1,2]
    }, error = function(e) {
      NA
    })
  }
}

estimation_fun_poisson <- function(data_sub){
  #' Estimate treatment effect using generalized linear mixed effect model
  #' @param data_sub: data table
  #' @return: estimated treatment effect
  
  if (length(unique(data_sub$X)) == 1 | length(unique(data_sub$Y_P)) <= 1 | 
      any(data_sub[,length(unique(as.numeric(Y_P))), by = X][,2] == 1)){ # only one value in Y_P
    return(NA)
  } else if (data_sub$R[1] == 1) { # only one sample in each cluster
    res <- tryCatch({
      model <- glm(as.numeric(Y_P) ~ X, data = data_sub, family = poisson())
      coef(model)[2]
    }, error = function(e) {
      NA
    })
    return(res)
  } else {
    res <- tryCatch({
      model <- glmer(as.numeric(Y_P) ~ X + (1|cluster), data = data_sub, family = poisson())
      coef(model)[[1]][1,2]
    }, error = function(e) {
      NA
    })
    return(res)
  }
}

main <- function(m){
  #' Main function for estimating treatment effect
  #' @param m: index of data file
  #' @return: data table with estimated treatment effect
  
  data <- read_data(m)
  res <- data[, .(beta_hat_n = as.numeric(estimation_fun_normal(.SD)), 
                  beta_hat_p = as.numeric(estimation_fun_poisson(.SD))),
              by = .(gamma, sigma, ratio, B, G, alpha, beta)]
  res$m <- m
  return(res)
}

est_res <- mclapply(1:100, function(x) main(x), mc.cores = 5)
est_res <- do.call(rbind, est_res)
write.csv(est_res, "../Result/estimation_result.csv", row.names = FALSE)

est_res <- as.data.table(est_res)
est_summary <- est_res[,.(Bias_n = mean(beta_hat_n-beta, na.rm = T), 
                          Var_n = sd(beta_hat_n, na.rm = T),
                          Bias_p = mean(beta_hat_p-beta, na.rm = T),
                          Var_p = sd(beta_hat_p, na.rm = T)),
                       by = .(gamma, sigma, ratio, B, G, alpha, beta)]
write.csv(est_summary, "../Result/estimation_summary.csv", row.names = FALSE)
