library(data.table)
library(parallel)
library(lme4)
library(dplyr)
setwd("/Users/zhaoxiangding/Documents/GitHub/2550_Projects/Project 3/Data")

read_data <- function(m){
  file_name <- paste0("../Data/data_", m, ".csv")
  data <- fread(file_name)
  return(data)
}

estimation_fun_normal <- function(data_sub){
  if (length(unique(data_sub$X)) == 1){
    return(NA)
  } else if (data_sub$R[1] == 1) {
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
  if (length(unique(data_sub$X)) == 1 | length(unique(data_sub$Y_P)) <= 1 | 
      any(data_sub[,length(unique(as.numeric(Y_P))), by = X][,2] == 1)){
    return(NA)
  } else if (data_sub$R[1] == 1) {
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

confint_fun_normal <- function(data_sub){
  tryCatch({
    model <- lmer(Y_N ~ X + (1|cluster), data = data_sub)
    return(confint(model)[4,])
  }, error = function(e) {
    return(numeric())
  })
}

confint_fun_poisson <- function(data_sub){
  tryCatch({
    model <- glmer(as.numeric(Y_P) ~ X + (1|cluster), data = data_sub, family = poisson())
    return(confint(model)[3,])
  }, error = function(e) {
    return(numeric())
  })
}

coverage_fun_normal <- function(data_sub){
  tryCatch({
    res <- data_sub[, .(cov = (upper_n >= beta & lower_n <= beta) == 1)]
    return(mean(res$cov, na.rm = T)*100)
  }, error = function(e) {
    return(numeric())
  })
}

coverage_fun_poisson <- function(data_sub){
  res <- data_sub[, .(cov = (upper_p >= beta & lower_p <= beta) == 1)]
  return(mean(res$cov, na.rm = T)*100)
}

main <- function(m){
  data <- read_data(m)
  res <- data[, .(beta_hat_n = as.numeric(estimation_fun_normal(.SD)), 
                  beta_hat_p = as.numeric(estimation_fun_poisson(.SD))),
              by = .(gamma, sigma, p, ratio, B, G, alpha, beta)]
  res$m <- m
  return(res)
}
# 
# res <- test[, .(beta_hat_n = as.numeric(estimation_fun_normal(.SD)), 
#                 beta_hat_p = as.numeric(estimation_fun_poisson(.SD))),
#             by = .(gamma, sigma, p, ratio, B, G, alpha, beta)]

est_res <- mclapply(1:100, function(x) main(x), mc.cores = 5)
est_res <- do.call(rbind, est_res)
write.csv(est_res, "../Result/estimation_result.csv", row.names = FALSE)

est_res <- as.data.table(est_res)
est_summary <- est_res[,.(Bias_n = mean(beta_hat_n-beta, na.rm = T), 
                          Var_n = var(beta_hat_n, na.rm = T),
                          Bias_p = mean(beta_hat_p-beta, na.rm = T),
                          Var_p = var(beta_hat_p, na.rm = T)),
                       by = .(gamma, sigma, p, ratio, B, G, alpha, beta)]
write.csv(est_summary, "../Result/estimation_summary.csv", row.names = FALSE)
