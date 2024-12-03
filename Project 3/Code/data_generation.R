library(parallel)
setwd("/Users/zhaoxiangding/Documents/GitHub/2550_Projects/Project 3/Data")

sample_generation_fun <- function(c, n_sample, alpha, beta, gamma, sigma,p){
  X <- rbinom(1,1,p)
  mu <- rnorm(n_sample, alpha + beta*X, sqrt(gamma))
  Y_N <- rnorm(n_sample, mu, sqrt(sigma))
  Y_P <- rpois(n_sample, exp(mu))
  cluster <- rep(c, n_sample)
  
  df <- data.frame(X, Y_N, Y_P, alpha, beta, gamma, sigma, p, cluster)
  return(df)
}


cluster_generate_fun <- function(n_sample, n_cluster, alpha, beta, gamma, sigma,p){
  
  df <- lapply(1:n_cluster, function(c) sample_generation_fun(c, n_sample, alpha, beta, gamma, sigma,p))
  df <- do.call(rbind, df)
  df$G <- n_cluster
  df$R <- n_sample
  return(df)
}

grid_search_fun <- function(B, c2, ratio, alpha, beta, gamma, sigma,p){
  c1 <- c2 * ratio
  g <- floor(B/c1)
  g_vec <- 2:g
  r_vec <- floor((B/g_vec - c1)/c2 + 1)
  df <- lapply(1:length(g_vec), function(x) cluster_generate_fun(r_vec[x], 
                                                              g_vec[x], 
                                                              alpha, 
                                                              beta, 
                                                              gamma, 
                                                              sigma,
                                                              p))
  df <- do.call(rbind, df)
  df$B <- B
  df$ratio <- ratio
  df$p <- p
  df$alpha <- alpha
  df$beta <- beta
  df$gamma <- gamma
  df$sigma <- sigma
  
  return(df)
}



vary_parm_fun <- function(c2){
  gamma_vec <- c(0.25, 100)
  sigma_vec <- c(0.25, 100)
  p_vec <- c(0.3, 0.7)
  ratio_vec <- c(10, 50, 100)
  alpha_vec <- c(10, 50)
  beta_vec <- c(10, 50)
  B_vec <- c(2000, 5000)
  
  df <- grid_search_fun(1000, c2, 5, 0, 1, 1, 1, 0.5)
  df_l <- lapply(gamma_vec, function(gamma) grid_search_fun(1000, c2, 5, 0, 1, gamma, 1, 0.5))
  df_l <- do.call(rbind, df_l)
  df <- rbind(df, df_l)
  
  df_l <- lapply(sigma_vec, function(sigma) grid_search_fun(1000, c2, 5, 0, 1, 1, sigma, 0.5))
  df_l <- do.call(rbind, df_l)
  df <- rbind(df, df_l)
  
  df_l <- lapply(p_vec, function(p) grid_search_fun(1000, c2, 5, 0, 1, 1, 1, p))
  df_l <- do.call(rbind, df_l)
  df <- rbind(df, df_l)
  
  df_l <- lapply(ratio_vec, function(ratio) grid_search_fun(1000, c2, ratio, 0, 1, 1, 1, 0.5))
  df_l <- do.call(rbind, df_l)
  df <- rbind(df, df_l)
  
  df_l <- lapply(alpha_vec, function(alpha) grid_search_fun(1000, c2, 5, alpha, 1, 1, 1, 0.5))
  df_l <- do.call(rbind, df_l)
  df <- rbind(df, df_l)
  
  df_l <- lapply(beta_vec, function(beta) grid_search_fun(1000, c2, 5, 0, beta, 1, 1, 0.5))
  df_l <- do.call(rbind, df_l)
  df <- rbind(df, df_l)
  
  return(df)
}

main <- function(c2,m){
  set.seed(m)
  data <- vary_parm_fun(c2)
  data$m <- m
  return(data)
}


data <- mclapply(1:100, function(x) main(1, x), mc.cores = getOption("mc.cores", 7L))
data <- do.call(rbind, data)
write.csv(data, "../Data/data.csv", row.names = FALSE)

start <- Sys.time()
test_df <- vary_parm_fun(c2)
time <- Sys.time() - start

ratio_fun <- function(B, c2, beta,m){
  df_all <- list()
  for (gamma in c(0.5, 1, 2, 10)){
    for (sigma in c(0.5, 1, 2, 10)){
      for(p in c(0.3, 0.5, 0.7)){
        data <- lapply(c(1,2,5,seq(10,100, by = 10)), function(ratio)
          scenario_fun(B, c2, beta, gamma, sigma, ratio, p)
        )
        data <- do.call(rbind, data)
        df_all[[length(df_all)+1]] <- data
      }
    }
  }
  df_all <- do.call(rbind, df_all)
  df_all$M <- m
  file_name <- paste0("../Data/data_", m, ".csv")
  write.csv(df_all, file_name, row.names = FALSE)
  return()
}

set.seed(2610)
mclapply(1:100, function(x) ratio_fun(1000, 1, 10,x), mc.cores = getOption("mc.cores", 7L))
