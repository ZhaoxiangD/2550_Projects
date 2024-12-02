library(parallel)
setwd("/Users/zhaoxiangding/Documents/GitHub/2550_Projects/Project 3/Data")
data_generate_fun <- function(n_sample, n_cluster, beta, gamma, sigma,p){
  alpha <- 10
  df_all <- list()
  for (c in 1:n_cluster){
    if (n_cluster == 2 & c == 2){
      X <- ifelse(df_all[[1]]$X[1] == 1, 0, 1)
    } else {
      X <- rbinom(1,1,p)
    }
    mu <- rnorm(n_sample, 10 + beta*X, sqrt(gamma))
    Y <- rnorm(n_sample, mu, sqrt(sigma))
    cluster <- rep(c, n_sample)
    
    df <- data.frame(X, Y, mu, alpha, beta, gamma, sigma, cluster)
    df_all[[length(df_all)+1]] <- df
  }
  return(do.call(rbind, df_all))
}

scenario_fun <- function(B, c1, beta, gamma, sigma, ratio, p){
  c2 <- c1 * ratio
  data_all <- list()
  for (s in 1:5){
    if (s == 1){
      n_sample <- 1
      n_cluster <- floor(B/c2)
    } else if (s == 5){
      n_cluster <- 2
      n_sample <- floor(((B-c2)/c1)/2)
    } else {
      r <- ifelse(s == 2, 0.5, ifelse(s == 3, 1, 2)) # sample/cluster ratio: scenario 2: 0.5, scenario 3: 1, scenario 4: 2
      n_cluster_1 <- (-c2 + sqrt((c2)**2 + 4*r*c1*B)) / 2*c1*r
      n_cluster_2 <- (-c2 - sqrt((c2)**2 + 4*r*c1*B)) / 2*c1*r
      n_cluster <- floor(max(n_cluster_1, n_cluster_2))
      n_cluster <- ifelse(n_cluster < 2, 2, n_cluster)
      n_sample <- r * n_cluster
    }
    
    data <- data_generate_fun(n_sample, n_cluster, beta, gamma, sigma, p)
    data$scenario <- s
    data$ratio <- ratio
    data$p <- p
    data_all[[s]] <- data
  }
  return(do.call(rbind, data_all))
}

ratio_fun <- function(B, c1, beta,m){
  df_all <- list()
  for (gamma in c(0.5, 1, 2, 10)){
    for (sigma in c(0.5, 1, 2, 10)){
      for(p in c(0.2, 0.5, 0.8)){
        data <- lapply(c(1,2,5,seq(10,100, by = 10)), function(ratio)
          scenario_fun(B, c1, beta, gamma, sigma, ratio, p)
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

mclapply(1:100, function(x) ratio_fun(1000, 1, 10,x), mc.cores = getOption("mc.cores", 7L))
