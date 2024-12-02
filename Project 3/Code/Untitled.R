data_generate_fun <- function(n_sample, n_cluster, beta, gamma, sigma){
  alpha <- 10
  for (c in 1:n_cluster){
    X <- rnorm(n_sample, 40, 10)
    mu <- rnorm(n_sample, 10 + beta*X, sqrt(gamma))
    Y <- rnorm(n_sample, mu, sqrt(sigma))
  }
  return(data.frame(X, Y, mu, alpha, beta, gamma, sigma, n_cluster, n_sample))
}

senario_fun <- function(B, c1, beta, gamma, sigma, ratio){
  c2 <- c1 * ratio
  data_all <- list()
  for (s in 1:5){
    if (s == 1){
      n_sample <- 1
      n_cluster <- floor(B/c2)
    } else if (s == 5){
      n_cluster <- 1
      n_sample <- floor((B-c2)/c1) + 1
    } else {
      r <- ifelse(s == 2, 0.5, ifelse(s == 3, 1, 2)) # sample/cluster ratio: senario 2: 0.5, senario 3: 1, senario 4: 2
      n_cluster_1 <- (-c2 + sqrt((c2)**2 + 4*r*c1*B)) / 2*c1*r
      n_cluster_2 <- (-c2 - sqrt((c2)**2 + 4*r*c1*B)) / 2*c1*r
      n_cluster <- floor(max(n_cluster_1, n_cluster_2))
      n_sample <- r * n_cluster + 1
    }
    
    data <- data_generate_fun(n_sample, n_cluster, beta, gamma, sigma)
    data$senario <- s
    data$ratio <- ratio
    data_all[[s]] <- data
  }
  return(do.call(rbind, data_all))
}

ratio_fun <- function(B, c1, beta){
  df_all <- list()
  for (gamma in c(0.5, 1, 2, 10)){
    for (sigma in c(0.5, 1, 2, 10)){
      data <- lapply(c(1,2,5,seq(10,100, by = 10)), function(ratio)
        senario_fun(B, c1, beta, gamma, sigma, ratio)
      )
      data <- do.call(rbind, data)
      data$sigma <- sigma
      data$gamma <- gamma
      df_all[[length(df_all)+1]] <- data
    }
  }
  
  return(do.call(rbind, df_all))
}

df <- ratio_fun(1000, 1, 10)
write.csv(df, "data.csv", row.names = FALSE)