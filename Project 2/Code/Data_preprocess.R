data <- read.csv("../Data/project2.csv")
num_col <- c(5,12,14:19,23,25)
ordinal_col <- c(10,11)
data[,num_col] <- lapply(data[,num_col], as.numeric)
data[,ordinal_col] <- lapply(data[,ordinal_col], factor, order = T)

data[,-c(num_col, ordinal_col)] <- lapply(data[,-c(num_col, ordinal_col)], factor)
