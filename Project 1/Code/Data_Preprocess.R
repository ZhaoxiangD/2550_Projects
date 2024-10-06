library(dplyr)
library(tidyr)


# Marathon performance data
data <- read.csv("../Data/project1.csv")
air <- read.csv("../Data/aqi_values.csv")
# change coloumn name to make it easier to understand
colnames(data)[[1]] <- 'Race'
colnames(data)[[3]] <- 'Sex'

data <- data[data$Flag != '',]

# Mrathon record data
record <- read.csv("../Data/course_record.csv")
record$Race <- case_when(
  record$Race == 'B' ~ 0,
  record$Race == 'C' ~ 1,
  record$Race == 'NY' ~ 2,
  record$Race == 'TC' ~ 3,
  record$Race == 'D' ~ 4
)

colnames(record)[[4]] <- 'Sex'
record$seconds <- period_to_seconds(hms(record$CR))
record$Sex <- ifelse(record$Sex == 'M', 1, 0)

# Aqi data
air <- air[!is.na(air$aqi),]