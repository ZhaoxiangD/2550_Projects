# load libraries
library(dplyr)
library(tidyr)
library(kableExtra)
library(gtsummary)
library(lubridate)
library(ggplot2)
library(stringr)
library(latex2exp)
library(ggpubr)
library(RColorBrewer)
library(gridExtra)

setwd('/Users/zhaoxiangding/Documents/GitHub/2550_Projects/Project 1/Data')

# Read data
data <- read.csv("../Data/project1.csv")
air <- read.csv("../Data/aqi_values.csv")
# change coloumn name to make it easier to understand
colnames(data)[[1]] <- 'Race'
colnames(data)[[3]] <- 'Sex'

data_na <- data[data$Flag == '',]
data <- data[data$Flag != '',]

data_na$location <- case_when(data_na$Race == 0 ~ 'Boston Marathon',
                              data_na$Race == 1 ~ 'Chicago Marathon',
                              data_na$Race == 2 ~ 'New York City Marathon',
                              data_na$Race == 3 ~ 'Twin Cities Marathon',
                              data_na$Race == 4 ~ 'Grandma’s Marathon')
data_na$marathon <- paste(data_na$location, data_na$Year, sep = ' in ')
na_df <- unique(data_na$marathon)
na_df <- str_split(na_df, ' in ')
na_df <- as.data.frame(do.call(rbind, na_df))
colnames(na_df) <- c('Race', 'Year')

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

# Merge data
data_record <- left_join(data, record, by = c('Race', 'Year', 'Sex'))

# fastest running time
data_record$Time <- data_record$seconds + (data_record$X.CR/100) * data_record$seconds
data_record$locations <- case_when(data_record$Race == 0 ~ 'Boston Marathon',
                                   data_record$Race == 1 ~ 'Chicago Marathon',
                                   data_record$Race == 2 ~ 'New York City Marathon',
                                   data_record$Race == 3 ~ 'Twin Cities Marathon',
                                   data_record$Race == 4 ~ 'Grandma’s Marathon')

# Table 1
table(data$Flag) %>%
  kable(col.names = c('Flag', 'Count')) %>%
  kable_styling()

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

# Merge data
data_record <- left_join(data, record, by = c('Race', 'Year', 'Sex'))

# fastest running time
data_record$Time <- data_record$seconds + (data_record$X.CR/100) * data_record$seconds
data_record$locations <- case_when(data_record$Race == 0 ~ 'Boston Marathon',
                                   data_record$Race == 1 ~ 'Chicago Marathon',
                                   data_record$Race == 2 ~ 'New York City Marathon',
                                   data_record$Race == 3 ~ 'Twin Cities Marathon',
                                   data_record$Race == 4 ~ 'Grandma’s Marathon')

# Fig 1
ggplot(data_record, aes(x = Age..yr., y = Time, color = as.factor(Sex))) + 
  geom_point(size = 0.2, alpha = 0.2) +
  geom_smooth(se = F) +
  scale_color_discrete(name = 'Gender', label = c('Female','Male'))+
  scale_y_continuous(breaks = c(10800,18000, 25200, 32400), 
                     labels = c(3,5,7,9),
                     name = 'Fastest Finish Time (Hour)') +
  scale_x_continuous(name = 'Age (Year)')+
  facet_wrap(~locations) + 
  theme_minimal() + 
  theme(legend.position = c(.85,.2))

sex_label <- c(`0` = 'Female', `1` = 'Male')

ggplot(data_record, aes(x = Age..yr., y = X.CR)) + 
  geom_point(aes(color = as.factor(Flag)),
             size = 0.05, alpha = 0.2, position = position_jitter(width = 1)) +
  geom_vline(xintercept = c(26, 52, 75), linetype = 'dashed') +
  geom_text(x = 18, y = 300, label = '< 26', vjust = -1) +
  geom_text(x = 39, y = 300, label = '26 - 52', vjust = -1) +
  geom_text(x = 63.6, y = 300, label = '52 - 75', vjust = -1) +
  geom_text(x = 85, y = 300, label = '75 + ', vjust = -1) +
  geom_smooth(aes(color = as.factor(Flag)), se = F) + 
  scale_color_manual(values = c('#06d6a0', '#ef476f', '#118ab2', '#ffd166'), name = 'Weather flag') +
  scale_y_continuous(name = 'Percent off course record (%)') +
  scale_x_continuous(name = 'Age (Year)') +
  facet_wrap(~Sex, labeller = as_labeller(sex_label)) +
  theme_minimal() + 
  theme(strip.text = element_text(face = 'bold'))
####
record_flag <- left_join(record, data[,1:4], by = c('Race', 'Year', 'Sex'))
record_flag <- record_flag[!is.na(record_flag$Flag),]
record_break <- record_flag %>%
  distinct(CR, .keep_all = T)

flag_break_record <- round(as.numeric(summary(as.factor(record_break$Flag))*100/nrow(record_break)),3)
flag_data <- round(as.numeric(summary(as.factor(data$Flag))*100/nrow(data)),3)

flag_df <- data.frame(Flag = c('Green', 'Red', 'White', 'Yellow'), 
                      Record = flag_break_record, 
                      Data = flag_data)

Avg_CR_unweighted <- data_record %>% #unweighted mean
  group_by(Sex, Age..yr.) %>%
  summarise(mean_CR_unweight = mean(X.CR))

Flag_Weight <- data_record %>% # calculate weight based on flags
  group_by(Flag) %>%
  summarise(weight = n()/nrow(data_record))

data_record_weight <- left_join(data_record, Flag_Weight, by = 'Flag')

# weighted mean
Avg_CR_weighted <- data_record_weight %>%
  group_by(Sex, Age..yr.) %>%
  summarise(mean_CR_weighted = weighted.mean(X.CR, w = weight),
            mean_TdC_weighted = weighted.mean(Td..C, w = weight),
            mean_TwC_weighted = weighted.mean(Tw..C, w = weight),
            mean_TgC_weighted = weighted.mean(Tg..C, w = weight),
            mean_WBGT_weighted = weighted.mean(WBGT, w = weight),
            mean_SR.W.m2_weighted = weighted.mean(SR.W.m2, w = weight),
            mean_DP_weighted = weighted.mean(DP, w = weight),
            mean_Wind_weighted = weighted.mean(Wind, w = weight))

# merge data
data_record_weight <- left_join(data_record_weight, 
                                Avg_CR_weighted, 
                                by = c('Sex', 'Age..yr.'))
data_record_weight <- left_join(data_record_weight, 
                                Avg_CR_unweighted, 
                                by = c('Sex', 'Age..yr.'))

data_record_weight$below_avg <- ifelse(data_record_weight$X.CR < data_record_weight$mean_CR_unweight, 1, 0)
data_record_weight$below_avg_w <- ifelse(data_record_weight$X.CR < data_record_weight$mean_CR_weighted, 1, 0)

#converge data to long format for Fig 3
data_weight_long <- pivot_longer(data_record_weight, 
                                 cols = c('Td..C', 'Tw..C', 'Tg..C', 'WBGT'), 
                                 names_to = 'Type', values_to = 'Temp')

data_weight_long <- pivot_longer(data_weight_long, 
                                 cols = c('below_avg', 'below_avg_w'), 
                                 names_to = 'Cr_type', 
                                 values_to = 'Below_avg_CR')
data_weight_long$Cr_type <- ifelse(data_weight_long$Cr_type == 'below_avg', 
                                   'Arithmetic mean ', 
                                   'Weighted Mean')
data_weight_long$Below_avg_CR <- ifelse(data_weight_long$Below_avg_CR == 1, 
                                        'Below average', 
                                        'Above average')

# Fig3
ggplot(data_weight_long, aes(x = Type, y = Temp, color = as.factor(Below_avg_CR))) + 
  geom_boxplot() + 
  scale_color_discrete(name = '') + 
  scale_x_discrete(name = 'Measurements', 
                   labels = c('Dry bulb', 'Black globe', 'Wet bulb', 'WBGT'),
                   guide = guide_axis(n.dodge=3)) +
  scale_y_continuous(name = 'Temperature (Celsius)') +
  facet_wrap(~Cr_type) +
  theme_minimal()

data_record_weight$Avg_Cr_Diff <- (data_record_weight$X.CR - 
                                     data_record_weight$mean_CR_weighted)/
  data_record_weight$mean_CR_weighted

data_record_weight$Flag <- factor(data_record_weight$Flag, 
                                  ordered = T, 
                                  levels = c('White','Green', 'Yellow', 'Red'))
data_record_weight$below_avg_w <- ifelse(data_record_weight$below_avg_w == 1, 
                                         'Below average', 
                                         'Above average')

# table 2
tbl_summary(data_record_weight, by = below_avg_w, 
            include = c(Flag, Td..C, Tw..C, Tg..C, WBGT, X.rh, SR.W.m2, DP, Wind)) %>% 
  add_p()

# Tg flag
# not used in the final report
data_record_weight$Tg_flag <- case_when(data_record_weight$Tg..C < 19.475000 ~ 1,
                                        data_record_weight$Tg..C < 24.955556 ~ 2,
                                        data_record_weight$Tg..C < 30.000000 ~ 3,
                                        TRUE ~ 4)
data_record_weight$Tg_flag <- as.factor(data_record_weight$Tg_flag)

data_record_weight$Avg_Cr_Diff <- (data_record_weight$X.CR - 
                                     data_record_weight$mean_CR_weighted)/
  data_record_weight$mean_CR_weighted

data_record_weight$Sex <- ifelse(data_record_weight$Sex == 1, 'Male', 'Female')

# Fig 4
data_record_weight %>%
  group_by(Sex, Age..yr., Flag) %>%
  summarise(mean_cr = mean(Avg_Cr_Diff)) %>%
  ggplot(aes(x = Age..yr., y = mean_cr, color = as.factor(Flag))) +
  geom_point(size= 0.5) +
  geom_smooth(se = T) +
  scale_color_manual(values = c('#118ab2','#06d6a0', '#ffd166', '#ef476f'),
                     name = 'Weather Flag') +
  scale_y_continuous(name = 'Normalized Difference') +
  scale_x_continuous(name = 'Age (Year)') +
  facet_wrap(~Sex, labeller = as_labeller(sex_label))+
  theme_minimal() + 
  theme(strip.text = element_text(face = 'bold'))
