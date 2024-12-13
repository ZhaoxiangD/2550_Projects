# Exploriatry data analysis on impact of weather on marathon performance across age and gender.

### Backgrounds
Aging is associated with impaired thermoregulation and reduced heat tolerance in runners, potentially affecting their marathon performance. However, it remains unclear whether this impact is consistent across genders. Additionally, weather conditions can influence marathon performance, but the specific effects on runners of different ages and sexes have not been fully investigated.

### Methods
This report examines the relationship between marathon performance and age across men and women. A novel variable is proposed to measure marathon performance, assuming that weather has no effect on performance. This variable is expected to be zero across all ages under this hypothesis. By using this new variable, the study also explores the relationship between weather and marathon performance, across all ages and genders.

### Results
Exploratory data analysis reveals a non-linear relationship between age and marathon performance. Performance initially increases with age until approximately 28 years old, then declines. Younger runners are most susceptible to weather effects, with a maximum of 50% reduction in marathon performance. However, older runners are more likely to be affected by weather. Female runners are less susceptible to weather effects than male runners. Air quality can amplify the impact of adverse weather conditions on marathon performance. The full report can be found [here](Report/EDA-Marathon.pdf).

## Files

### Code

`Data-Preprocess.R`: Preprocess the data for exploratory data analysis.

### Report

`EDA-Marathon.qmd`: Quarto mark down version of exploratory data analysis report.

`EDA-Marathon.pdf`: PDF version of exploratory data analysis report.

## Dependencies

The following packages were used in this analysis:

- Data Manipulation: `dplyr`, `tidyr`, `lubridate`, `stringr`
- Table Formatting: `gtsummary`, `kableExtra`
- Data Visualization: `ggplot2`, `RColorBrewer`, `gridExtra`, `ggpubr`, `latex2exp`


