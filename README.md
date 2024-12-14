# 2550_Projects

## Project 1
### Exploriatry data analysis on impact of weather on marathon performance across age and gender.

**Backgrounds**: Aging is associated with impaired thermoregulation and reduced heat tolerance in runners, potentially affecting their marathon performance. However, it remains unclear whether this impact is consistent across genders. Additionally, weather conditions can influence marathon performance, but the specific effects on runners of different ages and sexes have not been fully investigated.

**Methods**: This report examines the relationship between marathon performance and age across men and women. A novel variable is proposed to measure marathon performance, assuming that weather has no effect on performance. This variable is expected to be zero across all ages under this hypothesis. By using this new variable, the study also explores the relationship between weather and marathon performance, across all ages and genders.

**Results**: Exploratory data analysis reveals a non-linear relationship between age and marathon performance. Performance initially increases with age until approximately 26 years old, then declines. Younger runners are most susceptible to weather effects, with a maximum of 50% reduction in marathon performance. However, older runners are more likely to be affected by weather. Female runners are less susceptible to weather effects than male runners. Air quality can amplify the impact of adverse weather conditions on marathon performance.

## Project 2
### Evaluating the Impact of Behavioral Activation on Smoking Cessation in Major Depressive Disorder

**Backgrounds**: Behavioral Activation (BA) is a therapeutic approach that has shown promise for aiding smoking cessation in individuals with Major Depressive Disorder (MDD). However, evidence regarding its effectiveness in smoking cessation is limited, with previous studies, including a 2x2 randomized factorial design trial, indicating no significant effect of BA on cessation rates. This study aims to re-examine BA’s effectiveness using the same dataset from the prior trial but with an alternative analytical approach to uncover potential moderators of BA's effect on end-of-treatment (EOT) abstinence and to identify baseline predictors of abstinence.

**Methods**: This analysis uses logistic regression to model the odds of abstinence rather than just abstinence rates, calculating the causal effect through the odds ratio between the treatment and control groups under intent-to-treat (ITT) principles. We performed variable selection using techniques such as Lasso regression, subset selection with L0 penalty, and subset selection with ridge regression. Multiple imputation was applied to handle missing data, and models were evaluated based on their Area Under the Curve (AUC) and calibration result in test datasets, aiming to select the model with the highest predictive power and robustness.

**Results**: The results align with previous findings, showing that BA has no significant impact on smoking cessation in MDD patients. However, important baseline predictors for smoking cessation were identified: `ftcd_score` and `NHW` (non-Hispanic White) were significant predictors of abstinence, while `Age` moderated the effect of pharmacotherapy on cessation. The subset selection model with L0 penalty demonstrated the best performance with the highest AUC, underscoring its robustness in identifying key predictors and interactions within the dataset. Despite these findings, limitations include model dependency and sample size, which may restrict the exploration of interaction effects. 

## Project 3
### Optimizing Study Design: Balancing Clusters and Measurements for Efficient Estimation in Normal and Poisson Distributions

**Background**: This study investigates the factors affecting the standard error of the estimator in both normal and Poisson distributions. The focus is on how the number of clusters, measurements per cluster, and resource constraints influence the precision of the estimator. Key parameters such as cost ratio, $\gamma$ (variance of sampling error), and $\alpha$ (mean of the outcome) are examined to identify their roles in determining the optimal study design.

**Methods**: A simulation approach was used to evaluate the performance of the estimator under various parameter settings. The data followed hierarchical models with binary treatment assignment. The normal distribution setting assumed the variance of the outcome and sampling error as separate contributors, while the Poisson distribution assumed equality between the mean and variance. Different budget levels, cost ratios, and parameter values were tested to assess their effects on the estimator’s standard error. The analysis was conducted using grid search methods to identify the optimal balance between the number of clusters and measurements per cluster.

**Results**: For the normal distribution, the standard error consistently decreased with an increasing number of clusters under most settings, reflecting theoretical expectations. In contrast, for the Poisson distribution, the standard error showed less sensitivity to parameters like $\alpha$, even though theory predicts its influence due to the equality of mean and variance. Unexpectedly, when $\gamma$ = 1, fewer clusters were optimal compared to other $\gamma$ values. Across both distributions, higher budgets led to lower standard errors, as more samples could be collected. These results emphasize the importance of balancing clusters and measurements per cluster to optimize the standard error within resource constraints. However, some deviations from theoretical expectations, particularly in the Poisson distribution, suggest the need for further investigation.

