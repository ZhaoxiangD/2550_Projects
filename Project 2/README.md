# Evaluating the Impact of Behavioral Activation on Smoking Cessation in Major Depressive Disorder

### Backgrounds

Behavioral Activation (BA) is a therapeutic approach that has shown promise for aiding smoking cessation in individuals with Major Depressive Disorder (MDD). However, evidence regarding its effectiveness in smoking cessation is limited, with previous studies, including a 2x2 randomized factorial design trial, indicating no significant effect of BA on cessation rates. This study aims to re-examine BA’s effectiveness using the same dataset from the prior trial but with an alternative analytical approach to uncover potential moderators of BA's effect on end-of-treatment (EOT) abstinence and to identify baseline predictors of abstinence.

### Methods

This analysis uses logistic regression to model the odds of abstinence rather than just abstinence rates, calculating the causal effect through the odds ratio between the treatment and control groups under intent-to-treat (ITT) principles. Due to categorical data and potential interaction terms, we performed variable selection using techniques such as Lasso regression, subset selection with L0 penalty, and subset selection with ridge regression. Multiple imputation was applied to handle missing data, and models were evaluated based on their Area Under the Curve (AUC) in test datasets, aiming to select the model with the highest predictive power and robustness.

### Results

The results align with previous findings, showing that BA has no significant impact on smoking cessation in MDD patients. However, important baseline predictors for smoking cessation were identified: `ftcd_score`, `NHW` (non-Hispanic White), and `NMR` (nicotine metabolite ratio) were significant predictors of abstinence, while `Age` moderated the effect of pharmacotherapy on cessation. The subset selection model with L0 penalty demonstrated the best performance with the highest AUC, underscoring its robustness in identifying key predictors and interactions within the dataset. Despite these findings, limitations include model dependency and sample size, which may restrict the exploration of interaction effects. The report can be found [here](Report/Project2.pdf).

## Files

### Code

`Data-Preprocess.R`: Preprocess the data for exploratory data analysis.

### Report

`Project2.qmd`: Quarto mark down version of exploratory data analysis report.

`Project2.pdf`: PDF version of exploratory data analysis report.

## Dependencies

The following packages were used in this analysis:

- Data Manipulation: `dplyr`
- Table Formatting: `gtsummary`, `kableExtra`
- Model Fitting: `glmnet`, `mice`, `pROC`, `L0Learn`


