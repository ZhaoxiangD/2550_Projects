# Evaluating the Impact of Behavioral Activation on Smoking Cessation in Major Depressive Disorder

### Background 

This study investigates the factors affecting the standard error of the estimator in both normal and Poisson distributions. The focus is on how the number of clusters, measurements per cluster, and resource constraints influence the precision of the estimator. Key parameters such as cost ratio, $\gamma$ (variance of sampling error), and $\alpha$ (mean of the outcome) are examined to identify their roles in determining the optimal study design.

### Methods 

A simulation approach was used to evaluate the performance of the estimator under various parameter settings. The data followed hierarchical models with binary treatment assignment. The normal distribution setting assumed the variance of the outcome and sampling error as separate contributors, while the Poisson distribution assumed equality between the mean and variance. Different budget levels, cost ratios, and parameter values were tested to assess their effects on the estimator’s standard error. The analysis was conducted using grid search methods to identify the optimal balance between the number of clusters and measurements per cluster.

### Results 

For the normal distribution, the standard error consistently decreased with an increasing number of clusters under most settings, reflecting theoretical expectations. In contrast, for the Poisson distribution, the standard error showed less sensitivity to parameters like $\alpha$, even though theory predicts its influence due to the equality of mean and variance. Unexpectedly, when $\gamma$ = 1, fewer clusters were optimal compared to other $\gamma$ values. Across both distributions, higher budgets led to lower standard errors, as more samples could be collected. These results emphasize the importance of balancing clusters and measurements per cluster to optimize the standard error within resource constraints. However, some deviations from theoretical expectations, particularly in the Poisson distribution, suggest the need for further investigation. The report can be found [here](Report/Report.pdf).

## Files

### Code

`data_generation.R`: Generate simulation data.
`data_generation.R`: Estimate treatment effect from the generated datasets.

### Result

`estimation_result.csv`: Raw result from `data_generation.R`.
`estimation_summary.csv`: Calculated SE and Bias result from `estimation_result.csv`.

### Report

`Report.qmd`: Quarto mark down version of the report.

`Report.pdf`: PDF version of the report.

## Dependencies

The following packages were used in this analysis:

- Data Manipulation: `dplyr`, `tidyr`, `data.table`
- Table Formatting: `gtsummary`, `latex2exp`, `gt`
- Model Fitting: `lme4`
- Visualization: `ggplot2`, `ggpubr`
- parallelzation: `parallel`

