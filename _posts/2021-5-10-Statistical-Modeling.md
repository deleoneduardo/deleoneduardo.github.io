---
layout: post
title: "Modeling State Political Affiliation from Income Inequality and Health Care Costs"
author: "Eduardo De Leon"
---

## Introduction
This study examines health care access components such as private health insurance, Medicare, Medicaid, dental, physician, and clinical services to find a relationship with the income inequality ratio between the average income of the top 1% and the bottom 99% of income earners. Utilizing publicly available state-level data from 2013 on health care costs, data from the Economic Policy Institute on income inequality, and multiple other income and health related control variables from the Centers for Disease Control and Prevention will allow us to run multiple linear regression focusing on how health care affects income inequality. 
<br /><br />
The following regression model estimates the relationship between five components to health care access and the income inequality ratio: 
<center> (<i>Inequality Ratio</i>) = α + β<sub>1</sub>(<i>Private</i>) + β<sub>2</sub>(<i>Medicare</i>) + β<sub>3</sub>(<i>Medicaid</i>) + β<sub>4</sub>(<i>Dental</i>) + β<sub>5</sub>(<i>Clinical</i>) + β<sub>6</sub>(<i>Region</i>) + β<sub>7</sub>(<i>Age</i>) + β<sub>8</sub>(<i>Race</i>) + u </center> 
To describe state-by-state income inequality, the Economic Policy Institute, an independent think tank, investigated the impact of economic trends and policies in 2013 around [income inequality](https://www.epi.org/publication/income-inequality-in-the-us/#epi-toc-6). Data on health care components was provided by the Centers for Medicare & Medicaid Services and derived from the National Health Statistics Groups to examine [access to health care](https://www.cms.gov/research-statistics-data-and-systems/statistics-trends-and-reports/nationalhealthexpenddata/nhe-fact-sheet). Bridged-race population estimates were manually translated into spreadsheet format; the [data request](https://wonder.cdc.gov/bridged-race-population.html) can be made from the U.S. Census Bureau in collaboration with the National Center for Health Statistics. Finally, a dataset on U.S. Presidential Elections from 1976–2020 from the [Harvard Dataverse](https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/42MVDX) specified presidential results in 2012.
<br /><br />
Piketty and Saenz (2004) highlight the similarities between modern income inequality levels and 20th-century income inequality prior to World War II. As a potential association, I expect health care costs to be higher in states with greater wealth inequality.
<br /><br />

```
# Setting libraries and installing new packages
library(readxl)
library(tidyverse)
library(dplyr)
library(lmtest)
library(sandwich)
library(plotROC) 
library(ggplot2)
library("PerformanceAnalytics")

# Create a dataset with all state health care data
data = read_excel("health-care.xlsx")

# Create a dataset with U.S. election results
election = read_excel("election.xlsx")

# Tidy and merge
data = data %>% left_join(election, by = "state")

# Create a binary variable coded as 0 and 1
data = data %>% mutate(party = ifelse(vote == "Democrat", 1, 0))
```

This data is tidy because every column is a variable and every row is an observation. Furthermore, I merged the data into a single dataset named `data`.

## EDA

```
chart.Correlation(data %>% select(inequality_ratio, private, medicare, medicaid, dental, clinical), histogram = TRUE)
```

The correlation matrix above shows the distribution of each variable on the diagonal. On the bottom of the diagonal, the bivariate scatter plots with a fitted line are displayed. On the top of the diagonal, the value of the correlation plus the significance level as stars are displayed. Finally, each significance level is associated to an asterisk symbol.

## MANOVA

```
# Perform MANOVA with 4 response variables 
manova_y = manova(cbind(inequality_ratio, private, medicaid, dental) ~ party, data = data)

# Output of MANOVA
summary(manova_y)

# If MANOVA is significant then we can perform one-way ANOVA for each variable
summary.aov(manova_y)

# If ANOVA is significant then we can perform post-hoc analysis
pairwise.t.test(data$private, data$party, p.adj="none")
pairwise.t.test(data$dental, data$party, p.adj="none")

1 - 0.95^7
0.001/7
0.01/7
```

Since the MANOVA was significant, we performed univariate ANOVA to find responses showing a mean difference across groups, and performed post-hoc t-tests to find which groups differ. We calculated 7 tests, so the probability of at least one type I error is 0.3016627039.

After the Bonferroni correction, only one p-value that had been statistically significant is still statistically significant (the ANOVA on `private` healthcare cost variable).

The assumptions of this analysis of variance were most likely met, due to the sampling method of my data. The four assumptions that need to be fulfilled, interval data of the dependent variable, normality, homoskedasticity, and no multicollinearity, are then assumed. 

## Randomization Test

```
# Compute the t-statistic under H0: mu = 0.5 (which is half of the states)
t = (mean(data$party))/(sd(data$party)/sqrt(50)) 
t
t.test(data$party, mu = 0.5)

# Create an empty vector to store simulated t-statistics
t = vector() 

# Create many samples with a for loop
for(i in 1:5000){
  # Take a sample size of 25 from a population with mean mu=5 and sd=1 (default)
  samp = rnorm(50, mean = 0.5)  
  # Compute the t-statistic under H0: mu=5 (which is true)
  t[i] = (mean(samp) - 0.5)/(sd(samp)/sqrt(50)) 
}

# Consider the vector t as a data frame
data.frame(t) %>% ggplot(aes(t)) + geom_histogram(aes(y=..density..), bins = 30) + stat_function(fun = dt, args=list(df = 49), geom="line")
```

The null hypothesis H<sub>0</sub>: mu = 0.5, when in reality, this proportion is 0.5294. Our p-value of 0.678 is statistically insignificant. Thus. we fail to reject the null hypothesis and have convincing evidence for the null. 

The null hypothesis means that 50% of the states voted for the Democratic party. Through a randomization test, we repeat this 3000 times. Finally, we create a plot visualizing the null distribution and the test statistic.

## Linear Regression

```
# Fit a linear regression model
fit = lm(inequality_ratio ~ private + medicare + medicaid + dental + clinical + far_west + great_lakes + mideast + new_england + plains + rocky_mountains + southeast + southwest + age + white + black + asian + native + hispanic, data = data)

# Residuals against fitted values plot to check for any problematic patterns (nonlinear, equal variance)
plot(fit, which = 1)

# Q-Q plot to check for normality of the residuals
plot(fit, which = 2)

# Model
summary(fit)

# Mean-center the numeric variables involved in the interaction (helps reduce multicollinearity)
center_scale = function(x) {
  scale(x, scale = FALSE)
}
data$private = center_scale(data$private)
data$medicaid = center_scale(data$medicaid)
data$dental = center_scale(data$dental)

# Robust standard errors
coeftest(fit, vcov = vcovHC(fit))

# Example of estimating coefficients standard errors
samp_SEs = replicate(5000, {
  # Bootstrap your data (resample observations)
  boot_data = sample_frac(data, replace = TRUE)
  # Fit regression model
  fitboot = lm(inequality_ratio ~ private + medicare + medicaid + dental + clinical + far_west + great_lakes + mideast + new_england + plains + rocky_mountains + southeast + southwest + age + white + black + asian + native + hispanic, data = boot_data)
  # Save the coefficients
  coef(fitboot)})

# Estimated standard errors
samp_SEs %>% t %>% as.data.frame %>% summarize_all(sd)
```

A regression model to predict the average income of the lower 99% of income earners from healthcare costs was built. To interpret the coefficient `Personal:Medicaid:Dental`: I used an interaction term of order 3 to represent the three-way interaction between the `Personal`, `Medicaid`, and `Dental` variables. In this case, the relationship between the `Lower` variable and `Medicaid` costs depends on both `Personal` healthcare and `Dental` costs. 

Since the R-squared is equal to 0.5434, our model explains for 54.34% of the variation in the response. 

The assumptions for linear regression were met by plotting the residuals against the fittest values to check for nonlinearity and equal variance, and by constructing a Q-Q plot to check for normality of the residuals. 

Then, we calculated the robust SEs and bootstrapped SEs. The bootstrapped standard errors are lower than the robust SEs. Conversely, the robust SEs are lower compared to the original SEs of the model. The p-values of the robust SEs, however, are higher than the original p-values from the model.

## Logistic Regression

```
# Logistic model
mylogit = glm(party ~ inequality_ratio + private + medicare + medicaid + dental + clinical, data = data, family = "binomial")
summary(mylogit)

# Compare to the exponentiated coefficients of the model
exp(coef(mylogit))

# Predicted log odds 
data$logit = predict(mylogit, type = "link") 

# Density plot of log-odds for each outcome
data %>% ggplot() + geom_density(aes(logit, color = vote, fill = vote), alpha = 0.4) + geom_rug(aes(logit, color = vote)) + geom_text(x = -5, y = .07, label = "TN = 431") + geom_text(x = -1.75, y = .008, label = "FN = 19") + geom_text(x = 1, y = 0.006, label = "FP = 13") + geom_text(x = 5, y = 0.04, label = "TP = 220") + theme(legend.position = c(.85,.85)) + geom_vline(xintercept = 0) + xlab("logit (log-odds)") + ggtitle("Density of log-odds")

# Predicted probabilities
data$prob1 = predict(mylogit, type = "response")
data$predicted = ifelse(data$prob1 > 0.5, "Democratic", "Republican")

# Confusion matrix: compare true to predicted condition
table(true_condition = data$party, predicted_condition = data$predicted) %>% 
  addmargins

# Accuracy = (TN + TP)/(TN + FP + FN + TP)
(21 + 18)/(21 + 3 + 9 + 18)

# Precision = TP/(FP + TP)
18/(3 + 18)

# Sensitivity = TP/(TP + FN)
18/(18 + 9)

# Specificity = TN/(TN + FP)
21/(21 + 3)

# Plot ROC depending on values of y and its probabilities displaying some cutoff values
ROCplot1 = ggplot(data) + geom_roc(aes(d = party, m = prob1), cutoffs.at = list(0.1, 0.5, 0.9)) + ggtitle("ROC Curve")
ROCplot1

# Calculate the area under the curve 
calc_auc(ROCplot1)
```

Controlling for private, Medicaid, and dental health care costs, for every 1-unit increase in the income inequality ratio, the odds of a state voting for the Democratic party change by a factor of 0.9771875 (i.e., decrease by 2.28%).

Controlling for the inequality ratio, Medicaid, and dental health care costs, for every 1-unit increase in the private health care spending per capita, the odds of a state voting for the Democratic party change by a factor of 1.0011 (i.e., increase by 0.11%).

Controlling for the income inequality ratio, private and dental health care costs, for every 1-unit increase in Medicaid costs per enrollee, the odds of a state voting for the Democratic party change by a factor of 0.995981 (i.e., decrease by 0.401%).

Controlling for the income inequality ratio, private and Medicaid costs, for every 1-unit increase in dental care costs per capita, the odds of a state voting for the Democratic party change by a factor of 1.0309 (i.e., increase by 3.09%).

Then, we computed the accuracy, sensitivity, specificity, and recall. We found that, since our false negatives and false positives counts are close, an accuracy of 0.7647 is a good measure since we have a somewhat symmetric data set. Our precision is 0.8571, our sensitivity is 0.6666, and the specificity is 0.875. SInce we know that precision and recall are inversely proportional, this leads us to believe that the measure of recall is fairly low. Lastly, these measures point to the fact that there was appropriate-fitting from the data.

The AUC, which measures how true positive rate (recall) and false positive rate trade off came out to be 0.8101852. Therefore, the AUC has a moderate classification accuracy, judging from our rule of thumb.
