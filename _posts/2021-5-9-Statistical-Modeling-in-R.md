---
layout: post
title: "Statistical Modeling in R"
author: "Eduardo De Leon"
---
I examine several components of health care access, such as Medicare and Medicaid, to find a relationship with statewide income inequality in the United States.

## Introduction

Utilizing publicly available state-level data from 2013 on health care costs, such as Medicare and Medicaid costs, data from the Economic Policy Institute on income inequality, and multiple other income and health related control variables from the Centers for Disease Control and Prevention will allow us to run multiple linear regression focusing on how health care affects income inequality in the United States. 

To describe state-by-state income inequality, the [Economic Policy Institute](https://www.epi.org/publication/income-inequality-in-the-us/#epi-toc-6), an independent think tank, investigated the impact of economic trends and policies in 2013 around income inequality. Data sets on private health care spending per capita, dental services per capita, Medicare costs per beneficiary, Medicaid costs per enrollee, physician and clinical costs per capita were provided by the [Centers for Medicare & Medicaid Services](https://www.cms.gov/research-statistics-data-and-systems/statistics-trends-and-reports/nationalhealthexpenddata/nhe-fact-sheet) and derived from the National Health Statistics Groups to examine access to health care. Bridged-race population estimates were manually translated into spreadsheet format; the data request can be made from the U.S. Census Bureau in collaboration with the [National Center for Health Statistics](https://wonder.cdc.gov/bridged-race-population.html). Finally, a dataset on U.S. Presidential Elections from 1976–2020 from the [Harvard Dataverse](https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/42MVDX) specified presidential results in 2012.


Piketty and Saenz (2004) highlight the similarities between modern income inequality levels and 20th-century income inequality prior to World War II. As a potential association, I expect health care costs to be higher in states with greater wealth inequality.


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

![corrplot.png]({{ site.baseurl }}/images/corrplot.png)

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

```
          Df  Pillai approx F num Df den Df   Pr(>F)   
party      1 0.25435   3.9228      4     46 0.008016 **
Residuals 49                                           
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
 Response inequality_ratio :
            Df  Sum Sq Mean Sq F value Pr(>F)
party        1  130.11 130.108  2.4579 0.1234
Residuals   49 2593.83  52.935               

 Response private :
            Df   Sum Sq Mean Sq F value Pr(>F)  
party        1  2923371 2923371  5.0147 0.0297 *
Residuals   49 28565073  582961                 
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

 Response medicaid :
            Df    Sum Sq Mean Sq F value Pr(>F)
party        1   1750815 1750815  0.5855 0.4478
Residuals   49 146533355 2990477               

 Response dental :
            Df Sum Sq Mean Sq F value   Pr(>F)   
party        1  36053   36053  10.537 0.002113 **
Residuals   49 167653    3421                    
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


	Pairwise comparisons using t tests with pooled SD 

data:  data$private and data$party 

  0   
1 0.03

P value adjustment method: none 

	Pairwise comparisons using t tests with pooled SD 

data:  data$dental and data$party 

  0     
1 0.0021

P value adjustment method: none 
[1] 0.3016627
[1] 0.0001428571
[1] 0.001428571
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

The null hypothesis states that mu = 0.5, when in reality, this proportion is 0.5294. Our p-value of 0.678 is statistically insignificant. Thus. we fail to reject the null hypothesis and have convincing evidence for the null. 

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

![residuals.png]({{ site.baseurl }}/images/residuals.png)

![qq.png]({{ site.baseurl }}/images/qq.png)

```
Call:
lm(formula = inequality_ratio ~ private + medicare + medicaid + dental + clinical + far_west + great_lakes + mideast + new_england + plains + rocky_mountains + southeast + southwest + age + white + black + asian + native + hispanic, data = data)

Residuals:
    Min      1Q  Median      3Q     Max 
-9.1249 -2.9801 -0.2786  2.7958 12.6902 

Coefficients: (1 not defined because of singularities)
                  Estimate Std. Error t value Pr(>|t|)    
(Intercept)     -8.654e+01  4.953e+01  -1.747 0.090190 .  
private          5.090e-04  1.715e-03   0.297 0.768537    
medicare         4.901e-03  1.171e-03   4.186 0.000207 ***
medicaid        -5.834e-04  7.793e-04  -0.749 0.459611    
dental           4.894e-02  2.039e-02   2.400 0.022386 *  
clinical        -1.073e-02  6.058e-03  -1.770 0.086173 .  
far_west         8.038e-02  7.659e+00   0.010 0.991692    
great_lakes      7.044e-01  9.279e+00   0.076 0.939966    
mideast         -5.382e+00  9.191e+00  -0.586 0.562227    
new_england     -8.005e+00  8.902e+00  -0.899 0.375250    
plains           1.926e+00  7.494e+00   0.257 0.798844    
rocky_mountains  2.588e+00  7.430e+00   0.348 0.729885    
southeast       -5.409e+00  7.961e+00  -0.679 0.501750    
southwest               NA         NA      NA       NA    
age              8.126e-01  6.746e-01   1.205 0.237220    
white           -5.498e-07  6.329e-07  -0.869 0.391488    
black            2.563e-06  2.202e-06   1.164 0.253156    
asian           -4.885e-06  3.637e-06  -1.343 0.188730    
native          -2.286e-06  2.783e-05  -0.082 0.935043    
hispanic         2.332e-06  1.269e-06   1.838 0.075434 .  
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 5.771 on 32 degrees of freedom
Multiple R-squared:  0.6088,	Adjusted R-squared:  0.3887 
F-statistic: 2.766 on 18 and 32 DF,  p-value: 0.005835


t test of coefficients:

                   Estimate  Std. Error t value Pr(>|t|)   
(Intercept)     -8.6537e+01  9.6780e+01 -0.8942 0.377912   
private          5.0903e-04  4.3915e-03  0.1159 0.908446   
medicare         4.9006e-03  1.6440e-03  2.9810 0.005453 **
medicaid        -5.8335e-04  9.2359e-04 -0.6316 0.532126   
dental           4.8937e-02  2.4249e-02  2.0181 0.052027 . 
clinical        -1.0726e-02  9.5820e-03 -1.1194 0.271312   
far_west         8.0377e-02  1.1320e+01  0.0071 0.994379   
great_lakes      7.0437e-01  1.4069e+01  0.0501 0.960382   
mideast         -5.3825e+00  1.2888e+01 -0.4176 0.678998   
new_england     -8.0049e+00  1.2841e+01 -0.6234 0.537458   
plains           1.9257e+00  1.3449e+01  0.1432 0.887044   
rocky_mountains  2.5879e+00  1.5277e+01  0.1694 0.866548   
southeast       -5.4087e+00  1.3211e+01 -0.4094 0.684975   
age              8.1260e-01  1.1659e+00  0.6970 0.490859   
white           -5.4976e-07  7.9979e-07 -0.6874 0.496794   
black            2.5628e-06  2.0497e-06  1.2503 0.220260   
asian           -4.8846e-06  7.2064e-06 -0.6778 0.502756   
native          -2.2864e-06  4.6721e-05 -0.0489 0.961273   
hispanic         2.3323e-06  3.0361e-06  0.7682 0.448002   
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
```

A regression model to predict the average income of the lower 99% of income earners from healthcare costs was built. Interpreting the coefficients, I used five components to health care access, while using region, age, and race as control variables. In this case, the variables `private`, `medicare`, `medicaid`, `dental`, and `clinical` are used to predict the variable `inequality_ratio`.

Since the R-squared is equal to 0.6088, our model explains for 60.88% of the variation in the response. 

The assumptions for linear regression were met by plotting the residuals against the fittest values to check for nonlinearity and equal variance, and by constructing a Q-Q plot to check for normality of the residuals. 

Then, we calculated the robust standard errors and bootstrapped standard errors. The bootstrapped standard errors are lower than the robust standard errors. Conversely, the robust standard errors are lower compared to the original standard errors of the model. The p-values of the robust standard errors, however, are higher than the original p-values from the model.

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
table(true_condition = data$party, predicted_condition = data$predicted) %>% addmargins

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
calc_auc(ROCplot1)$AUC
```

```
Call:
glm(formula = party ~ inequality_ratio + private + medicare + medicaid + dental + clinical, family = "binomial", data = data)

Deviance Residuals: 
    Min       1Q   Median       3Q      Max  
-1.8465  -0.9412   0.1754   0.7585   2.0276  

Coefficients:
                   Estimate Std. Error z value Pr(>|z|)   
(Intercept)      -5.8137668  4.2306624  -1.374  0.16938   
inequality_ratio -0.0230768  0.0576956  -0.400  0.68918   
private           0.0011401  0.0009830   1.160  0.24613   
medicare          0.0009924  0.0005018   1.978  0.04794 * 
medicaid         -0.0004020  0.0002667  -1.507  0.13174   
dental            0.0304332  0.0110810   2.746  0.00602 **
clinical         -0.0019061  0.0019850  -0.960  0.33693   
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 70.524  on 50  degrees of freedom
Residual deviance: 50.457  on 44  degrees of freedom
AIC: 64.457

Number of Fisher Scoring iterations: 5

     (Intercept) inequality_ratio          private         medicare         medicaid           dental         clinical 
     0.002986161      0.977187453      1.001140706      1.000992933      0.999598064      1.030900986      0.998095680 

              predicted_condition
true_condition Democratic Republican Sum
           0            6         18  24
           1           18          9  27
           Sum         24         27  51           

[1] 0.7647059
[1] 0.8571429
[1] 0.6666667
[1] 0.875
[1] 0.8101852
```

![density.png]({{ site.baseurl }}/images/density.png)

![roc.png]({{ site.baseurl }}/images/roc.png)

Controlling for private, Medicaid, and dental health care costs, for every 1-unit increase in the income inequality ratio, the odds of a state voting for the Democratic party change by a factor of 0.9771875 (i.e., decrease by 2.28%).

Controlling for the inequality ratio, Medicaid, and dental health care costs, for every 1-unit increase in the private health care spending per capita, the odds of a state voting for the Democratic party change by a factor of 1.0011 (i.e., increase by 0.11%).

Controlling for the income inequality ratio, private and dental health care costs, for every 1-unit increase in Medicaid costs per enrollee, the odds of a state voting for the Democratic party change by a factor of 0.995981 (i.e., decrease by 0.401%).

Controlling for the income inequality ratio, private and Medicaid costs, for every 1-unit increase in dental care costs per capita, the odds of a state voting for the Democratic party change by a factor of 1.0309 (i.e., increase by 3.09%).

Then, we computed the accuracy, sensitivity, specificity, and recall. We found that, since our false negatives and false positives counts are close, an accuracy of 0.7647 is a good measure since we have a somewhat symmetric data set. Our precision is 0.8571, our sensitivity is 0.6666, and the specificity is 0.875. SInce we know that precision and recall are inversely proportional, this leads us to believe that the measure of recall is fairly low. Lastly, these measures point to the fact that there was appropriate-fitting from the data.

The AUC, which measures how true positive rate (recall) and false positive rate trade off came out to be 0.8101852. Therefore, the AUC has a moderate classification accuracy, judging from our rule of thumb.
