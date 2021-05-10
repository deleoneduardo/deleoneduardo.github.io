---
layout: post
title: Exploratory Data Analysis in R
author: Eduardo De Leon
---

I explore two data sets on health economics to identifying or discover the trends and patterns inherent in the data using data wrangling and visualization. 

# Introduction

Health care costs are particularly important for older individuals, as most people retire at age 65, and they cannot afford to pay for expensive fees. In this report, I examine the impact wealth inequality has on elements of health care expenditure per capita.

To study these two economic factors, I chose a table from the Economic Policy Institute that describes income inequality in 2013 by state. Additionally, I gathered a data set from the Centers for Medicaid and Medicare Services that breaks down costs by state from 2007-2018, and selected the data from 2013. I chose a third Excel dataset that lists each state with its abbreviation. Lastly, I chose a 2013 State and Legislative Partisan Composition data set from the National Conference of State Legislatures to group categorically by party. 

```
# Setting libraries and installing new packages
library(tidyverse)
library(dplyr)
library(cluster)
library(factoextra)
library(readxl)
library(kableExtra)
library(readr)
library(ggcorrplot)
library(reshape2)
library(ggplot2)
library(heatmaply)
library(cluster)
```

```
# Data set with all state health care data
healthcare = read_excel("health-care.xlsx") 

# Data set of state party control
party = read_excel("party.xlsx")
```

# Tidy

```
# Prepare the state partisan composition data set
party = party %>% rename("control" = "State Control")
```

This data is tidy because every column is a variable and every row is an observation. Furthermore, I merged the data into a single data set named `data`.

# Join/Merge

```
# Merge all data sets into one 
states = healthcare %>% left_join(party, by = "state")
summarize(states, n())
```

```
n()
51	
```

We selected the variables we want to compare, and we used `left_join` to form `states` by state code. The size of the cleaned, joined dataset is 51 rows and 24 variables.

# Summary statistics

```
# Breakdown of inequality ratio by state party
states %>% group_by(Party = control) %>% summarise(Mean = mean(inequality_ratio), SD = sd(inequality_ratio), Variance = var(inequality_ratio), Min = min(inequality_ratio), Q1 = quantile(inequality_ratio, probs = 0.25), Median = median(inequality_ratio), Q3 = quantile(inequality_ratio, probs = 0.75), Max = max(inequality_ratio), IQR = IQR(inequality_ratio), Count = n()) %>% kbl(caption = "U.S. Income Inequality") %>% kable_classic(full_width = F, html_font = "Cambria")

# Private Health Insurance Spending Per Capita
states %>% group_by(Party = control) %>% summarise(Mean = mean(private), SD = sd(private), Variance = var(private), Min = min(private), Q1 = quantile(private, probs = 0.25), Median = median(private), Q3 = quantile(private, probs = 0.75), Max = max(private), IQR = IQR(private), Count = n()) %>% kbl(caption = "Private Health Insurance Spending Per Capita") %>% kable_classic(full_width = F, html_font = "Cambria")

# Medicare expenditures per beneficiary
states %>% group_by(Party = control) %>% summarise(Mean = mean(medicare), SD = sd(medicare), Variance = var(medicare), Min = min(medicare), Q1 = quantile(medicare, probs = 0.25), Median = median(medicare), Q3 = quantile(medicare, probs = 0.75), Max = max(medicare), IQR = IQR(medicare), Count = n()) %>% kbl(caption = "Medicare Expenditures Per Beneficiary") %>% kable_classic(full_width = F, html_font = "Cambria")

# Medicaid expenditures per enrollee
states %>% group_by(Party = control) %>% summarise(Mean = mean(medicaid), SD = sd(medicaid), Variance = var(medicaid), Min = min(medicaid), Q1 = quantile(medicaid, probs = 0.25), Median = median(medicaid), Q3 = quantile(medicaid, probs = 0.75), Max = max(medicaid), IQR = IQR(medicaid), Count = n()) %>% kbl(caption = "Medicaid Expenditures Per Enrollee") %>% kable_classic(full_width = F, html_font = "Cambria")

# Dental services per capita
states %>% group_by(Party = control) %>% summarise(Mean = mean(dental), SD = sd(dental), Variance = var(dental), Min = min(dental), Q1 = quantile(dental, probs = 0.25), Median = median(dental), Q3 = quantile(dental, probs = 0.75), Max = max(dental), IQR = IQR(dental), Count = n()) %>% kbl(caption = "Dental Services Per Capita") %>% kable_classic(full_width = F, html_font = "Cambria")

# Physician and clinical services per capita
states %>% group_by(Party = control) %>% summarise(Mean = mean(clinical), SD = sd(clinical), Variance = var(clinical), Min = min(clinical), Q1 = quantile(clinical, probs = 0.25), Median = median(clinical), Q3 = quantile(clinical, probs = 0.75), Max = max(clinical), IQR = IQR(clinical), Count = n()) %>% kbl(caption = "Physician/Clinical Services Per Capita") %>% kable_classic(full_width = F, html_font = "Cambria")
```

![income.jpg]({{ site.baseurl }}/images/income.jpg)

![private.jpg]({{ site.baseurl }}/images/private.jpg)

![medicare.jpg]({{ site.baseurl }}/images/medicare.jpg)

![medicaid.jpg]({{ site.baseurl }}/images/medicaid.jpg)

![dental.jpg]({{ site.baseurl }}/images/dental.jpg)

![clinical.jpg]({{ site.baseurl }}/images/clinical.jpg)

For the 6 numeric variables examined, we gathered a summary of 10 different statistics grouping by the categorical variable of state partisan composition in 2013. Furthermore, I used the `kable` package to visualize these summary statistics in easy-to-read tables. 

# Visualizations

```
# Correlation heatmap of the numeric variables
ggcorrplot(states %>% select(inequality_ratio, private, medicare, medicaid, dental, clinical, age) %>% cor(), hc.order = TRUE, type = "lower", lab = TRUE, outline.col = "white", ggtheme = ggplot2::theme_gray, colors = c("red", "white", "dodgerblue")) + ggtitle("Correlation Matrix for Numeric Variables")

# Inequality plot
col = c("dodgerblue", "green4", "firebrick1")
ggplot(states %>% select(medicaid, inequality_ratio, control), aes(x = medicaid, y = inequality_ratio, colour = control)) + scale_color_manual(values = col) + ggtitle("Statewide Medicaid Costs and Income Inequality") + geom_smooth(method = lm, formula = y ~ log(x)) + geom_point() 

# Medicare plot
ggplot(states %>% select(medicare, inequality_ratio, control), aes(x = medicare, y = inequality_ratio, colour = control)) + scale_color_manual(values = col) + ggtitle("Statewide Medicare Costs and Income Inequality") + geom_smooth(method = lm, formula = y ~ x) + geom_point() 
```

![corrmat.jpg]({{ site.baseurl }}/images/corrmat.png)

![plot1.jpg]({{ site.baseurl }}/images/plot1.png)

![plot2.jpg]({{ site.baseurl }}/images/plot2.png)

The first plot shows a heatmap that provides a correlation matrix for all the variables in `states`. There is a 0.53 correlation between `inequality_ratio` and `medicaid`, and there is a -0.42 correlation between `Hospice` and `Bottom`. Therefore, we constructed two additional plots that present the relationships among these variables. 

In the Medicaid-Ratio plot, we see that there is a greater magnitude in income inequality in states where Medicaid costs are higher. Finally, in the Bottom-Hospice plot, we see that Republican states with a lesser average income among the bottom 99% population tend to have higher hospice prices for patients. 

# Dimensionality Reduction

```
# Prepare the data and perform PCA
states_pca = states %>% select(-c(state, control)) %>% scale() %>% prcomp()

# Visualize percentage of variances for each PC in a scree plot
fviz_eig(states_pca, addlabels = TRUE, ylim = c(0, 45))

pca1 = as.data.frame(states_pca[["x"]])
pca2 = pca1 %>% mutate(Control = states$control) 

pam1 = pca1 %>% pam(k = 3)
pam1values = pam1$silinfo
pam1data = as.data.frame(pam1$silinfo[["widths"]])

pam1data$index = as.numeric(row.names(pam1data))
pam1data = pam1data %>% arrange(pam1data$index) # Debugging
pam = bind_cols(pca2, (pam1data %>% arrange(pam1data$index))['cluster'])

ggplot(pam, mapping = aes(x = PC1, y = PC2, color = control)) + geom_point() + stat_ellipse(aes(group = cluster)) + ggtitle("Observations of Clusters Along PC1 and PC2")

# Visualize the contributions of the variables to the PCs in a correlation circle
fviz_pca_var(states_pca, col.var = "black", repel = TRUE) # Avoid text overlapping

# Visualize the individuals according to PC1 and PC2
fviz_pca_ind(states_pca, geom.ind = "point", col.ind = states$control, palette = c("#00AFBB", "#E7B800", "#FC4E07"), addEllipses = TRUE, legend.title = "Control")
```

From the constructed scree plot, we gathered that more than 65% of the variance is cumulatively explained through the first two principal components. We went on to cluster PC1 and PC2 along partisan composition, and graphed each of the 6 variables along a correlation circle. Lastly, we used Principal Component Analysis to visualize the individual principal components by party control. 
