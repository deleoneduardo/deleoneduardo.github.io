---
layout: post
title: Exploratory Data Analysis in Python
author: Eduardo De Leon
---

I explore a data set from the Centers for Disease Control and Prevention that describes the interaction between obesity and happiness in the United States.

# Introduction

```python
# Import packages
import pandas as pd
import seaborn as sns
import statistics

# Import happiness data set
data = pd.read_excel (r'C:\Users\eduar\Documents\Website\Happiness.xlsx')
```

# Data Wrangling

```python
# Number of rows
print(data.shape[0])

# Number of columns
print(data.shape[1])
```

    51
    14
    

The data was tidied in spreadsheet format beforehand. There are 51 observations and 14 variables that describe each observation's characteristics, including control variables. 

# Data Visualization


```python
# Mean adult obesity
print(statistics.mean(data['Adult Obesity, 2019']))
print(statistics.mean(data['Overweight&Obese, 2016, kff']))

# Scatterplot of overall happiness and adult obesity
sns.regplot(data = data, x = 'Adult Obesity, 2019', y = 'Happiness').set(title = 'Happiness and Adult Obesity Rates in the United States\n')
```

```
    31.90392156862745
    65.09019607843138

    [Text(0.5, 1.0, 'Happiness and Adult Obesity Rates in the United States\n')]
```

![output_8_2.png](images/output_8_2.png)

I discovered that the overall adult obesity in the United States was 31.90% in 2019, while the overall overweight population rate was 65.09% in 2016. 

From the scatterplot, there was a negative, linear relationship between adult obesity and statewide happiness in 2019.

```python
# Descriptive regional statistics for happiness index
print(data.groupby('Region')['Happiness'].describe())

# Bar graph of regional happiness index
sns.barplot(x = 'Region', y = 'Happiness', data = data).set(title = 'Happiness Index by Region in the United States\n')
```

```
               count       mean       std    min      25%    50%      75%    max
    Region                                                                      
    Midwest     12.0  53.845000  6.565745  45.79  48.2675  52.23  59.2075  65.87
    Northeast    9.0  55.075556  4.644357  50.12  50.9600  54.86  57.3000  64.10
    South       16.0  45.196250  8.664423  30.58  38.2550  44.53  51.3425  61.78
    West        13.0  54.083846  8.961838  40.85  47.2500  53.47  59.5800  69.58
    




    [Text(0.5, 1.0, 'Happiness Index by Region in the United States\n')]
```
    
![output_10_2.png](images/output_10_2.png)

From the descriptive statistics table, we see that the western region of the United States has the most variation in happiness since it has a standard deviation of 8.96 points, while the northeast has the least at 4.64. The mean happiness index is 45.19 in the south, 54.08 in the west, 55.07 in the northeast, and 53.845 in the midwest.

Finally, we find that happiness varies by region, with South being the least happy region, on average. 

# Conclusion

Further statistical modeling can be used to conduct hypothesis testing and diagnostic tests. There is much to learn about the implications of obesity on health policy. 
