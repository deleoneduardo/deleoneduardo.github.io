--- 
layout: post
title: Exploratory Data Analysis in Python
author: Eduardo De Leon
---

I explore the library Seaborn for making statistical graphics in Python. I use the data set `tips` to create visualizations and wrangle data for one numerical and one categorical variable.

# Introduction
 
To perform data analysis, I imported the data set from the package seaborn as `sns`.

```python
# Import package pandas
import pandas as pd

# Import package seaborn
import seaborn as sns
import statistics

# Load tips dataset
data = sns.load_dataset("tips")
```

# Data Wrangling


```python
# Number of rows
print(data.shape[0])

# Number of columns
print(data.shape[1])
```

```
244
7
```

After the author tidied the data, there are 244 observations and 7 variables that describe each observation's characteristics. 

# Data Analysis on Numerical Variable

```python
# Average total bill 
print(statistics.mean(data['total_bill']))

# Scatterplot of tip and total bill size
sns.relplot(data = data, x = "total_bill", y = "tip")
```

```
19.78594262295082
```

![scatterplot.png](output_9_2.png)

We see that the average total bill throughout the day was 19.78 dollars. Furthermore, it appears `tip` and `total_bill` have a linear relationship.

# Data Analysis on Categorical Variable

```python
# Average total bill during Friday
print(data.groupby('day').std()['total_bill'])
print(data.groupby('day').mean()['total_bill'])

# Scatterplot of total bills for each day
sns.catplot(data = data, x = "day", y = "total_bill")
```

```
    day
    Thur    7.886170
    Fri     8.302660
    Sat     9.480419
    Sun     8.832122
    Name: total_bill, dtype: float64
    day
    Thur    17.682742
    Fri     17.151579
    Sat     20.441379
    Sun     21.410000
    Name: total_bill, dtype: float64
```

![stripplot.jpg](output_12_2.png)

From the strip plot, we see that Sunday has the largest total bills, on average, but Saturday has a larger spread of values. In context, this can come beneficial to a business owner. In this manner, we are able to apply data anlytics to the growth of businesses.
