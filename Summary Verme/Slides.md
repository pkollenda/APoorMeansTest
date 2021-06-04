Which Model for Poverty Predictions? (Verme 2020)
========================================================
author: Philipp Kollenda
date: 11 June 2021
autosize: true


========================================================

- In Verme draw coverage X leakage curves when it changes).
- combined framework. Modelling: y_i (or p = 1(y_i <= z)) = beta * x_i + e_i (e_i split in model fitting and random error? why if cant disentangle). Prediction: y_hat_i or p_hat_i = beta_hat * x_i). Classification: y_hat_i < z then poor and p_hat_i > arbitrary cut-off then poor.  
- Confusion matrix (false positive, false negative and relation to inclusion and exclusion errors from BRV).
- Objective function: we need a metric to i) compare models and ii) fine-tune parameters (sometimes). Many contenders and complications: minimize poverty means minimizing false negatives (Verme; BRV call it EER)  but adds false positives (leakage, BRV say IER). Verme say choose model which minimizes false negatives for a given level of leakage. In BRV test various (IER, EER, TER but also results on headcount ratio for a given budget).

Slide With Code
========================================================


```r
summary(cars)
```

```
     speed           dist       
 Min.   : 4.0   Min.   :  2.00  
 1st Qu.:12.0   1st Qu.: 26.00  
 Median :15.0   Median : 36.00  
 Mean   :15.4   Mean   : 42.98  
 3rd Qu.:19.0   3rd Qu.: 56.00  
 Max.   :25.0   Max.   :120.00  
```

Slide With Plot
========================================================

![plot of chunk unnamed-chunk-2](Slides-figure/unnamed-chunk-2-1.png)

Discussions
========================================================
- Models need to be tested and depend on income distribution, poverty line and specific data. But how test if outcome variable is not available?
