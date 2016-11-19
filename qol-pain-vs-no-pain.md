
Factors associated with having reduced quality of life in patients with HIV-SN
------------------------------------------------------------------------------

*(painful or non-painful HIV-SN)*

### Authors

Peter Kamerman, Antonia Wadley, Prinisha Pillay

**Date: November 19, 2016**

------------------------------------------------------------------------

Session setup
-------------

Load data
---------

``` r
data <- read_csv('./data/qol-pain-vs-no-pain.csv')
```

Quick look
----------

``` r
dim(data)
```

    ## [1] 201   8

``` r
names(data)
```

    ## [1] "eq5d_vas"              "age"                   "painful_sn"           
    ## [4] "other_pain_sites"      "sex"                   "education"            
    ## [7] "hscl_anxiety_score"    "hscl_depression_score"

``` r
head(data)
```

    ## # A tibble: 6 × 8
    ##   eq5d_vas   age painful_sn other_pain_sites    sex education
    ##      <int> <int>      <chr>            <chr>  <chr>     <int>
    ## 1       80    44        yes              yes female         5
    ## 2       50    46        yes              yes   male         6
    ## 3       70    67        yes              yes female         4
    ## 4       30    47        yes              yes female         6
    ## 5       60    61        yes              yes   male         5
    ## 6       80    48        yes              yes   male         6
    ## # ... with 2 more variables: hscl_anxiety_score <dbl>,
    ## #   hscl_depression_score <dbl>

``` r
tail(data)
```

    ## # A tibble: 6 × 8
    ##   eq5d_vas   age painful_sn other_pain_sites    sex education
    ##      <int> <int>      <chr>            <chr>  <chr>     <int>
    ## 1      100    56         no              yes   male         5
    ## 2      100    37         no               no female         6
    ## 3       60    45         no               no female         6
    ## 4      100    40         no               no female         8
    ## 5      100    55         no               no female         5
    ## 6      100    34         no               no female         7
    ## # ... with 2 more variables: hscl_anxiety_score <dbl>,
    ## #   hscl_depression_score <dbl>

``` r
glimpse(data)
```

    ## Observations: 201
    ## Variables: 8
    ## $ eq5d_vas              <int> 80, 50, 70, 30, 60, 80, 70, 20, 60, 0, 1...
    ## $ age                   <int> 44, 46, 67, 47, 61, 48, 44, 48, 32, 40, ...
    ## $ painful_sn            <chr> "yes", "yes", "yes", "yes", "yes", "yes"...
    ## $ other_pain_sites      <chr> "yes", "yes", "yes", "yes", "yes", "yes"...
    ## $ sex                   <chr> "female", "male", "female", "female", "m...
    ## $ education             <int> 5, 6, 4, 6, 5, 6, 5, 6, 6, 5, 0, 3, 5, 4...
    ## $ hscl_anxiety_score    <dbl> 1.5, 2.9, 3.5, 3.6, 1.9, 1.1, 1.8, 2.4, ...
    ## $ hscl_depression_score <dbl> 3.27, 2.87, 2.60, 3.13, 2.33, 1.67, 2.87...

``` r
summary(data)
```

    ##     eq5d_vas           age        painful_sn        other_pain_sites  
    ##  Min.   :  0.00   Min.   :20.0   Length:201         Length:201        
    ##  1st Qu.: 50.00   1st Qu.:35.0   Class :character   Class :character  
    ##  Median : 80.00   Median :42.0   Mode  :character   Mode  :character  
    ##  Mean   : 72.17   Mean   :43.1                                        
    ##  3rd Qu.:100.00   3rd Qu.:52.0                                        
    ##  Max.   :100.00   Max.   :74.0                                        
    ##      sex              education      hscl_anxiety_score
    ##  Length:201         Min.   : 0.000   Min.   :1.000     
    ##  Class :character   1st Qu.: 5.000   1st Qu.:2.100     
    ##  Mode  :character   Median : 6.000   Median :2.600     
    ##                     Mean   : 6.129   Mean   :2.717     
    ##                     3rd Qu.: 8.000   3rd Qu.:3.400     
    ##                     Max.   :10.000   Max.   :4.000     
    ##  hscl_depression_score
    ##  Min.   :1.00         
    ##  1st Qu.:2.07         
    ##  Median :2.53         
    ##  Mean   :2.59         
    ##  3rd Qu.:3.07         
    ##  Max.   :4.00

Process data
------------

``` r
data.gam <- data %>%
    mutate(eq5d_vas = eq5d_vas/100, # Convert to a proportion
           painful_sn = factor(painful_sn), # Convert to a factor
           other_pain_sites = factor(other_pain_sites), # Convert to a factor
           sex = factor(sex)) %>% # Convert to a factor
    filter(complete.cases(.)) # Retain complete cases only

# Check dimensions after removing incomplete cases
dim(data.gam)
```

    ## [1] 201   8

Build model
-----------

### Generalized Additive Model

*(with extended beta inflated distribution)*

``` r
# GAM on full model (all variables p<0.1 on univariate analysis: age, painful_sn, 
# other_pain_sites, sex, education, depression, anxiety).
# Use beta-inflated distribution [0, 1] for VAS data
# Select best model using generalized Akaike's information criterion (AIC).

# Model
mod.gam <- gamlss(eq5d_vas ~ 
                      painful_sn +
                      other_pain_sites +
                      age +
                      sex +
                      hscl_depression_score +
                      hscl_anxiety_score +
                      education, 
           data = data.gam,
           family = BEINF())
```

    ## GAMLSS-RS iteration 1: Global Deviance = 201.9832 
    ## GAMLSS-RS iteration 2: Global Deviance = 200.9849 
    ## GAMLSS-RS iteration 3: Global Deviance = 200.9822 
    ## GAMLSS-RS iteration 4: Global Deviance = 200.9822

``` r
# Model summary 
summary(mod.gam)
```

    ## ******************************************************************
    ## Family:  c("BEINF", "Beta Inflated") 
    ## 
    ## Call:  gamlss(formula = eq5d_vas ~ painful_sn + other_pain_sites +  
    ##     age + sex + hscl_depression_score + hscl_anxiety_score +  
    ##     education, family = BEINF(), data = data.gam) 
    ## 
    ## Fitting method: RS() 
    ## 
    ## ------------------------------------------------------------------
    ## Mu link function:  logit
    ## Mu Coefficients:
    ##                        Estimate Std. Error t value Pr(>|t|)   
    ## (Intercept)            1.989999   0.743972   2.675  0.00813 **
    ## painful_snyes         -0.831035   0.401553  -2.070  0.03985 * 
    ## other_pain_sitesyes   -0.096170   0.436884  -0.220  0.82601   
    ## age                    0.002142   0.008764   0.244  0.80715   
    ## sexmale                0.042542   0.179361   0.237  0.81277   
    ## hscl_depression_score -0.296958   0.125228  -2.371  0.01872 * 
    ## hscl_anxiety_score    -0.052601   0.111153  -0.473  0.63659   
    ## education             -0.001383   0.048148  -0.029  0.97712   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## ------------------------------------------------------------------
    ## Sigma link function:  logit
    ## Sigma Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  -0.3391     0.0797  -4.255 3.28e-05 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## ------------------------------------------------------------------
    ## Nu link function:  log 
    ## Nu Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  -4.2121     0.7123  -5.913 1.53e-08 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## ------------------------------------------------------------------
    ## Tau link function:  log 
    ## Tau Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  -0.7464     0.1518  -4.918 1.89e-06 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## ------------------------------------------------------------------
    ## No. of observations in the fit:  201 
    ## Degrees of Freedom for the fit:  11
    ##       Residual Deg. of Freedom:  190 
    ##                       at cycle:  4 
    ##  
    ## Global Deviance:     200.9822 
    ##             AIC:     222.9822 
    ##             SBC:     259.3186 
    ## ******************************************************************

``` r
# Use stepGAIC to select best models based on GAIC
step.gam <- stepGAIC(mod.gam)
```

    ## Distribution parameter:  mu 
    ## Start:  AIC= 222.98 
    ##  eq5d_vas ~ painful_sn + other_pain_sites + age + sex + hscl_depression_score +  
    ##     hscl_anxiety_score + education 
    ## 
    ##                         Df    AIC
    ## - education              1 220.98
    ## - other_pain_sites       1 221.03
    ## - sex                    1 221.04
    ## - age                    1 221.04
    ## - hscl_anxiety_score     1 221.21
    ## <none>                     222.98
    ## - painful_sn             1 225.56
    ## - hscl_depression_score  1 226.54
    ## 
    ## Step:  AIC= 220.98 
    ##  eq5d_vas ~ painful_sn + other_pain_sites + age + sex + hscl_depression_score +  
    ##     hscl_anxiety_score 
    ## 
    ##                         Df    AIC
    ## - other_pain_sites       1 219.03
    ## - sex                    1 219.04
    ## - age                    1 219.11
    ## - hscl_anxiety_score     1 219.21
    ## <none>                     220.98
    ## - painful_sn             1 223.74
    ## - hscl_depression_score  1 224.54
    ## 
    ## Step:  AIC= 219.03 
    ##  eq5d_vas ~ painful_sn + age + sex + hscl_depression_score + hscl_anxiety_score 
    ## 
    ##                         Df    AIC
    ## - sex                    1 217.10
    ## - age                    1 217.12
    ## - hscl_anxiety_score     1 217.25
    ## <none>                     219.03
    ## - hscl_depression_score  1 222.79
    ## - painful_sn             1 240.62
    ## 
    ## Step:  AIC= 217.1 
    ##  eq5d_vas ~ painful_sn + age + hscl_depression_score + hscl_anxiety_score 
    ## 
    ##                         Df    AIC
    ## - age                    1 215.18
    ## - hscl_anxiety_score     1 215.34
    ## <none>                     217.10
    ## - hscl_depression_score  1 220.90
    ## - painful_sn             1 238.88
    ## 
    ## Step:  AIC= 215.18 
    ##  eq5d_vas ~ painful_sn + hscl_depression_score + hscl_anxiety_score 
    ## 
    ##                         Df    AIC
    ## - hscl_anxiety_score     1 213.41
    ## <none>                     215.18
    ## - hscl_depression_score  1 219.13
    ## - painful_sn             1 237.58
    ## 
    ## Step:  AIC= 213.41 
    ##  eq5d_vas ~ painful_sn + hscl_depression_score 
    ## 
    ##                         Df    AIC
    ## <none>                     213.41
    ## - hscl_depression_score  1 221.12
    ## - painful_sn             1 235.60

``` r
step.gam$anova
```

    ## Stepwise Model Path 
    ## Analysis of Deviance Table
    ## 
    ## Initial
    ## mu
    ##  Model:
    ## eq5d_vas ~ painful_sn + other_pain_sites + age + sex + hscl_depression_score + 
    ##     hscl_anxiety_score + education
    ## 
    ## Final
    ## mu
    ##  Model:
    ## eq5d_vas ~ painful_sn + hscl_depression_score
    ## 
    ## 
    ##                   Step Df     Deviance Resid. Df Resid. Dev      AIC
    ## 1                                            190   200.9822 222.9822
    ## 2          - education  1 0.0008207615       191   200.9830 220.9830
    ## 3   - other_pain_sites  1 0.0502772185       192   201.0333 219.0333
    ## 4                - sex  1 0.0637751307       193   201.0971 217.0971
    ## 5                - age  1 0.0866330617       194   201.1837 215.1837
    ## 6 - hscl_anxiety_score  1 0.2235353664       195   201.4072 213.4072

``` r
summary(step.gam)
```

    ## ******************************************************************
    ## Family:  c("BEINF", "Beta Inflated") 
    ## 
    ## Call:  gamlss(formula = eq5d_vas ~ painful_sn + hscl_depression_score,  
    ##     family = BEINF(), data = data.gam, trace = FALSE) 
    ## 
    ## Fitting method: RS() 
    ## 
    ## ------------------------------------------------------------------
    ## Mu link function:  logit
    ## Mu Coefficients:
    ##                       Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)             2.0193     0.3344   6.039 7.71e-09 ***
    ## painful_snyes          -0.9098     0.1856  -4.901 2.00e-06 ***
    ## hscl_depression_score  -0.3349     0.1059  -3.163  0.00181 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## ------------------------------------------------------------------
    ## Sigma link function:  logit
    ## Sigma Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) -0.33642    0.07968  -4.222 3.71e-05 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## ------------------------------------------------------------------
    ## Nu link function:  log 
    ## Nu Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  -4.2121     0.7123  -5.913 1.48e-08 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## ------------------------------------------------------------------
    ## Tau link function:  log 
    ## Tau Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  -0.7464     0.1518  -4.918 1.85e-06 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## ------------------------------------------------------------------
    ## No. of observations in the fit:  201 
    ## Degrees of Freedom for the fit:  6
    ##       Residual Deg. of Freedom:  195 
    ##                       at cycle:  4 
    ##  
    ## Global Deviance:     201.4072 
    ##             AIC:     213.4072 
    ##             SBC:     233.2271 
    ## ******************************************************************

``` r
# OUTCOME: Final model retains hscl_depression_score

# Fit reduced model
mod.gam.new <- gamlss(eq5d_vas ~ 
                          painful_sn +
                          hscl_depression_score,
           data = data.gam,
           family = BEINF())
```

    ## GAMLSS-RS iteration 1: Global Deviance = 202.3726 
    ## GAMLSS-RS iteration 2: Global Deviance = 201.4098 
    ## GAMLSS-RS iteration 3: Global Deviance = 201.4072 
    ## GAMLSS-RS iteration 4: Global Deviance = 201.4072

``` r
# Diagnostic plots
plot(mod.gam.new)
```

![](./figures/qol-pain-no-pain/gam-1.png)

    ## ******************************************************************
    ##   Summary of the Randomised Quantile Residuals
    ##                            mean   =  0.02158645 
    ##                        variance   =  1.077762 
    ##                coef. of skewness  =  0.04230354 
    ##                coef. of kurtosis  =  3.142442 
    ## Filliben correlation coefficient  =  0.9978065 
    ## ******************************************************************

``` r
# OUTCOME: No pattern in residual vs fitted plot, 
# and residuals normally distributed

# Plot
library(ggplot2)
## hscl_depression_score
ggplot(data.gam, 
       aes(x = hscl_depression_score, y = eq5d_vas)) +
    geom_smooth()
```

![](./figures/qol-pain-no-pain/gam-2.png)

``` r
## OUTCOME: As depression score increases, QoL decreases.

## painful_sn
ggplot(data.gam, 
       aes(x = painful_sn, y = eq5d_vas)) +
    geom_boxplot()
```

![](./figures/qol-pain-no-pain/gam-3.png)

``` r
## OUTCOME: Presence of painful SN associated with 
## reduced QoL.
```

Session information
-------------------

``` r
sessionInfo()
```

    ## R version 3.3.1 (2016-06-21)
    ## Platform: x86_64-apple-darwin13.4.0 (64-bit)
    ## Running under: OS X 10.12.1 (Sierra)
    ## 
    ## locale:
    ## [1] en_GB.UTF-8/en_GB.UTF-8/en_GB.UTF-8/C/en_GB.UTF-8/en_GB.UTF-8
    ## 
    ## attached base packages:
    ## [1] parallel  splines   stats     graphics  grDevices utils     datasets 
    ## [8] methods   base     
    ## 
    ## other attached packages:
    ##  [1] gdtools_0.1.3     ggplot2_2.2.0     gamlss_5.0-0     
    ##  [4] nlme_3.1-128      gamlss.dist_5.0-0 MASS_7.3-45      
    ##  [7] gamlss.data_5.0-0 tidyr_0.6.0       dplyr_0.5.0      
    ## [10] readr_1.0.0       svglite_1.2.0     knitr_1.15       
    ## 
    ## loaded via a namespace (and not attached):
    ##  [1] Rcpp_0.12.8        magrittr_1.5       munsell_0.4.3     
    ##  [4] colorspace_1.3-0   lattice_0.20-34    R6_2.2.0          
    ##  [7] plyr_1.8.4         stringr_1.1.0      tools_3.3.1       
    ## [10] grid_3.3.1         gtable_0.2.0       DBI_0.5-1         
    ## [13] htmltools_0.3.5    lazyeval_0.2.0     survival_2.40-1   
    ## [16] yaml_2.1.14        assertthat_0.1     rprojroot_1.1     
    ## [19] digest_0.6.10      tibble_1.2         Matrix_1.2-7.1    
    ## [22] evaluate_0.10      rmarkdown_1.1.9017 labeling_0.3      
    ## [25] stringi_1.1.2      scales_0.4.1       backports_1.0.4
