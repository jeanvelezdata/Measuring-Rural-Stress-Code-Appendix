Measuring Rural Stress: Code Appendix
================
Jean Velez
2023-11-28

# Import Lavaan

``` r
library(lavaan)
library(httr)
library(readxl)
```

# Import Dataset

``` r
httr::GET("https://query.data.world/s/fif6xttgj4kvir4ixvnlnxdmlqhiuw?dws=00000", write_disk(tf <- tempfile(fileext = ".xlsx")))
```

    ## Response [https://download.data.world/file_download/janky22/measuring-rural-stress-dataset/Mid-Western%20State%205%20County%20Stress%20Data.xlsx?auth=eyJhbGciOiJIUzUxMiJ9.eyJzdWIiOiJwcm9kLXVzZXItY2xpZW50Omphbmt5MjIiLCJpc3MiOiJhZ2VudDpqYW5reTIyOjpjYzEzYjg3MC02MzhhLTRiMjMtYjJiNi0xYzQ3YWZlNGUyYWIiLCJpYXQiOjE3MDEyMTA4MTQsInJvbGUiOlsidXNlciIsInVzZXJfYXBpX2FkbWluIiwidXNlcl9hcGlfZW50ZXJwcmlzZV9hZG1pbiIsInVzZXJfYXBpX3JlYWQiLCJ1c2VyX2FwaV93cml0ZSJdLCJnZW5lcmFsLXB1cnBvc2UiOmZhbHNlLCJ1cmwiOiJhNDI4MmU0ZDdiMzZlMjg0NGJjZjY3Yjk5NTg1ZDQ3ZThiYWMyZTlhIn0.r5WNYktKSxdfQRpD0p8A1Akha7v2oc_GVQK0fARnnfvJgTuCN2GQAqTN8hzJWYetVSkPh2jxr5hRj5l8qsYqpA]
    ##   Date: 2023-11-28 22:36
    ##   Status: 200
    ##   Content-Type: application/vnd.openxmlformats-officedocument.spreadsheetml.sheet
    ##   Size: 18.3 kB
    ## <ON DISK>  C:\Users\janky\AppData\Local\Temp\Rtmp4eUe1S\file54d8f185ae3.xlsx

``` r
data <- readxl::read_excel(tf)
```

# Factorize Likert-type items

``` r
for (i in 2:5){
data[,i] = factor(dplyr::pull(data[,i]),ordered = TRUE)
}
```

# Build structual and measurement equations

``` r
m <- '
  # latent variable - measurement model
  overallstress =~ stress + stress_finance + stress_home + stress_employment
  
  # structural path - structural model
  overallstress ~ rural + hispanic + income_higher + income_lower
  
  #correlated error
  stress~~stress_home
'
```

# Create SEM object

``` r
m.fit <- lavaan::sem(m, data = data)
```

# Output summary of SEM

``` r
lavaan::summary(m.fit, standardized = T, fit.measures = T)
```

    ## lavaan 0.6.16 ended normally after 22 iterations
    ## 
    ##   Estimator                                       DWLS
    ##   Optimization method                           NLMINB
    ##   Number of model parameters                        25
    ## 
    ##   Number of observations                           251
    ## 
    ## Model Test User Model:
    ##                                               Standard      Scaled
    ##   Test Statistic                                10.517      21.397
    ##   Degrees of freedom                                13          13
    ##   P-value (Chi-square)                           0.651       0.065
    ##   Scaling correction factor                                  0.543
    ##   Shift parameter                                            2.037
    ##     simple second-order correction                                
    ## 
    ## Model Test Baseline Model:
    ## 
    ##   Test statistic                              1895.905    1383.589
    ##   Degrees of freedom                                 6           6
    ##   P-value                                        0.000       0.000
    ##   Scaling correction factor                                  1.372
    ## 
    ## User Model versus Baseline Model:
    ## 
    ##   Comparative Fit Index (CFI)                    1.000       0.994
    ##   Tucker-Lewis Index (TLI)                       1.001       0.997
    ##                                                                   
    ##   Robust Comparative Fit Index (CFI)                            NA
    ##   Robust Tucker-Lewis Index (TLI)                               NA
    ## 
    ## Root Mean Square Error of Approximation:
    ## 
    ##   RMSEA                                          0.000       0.051
    ##   90 Percent confidence interval - lower         0.000       0.000
    ##   90 Percent confidence interval - upper         0.052       0.088
    ##   P-value H_0: RMSEA <= 0.050                    0.942       0.441
    ##   P-value H_0: RMSEA >= 0.080                    0.002       0.106
    ##                                                                   
    ##   Robust RMSEA                                                  NA
    ##   90 Percent confidence interval - lower                        NA
    ##   90 Percent confidence interval - upper                        NA
    ##   P-value H_0: Robust RMSEA <= 0.050                            NA
    ##   P-value H_0: Robust RMSEA >= 0.080                            NA
    ## 
    ## Standardized Root Mean Square Residual:
    ## 
    ##   SRMR                                           0.020       0.020
    ## 
    ## Parameter Estimates:
    ## 
    ##   Standard errors                           Robust.sem
    ##   Information                                 Expected
    ##   Information saturated (h1) model        Unstructured
    ## 
    ## Latent Variables:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##   overallstress =~                                                      
    ##     stress            1.000                               0.962    0.899
    ##     stress_finance    0.961    0.081   11.805    0.000    0.924    0.868
    ##     stress_home       0.690    0.049   14.004    0.000    0.663    0.641
    ##     stress_mplymnt    0.688    0.060   11.496    0.000    0.661    0.640
    ## 
    ## Regressions:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##   overallstress ~                                                       
    ##     rural             0.438    0.140    3.136    0.002    0.456    0.218
    ##     hispanic         -0.471    0.197   -2.386    0.017   -0.490   -0.174
    ##     income_higher     0.130    0.173    0.753    0.452    0.135    0.055
    ##     income_lower      0.686    0.156    4.400    0.000    0.714    0.333
    ## 
    ## Covariances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##  .stress ~~                                                             
    ##    .stress_home       0.245    0.060    4.077    0.000    0.245    0.658
    ## 
    ## Intercepts:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##    .stress            0.000                               0.000    0.000
    ##    .stress_finance    0.000                               0.000    0.000
    ##    .stress_home       0.000                               0.000    0.000
    ##    .stress_mplymnt    0.000                               0.000    0.000
    ##    .overallstress     0.000                               0.000    0.000
    ## 
    ## Thresholds:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##     stress|t1        -0.831    0.276   -3.015    0.003   -0.831   -0.776
    ##     stress|t2        -0.417    0.273   -1.530    0.126   -0.417   -0.390
    ##     stress|t3         0.566    0.270    2.094    0.036    0.566    0.529
    ##     stress|t4         1.452    0.296    4.910    0.000    1.452    1.357
    ##     stress_fnnc|t1   -0.831    0.257   -3.237    0.001   -0.831   -0.780
    ##     stress_fnnc|t2   -0.586    0.250   -2.344    0.019   -0.586   -0.550
    ##     stress_fnnc|t3    0.393    0.244    1.609    0.108    0.393    0.369
    ##     stress_fnnc|t4    0.957    0.248    3.863    0.000    0.957    0.899
    ##     stress_home|t1   -0.342    0.251   -1.365    0.172   -0.342   -0.331
    ##     stress_home|t2    0.028    0.251    0.110    0.912    0.028    0.027
    ##     stress_home|t3    1.035    0.254    4.071    0.000    1.035    1.001
    ##     stress_home|t4    1.696    0.298    5.689    0.000    1.696    1.640
    ##     strss_mplymn|1   -0.298    0.265   -1.127    0.260   -0.298   -0.288
    ##     strss_mplymn|2    0.098    0.262    0.375    0.708    0.098    0.095
    ##     strss_mplymn|3    0.910    0.265    3.431    0.001    0.910    0.880
    ##     strss_mplymn|4    1.379    0.272    5.068    0.000    1.379    1.334
    ## 
    ## Variances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##    .stress            0.221                               0.221    0.193
    ##    .stress_finance    0.280                               0.280    0.247
    ##    .stress_home       0.629                               0.629    0.589
    ##    .stress_mplymnt    0.631                               0.631    0.591
    ##    .overallstress     0.779    0.073   10.720    0.000    0.843    0.843
    ## 
    ## Scales y*:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##     stress            1.000                               1.000    1.000
    ##     stress_finance    1.000                               1.000    1.000
    ##     stress_home       1.000                               1.000    1.000
    ##     stress_mplymnt    1.000                               1.000    1.000
