data\_analyses\_SxE
================
Rebecca Batstone
2019-08-22

Load packages
-------------

``` r
# packages
library("tidyverse") ## includes ggplot2, dplyr, readr, stringr
library("cowplot") ## paneled graphs
library("car") ## Anova function
```

Load raw means
--------------

``` r
# mean across environments
load("./G_combined_raw.Rdata")
## mean within each environment:
load("./GxE_combined_raw.Rdata")
```

S x E option 1: global means (as used in first version of the manuscript)
-------------------------------------------------------------------------

### Calculate rel/stand means

``` r
# calculate relativized and standardized traits
GE_comb_raw_sel <- GE_comb_raw %>%
  mutate(rel_surv = surv/mean(surv, na.rm=TRUE),
         rel_shoot = shoot/mean(shoot, na.rm=TRUE),
         rel_leaf = leaf/mean(leaf, na.rm=TRUE),
         rel_flower = flower/mean(flower, na.rm=TRUE),
         rel_fruit = fruit/mean(fruit, na.rm=TRUE),
         stand_nod = (nod - mean(nod, na.rm=TRUE))/sd(nod, na.rm=TRUE)) %>%
  as.data.frame(.)
```

### S x E models

``` r
SxE_function <- function(df, trait){
   lin_mod <- lm(get(trait) ~ stand_nod*env, data = df)
   lin_mod_sum <- summary(lin_mod)
   print(lin_mod_aov <- Anova(lin_mod, type = 3))
   quad_mod <- lm(get(trait) ~ (stand_nod + I(stand_nod^2))*env, data = df)
   quad_mod_sum <- summary(quad_mod)
   print(quad_mod_aov <- Anova(quad_mod, type = 3))
   mod_comp <- anova(lin_mod, quad_mod)
   
   return(list(lin_mod_sum, quad_mod_sum, lin_mod_aov, quad_mod_aov, mod_comp))
 }
 
 # specify vars to loop over
var.list <- c("rel_surv","rel_shoot","rel_leaf","rel_flower","rel_fruit")
 
SxE_out <- lapply(var.list, SxE_function, df = GE_comb_raw_sel)
```

    ## Anova Table (Type III tests)
    ## 
    ## Response: get(trait)
    ##               Sum Sq  Df  F value    Pr(>F)    
    ## (Intercept)   5.0727   1 110.2389 < 2.2e-16 ***
    ## stand_nod     0.3402   1   7.3939  0.007439 ** 
    ## env           1.1808   4   6.4153 9.624e-05 ***
    ## stand_nod:env 0.1020   4   0.5540  0.696410    
    ## Residuals     5.9821 130                       
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## Anova Table (Type III tests)
    ## 
    ## Response: get(trait)
    ##                    Sum Sq  Df F value    Pr(>F)    
    ## (Intercept)        2.3765   1 52.8937 3.389e-11 ***
    ## stand_nod          0.1418   1  3.1561 0.0780757 .  
    ## I(stand_nod^2)     0.0437   1  0.9716 0.3261864    
    ## env                1.1233   4  6.2503 0.0001284 ***
    ## stand_nod:env      0.3153   4  1.7542 0.1423068    
    ## I(stand_nod^2):env 0.3235   4  1.8000 0.1329748    
    ## Residuals          5.6161 125                      
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## Anova Table (Type III tests)
    ## 
    ## Response: get(trait)
    ##               Sum Sq  Df F value    Pr(>F)    
    ## (Intercept)    0.052   1  0.0824   0.77451    
    ## stand_nod      0.242   1  0.3846   0.53632    
    ## env           43.779   4 17.3862 2.819e-11 ***
    ## stand_nod:env  8.171   4  3.2448   0.01445 *  
    ## Residuals     75.541 120                      
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## Anova Table (Type III tests)
    ## 
    ## Response: get(trait)
    ##                    Sum Sq  Df F value    Pr(>F)    
    ## (Intercept)         0.025   1  0.0423  0.837505    
    ## stand_nod           0.025   1  0.0437  0.834843    
    ## I(stand_nod^2)      0.000   1  0.0003  0.987101    
    ## env                16.433   4  7.0835 3.911e-05 ***
    ## stand_nod:env       8.437   4  3.6368  0.007908 ** 
    ## I(stand_nod^2):env  8.843   4  3.8120  0.006012 ** 
    ## Residuals          66.696 115                      
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## Anova Table (Type III tests)
    ## 
    ## Response: get(trait)
    ##               Sum Sq  Df F value    Pr(>F)    
    ## (Intercept)    0.207   1  0.6682    0.4152    
    ## stand_nod      0.052   1  0.1685    0.6821    
    ## env           31.847   4 25.6404 1.131e-15 ***
    ## stand_nod:env 10.880   4  8.7595 2.714e-06 ***
    ## Residuals     40.366 130                      
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## Anova Table (Type III tests)
    ## 
    ## Response: get(trait)
    ##                    Sum Sq  Df F value    Pr(>F)    
    ## (Intercept)         0.104   1  0.3301    0.5666    
    ## stand_nod           0.009   1  0.0276    0.8683    
    ## I(stand_nod^2)      0.001   1  0.0020    0.9645    
    ## env                18.163   4 14.3481 1.146e-09 ***
    ## stand_nod:env       2.146   4  1.6952    0.1552    
    ## I(stand_nod^2):env  0.804   4  0.6349    0.6385    
    ## Residuals          39.559 125                      
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## Anova Table (Type III tests)
    ## 
    ## Response: get(trait)
    ##                Sum Sq Df F value  Pr(>F)  
    ## (Intercept)    10.477  1  4.8616 0.03041 *
    ## stand_nod       2.131  1  0.9889 0.32308  
    ## env            16.933  2  3.9289 0.02367 *
    ## stand_nod:env   0.664  2  0.1541 0.85745  
    ## Residuals     168.086 78                  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## Anova Table (Type III tests)
    ## 
    ## Response: get(trait)
    ##                     Sum Sq Df F value  Pr(>F)  
    ## (Intercept)         11.083  1  4.9791 0.02864 *
    ## stand_nod            0.033  1  0.0150 0.90275  
    ## I(stand_nod^2)       0.615  1  0.2765 0.60055  
    ## env                 12.799  2  2.8752 0.06265 .
    ## stand_nod:env        0.925  2  0.2078 0.81280  
    ## I(stand_nod^2):env   0.828  2  0.1861 0.83057  
    ## Residuals          166.941 75                  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## Anova Table (Type III tests)
    ## 
    ## Response: get(trait)
    ##                Sum Sq Df F value Pr(>F)
    ## (Intercept)     8.285  1  2.2279 0.1396
    ## stand_nod       1.644  1  0.4421 0.5081
    ## env            10.783  2  1.4498 0.2409
    ## stand_nod:env   3.030  2  0.4073 0.6668
    ## Residuals     290.055 78               
    ## Anova Table (Type III tests)
    ## 
    ## Response: get(trait)
    ##                     Sum Sq Df F value Pr(>F)
    ## (Intercept)          7.673  1  2.0029 0.1611
    ## stand_nod            0.827  1  0.2159 0.6435
    ## I(stand_nod^2)       0.049  1  0.0127 0.9106
    ## env                  5.949  2  0.7764 0.4637
    ## stand_nod:env        0.585  2  0.0763 0.9266
    ## I(stand_nod^2):env   2.032  2  0.2652 0.7677
    ## Residuals          287.302 75

S x E option 2: means within envs (as suggested by R1)
------------------------------------------------------

### Calculate rel/stand means

``` r
# calculate relativized and standardized traits
GE_comb_raw_sel2 <- GE_comb_raw %>%
  group_by(env) %>%
  mutate(rel_surv = surv/mean(surv, na.rm=TRUE),
         rel_shoot = shoot/mean(shoot, na.rm=TRUE),
         rel_leaf = leaf/mean(leaf, na.rm=TRUE),
         rel_flower = flower/mean(flower, na.rm=TRUE),
         rel_fruit = fruit/mean(fruit, na.rm=TRUE),
         stand_nod = (nod - mean(nod, na.rm=TRUE))/sd(nod, na.rm=TRUE)) %>%
  as.data.frame(.)
```

### S x E models

``` r
SxE_out2 <- lapply(var.list, SxE_function, df = GE_comb_raw_sel2)
```

    ## Anova Table (Type III tests)
    ## 
    ## Response: get(trait)
    ##                Sum Sq  Df  F value    Pr(>F)    
    ## (Intercept)   29.2479   1 588.3350 < 2.2e-16 ***
    ## stand_nod      0.4103   1   8.2529  0.004753 ** 
    ## env            0.0487   4   0.2448  0.912368    
    ## stand_nod:env  0.3852   4   1.9372  0.108052    
    ## Residuals      6.4627 130                       
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## Anova Table (Type III tests)
    ## 
    ## Response: get(trait)
    ##                     Sum Sq  Df  F value Pr(>F)    
    ## (Intercept)        16.6362   1 343.6212 <2e-16 ***
    ## stand_nod           0.4610   1   9.5221 0.0025 ** 
    ## I(stand_nod^2)      0.0526   1   1.0873 0.2991    
    ## env                 0.3081   4   1.5909 0.1808    
    ## stand_nod:env       0.3180   4   1.6419 0.1679    
    ## I(stand_nod^2):env  0.3918   4   2.0229 0.0952 .  
    ## Residuals           6.0518 125                    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## Anova Table (Type III tests)
    ## 
    ## Response: get(trait)
    ##               Sum Sq  Df F value    Pr(>F)    
    ## (Intercept)   25.540   1 47.8286 2.423e-10 ***
    ## stand_nod      4.475   1  8.3808  0.004506 ** 
    ## env            0.191   4  0.0895  0.985585    
    ## stand_nod:env  2.602   4  1.2180  0.306782    
    ## Residuals     64.079 120                      
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## Anova Table (Type III tests)
    ## 
    ## Response: get(trait)
    ##                    Sum Sq  Df F value   Pr(>F)    
    ## (Intercept)        14.405   1 26.9983 8.92e-07 ***
    ## stand_nod           4.016   1  7.5266 0.007054 ** 
    ## I(stand_nod^2)      0.003   1  0.0053 0.942229    
    ## env                 1.009   4  0.4726 0.755767    
    ## stand_nod:env       3.409   4  1.5972 0.179783    
    ## I(stand_nod^2):env  2.443   4  1.1446 0.339176    
    ## Residuals          61.359 115                     
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## Anova Table (Type III tests)
    ## 
    ## Response: get(trait)
    ##               Sum Sq  Df F value   Pr(>F)    
    ## (Intercept)   28.000   1 70.8022 6.18e-14 ***
    ## stand_nod      1.086   1  2.7452  0.09996 .  
    ## env            0.000   4  0.0000  1.00000    
    ## stand_nod:env  1.077   4  0.6809  0.60641    
    ## Residuals     51.411 130                     
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## Anova Table (Type III tests)
    ## 
    ## Response: get(trait)
    ##                    Sum Sq  Df F value  Pr(>F)    
    ## (Intercept)        15.326   1 39.4474 5.1e-09 ***
    ## stand_nod           1.067   1  2.7465 0.09998 .  
    ## I(stand_nod^2)      0.013   1  0.0336 0.85494    
    ## env                 0.804   4  0.5175 0.72299    
    ## stand_nod:env       1.701   4  1.0945 0.36229    
    ## I(stand_nod^2):env  1.800   4  1.1580 0.33268    
    ## Residuals          48.564 125                    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## Anova Table (Type III tests)
    ## 
    ## Response: get(trait)
    ##                Sum Sq Df F value  Pr(>F)  
    ## (Intercept)    28.000  1  6.9483 0.01012 *
    ## stand_nod       5.072  1  1.2585 0.26537  
    ## env             0.000  2  0.0000 1.00000  
    ## stand_nod:env   2.114  2  0.2623 0.76997  
    ## Residuals     314.320 78                  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## Anova Table (Type III tests)
    ## 
    ## Response: get(trait)
    ##                     Sum Sq Df F value  Pr(>F)  
    ## (Intercept)         23.519  1  5.8273 0.01822 *
    ## stand_nod            6.416  1  1.5898 0.21127  
    ## I(stand_nod^2)       1.465  1  0.3629 0.54871  
    ## env                  1.657  2  0.2052 0.81492  
    ## stand_nod:env        3.180  2  0.3939 0.67579  
    ## I(stand_nod^2):env   4.550  2  0.5637 0.57152  
    ## Residuals          302.700 75                  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## Anova Table (Type III tests)
    ## 
    ## Response: get(trait)
    ##               Sum Sq Df F value  Pr(>F)  
    ## (Intercept)    28.00  1  5.9228 0.01723 *
    ## stand_nod       4.88  1  1.0323 0.31277  
    ## env             0.00  2  0.0000 1.00000  
    ## stand_nod:env   3.79  2  0.4010 0.67104  
    ## Residuals     368.75 78                  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## Anova Table (Type III tests)
    ## 
    ## Response: get(trait)
    ##                    Sum Sq Df F value  Pr(>F)  
    ## (Intercept)         14.72  1  3.1210 0.08136 .
    ## stand_nod            2.44  1  0.5178 0.47401  
    ## I(stand_nod^2)       0.14  1  0.0306 0.86155  
    ## env                  3.89  2  0.4129 0.66321  
    ## stand_nod:env        9.74  2  1.0329 0.36098  
    ## I(stand_nod^2):env  11.21  2  1.1881 0.31047  
    ## Residuals          353.68 75                  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

### Combine results

``` r
SxE_lin_aov1 <- rbind(SxE_out[[1]][[3]], SxE_out[[2]][[3]], SxE_out[[3]][[3]], 
                     SxE_out[[4]][[3]],SxE_out[[5]][[3]])

SxE_quad_aov1 <- rbind(SxE_out[[1]][[4]], SxE_out[[2]][[4]], SxE_out[[3]][[4]], 
                     SxE_out[[4]][[4]],SxE_out[[5]][[4]])

SxE_lin_aov2 <- rbind(SxE_out2[[1]][[3]], SxE_out2[[2]][[3]], SxE_out2[[3]][[3]], 
                     SxE_out2[[4]][[3]],SxE_out2[[5]][[3]])

SxE_quad_aov2 <- rbind(SxE_out2[[1]][[4]], SxE_out2[[2]][[4]], SxE_out2[[3]][[4]], 
                     SxE_out2[[4]][[4]],SxE_out2[[5]][[4]])

SxE_lin_comp <- cbind(SxE_lin_aov1, SxE_lin_aov2) ## compare global models to within-environment models
SxE_quad_comp <- cbind(SxE_quad_aov1, SxE_quad_aov2) ## compare global models to within-environment models

write.csv(SxE_lin_comp, "SxE_res_lin_comp.csv")
write.csv(SxE_quad_comp, "SxE_res_quad_comp.csv")
```
