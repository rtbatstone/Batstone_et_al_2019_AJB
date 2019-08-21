data\_analyses\_GxE
================
Rebecca Batstone
2019-08-21

Load packages
-------------

``` r
# packages
library("tidyverse") ## includes ggplot2, dplyr, readr, stringr
library("cowplot") ## paneled graphs
library("reshape2") ## dcast function
library("lme4") ## mixed effects models
library("emmeans") ## calc model-estimated means
library("DHARMa") ## residual diagnostics for glmm
library("fitdistrplus") ## probability distributions of data
library("car") ## Anova function
library("logistf") ## firth correction (binomial responses)
library("brglm2") ## check separation issue 
```

Spreadsheets
------------

``` r
# created using "data_setup.Rmd"
date <- format(Sys.Date())
load(paste0("combined_field_GH_", date, ".Rdata"))
load("./dataset_cleaned/shoot_cleaned.Rdata")
load("./dataset_cleaned/survival_cleaned.Rdata")
load("./dataset_cleaned/leaves_cleaned.Rdata")
load("./dataset_cleaned/nods_cleaned.Rdata")
load("./dataset_cleaned/choice_cleaned.Rdata")
load("./dataset_cleaned/red_nod_cleaned.Rdata")
load("./dataset_cleaned/flowers_cleaned.Rdata")
load("./dataset_cleaned/fruits_cleaned.Rdata")
```

Set to effects contrasts
------------------------

``` r
options(contrasts = rep ("contr.sum", 2)) 
```

GLMs (line as fixed effect)
---------------------------

### glm1: shoot

Summary:

-   both models perform well, residual diagnostics hold up
-   lm type III SS can be calculated, but interaction is not significant.
-   glm type III SS cannot be calculated using Anova function, but joint\_tests indicates all terms sig.

``` r
# prob dist
ggplot(data=shoot_cc, aes(x=shoot)) + geom_density() + facet_grid(env ~.) ## highly right-skewed
```

![](data_analyses_GxE_files/figure-markdown_github/shoot-1.png)

``` r
descdist(shoot_cc$shoot, discrete = FALSE)
```

![](data_analyses_GxE_files/figure-markdown_github/shoot-2.png)

    ## summary statistics
    ## ------
    ## min:  0.8   max:  5131.7 
    ## median:  49.7 
    ## mean:  197.8709 
    ## estimated sd:  415.7647 
    ## estimated skewness:  4.925166 
    ## estimated kurtosis:  39.46961

``` r
## normal, gamma, log normal options
fit.gamma <- fitdist(shoot_cc$shoot, "gamma")
fit.norm <- fitdist(shoot_cc$shoot, "norm")
fit.lnorm <- fitdist(shoot_cc$shoot, "lnorm")
plot(fit.gamma)
```

![](data_analyses_GxE_files/figure-markdown_github/shoot-3.png)

``` r
plot(fit.norm)
```

![](data_analyses_GxE_files/figure-markdown_github/shoot-4.png)

``` r
plot(fit.lnorm)
```

![](data_analyses_GxE_files/figure-markdown_github/shoot-5.png)

``` r
fit.gamma$aic 
```

    ## [1] 9909.188

``` r
fit.norm$aic 
```

    ## [1] 12264.15

``` r
fit.lnorm$aic
```

    ## [1] 9686.514

``` r
## lnorm, gamma, norm (best to worst)

# model
<<<<<<< HEAD

lm1 <- lm(log(shoot) ~ env * line,
            data = shoot_cc)

=======
>>>>>>> 2ddd9ca6ffb84cb35f54d0e528da88655a057fe0
glm1 <- glm(shoot ~ env * line,
            family = Gamma(link="log"),
            data = shoot_cc)

# residual diagnostics
plot(lm1) ## OK
```

    ## Warning: not plotting observations with leverage one:
    ##   689, 698, 723, 726, 736, 737, 758

![](data_analyses_GxE_files/figure-markdown_github/shoot-6.png)![](data_analyses_GxE_files/figure-markdown_github/shoot-7.png)

    ## Warning: not plotting observations with leverage one:
    ##   689, 698, 723, 726, 736, 737, 758

![](data_analyses_GxE_files/figure-markdown_github/shoot-8.png)![](data_analyses_GxE_files/figure-markdown_github/shoot-9.png)

``` r
simuOut_glm1 <- simulateResiduals(fittedModel = glm1, n = 1000)
```

    ## Model family was recognized or set as continuous, but duplicate values were detected in the response. Consider if you are fitting an appropriate model.

``` r
plot(simuOut_glm1) ## OK
```

![](data_analyses_GxE_files/figure-markdown_github/shoot-10.png)

``` r
testDispersion(simuOut_glm1) ## NS
```

![](data_analyses_GxE_files/figure-markdown_github/shoot-11.png)

    ## 
    ##  DHARMa nonparametric dispersion test via sd of residuals fitted
    ##  vs. simulated
    ## 
    ## data:  simulationOutput
    ## ratioObsSim = 1.0331, p-value = 0.698
    ## alternative hypothesis: two.sided

``` r
# model summary
(ANODEV_lm_shoot <- Anova(lm1, type = 2)) ## NS interaction, type 2 used
```

    ## Anova Table (Type II tests)
    ## 
<<<<<<< HEAD
    ## Response: log(shoot)
    ##            Sum Sq  Df F value  Pr(>F)    
    ## env        570.48   4 64.1378 < 2e-16 ***
    ## line        99.15  26  1.7149 0.01531 *  
    ## env:line   266.57 104  1.1527 0.15688    
    ## Residuals 1529.88 688                    
=======
    ## Call:
    ## glm(formula = shoot ~ env * line, family = Gamma(link = "log"), 
    ##     data = shoot_cc)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -2.9530  -1.3287  -0.5192   0.3713   2.6646  
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  4.6174210  0.0521169  88.597  < 2e-16 ***
    ## env1        -0.9475417  0.0822678 -11.518  < 2e-16 ***
    ## env2         0.3904331  0.0974691   4.006 6.86e-05 ***
    ## env3         1.5168315  0.0957829  15.836  < 2e-16 ***
    ## env4         0.2133863  0.0981675   2.174 0.030068 *  
    ## line1       -0.1419707  0.3617949  -0.392 0.694879    
    ## line2        0.4117769  0.2282808   1.804 0.071697 .  
    ## line3        0.2600551  0.2323049   1.119 0.263336    
    ## line4        0.1187207  0.2211255   0.537 0.591515    
    ## line5       -0.1163227  0.2668009  -0.436 0.662980    
    ## line6       -0.2184729  0.2485178  -0.879 0.379652    
    ## line7        0.1591760  0.3225045   0.494 0.621773    
    ## line8        0.4565233  0.3093936   1.476 0.140524    
    ## line9        0.1100931  0.2077486   0.530 0.596328    
    ## line10       0.1641373  0.2159654   0.760 0.447505    
    ## line11      -0.3196670  0.2432002  -1.314 0.189143    
    ## line12       0.2953492  0.2036728   1.450 0.147482    
    ## line13      -0.1457628  0.2999011  -0.486 0.627096    
    ## line14       0.6618089  0.2668009   2.481 0.013357 *  
    ## line15       0.3040882  0.2605764   1.167 0.243622    
    ## line16      -0.9910716  0.2755707  -3.596 0.000346 ***
    ## line17      -0.1468094  0.3084900  -0.476 0.634299    
    ## line18      -0.1457012  0.2206200  -0.660 0.509207    
    ## line19       0.3297243  0.2619014   1.259 0.208470    
    ## line20      -0.5807745  0.2688437  -2.160 0.031097 *  
    ## line21      -0.0055475  0.2730061  -0.020 0.983794    
    ## line22      -0.3603603  0.2595929  -1.388 0.165533    
    ## line23      -0.4132712  0.2361660  -1.750 0.080578 .  
    ## line24      -0.1812549  0.2146690  -0.844 0.398770    
    ## line25       0.4903951  0.2607840   1.880 0.060467 .  
    ## line26       0.1605733  0.3159994   0.508 0.611515    
    ## env1:line1   0.2318424  0.4782647   0.485 0.628003    
    ## env2:line1   1.2284009  0.6473399   1.898 0.058165 .  
    ## env3:line1   0.5546471  0.6470882   0.857 0.391664    
    ## env4:line1  -0.2166266  0.7482448  -0.290 0.772276    
    ## env1:line2  -0.0433338  0.3872386  -0.112 0.910932    
    ## env2:line2  -0.7790937  0.4771978  -1.633 0.103002    
    ## env3:line2  -0.0462925  0.4463799  -0.104 0.917433    
    ## env4:line2   0.8699911  0.4773410   1.823 0.068801 .  
    ## env1:line3   0.1589804  0.4043896   0.393 0.694340    
    ## env2:line3   1.1036456  0.5849094   1.887 0.059599 .  
    ## env3:line3  -0.1233462  0.4254535  -0.290 0.771967    
    ## env4:line3  -0.6229859  0.4259966  -1.462 0.144083    
    ## env1:line4   0.3979926  0.3830641   1.039 0.299183    
    ## env2:line4  -0.9321696  0.4198421  -2.220 0.026724 *  
    ## env3:line4   0.1704090  0.4427633   0.385 0.700448    
    ## env4:line4   0.2117661  0.4739607   0.447 0.655159    
    ## env1:line5  -0.8404088  0.3995564  -2.103 0.035797 *  
    ## env2:line5  -0.0978920  0.4967780  -0.197 0.843844    
    ## env3:line5   0.3775543  0.4672532   0.808 0.419353    
    ## env4:line5   1.3295723  0.7072181   1.880 0.060530 .  
    ## env1:line6   1.1653451  0.4139160   2.815 0.005011 ** 
    ## env2:line6  -0.0172147  0.5287458  -0.033 0.974037    
    ## env3:line6  -0.1757455  0.5284376  -0.333 0.739556    
    ## env4:line6   0.8263376  0.3813383   2.167 0.030582 *  
    ## env1:line7   0.3803211  0.4387160   0.867 0.386301    
    ## env2:line7   0.5418725  0.5287862   1.025 0.305842    
    ## env3:line7  -0.6993369  0.6259686  -1.117 0.264296    
    ## env4:line7  -1.3440984  0.5016119  -2.680 0.007548 ** 
    ## env1:line8  -1.4024601  0.4399613  -3.188 0.001499 ** 
    ## env2:line8   0.3078421  0.5599423   0.550 0.582652    
    ## env3:line8  -0.6675455  0.4928156  -1.355 0.176004    
    ## env4:line8   0.1077163  0.4562440   0.236 0.813430    
    ## env1:line9   0.4344626  0.3755011   1.157 0.247665    
    ## env2:line9   0.3857569  0.3665437   1.052 0.292978    
    ## env3:line9   0.0736392  0.3554641   0.207 0.835943    
    ## env4:line9  -0.4684049  0.5109865  -0.917 0.359638    
    ## env1:line10 -0.5878759  0.3801087  -1.547 0.122420    
    ## env2:line10  0.2735958  0.3836859   0.713 0.476043    
    ## env3:line10  0.5967993  0.3982622   1.499 0.134460    
    ## env4:line10 -0.4399106  0.3714466  -1.184 0.236696    
    ## env1:line11 -0.8353667  0.3841991  -2.174 0.030022 *  
    ## env2:line11 -0.9853432  0.4140579  -2.380 0.017597 *  
    ## env3:line11  1.4166050  0.5890449   2.405 0.016439 *  
    ## env4:line11 -0.6322146  0.3998210  -1.581 0.114282    
    ## env1:line12 -0.2881206  0.3604792  -0.799 0.424409    
    ## env2:line12  0.0194000  0.4346853   0.045 0.964415    
    ## env3:line12  0.0595525  0.3764712   0.158 0.874357    
    ## env4:line12  0.5877595  0.3770850   1.559 0.119529    
    ## env1:line13 -0.4222506  0.4223781  -1.000 0.317808    
    ## env2:line13  0.7077024  0.5153119   1.373 0.170092    
    ## env3:line13 -0.1180870  0.4083396  -0.289 0.772524    
    ## env4:line13  1.2748863  0.4873869   2.616 0.009098 ** 
    ## env1:line14 -0.0737217  0.3995564  -0.185 0.853669    
    ## env2:line14 -0.2636130  0.7071215  -0.373 0.709414    
    ## env3:line14 -0.4543142  0.5372782  -0.846 0.398078    
    ## env4:line14  1.1435708  0.4677479   2.445 0.014741 *  
    ## env1:line15 -0.4510918  0.4212664  -1.071 0.284635    
    ## env2:line15  0.1606433  0.5345195   0.301 0.763858    
    ## env3:line15 -0.7890731  0.3803953  -2.074 0.038418 *  
    ## env4:line15 -0.0003921  0.4936014  -0.001 0.999366    
    ## env1:line16  0.8393053  0.4307024   1.949 0.051739 .  
    ## env2:line16 -0.1278235  0.5015425  -0.255 0.798907    
    ## env3:line16  0.0376391  0.4505377   0.084 0.933444    
    ## env4:line16 -0.6520067  0.6035141  -1.080 0.280365    
    ## env1:line17  0.9475375  0.4524726   2.094 0.036614 *  
    ## env2:line17  0.1130253  0.4717383   0.240 0.810717    
    ## env3:line17 -0.6402165  0.5591523  -1.145 0.252617    
    ## env4:line17  0.0439428  0.4556317   0.096 0.923196    
    ## env1:line18  0.6778139  0.3703185   1.830 0.067629 .  
    ## env2:line18 -0.6653270  0.5162211  -1.289 0.197887    
    ## env3:line18 -0.0474068  0.3735534  -0.127 0.899051    
    ## env4:line18 -0.0233666  0.4737251  -0.049 0.960674    
    ## env1:line19 -0.5281570  0.4079631  -1.295 0.195886    
    ## env2:line19 -0.4596586  0.4426774  -1.038 0.299467    
    ## env3:line19  0.2411317  0.5970091   0.404 0.686412    
    ## env4:line19 -0.5739499  0.3819101  -1.503 0.133339    
    ## env1:line20  0.0002493  0.4124543   0.001 0.999518    
    ## env2:line20 -0.1908016  0.4687704  -0.407 0.684116    
    ## env3:line20  0.0733784  0.4292419   0.171 0.864314    
    ## env4:line20  0.4549826  0.6004723   0.758 0.448885    
    ## env1:line21  0.5971365  0.4037264   1.479 0.139581    
    ## env2:line21 -0.0849569  0.5001380  -0.170 0.865165    
    ## env3:line21  0.1897739  0.4489737   0.423 0.672658    
    ## env4:line21 -0.6042827  0.6023474  -1.003 0.316111    
    ## env1:line22  0.9150633  0.4206588   2.175 0.029947 *  
    ## env2:line22 -0.3863645  0.3982252  -0.970 0.332280    
    ## env3:line22 -1.0675202  0.7042022  -1.516 0.129997    
    ## env4:line22  0.5122251  0.3886467   1.318 0.187952    
    ## env1:line23  0.7780374  0.3797855   2.049 0.040878 *  
    ## env2:line23 -0.0031737  0.4508249  -0.007 0.994385    
    ## env3:line23 -0.0569798  0.4806810  -0.119 0.905675    
    ## env4:line23 -0.0398045  0.4281144  -0.093 0.925949    
    ## env1:line24  0.0386624  0.3668043   0.105 0.916086    
    ## env2:line24 -0.7203811  0.4399449  -1.637 0.101997    
    ## env3:line24  0.3146636  0.4160863   0.756 0.449761    
    ## env4:line24 -0.1941722  0.3831361  -0.507 0.612460    
    ## env1:line25 -0.8010571  0.5944993  -1.347 0.178279    
    ## env2:line25  0.2986966  0.3737964   0.799 0.424515    
    ## env3:line25  0.3439362  0.4638439   0.741 0.458649    
    ## env4:line25 -0.5841272  0.5969073  -0.979 0.328127    
    ## env1:line26 -0.0212537  0.4738083  -0.045 0.964234    
    ## env2:line26 -0.0729916  0.5636192  -0.130 0.896996    
    ## env3:line26  0.2432899  0.4969894   0.490 0.624624    
    ## env4:line26  0.1380248  0.4974545   0.277 0.781509    
>>>>>>> 2ddd9ca6ffb84cb35f54d0e528da88655a057fe0
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
(joint_tests(lm1)) # matches Anova results
```

    ##  model term df1 df2 F.ratio p.value
    ##  env          4 688  54.457 <.0001 
    ##  line        26 688   1.348 0.1164 
    ##  env:line   104 688   1.153 0.1569

``` r
(ANODEV_shoot <- Anova(glm1, type = 2)) ## error if type 3, type 2 used
```

    ## Analysis of Deviance Table (Type II tests)
    ## 
    ## Response: shoot
    ##          LR Chisq  Df Pr(>Chisq)    
    ## env        429.91   4  < 2.2e-16 ***
    ## line        55.65  26  0.0006269 ***
    ## env:line   169.50 104  5.257e-05 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
(joint_tests(glm1)) ## all terms sig. 
```

    ##  model term df1 df2 F.ratio p.value
    ##  env          4 Inf 103.456 <.0001 
    ##  line        26 Inf   1.869 0.0046 
    ##  env:line   104 Inf   1.614 0.0001

### glm2: survival

Summary:

-   residual diagnostics hold up
-   type III SS can be calculated using Anova, but warning about fitted probabilities issued
-   checked separation in model, and uncovered Inf's
-   used Firth's correction to deal with separation, and detected sig. ME's of line and env, but no interaction

``` r
ggplot(data=survival_cc, aes(x=survival)) + geom_density() + facet_grid(env ~.) 
```

![](data_analyses_GxE_files/figure-markdown_github/survival-1.png)

``` r
# prob dist
descdist(survival_cc$survival, discrete = TRUE)
```

![](data_analyses_GxE_files/figure-markdown_github/survival-2.png)

    ## summary statistics
    ## ------
    ## min:  0   max:  1 
    ## median:  1 
    ## mean:  0.7445152 
    ## estimated sd:  0.4362878 
    ## estimated skewness:  -1.122478 
    ## estimated kurtosis:  2.258907

``` r
## normal, poisson, negative binomial options
fit.poiss <- fitdist(survival_cc$survival, "pois")
fit.norm <- fitdist(survival_cc$survival, "norm")
fit.nbinom <- fitdist(survival_cc$survival, "nbinom")
plot(fit.norm)
```

![](data_analyses_GxE_files/figure-markdown_github/survival-3.png)

``` r
plot(fit.nbinom)
```

![](data_analyses_GxE_files/figure-markdown_github/survival-4.png)

``` r
plot(fit.poiss)
```

![](data_analyses_GxE_files/figure-markdown_github/survival-5.png)

``` r
fit.poiss$aic 
```

    ## [1] 2726.726

``` r
fit.norm$aic 
```

    ## [1] 1668.885

``` r
fit.nbinom$aic
```

    ## [1] 2728.726

``` r
## norm, poisson, nbinom (best to worst)

# model
glm2 <- glm(survival ~ env * line,
             family = binomial,
               data = survival_cc)

# residual diagnostics
simuOut_glm2 <- simulateResiduals(fittedModel = glm2, n = 1000)
plot(simuOut_glm2) ## OK
```

![](data_analyses_GxE_files/figure-markdown_github/survival-6.png)

``` r
testDispersion(simuOut_glm2) ## NS
```

![](data_analyses_GxE_files/figure-markdown_github/survival-7.png)

    ## 
    ##  DHARMa nonparametric dispersion test via sd of residuals fitted
    ##  vs. simulated
    ## 
    ## data:  simulationOutput
    ## ratioObsSim = 1.001, p-value = 0.958
    ## alternative hypothesis: two.sided

``` r
# model summary
(ANODEV_survival <- Anova(glm2, type = 3)) ## interaction sig., fitted probability warning issued
```

    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

    ## Analysis of Deviance Table (Type III tests)
    ## 
    ## Response: survival
    ##          LR Chisq  Df Pr(>Chisq)    
    ## env         0.000   4  1.0000000    
    ## line       45.457  29  0.0265494 *  
    ## env:line  180.003 116  0.0001299 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
(joint_tests(glm2)) ## does not match Anova function results
```

    ##  model term df1 df2 F.ratio p.value
    ##  env          4 Inf   0.000 1.0000 
    ##  line        29 Inf   0.244 1.0000 
    ##  env:line   116 Inf   0.674 0.9973

``` r
# Check separation issue
(glm2_check <- glm(survival ~ env * line,
             family = binomial,
               data = survival_cc,
            method = "detect_separation")) ## Inf detected
```

    ## Separation: TRUE 
    ## Existence of maximum likelihood estimates
    ## (Intercept)        env1        env2        env3        env4       line1 
    ##         Inf        -Inf         Inf        -Inf         Inf        -Inf 
    ##       line2       line3       line4       line5       line6       line7 
    ##        -Inf        -Inf        -Inf        -Inf        -Inf         Inf 
    ##       line8       line9      line10      line11      line12      line13 
    ##        -Inf        -Inf        -Inf        -Inf        -Inf        -Inf 
    ##      line14      line15      line16      line17      line18      line19 
    ##        -Inf        -Inf        -Inf        -Inf        -Inf        -Inf 
    ##      line20      line21      line22      line23      line24      line25 
    ##         Inf        -Inf        -Inf        -Inf        -Inf        -Inf 
    ##      line26      line27      line28      line29  env1:line1  env2:line1 
    ##        -Inf        -Inf        -Inf        -Inf        -Inf         Inf 
    ##  env3:line1  env4:line1  env1:line2  env2:line2  env3:line2  env4:line2 
    ##        -Inf        -Inf         Inf        -Inf         Inf        -Inf 
    ##  env1:line3  env2:line3  env3:line3  env4:line3  env1:line4  env2:line4 
    ##         Inf        -Inf         Inf        -Inf        -Inf         Inf 
    ##  env3:line4  env4:line4  env1:line5  env2:line5  env3:line5  env4:line5 
    ##        -Inf        -Inf        -Inf         Inf        -Inf        -Inf 
    ##  env1:line6  env2:line6  env3:line6  env4:line6  env1:line7  env2:line7 
    ##         Inf        -Inf         Inf        -Inf        -Inf        -Inf 
    ##  env3:line7  env4:line7  env1:line8  env2:line8  env3:line8  env4:line8 
    ##         Inf         Inf         Inf        -Inf         Inf        -Inf 
    ##  env1:line9  env2:line9  env3:line9  env4:line9 env1:line10 env2:line10 
    ##        -Inf         Inf        -Inf        -Inf         Inf        -Inf 
    ## env3:line10 env4:line10 env1:line11 env2:line11 env3:line11 env4:line11 
    ##         Inf        -Inf        -Inf         Inf        -Inf        -Inf 
    ## env1:line12 env2:line12 env3:line12 env4:line12 env1:line13 env2:line13 
    ##         Inf        -Inf         Inf        -Inf         Inf        -Inf 
    ## env3:line13 env4:line13 env1:line14 env2:line14 env3:line14 env4:line14 
    ##         Inf        -Inf        -Inf         Inf        -Inf        -Inf 
    ## env1:line15 env2:line15 env3:line15 env4:line15 env1:line16 env2:line16 
    ##        -Inf         Inf        -Inf        -Inf        -Inf         Inf 
    ## env3:line16 env4:line16 env1:line17 env2:line17 env3:line17 env4:line17 
    ##        -Inf        -Inf        -Inf         Inf        -Inf        -Inf 
    ## env1:line18 env2:line18 env3:line18 env4:line18 env1:line19 env2:line19 
    ##         Inf        -Inf         Inf        -Inf         Inf        -Inf 
    ## env3:line19 env4:line19 env1:line20 env2:line20 env3:line20 env4:line20 
    ##         Inf        -Inf        -Inf         Inf        -Inf         Inf 
    ## env1:line21 env2:line21 env3:line21 env4:line21 env1:line22 env2:line22 
    ##        -Inf         Inf        -Inf        -Inf         Inf        -Inf 
    ## env3:line22 env4:line22 env1:line23 env2:line23 env3:line23 env4:line23 
    ##         Inf        -Inf         Inf        -Inf         Inf        -Inf 
    ## env1:line24 env2:line24 env3:line24 env4:line24 env1:line25 env2:line25 
    ##        -Inf         Inf        -Inf        -Inf        -Inf         Inf 
    ## env3:line25 env4:line25 env1:line26 env2:line26 env3:line26 env4:line26 
    ##        -Inf        -Inf         Inf         Inf         Inf        -Inf 
    ## env1:line27 env2:line27 env3:line27 env4:line27 env1:line28 env2:line28 
    ##         Inf        -Inf         Inf        -Inf         Inf        -Inf 
    ## env3:line28 env4:line28 env1:line29 env2:line29 env3:line29 env4:line29 
    ##         Inf        -Inf         Inf        -Inf         Inf        -Inf 
    ## 0: finite value, Inf: infinity, -Inf: -infinity

``` r
# firth correction
glm_firth <- logistf(survival ~ env * line, data = survival_cc)

glm_firth2 <- logistf(survival ~ env + line, data = survival_cc)

glm_firth3 <- logistf(survival ~ env, data = survival_cc)

glm_firth4 <- logistf(survival ~ line, data = survival_cc)

(firth_anova_int <- anova(glm_firth, glm_firth2, method = "PLR")) ## NS interaction
```

    ## Comparison of logistf models:
    ##                 Formula ChiSquared 
    ## 1 survival ~ env * line   216.6221 
    ## 2 survival ~ env + line   100.0312 
    ## 
    ## Method:  PLR 
    ## Chi-Squared:  116.5909   df= 116   P= 0.467125

``` r
(firth_anova_line <- anova(glm_firth2, glm_firth3, method = "PLR")) ## sig ME of line
```

    ## Comparison of logistf models:
    ##                 Formula ChiSquared 
    ## 1 survival ~ env + line  100.03121 
    ## 2        survival ~ env   55.82799 
    ## 
    ## Method:  PLR 
    ## Chi-Squared:  44.20323   df= 29   P= 0.03510493

``` r
(firth_anova_env <- anova(glm_firth2, glm_firth4, method = "PLR")) ## sig ME of env
```

    ## Comparison of logistf models:
    ##                 Formula ChiSquared 
    ## 1 survival ~ env + line  100.03121 
    ## 2       survival ~ line   41.42589 
    ## 
    ## Method:  PLR 
    ## Chi-Squared:  58.60532   df= 4   P= 5.695e-12

### glm3: leaf

Summary:

-   glm residual diagnostics hold up, but not for lm.
-   lm type III SS can be calculated, but interaction is not significant.
-   glm type III SS cannot be calculated using Anova function, but joint\_tests indicates all terms sig.

``` r
# prob dist
ggplot(data=leaf_cc, aes(x=leaf)) + geom_density() + facet_grid(env ~.) ## highly right-skewed
```

![](data_analyses_GxE_files/figure-markdown_github/leaf-1.png)

``` r
descdist(leaf_cc$leaf, discrete = TRUE)
```

![](data_analyses_GxE_files/figure-markdown_github/leaf-2.png)

    ## summary statistics
    ## ------
    ## min:  0   max:  541 
    ## median:  16 
    ## mean:  45.17661 
    ## estimated sd:  70.56839 
    ## estimated skewness:  3.046316 
    ## estimated kurtosis:  14.34338

``` r
## normal, poisson, negative binomial options
fit.poiss <- fitdist(leaf_cc$leaf, "pois")
fit.norm <- fitdist(leaf_cc$leaf, "norm")
fit.nbinom <- fitdist(leaf_cc$leaf, "nbinom")
plot(fit.norm)
```

![](data_analyses_GxE_files/figure-markdown_github/leaf-3.png)

``` r
plot(fit.nbinom)
```

![](data_analyses_GxE_files/figure-markdown_github/leaf-4.png)

``` r
plot(fit.poiss)
```

![](data_analyses_GxE_files/figure-markdown_github/leaf-5.png)

``` r
fit.poiss$aic
```

    ## [1] 64536.3

``` r
fit.norm$aic 
```

    ## [1] 9708.14

``` r
fit.nbinom$aic 
```

    ## [1] 8140.491

``` r
## nbinom, norm, poiss (best to worst)

# model
lm3 <- lm(sqrt(leaf) ~ env * line, 
            data = leaf_cc)

glm3 <- glm.nb(leaf ~ env * line, 
            data = leaf_cc)

# residual diagnostics
plot(lm3) ## not great
```

    ## Warning: not plotting observations with leverage one:
    ##   713, 716, 725, 752, 765, 766, 822

![](data_analyses_GxE_files/figure-markdown_github/leaf-6.png)![](data_analyses_GxE_files/figure-markdown_github/leaf-7.png)

    ## Warning: not plotting observations with leverage one:
    ##   713, 716, 725, 752, 765, 766, 822

![](data_analyses_GxE_files/figure-markdown_github/leaf-8.png)![](data_analyses_GxE_files/figure-markdown_github/leaf-9.png)

``` r
simOut_glm3 <- simulateResiduals(fittedModel = glm3, n = 1000)
plot(simOut_glm3) ## OK
```

![](data_analyses_GxE_files/figure-markdown_github/leaf-10.png)

``` r
testDispersion(simOut_glm3) ## NS
```

![](data_analyses_GxE_files/figure-markdown_github/leaf-11.png)

    ## 
    ##  DHARMa nonparametric dispersion test via sd of residuals fitted
    ##  vs. simulated
    ## 
    ## data:  simulationOutput
    ## ratioObsSim = 1.0932, p-value = 0.256
    ## alternative hypothesis: two.sided

``` r
# model summary
(ANODEV_lm_leaf <- Anova(lm3, type = 2)) ## NS interaction, type 2 used
```

    ## Anova Table (Type II tests)
    ## 
    ## Response: sqrt(leaf)
    ##           Sum Sq  Df F value Pr(>F)    
    ## env       4141.5   4 93.3810 <2e-16 ***
    ## line       394.3  27  1.3172 0.1312    
    ## env:line  1149.1 108  0.9596 0.5966    
    ## Residuals 7927.6 715                   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
(joint_tests(lm3)) # matches Anova results
```

    ##  model term df1 df2 F.ratio p.value
    ##  env          4 715  81.604 <.0001 
    ##  line        27 715   1.169 0.2538 
    ##  env:line   108 715   0.960 0.5966

``` r
(ANODEV_glm_leaf <- Anova(glm3, type = 2)) ## error if set to type 3, type 2 used
```

    ## Analysis of Deviance Table (Type II tests)
    ## 
    ## Response: leaf
    ##          LR Chisq  Df Pr(>Chisq)    
    ## env        773.71   4  < 2.2e-16 ***
    ## line        71.71  27  6.368e-06 ***
    ## env:line   175.30 108  4.519e-05 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
(joint_tests(glm3)) ## all terms sig.
```

    ##  model term df1 df2 F.ratio p.value
    ##  env          4 Inf 162.367 <.0001 
    ##  line        27 Inf   2.512 <.0001 
    ##  env:line   108 Inf   1.466 0.0012

### glm4: nod

Summary:

-   glm residual diagnostics hold up, but not for lm.
-   lm type III SS can be calculated, but interaction is not significant.
-   glm type III SS can be calculated using Anova function and joint\_tests closely matches, but non-convergence warning issued

``` r
# prob dist
ggplot(data=nod_cc, aes(x=nod)) + geom_density() + facet_grid(env ~.) ## highly right-skewed
```

![](data_analyses_GxE_files/figure-markdown_github/nod-1.png)

``` r
descdist(nod_cc$nod, discrete = TRUE)
```

![](data_analyses_GxE_files/figure-markdown_github/nod-2.png)

    ## summary statistics
    ## ------
    ## min:  0   max:  856 
    ## median:  19 
    ## mean:  62.20609 
    ## estimated sd:  118.9718 
    ## estimated skewness:  3.782906 
    ## estimated kurtosis:  19.67637

``` r
## normal, poisson, negative binomial options
fit.poiss <- fitdist(nod_cc$nod, "pois")
fit.norm <- fitdist(nod_cc$nod, "norm")
fit.nbinom <- fitdist(nod_cc$nod, "nbinom")
plot(fit.norm)
```

![](data_analyses_GxE_files/figure-markdown_github/nod-3.png)

``` r
plot(fit.nbinom)
```

![](data_analyses_GxE_files/figure-markdown_github/nod-4.png)

``` r
plot(fit.poiss)
```

![](data_analyses_GxE_files/figure-markdown_github/nod-5.png)

``` r
fit.poiss$aic
```

    ## [1] 107953.7

``` r
fit.norm$aic 
```

    ## [1] 10588.89

``` r
fit.nbinom$aic 
```

    ## [1] 8559.935

``` r
## nbinom, norm, poisson (best to worst)

# model
lm4 <- lm(sqrt(nod) ~ env * line,
          data = nod_cc)

glm4 <- glm.nb(nod ~ env * line, 
            data = nod_cc)

# residual diagnostics
plot(lm4) ## bad
```

    ## Warning: not plotting observations with leverage one:
    ##   711, 714, 723, 751, 763, 764, 820

![](data_analyses_GxE_files/figure-markdown_github/nod-6.png)![](data_analyses_GxE_files/figure-markdown_github/nod-7.png)

    ## Warning: not plotting observations with leverage one:
    ##   711, 714, 723, 751, 763, 764, 820

![](data_analyses_GxE_files/figure-markdown_github/nod-8.png)![](data_analyses_GxE_files/figure-markdown_github/nod-9.png)

``` r
simOut_glm4 <- simulateResiduals(fittedModel = glm4, n = 1000)
plot(simOut_glm4) ## OK
```

![](data_analyses_GxE_files/figure-markdown_github/nod-10.png)

``` r
testDispersion(simOut_glm4) ## NS
```

![](data_analyses_GxE_files/figure-markdown_github/nod-11.png)

    ## 
    ##  DHARMa nonparametric dispersion test via sd of residuals fitted
    ##  vs. simulated
    ## 
    ## data:  simulationOutput
    ## ratioObsSim = 1.0336, p-value = 0.672
    ## alternative hypothesis: two.sided

``` r
# model summary
(ANODEV_lm_nod <- Anova(lm4, type = 2)) ## NS interaction, type 2 used
```

    ## Anova Table (Type II tests)
    ## 
    ## Response: sqrt(nod)
    ##            Sum Sq  Df F value Pr(>F)    
    ## env        6064.1   4 80.4370 <2e-16 ***
    ## line        694.9  27  1.3656 0.1032    
    ## env:line   1656.8 108  0.8139 0.9095    
    ## Residuals 13456.9 714                   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
(joint_tests(lm4)) ## matches Anova results
```

    ##  model term df1 df2 F.ratio p.value
    ##  env          4 714  70.498 <.0001 
    ##  line        27 714   0.607 0.9429 
    ##  env:line   108 714   0.814 0.9095

``` r
(ANODEV_glm_nod <- Anova(glm4, type = 3)) ## non-convergence warning
```

    ## Warning: glm.fit: algorithm did not converge

    ## Warning: glm.fit: algorithm did not converge

    ## Analysis of Deviance Table (Type III tests)
    ## 
    ## Response: nod
    ##          LR Chisq  Df Pr(>Chisq)    
    ## env        485.65   4  < 2.2e-16 ***
    ## line        59.41  27  0.0003154 ***
    ## env:line   142.27 108  0.0151165 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
(joint_tests(glm4)) ## close to Anova results
```

    ##  model term df1 df2 F.ratio p.value
    ##  env          4 Inf 141.410 <.0001 
    ##  line        27 Inf   1.642 0.0191 
    ##  env:line   108 Inf   1.306 0.0181

### glm5: choice

Summary:

-   type II SS used, no interaction

``` r
# prob dist
ggplot(data=choice_cc, aes(x=choice)) + geom_density() + facet_grid(env ~.) ## normal-ish
```

![](data_analyses_GxE_files/figure-markdown_github/choice-1.png)

``` r
descdist(choice_cc$choice, discrete = FALSE)
```

![](data_analyses_GxE_files/figure-markdown_github/choice-2.png)

    ## summary statistics
    ## ------
    ## min:  0   max:  1 
    ## median:  0.5531915 
    ## mean:  0.4630196 
    ## estimated sd:  0.2883259 
    ## estimated skewness:  -0.5028709 
    ## estimated kurtosis:  2.006805

``` r
## normal, poisson, negative binomial options
fit.unif <- fitdist(choice_cc$choice, "unif")
fit.norm <- fitdist(choice_cc$choice, "norm")
plot(fit.norm)
```

![](data_analyses_GxE_files/figure-markdown_github/choice-3.png)

``` r
plot(fit.unif)
```

![](data_analyses_GxE_files/figure-markdown_github/choice-4.png)

``` r
fit.unif$aic 
```

    ## [1] NA

``` r
fit.norm$aic
```

    ## [1] 91.6871

``` r
## norm (best to worst)

# model
glm5 <- lm(choice ~ line, 
             data = choice_cc)

# residual diagnostics
plot(glm5)
```

![](data_analyses_GxE_files/figure-markdown_github/choice-5.png)![](data_analyses_GxE_files/figure-markdown_github/choice-6.png)![](data_analyses_GxE_files/figure-markdown_github/choice-7.png)![](data_analyses_GxE_files/figure-markdown_github/choice-8.png)

``` r
# model summary
(ANODEV_choice <- Anova(glm5, type = 2)) # no interaction term, type 2 used
```

    ## Anova Table (Type II tests)
    ## 
    ## Response: choice
    ##            Sum Sq  Df F value    Pr(>F)    
    ## line       4.6621  29  2.2011 0.0007277 ***
    ## Residuals 16.2871 223                      
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

### glm6: red nodules

Summary:

-   type II SS used, no interaction

``` r
# prob dist
ggplot(data=red_nod_cc, aes(x=totalred)) + geom_density() + facet_grid(env ~.) ## right skewed
```

![](data_analyses_GxE_files/figure-markdown_github/red_nod-1.png)

``` r
descdist(red_nod_cc$totalred, discrete = TRUE)
```

![](data_analyses_GxE_files/figure-markdown_github/red_nod-2.png)

    ## summary statistics
    ## ------
    ## min:  0   max:  635 
    ## median:  48 
    ## mean:  96.9387 
    ## estimated sd:  128.298 
    ## estimated skewness:  1.865651 
    ## estimated kurtosis:  6.256071

``` r
## normal, poisson, negative binomial options
fit.poiss <- fitdist(red_nod_cc$totalred, "pois")
fit.norm <- fitdist(red_nod_cc$totalred, "norm")
fit.nbinom <- fitdist(red_nod_cc$totalred, "nbinom")
plot(fit.norm)
```

![](data_analyses_GxE_files/figure-markdown_github/red_nod-3.png)

``` r
plot(fit.nbinom)
```

![](data_analyses_GxE_files/figure-markdown_github/red_nod-4.png)

``` r
plot(fit.poiss)
```

![](data_analyses_GxE_files/figure-markdown_github/red_nod-5.png)

``` r
fit.poiss$aic 
```

    ## [1] 38951.16

``` r
fit.norm$aic 
```

    ## [1] 3277.657

``` r
fit.nbinom$aic 
```

    ## [1] 2659.14

``` r
## nbinom, norm, poisson (best to worst)

# model
glm6 <- glm.nb(totalred ~ line,
                data = subset(red_nod_cc, survival > 0))

# residual diagnostics
simOut_glm6 <- simulateResiduals(fittedModel = glm6, n = 1000)
plot(simOut_glm6) ## OK
```

![](data_analyses_GxE_files/figure-markdown_github/red_nod-6.png)

``` r
testDispersion(simOut_glm6) ## NS
```

![](data_analyses_GxE_files/figure-markdown_github/red_nod-7.png)

    ## 
    ##  DHARMa nonparametric dispersion test via sd of residuals fitted
    ##  vs. simulated
    ## 
    ## data:  simulationOutput
    ## ratioObsSim = 0.8601, p-value = 0.246
    ## alternative hypothesis: two.sided

``` r
# model summary
(ANODEV_totalred <- Anova(glm6, type = 2)) ## no interaction term, type 2 used
```

    ## Analysis of Deviance Table (Type II tests)
    ## 
    ## Response: totalred
    ##      LR Chisq Df Pr(>Chisq)   
    ## line   56.932 29   0.001464 **
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

### glm7: flower

Summary:

-   residual diagnostics hold up
-   type III SS can be calculated using Anova, but warning about fitted probabilities issued
-   checked separation in model, and uncovered Inf's
-   used Firth's correction to deal with separation, and detected sig. ME's of line and env, but no interaction

``` r
# prob dist
ggplot(data=flower_cc, aes(x=flo)) + geom_density() + facet_grid(env ~ .) ## highly right-skewed
```

![](data_analyses_GxE_files/figure-markdown_github/flower-1.png)

``` r
descdist(flower_cc$flo, discrete = TRUE)
```

![](data_analyses_GxE_files/figure-markdown_github/flower-2.png)

    ## summary statistics
    ## ------
    ## min:  0   max:  23 
    ## median:  0 
    ## mean:  0.5977011 
    ## estimated sd:  2.283849 
    ## estimated skewness:  6.045295 
    ## estimated kurtosis:  49.66962

``` r
## normal, poisson, negative binomial options
fit.poiss <- fitdist(flower_cc$flo, "pois")
fit.norm <- fitdist(flower_cc$flo, "norm")
fit.nbinom <- fitdist(flower_cc$flo, "nbinom")
plot(fit.norm)
```

![](data_analyses_GxE_files/figure-markdown_github/flower-3.png)

``` r
plot(fit.nbinom)
```

![](data_analyses_GxE_files/figure-markdown_github/flower-4.png)

``` r
plot(fit.poiss)
```

![](data_analyses_GxE_files/figure-markdown_github/flower-5.png)

``` r
fit.poiss$aic 
```

    ## [1] 1735.791

``` r
fit.norm$aic 
```

    ## [1] 2346.571

``` r
fit.nbinom$aic 
```

    ## [1] 697.0818

``` r
## nbinom, poisson, norm (best to worst)

# calculate flower success
flower_cc$flo_succ <- as.numeric(flower_cc$flo > 0) 

# model
glm7 <- glm(flo_succ ~ env * line,
             family= binomial,
             data = subset(flower_cc, survival > 0))

# residual diagnostics
simOut_glm7 <- simulateResiduals(fittedModel = glm7, n = 1000)
plot(simOut_glm7) ## OK
```

![](data_analyses_GxE_files/figure-markdown_github/flower-6.png)

``` r
testDispersion(simOut_glm7) ## NS
```

![](data_analyses_GxE_files/figure-markdown_github/flower-7.png)

    ## 
    ##  DHARMa nonparametric dispersion test via sd of residuals fitted
    ##  vs. simulated
    ## 
    ## data:  simulationOutput
    ## ratioObsSim = 1.0018, p-value = 0.958
    ## alternative hypothesis: two.sided

``` r
# model summary
(ANODEV_flo <- Anova(glm7, type = 3)) ## no terms sig., fitted probability warning issued
```

    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

    ## Analysis of Deviance Table (Type III tests)
    ## 
    ## Response: flo_succ
    ##          LR Chisq Df Pr(>Chisq)
    ## env          0.00  2     1.0000
    ## line         0.00 27     1.0000
    ## env:line    37.98 54     0.9517

``` r
(joint_tests(glm7)) ## matches Anova results
```

    ##  model term df1 df2 F.ratio p.value
    ##  env          2 Inf   0.000 1.0000 
    ##  line        27 Inf   0.000 1.0000 
    ##  env:line    54 Inf   0.089 1.0000

``` r
# Check separation issue
(glm7_check <- glm(flo_succ ~ env * line,
             family= binomial,
             data = subset(flower_cc, survival > 0),
            method = "detect_separation")) ## Inf detected
```

    ## Separation: TRUE 
    ## Existence of maximum likelihood estimates
    ## (Intercept)        env1        env2       line1       line2       line3 
    ##        -Inf         Inf         Inf         Inf        -Inf        -Inf 
    ##       line4       line5       line6       line7       line8       line9 
    ##        -Inf         Inf         Inf        -Inf         Inf        -Inf 
    ##      line10      line11      line12      line13      line14      line15 
    ##        -Inf         Inf         Inf        -Inf         Inf        -Inf 
    ##      line16      line17      line18      line19      line20      line21 
    ##        -Inf         Inf        -Inf         Inf         Inf         Inf 
    ##      line22      line23      line24      line25      line26      line27 
    ##         Inf         Inf        -Inf        -Inf        -Inf        -Inf 
    ##  env1:line1  env2:line1  env1:line2  env2:line2  env1:line3  env2:line3 
    ##        -Inf        -Inf         Inf         Inf         Inf         Inf 
    ##  env1:line4  env2:line4  env1:line5  env2:line5  env1:line6  env2:line6 
    ##         Inf         Inf        -Inf        -Inf        -Inf        -Inf 
    ##  env1:line7  env2:line7  env1:line8  env2:line8  env1:line9  env2:line9 
    ##         Inf         Inf        -Inf        -Inf         Inf         Inf 
    ## env1:line10 env2:line10 env1:line11 env2:line11 env1:line12 env2:line12 
    ##         Inf         Inf        -Inf        -Inf        -Inf        -Inf 
    ## env1:line13 env2:line13 env1:line14 env2:line14 env1:line15 env2:line15 
    ##         Inf         Inf        -Inf        -Inf         Inf         Inf 
    ## env1:line16 env2:line16 env1:line17 env2:line17 env1:line18 env2:line18 
    ##         Inf        -Inf        -Inf        -Inf         Inf         Inf 
    ## env1:line19 env2:line19 env1:line20 env2:line20 env1:line21 env2:line21 
    ##        -Inf        -Inf        -Inf        -Inf        -Inf        -Inf 
    ## env1:line22 env2:line22 env1:line23 env2:line23 env1:line24 env2:line24 
    ##        -Inf        -Inf        -Inf        -Inf         Inf         Inf 
    ## env1:line25 env2:line25 env1:line26 env2:line26 env1:line27 env2:line27 
    ##         Inf         Inf         Inf         Inf        -Inf         Inf 
    ## 0: finite value, Inf: infinity, -Inf: -infinity

``` r
# firth correction
glm7_firth <- logistf(flo_succ ~ env * line, data = subset(flower_cc, survival > 0))

glm7_firth2 <- logistf(flo_succ ~ env + line, data = subset(flower_cc, survival > 0))

glm7_firth3 <- logistf(flo_succ ~ env, data = subset(flower_cc, survival > 0))

glm7_firth4 <- logistf(flo_succ ~ line, data = subset(flower_cc, survival > 0))

(firth_anova_int <- anova(glm7_firth, glm7_firth2, method = "PLR")) ## NS interaction
```

    ## Comparison of logistf models:
    ##                 Formula ChiSquared 
    ## 1 flo_succ ~ env * line   79.77240 
    ## 2 flo_succ ~ env + line   78.48999 
    ## 
    ## Method:  PLR 
    ## Chi-Squared:  1.282415   df= 54   P= 1

``` r
(firth_anova_line <- anova(glm7_firth2, glm7_firth3, method = "PLR")) ## sig ME of line
```

    ## Comparison of logistf models:
    ##                 Formula ChiSquared 
    ## 1 flo_succ ~ env + line   78.48999 
    ## 2        flo_succ ~ env   33.98819 
    ## 
    ## Method:  PLR 
    ## Chi-Squared:  44.5018   df= 27   P= 0.01834046

``` r
(firth_anova_env <- anova(glm7_firth2, glm7_firth4, method = "PLR")) ## sig ME of env
```

    ## Comparison of logistf models:
    ##                 Formula ChiSquared 
    ## 1 flo_succ ~ env + line   78.48999 
    ## 2       flo_succ ~ line   49.36539 
    ## 
    ## Method:  PLR 
    ## Chi-Squared:  29.1246   df= 2   P= 4.738862e-07

### glm8: fruit

Summary:

-   residual diagnostics hold up on binomial data, not count data

Binomial: - type III SS can be calculated using Anova, but warning about fitted probabilities issued - checked separation in model, and uncovered Inf's - used Firth's correction to deal with separation, and detected sig. ME's of line and env, but no interaction

``` r
# prob dist
ggplot(data=fruit_cc, aes(x=fru)) + geom_density() + facet_grid(env ~.) ## highly right-skewed
```

![](data_analyses_GxE_files/figure-markdown_github/fruit-1.png)

``` r
descdist(fruit_cc$fru, discrete = TRUE)
```

![](data_analyses_GxE_files/figure-markdown_github/fruit-2.png)

    ## summary statistics
    ## ------
    ## min:  0   max:  72 
    ## median:  0 
    ## mean:  2.097701 
    ## estimated sd:  7.792907 
    ## estimated skewness:  5.194199 
    ## estimated kurtosis:  34.60909

``` r
## normal, poisson, negative binomial options
fit.poiss <- fitdist(fruit_cc$fru, "pois")
fit.norm <- fitdist(fruit_cc$fru, "norm")
fit.nbinom <- fitdist(fruit_cc$fru, "nbinom")
plot(fit.norm)
```

![](data_analyses_GxE_files/figure-markdown_github/fruit-3.png)

``` r
plot(fit.nbinom)
```

![](data_analyses_GxE_files/figure-markdown_github/fruit-4.png)

``` r
plot(fit.poiss)
```

![](data_analyses_GxE_files/figure-markdown_github/fruit-5.png)

``` r
fit.poiss$aic 
```

    ## [1] 5597.182

``` r
fit.norm$aic 
```

    ## [1] 3627.926

``` r
fit.nbinom$aic 
```

    ## [1] 995.7986

``` r
## nbinom, norm, poisson  (best to worst)

# calculate fruit success
fruit_cc$fru_succ <- as.numeric(fruit_cc$fru > 0) 

# models
lm8_nz <- lm(sqrt(fru) ~ env * line, 
              data = subset(fruit_cc, fru > 0))

glm8_nz <- glm.nb(fru ~ env * line, 
              data = subset(fruit_cc, fru > 0))

glm8_succ <- glm(fru_succ ~ env * line, 
              family = binomial,   
              data = fruit_cc)

# residual diagnostics
plot(lm8_nz) ## OK
```

    ## Warning: not plotting observations with leverage one:
    ##   5, 6, 7, 9, 15, 16, 17, 19, 21, 22, 30, 41, 50, 51, 55, 61, 62, 64, 66, 67, 71

![](data_analyses_GxE_files/figure-markdown_github/fruit-6.png)![](data_analyses_GxE_files/figure-markdown_github/fruit-7.png)

    ## Warning: not plotting observations with leverage one:
    ##   5, 6, 7, 9, 15, 16, 17, 19, 21, 22, 30, 41, 50, 51, 55, 61, 62, 64, 66, 67, 71

![](data_analyses_GxE_files/figure-markdown_github/fruit-8.png)

    ## Warning in sqrt(crit * p * (1 - hh)/hh): NaNs produced

    ## Warning in sqrt(crit * p * (1 - hh)/hh): NaNs produced

![](data_analyses_GxE_files/figure-markdown_github/fruit-9.png)

``` r
simOut_glm8_nz <- simulateResiduals(fittedModel = glm8_nz, n = 1000)
plot(simOut_glm8_nz) ## bad
```

![](data_analyses_GxE_files/figure-markdown_github/fruit-10.png)

``` r
testDispersion(simOut_glm8_nz) ## NS
```

![](data_analyses_GxE_files/figure-markdown_github/fruit-11.png)

    ## 
    ##  DHARMa nonparametric dispersion test via sd of residuals fitted
    ##  vs. simulated
    ## 
    ## data:  simulationOutput
    ## ratioObsSim = 0.85933, p-value = 0.442
    ## alternative hypothesis: two.sided

``` r
simOut_glm8_succ <- simulateResiduals(fittedModel = glm8_succ, n = 1000)
plot(simOut_glm8_succ) ## OK
```

![](data_analyses_GxE_files/figure-markdown_github/fruit-12.png)

``` r
testDispersion(simOut_glm8_succ) ## NS
```

![](data_analyses_GxE_files/figure-markdown_github/fruit-13.png)

    ## 
    ##  DHARMa nonparametric dispersion test via sd of residuals fitted
    ##  vs. simulated
    ## 
    ## data:  simulationOutput
    ## ratioObsSim = 1.0014, p-value = 0.996
    ## alternative hypothesis: two.sided

``` r
# model summaries
(ANODEV_lm_fru_nz <- Anova(lm8_nz, type = 2)) ## singular, cannot assess type 3, type 2 used
```

    ## Note: model has aliased coefficients
    ##       sums of squares computed by model comparison

    ## Anova Table (Type II tests)
    ## 
    ## Response: sqrt(fru)
    ##            Sum Sq Df F value  Pr(>F)  
    ## env        20.533  2  4.2098 0.02353 *
    ## line      122.308 24  2.0897 0.02491 *
    ## env:line   42.365 16  1.0857 0.40523  
    ## Residuals  80.479 33                  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
(joint_tests(lm8_nz)) ## matches Anova results
```

    ##  model term df1 df2 F.ratio p.value note
    ##  line         1  33   0.029 0.8648     e
    ##  env:line    16  33   1.086 0.4052     e
    ## 
    ## e: df1 reduced due to non-estimability

``` r
(ANODEV_glm_fru_nz <- Anova(glm8_nz, type = 2)) ## singular, cannot assess type 3, type 2 used
```

    ## Analysis of Deviance Table (Type II tests)
    ## 
    ## Response: fru
    ##          LR Chisq Df Pr(>Chisq)    
    ## env        17.272  2  0.0001776 ***
    ## line      104.848 24  4.424e-12 ***
    ## env:line   41.455 16  0.0004756 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
(joint_tests(glm8_nz)) ## matches Anova results
```

    ##  model term df1 df2 F.ratio p.value note
    ##  line         1 Inf   0.180 0.6716     e
    ##  env:line    16 Inf   2.452 0.0010     e
    ## 
    ## e: df1 reduced due to non-estimability

``` r
(ANODEV_fru_succ <- Anova(glm8_succ, type = 3)) ## no terms sig., fitted probability warning issued
```

    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

    ## Analysis of Deviance Table (Type III tests)
    ## 
    ## Response: fru_succ
    ##          LR Chisq Df Pr(>Chisq)
    ## env         0.000  2     1.0000
    ## line       29.829 27     0.3219
    ## env:line   55.603 54     0.4142

``` r
(joint_tests(glm8_succ)) ## close to Anova results
```

    ##  model term df1 df2 F.ratio p.value
    ##  env          2 Inf   0.000 0.9999 
    ##  line        27 Inf   0.000 1.0000 
    ##  env:line    54 Inf   0.198 1.0000

``` r
# Check separation issue
(glm8_check <- glm(fru_succ ~ env * line, 
              family = binomial,   
              data = fruit_cc,
            method = "detect_separation")) ## Inf detected
```

    ## Separation: TRUE 
    ## Existence of maximum likelihood estimates
    ## (Intercept)        env1        env2       line1       line2       line3 
    ##        -Inf         Inf         Inf         Inf        -Inf        -Inf 
    ##       line4       line5       line6       line7       line8       line9 
    ##        -Inf         Inf         Inf        -Inf        -Inf        -Inf 
    ##      line10      line11      line12      line13      line14      line15 
    ##        -Inf         Inf         Inf        -Inf         Inf        -Inf 
    ##      line16      line17      line18      line19      line20      line21 
    ##        -Inf        -Inf        -Inf         Inf         Inf         Inf 
    ##      line22      line23      line24      line25      line26      line27 
    ##         Inf        -Inf         Inf        -Inf        -Inf         Inf 
    ##  env1:line1  env2:line1  env1:line2  env2:line2  env1:line3  env2:line3 
    ##        -Inf        -Inf         Inf         Inf         Inf         Inf 
    ##  env1:line4  env2:line4  env1:line5  env2:line5  env1:line6  env2:line6 
    ##         Inf         Inf        -Inf        -Inf        -Inf        -Inf 
    ##  env1:line7  env2:line7  env1:line8  env2:line8  env1:line9  env2:line9 
    ##         Inf         Inf         Inf         Inf         Inf         Inf 
    ## env1:line10 env2:line10 env1:line11 env2:line11 env1:line12 env2:line12 
    ##         Inf         Inf        -Inf        -Inf        -Inf        -Inf 
    ## env1:line13 env2:line13 env1:line14 env2:line14 env1:line15 env2:line15 
    ##         Inf         Inf        -Inf        -Inf         Inf         Inf 
    ## env1:line16 env2:line16 env1:line17 env2:line17 env1:line18 env2:line18 
    ##         Inf        -Inf        -Inf         Inf         Inf         Inf 
    ## env1:line19 env2:line19 env1:line20 env2:line20 env1:line21 env2:line21 
    ##        -Inf        -Inf        -Inf        -Inf        -Inf        -Inf 
    ## env1:line22 env2:line22 env1:line23 env2:line23 env1:line24 env2:line24 
    ##        -Inf        -Inf         Inf         Inf        -Inf        -Inf 
    ## env1:line25 env2:line25 env1:line26 env2:line26 env1:line27 env2:line27 
    ##         Inf         Inf         Inf         Inf        -Inf        -Inf 
    ## 0: finite value, Inf: infinity, -Inf: -infinity

``` r
# firth correction
glm8_firth <- logistf(fru_succ ~ env * line, data = fruit_cc)

glm8_firth2 <- logistf(fru_succ ~ env + line, data = fruit_cc)

glm8_firth3 <- logistf(fru_succ ~ env, data = fruit_cc)

glm8_firth4 <- logistf(fru_succ ~ line, data = fruit_cc)

(firth_anova_int <- anova(glm8_firth, glm8_firth2, method = "PLR")) ## NS interaction
```

    ## Comparison of logistf models:
    ##                 Formula ChiSquared 
    ## 1 fru_succ ~ env * line   88.42753 
    ## 2 fru_succ ~ env + line   73.19702 
    ## 
    ## Method:  PLR 
    ## Chi-Squared:  15.23051   df= 54   P= 1

``` r
(firth_anova_line <- anova(glm8_firth2, glm8_firth3, method = "PLR")) ## marg sig ME of line
```

    ## Comparison of logistf models:
    ##                 Formula ChiSquared 
    ## 1 fru_succ ~ env + line   73.19702 
    ## 2        fru_succ ~ env   33.94419 
    ## 
    ## Method:  PLR 
    ## Chi-Squared:  39.25284   df= 27   P= 0.06008161

``` r
(firth_anova_env <- anova(glm8_firth2, glm8_firth4, method = "PLR")) ## sig ME of env
```

    ## Comparison of logistf models:
    ##                 Formula ChiSquared 
    ## 1 fru_succ ~ env + line   73.19702 
    ## 2       fru_succ ~ line   42.05208 
    ## 
    ## Method:  PLR 
    ## Chi-Squared:  31.14494   df= 2   P= 1.725688e-07
