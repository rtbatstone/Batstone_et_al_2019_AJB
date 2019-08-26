data\_analyses\_SxE
================
Rebecca Batstone
2019-08-26

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

Option 1: global means (as used in first version of the manuscript)
-------------------------------------------------------------------

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

### Option 1: S x E models

``` r
SxE_function <- function(df, trait){
   print(paste(trait))
   lin_mod <- lm(get(trait) ~ stand_nod*env, data = df)
   lin_mod_sum <- summary(lin_mod)
   lin_mod_aov <- Anova(lin_mod, type = 3)
   quad_mod <- lm(get(trait) ~ (stand_nod + I(stand_nod^2))*env, data = df)
   quad_mod_sum <- summary(quad_mod)
   quad_mod_aov <- Anova(quad_mod, type = 3)
   mod_comp <- anova(lin_mod, quad_mod)
   
   return(list(lin_mod_sum, quad_mod_sum, lin_mod_aov, quad_mod_aov, mod_comp))
 }
 
# specify vars to loop over
var.list <- c("rel_surv","rel_shoot","rel_leaf","rel_flower","rel_fruit")
 
SxE_out <- lapply(var.list, SxE_function, df = GE_comb_raw_sel)
```

    ## [1] "rel_surv"
    ## [1] "rel_shoot"
    ## [1] "rel_leaf"
    ## [1] "rel_flower"
    ## [1] "rel_fruit"

Option 2: means within envs (as suggested by R1)
------------------------------------------------

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

### Option 2: S x E models

``` r
SxE_out2 <- lapply(var.list, SxE_function, df = GE_comb_raw_sel2)
```

    ## [1] "rel_surv"
    ## [1] "rel_shoot"
    ## [1] "rel_leaf"
    ## [1] "rel_flower"
    ## [1] "rel_fruit"

### Option 3: standardize traits across environments, absolute fitness

``` r
# new var list (absolute fitness values
var.list2 <- c("surv","shoot","leaf","flower","fruit")
SxE_out3 <- lapply(var.list2, SxE_function, df = GE_comb_raw_sel)
```

    ## [1] "surv"
    ## [1] "shoot"
    ## [1] "leaf"
    ## [1] "flower"
    ## [1] "fruit"

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

SxE_lin_aov3 <- rbind(SxE_out3[[1]][[3]], SxE_out3[[2]][[3]], SxE_out3[[3]][[3]], 
                     SxE_out3[[4]][[3]],SxE_out3[[5]][[3]])

SxE_quad_aov3 <- rbind(SxE_out3[[1]][[4]], SxE_out3[[2]][[4]], SxE_out3[[3]][[4]], 
                     SxE_out3[[4]][[4]],SxE_out3[[5]][[4]])

SxE_lin_comp <- cbind(SxE_lin_aov1, SxE_lin_aov2, SxE_lin_aov3) ## compare global models to within-environment models
SxE_quad_comp <- cbind(SxE_quad_aov1, SxE_quad_aov2, SxE_quad_aov3) ## compare global models to within-environment models

write.csv(SxE_lin_comp, "SxE_res_lin_comp.csv")
write.csv(SxE_quad_comp, "SxE_res_quad_comp.csv")
```

Selection coefficients/gradients within each environment
--------------------------------------------------------

``` r
# function to iterate over environments, and traits

sel_win_env_function <- function(df, trait){
   trait <- print(paste(trait))  
   lin_mod <- lm(get(trait) ~ stand_nod, data = df)
   lin_mod_sum <- summary(lin_mod)
   lin_mod_aov <- Anova(lin_mod, type = 2)
   quad_mod <- lm(get(trait) ~ stand_nod + I(stand_nod^2), data = df)
   quad_mod_sum <- summary(quad_mod)
   quad_mod_aov <- Anova(quad_mod, type = 2)
   mod_comp <- anova(lin_mod, quad_mod)
   
   return(list(trait, lin_mod_sum, quad_mod_sum, lin_mod_aov, quad_mod_aov, mod_comp))
}

# apply to traits with data in all envs

# assign vars to loop over
env.list1 <- unique(GE_comb_raw_sel$env)

sel_out1 <- lapply(env.list1, FUN = function(e){
  df.use <- filter(GE_comb_raw_sel, env==e)
  surv.out <- sel_win_env_function(df.use, var.list[1])
  shoot.out <- sel_win_env_function(df.use, var.list[2])
  leaf.out <- sel_win_env_function(df.use, var.list[3])
  env <- print(paste(e))  
  
  return(list(env, surv.out, shoot.out, leaf.out))
})
```

    ## [1] "rel_surv"
    ## [1] "rel_shoot"
    ## [1] "rel_leaf"
    ## [1] "GH"
    ## [1] "rel_surv"
    ## [1] "rel_shoot"
    ## [1] "rel_leaf"
    ## [1] "plot_1"
    ## [1] "rel_surv"
    ## [1] "rel_shoot"
    ## [1] "rel_leaf"
    ## [1] "plot_2"
    ## [1] "rel_surv"
    ## [1] "rel_shoot"
    ## [1] "rel_leaf"
    ## [1] "plot_3"
    ## [1] "rel_surv"
    ## [1] "rel_shoot"
    ## [1] "rel_leaf"
    ## [1] "plot_4"

``` r
# apply to traits with data in only some envs

# assign vars to loop over
env.list2 <- c("plot_1","plot_2","plot_3")

sel_out2 <- lapply(env.list2, FUN = function(e){
  df.use <- filter(GE_comb_raw_sel, env==e)
  flower.out <- sel_win_env_function(df.use, var.list[4])
  fruit.out <- sel_win_env_function(df.use, var.list[5])
  env <- print(paste(e))  
  
  return(list(env, flower.out, fruit.out))
})
```

    ## [1] "rel_flower"
    ## [1] "rel_fruit"
    ## [1] "plot_1"
    ## [1] "rel_flower"
    ## [1] "rel_fruit"
    ## [1] "plot_2"
    ## [1] "rel_flower"
    ## [1] "rel_fruit"
    ## [1] "plot_3"

``` r
# Combine dfs (ANOVA res)

ANOVA_sel_survival <- rbind(sel_out1[[1]][[2]][[4]],sel_out1[[1]][[2]][[5]], ## GH
                            sel_out1[[2]][[2]][[4]],sel_out1[[2]][[2]][[5]], ## plot_1
                            sel_out1[[3]][[2]][[4]],sel_out1[[3]][[2]][[5]], ## plot_2
                            sel_out1[[4]][[2]][[4]],sel_out1[[4]][[2]][[5]], ## plot_3
                            sel_out1[[5]][[2]][[4]],sel_out1[[5]][[2]][[5]]) ## plot_4

ANOVA_sel_shoot <- rbind(sel_out1[[1]][[3]][[4]],sel_out1[[1]][[3]][[5]], 
                         sel_out1[[2]][[3]][[4]],sel_out1[[2]][[3]][[5]],
                         sel_out1[[3]][[3]][[4]],sel_out1[[3]][[3]][[5]], 
                         sel_out1[[4]][[3]][[4]],sel_out1[[4]][[3]][[5]],
                         sel_out1[[5]][[3]][[4]],sel_out1[[5]][[3]][[5]])

ANOVA_sel_leaf <- rbind(sel_out1[[1]][[4]][[4]],sel_out1[[1]][[4]][[5]], 
                        sel_out1[[2]][[4]][[4]],sel_out1[[2]][[4]][[5]],
                        sel_out1[[3]][[4]][[4]],sel_out1[[3]][[4]][[5]], 
                        sel_out1[[4]][[4]][[4]],sel_out1[[4]][[4]][[5]],
                        sel_out1[[5]][[4]][[4]],sel_out1[[5]][[4]][[5]])

ANOVA_sel_flower <- rbind(sel_out2[[1]][[2]][[4]],sel_out2[[1]][[2]][[5]], 
                          sel_out2[[2]][[2]][[4]],sel_out2[[2]][[2]][[5]],
                          sel_out2[[3]][[2]][[4]],sel_out2[[3]][[2]][[5]])

ANOVA_sel_fruit <- rbind(sel_out2[[1]][[3]][[4]],sel_out2[[1]][[3]][[5]], 
                         sel_out2[[2]][[3]][[4]],sel_out2[[2]][[3]][[5]],
                         sel_out2[[3]][[3]][[4]],sel_out2[[3]][[3]][[5]])

ANOVA_sel_comb1 <- cbind(ANOVA_sel_survival, ANOVA_sel_shoot, ANOVA_sel_leaf)
ANOVA_sel_comb2 <- cbind(ANOVA_sel_flower, ANOVA_sel_fruit)
write.csv(ANOVA_sel_comb1, "ANOVA_sel1_win_env.csv")
write.csv(ANOVA_sel_comb2, "ANOVA_sel2_win_env.csv")

# combine dfs (regression res)
reg_sel_survival <- rbind(sel_out1[[1]][[2]][[2]]$coefficients, ## GH
                            sel_out1[[2]][[2]][[2]]$coefficients, ## plot_1
                            sel_out1[[3]][[2]][[2]]$coefficients, ## plot_2
                            sel_out1[[4]][[2]][[2]]$coefficients, ## plot_3
                            sel_out1[[5]][[2]][[2]]$coefficients) ## plot_4

reg_sel_shoot <- rbind(sel_out1[[1]][[3]][[2]]$coefficients, 
                         sel_out1[[2]][[3]][[2]]$coefficients,
                         sel_out1[[3]][[3]][[2]]$coefficients, 
                         sel_out1[[4]][[3]][[2]]$coefficients,
                         sel_out1[[5]][[3]][[2]]$coefficients)

reg_sel_leaf <- rbind(sel_out1[[1]][[4]][[2]]$coefficients, 
                        sel_out1[[2]][[4]][[2]]$coefficients,
                        sel_out1[[3]][[4]][[2]]$coefficients, 
                        sel_out1[[4]][[4]][[2]]$coefficients,
                        sel_out1[[5]][[4]][[2]]$coefficients)

reg_sel_flower <- rbind(sel_out2[[1]][[2]][[2]]$coefficients, 
                          sel_out2[[2]][[2]][[2]]$coefficients,
                          sel_out2[[3]][[2]][[2]]$coefficients)

reg_sel_fruit <- rbind(sel_out2[[1]][[3]][[2]]$coefficients, 
                         sel_out2[[2]][[3]][[2]]$coefficients,
                         sel_out2[[3]][[3]][[2]]$coefficients)

reg_sel_comb1 <- cbind(reg_sel_survival, reg_sel_shoot, reg_sel_leaf)
reg_sel_comb2 <- cbind(reg_sel_flower, reg_sel_fruit)
write.csv(reg_sel_comb1, "reg_sel1_win_env.csv")
write.csv(reg_sel_comb2, "reg_sel2_win_env.csv")
```
