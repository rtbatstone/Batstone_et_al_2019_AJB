data\_analyses\_heritability
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
library("lme4") ## linear mixed models
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

Models to calculate heritability
--------------------------------

``` r
# function (model structure depends on trait and env)
herit_fun_GH <- function(df, trait){
  
  # shoot
  if (trait == "shoot"){
    lm_herit <- lmer(log(shoot) ~ (1|line) + (1|position) + block,
             data = df)
    lm_herit_dline <- lmer(log(shoot) ~ (1|position) + block,
             data = df)
  }
  
    # leaf
    else if (trait == "leaf"){
    lm_herit <- lmer(sqrt(leaf) ~ (1|line) + (1|position) + block,
             data = df)
    lm_herit_dline <- lmer(sqrt(leaf) ~ (1|position) + block,
             data = df)
  }
  
    # nod
    else if (trait == "nod"){
    lm_herit <- lmer(sqrt(nod) ~ (1|line) + (1|position) + block,
             data = df)
    lm_herit_dline <- lmer(sqrt(nod) ~ (1|position) + block,
             data = df)
  }
  
    # choice
    else if (trait == "choice"){
    lm_herit <- lmer(choice ~ (1|line) + (1|position) + block,
             data = df)
    lm_herit_dline <- lmer(choice ~ (1|position) + block,
             data = df)
  }

   # red_nod
    else if (trait == "totalred"){
    lm_herit <- lmer(sqrt(totalred) ~ (1|line) + (1|position) + block,
             data = df)
    lm_herit_dline <- lmer(sqrt(totalred) ~ (1|position) + block,
             data = df)
  }
  
  trait.out <- paste(trait)
  out1 <- ranef(lm_herit)
  out2 <- VarCorr(lm_herit)
  out3 <- anova(lm_herit, lm_herit_dline)
  
  return(list(trait.out,out1,out2,out3))
  
}

herit_fun_F <- function(df, trait){
  
    # shoot
  if (trait == "shoot"){
    lm_herit <- lmer(log(shoot) ~ (1|line) + (1|diss) + block,
             data = df)
    lm_herit_dline <- lmer(log(shoot) ~ (1|diss) + block,
             data = df)
  }
  
    # leaf
    else if (trait == "leaf"){
    lm_herit <- lmer(sqrt(leaf) ~ (1|line) + (1|diss) + block,
             data = df)
    lm_herit_dline <- lmer(sqrt(leaf) ~ (1|diss) + block,
             data = df)
  }
  
    # nod
    else if (trait == "nod"){
    lm_herit <- lmer(sqrt(nod) ~ (1|line) + (1|diss) + block,
             data = df)
    lm_herit_dline <- lmer(sqrt(nod) ~ (1|diss) + block,
             data = df)
  }
  
    # fruit
    else if (trait == "fru"){
    lm_herit <- lmer(sqrt(fru) ~ (1|line) + (1|diss) + block,
             data = df)
    lm_herit_dline <- lmer(sqrt(fru) ~ (1|diss) + block,
             data = df)
    }
  
  trait.out <- paste(trait)
  out1 <- ranef(lm_herit)
  out2 <- VarCorr(lm_herit)
  out3 <- anova(lm_herit, lm_herit_dline)
  
  return(list(trait.out, out1,out2,out3))
  
}

# specify environments to loop over

env.list_GH <- c("GH") ## shoot, leaf, nod, choice, totalred

env.list_F1 <- c("plot_1","plot_2","plot_3","plot_4") ## shoot, leaf, nod
  
env.list_F2 <- c("plot_1","plot_2","plot_3") ## fruits

# Subset by environment and calculate variable 1 and variable 2
herit_out_GH <- lapply(env.list_GH, FUN = function(e){
  
  df.use <- filter(F_GH_ds,env==e)
  
  env.out <- paste(e)
  shoot.out <- herit_fun_GH(df.use,"shoot")
  leaf.out <- herit_fun_GH(df.use,"leaf")
  nod.out <- herit_fun_GH(df.use,"nod")
  choice.out <- herit_fun_GH(df.use,"choice")
  totalred.out <- herit_fun_GH(df.use,"totalred")
  
  return(list(env.out,shoot.out,leaf.out, nod.out, choice.out,totalred.out))
  
})
```

    ## refitting model(s) with ML (instead of REML)

    ## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl =
    ## control$checkConv, : Model failed to converge with max|grad| = 0.00251194
    ## (tol = 0.002, component 1)

    ## refitting model(s) with ML (instead of REML)
    ## refitting model(s) with ML (instead of REML)

    ## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl =
    ## control$checkConv, : Model failed to converge with max|grad| = 0.00261928
    ## (tol = 0.002, component 1)

    ## refitting model(s) with ML (instead of REML)
    ## refitting model(s) with ML (instead of REML)

``` r
# Subset by environment and calculate variable 1 and variable 2
herit_out_F1 <- lapply(env.list_F1, FUN = function(e){
  
  df.use <- filter(F_GH_ds,env==e)
  
  env.out <- paste(e)
  shoot.out <- herit_fun_F(df.use,"shoot")
  leaf.out <- herit_fun_F(df.use,"leaf")
  nod.out <- herit_fun_F(df.use,"nod")
  
  return(list(env.out,shoot.out,leaf.out, nod.out))
  
})
```

    ## refitting model(s) with ML (instead of REML)
    ## refitting model(s) with ML (instead of REML)
    ## refitting model(s) with ML (instead of REML)
    ## refitting model(s) with ML (instead of REML)
    ## refitting model(s) with ML (instead of REML)
    ## refitting model(s) with ML (instead of REML)
    ## refitting model(s) with ML (instead of REML)
    ## refitting model(s) with ML (instead of REML)

    ## boundary (singular) fit: see ?isSingular

    ## refitting model(s) with ML (instead of REML)
    ## refitting model(s) with ML (instead of REML)
    ## refitting model(s) with ML (instead of REML)

    ## boundary (singular) fit: see ?isSingular
    ## boundary (singular) fit: see ?isSingular

    ## refitting model(s) with ML (instead of REML)

``` r
# Subset by environment and calculate variable 1 and variable 2
herit_out_F2 <- lapply(env.list_F2, FUN = function(e){
  
  df.use <- filter(F_GH_ds,env==e)
  
  env.out <- paste(e)
  fru.out <- herit_fun_F(df.use,"fru")
  
  return(list(env.out,fru.out))
  
})
```

    ## refitting model(s) with ML (instead of REML)
    ## refitting model(s) with ML (instead of REML)
    ## refitting model(s) with ML (instead of REML)

``` r
# Combine dfs

# VarCorr of model terms (rand effs)
herit_vc_comb <-  list(herit_out_GH[[1]][[2]][[3]], herit_out_GH[[1]][[3]][[3]], herit_out_GH[[1]][[4]][[3]],
                       herit_out_GH[[1]][[5]][[3]], herit_out_GH[[1]][[6]][[3]], ## GH only
                       herit_out_F1[[1]][[2]][[3]], herit_out_F1[[1]][[3]][[3]], herit_out_F1[[1]][[4]][[3]], ## p1
                       herit_out_F1[[2]][[2]][[3]], herit_out_F1[[2]][[3]][[3]], herit_out_F1[[2]][[4]][[3]], ## p2
                       herit_out_F1[[3]][[2]][[3]], herit_out_F1[[3]][[3]][[3]], herit_out_F1[[3]][[4]][[3]], ## p3
                       herit_out_F1[[4]][[2]][[3]], herit_out_F1[[4]][[3]][[3]], herit_out_F1[[4]][[4]][[3]], ## p4
                       herit_out_F2[[1]][[2]][[3]], herit_out_F2[[2]][[2]][[3]], herit_out_F2[[3]][[2]][[3]]) ## fruit

herit_vc_comb_df <- lapply(herit_vc_comb, data.frame) ## adds vcov
herit_vc_comb_rb <- do.call(rbind.data.frame, herit_vc_comb_df)
herit_vc_comb_rb$env <- c(rep("GH", 5*3), rep("plot_1", 3*3), rep("plot_2", 3*3), rep("plot_3", 3*3), rep("plot_4", 3*3),
                          rep("plot_1",3), rep("plot_2", 3), rep("plot_3",3))
herit_vc_comb_rb$trait <- c(rep("shoot",3), rep("leaf",3), rep("nod",3), rep("choice",3), rep("red_nod",3),
                            rep(c(rep("shoot",3),rep("leaf",3),rep("nod",3)), 4),rep("fruit", 3*3))
write.csv(herit_vc_comb_rb, "heritability_win_env.csv", row.names = FALSE)

# Chisq test for line term
herit_cs_comb <- list(herit_out_GH[[1]][[2]][[4]], herit_out_GH[[1]][[3]][[4]], herit_out_GH[[1]][[4]][[4]],
                       herit_out_GH[[1]][[5]][[4]], herit_out_GH[[1]][[6]][[4]], ## GH only
                       herit_out_F1[[1]][[2]][[4]], herit_out_F1[[1]][[3]][[4]], herit_out_F1[[1]][[4]][[4]], ## p1
                       herit_out_F1[[2]][[2]][[4]], herit_out_F1[[2]][[3]][[4]], herit_out_F1[[2]][[4]][[4]], ## p2
                       herit_out_F1[[3]][[2]][[4]], herit_out_F1[[3]][[3]][[4]], herit_out_F1[[3]][[4]][[4]], ## p3
                       herit_out_F1[[4]][[2]][[4]], herit_out_F1[[4]][[3]][[4]], herit_out_F1[[4]][[4]][[4]], ## p4
                       herit_out_F2[[1]][[2]][[4]], herit_out_F2[[2]][[2]][[4]], herit_out_F2[[3]][[2]][[4]]) ## fruit


herit_cs_comb_df <- lapply(herit_cs_comb, data.frame) ## adds vcov
herit_cs_comb_rb <- do.call(rbind.data.frame, herit_cs_comb_df)
herit_cs_comb_rb$env <- c(rep("GH", 5*2), rep("plot_1", 3*2), rep("plot_2", 3*2), rep("plot_3", 3*2), rep("plot_4", 3*2),
                          rep("plot_1",2), rep("plot_2", 2), rep("plot_3",2))
herit_cs_comb_rb$trait <- c(rep("shoot",2), rep("leaf",2), rep("nod",2), rep("choice",2), rep("red_nod",2),
                            rep(c(rep("shoot",2),rep("leaf",2),rep("nod",2)), 4),rep("fruit", 3*2))
write.csv(herit_cs_comb_rb, "Chisq_win_env.csv", row.names = FALSE)
```
