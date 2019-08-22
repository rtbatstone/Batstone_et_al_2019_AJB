data\_analyses\_raw\_means
================
Rebecca Batstone
2019-08-22

Load packages
-------------

``` r
# packages
library("tidyverse") #includes ggplot2, dplyr, readr, stringr
library("cowplot") # paneled graphs
library("reshape2") # dcast function
library("corrplot") # to make correlation plots
library("RColorBrewer") # to specify color brewers
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

Calculate raw means
-------------------

``` r
# shoot
sum_shoot_E.raw <- shoot_cc %>%
  group_by(env) %>%
  summarize(mean_shoot = mean(shoot), SE_shoot = (sd(shoot))/(sqrt(length(shoot)))) %>%
  as.data.frame(.)

sum_shoot_G.raw <- shoot_cc %>%
  group_by(line) %>%
  summarize(mean_shoot = mean(shoot), SE_shoot = (sd(shoot))/(sqrt(length(shoot)))) %>%
  as.data.frame(.)

sum_shoot_GE.raw <- shoot_cc %>%
  group_by(env, line) %>%
  summarize(mean_shoot = mean(shoot), SE_shoot = (sd(shoot))/(sqrt(length(shoot)))) %>%
  as.data.frame(.)

# survival
sum_surv_E.raw <- survival_cc %>%
  group_by(env) %>%
  summarize(mean_surv = mean(survival), SE_surv = (sd(survival))/(sqrt(length(survival)))) %>%
  as.data.frame(.)

sum_surv_G.raw <- survival_cc %>%
  group_by(line) %>%
  summarize(mean_surv = mean(survival), SE_surv = (sd(survival))/(sqrt(length(survival)))) %>%
  as.data.frame(.)

sum_surv_GE.raw <- survival_cc %>%
  group_by(env, line) %>%
  summarize(mean_surv = mean(survival), SE_surv = (sd(survival))/(sqrt(length(survival)))) %>%
  as.data.frame(.)

# leaves
sum_leaf_E.raw <- leaf_cc %>%
  group_by(env) %>%
  summarize(mean_leaf = mean(leaf), SE_leaf = (sd(leaf))/(sqrt(length(leaf)))) %>%
  as.data.frame(.)

sum_leaf_G.raw <- leaf_cc %>%
  group_by(line) %>%
  summarize(mean_leaf = mean(leaf), SE_leaf = (sd(leaf))/(sqrt(length(leaf)))) %>%
  as.data.frame(.)

sum_leaf_GE.raw <- leaf_cc %>%
  group_by(env, line) %>%
  summarize(mean_leaf = mean(leaf), SE_leaf = (sd(leaf))/(sqrt(length(leaf)))) %>%
  as.data.frame(.)

# nodules
sum_nod_E.raw <- nod_cc %>%
  group_by(env) %>%
  summarize(mean_nod = mean(nod), SE_nod = (sd(nod))/(sqrt(length(nod)))) %>%
  as.data.frame(.)

sum_nod_G.raw <- nod_cc %>%
  group_by(line) %>%
  summarize(mean_nod = mean(nod), SE_nod = (sd(nod))/(sqrt(length(nod)))) %>%
  as.data.frame(.)

sum_nod_GE.raw <- nod_cc %>%
  group_by(env, line) %>%
  summarize(mean_nod = mean(nod), SE_nod = (sd(nod))/(sqrt(length(nod)))) %>%
  as.data.frame(.)

# choice
sum_choice_G.raw <- choice_cc %>%
  group_by(line) %>%
  summarize(mean_choice = mean(choice), SE_choice = (sd(choice))/(sqrt(length(choice)))) %>%
  as.data.frame(.)

# red nodules
sum_totalred_G.raw <- red_nod_cc %>%
  group_by(line) %>%
  summarize(mean_totalred = mean(totalred), SE_totalred = (sd(totalred))/(sqrt(length(totalred)))) %>%
  as.data.frame(.)

# flowers
sum_flo_E.raw <- flower_cc %>%
  group_by(env) %>%
  summarize(mean_flo = mean(flo), SE_flo = (sd(flo))/(sqrt(length(flo)))) %>%
  as.data.frame(.)

sum_flo_G.raw <- flower_cc %>%
  group_by(line) %>%
  summarize(mean_flo = mean(flo), SE_flo = (sd(flo))/(sqrt(length(flo)))) %>%
  as.data.frame(.)

sum_flo_GE.raw <- flower_cc %>%
  group_by(env, line) %>%
  summarize(mean_flo = mean(flo), SE_flo = (sd(flo))/(sqrt(length(flo)))) %>%
  as.data.frame(.)

# fruit success
sum_fru_E.raw <- fruit_cc %>%
  group_by(env) %>%
  summarize(mean_fru = mean(fru), SE_fru = (sd(fru))/(sqrt(length(fru)))) %>%
  as.data.frame(.)

sum_fru_G.raw <- fruit_cc %>%
  group_by(line) %>%
  summarize(mean_fru = mean(fru), SE_fru = (sd(fru))/(sqrt(length(fru)))) %>%
  as.data.frame(.)

sum_fru_GE.raw <- fruit_cc %>%
  group_by(env, line) %>%
  summarize(mean_fru = mean(fru), SE_fru = (sd(fru))/(sqrt(length(fru)))) %>%
  as.data.frame(.)
```

Combine raw means
-----------------

``` r
# main effect of line across environments
G_comb_raw_SE1 <- merge(y=sum_shoot_G.raw, x=sum_surv_G.raw, by = "line", all = TRUE)
G_comb_raw_SE2 <- merge(y=G_comb_raw_SE1, x=sum_leaf_G.raw, by = "line", all = TRUE)
G_comb_raw_SE3 <- merge(y=G_comb_raw_SE2, x=sum_nod_G.raw, by = "line", all = TRUE)
G_comb_raw_SE4 <- merge(y=G_comb_raw_SE3, x=sum_choice_G.raw, by = "line", all = TRUE)
G_comb_raw_SE5 <- merge(y=G_comb_raw_SE4, x=sum_totalred_G.raw, by = "line", all = TRUE)
G_comb_raw_SE6 <- merge(y=G_comb_raw_SE5, x=sum_flo_G.raw, by = "line", all = TRUE)
G_comb_raw_SE <- merge(y=G_comb_raw_SE6, x=sum_fru_G.raw, by = "line", all = TRUE)

# drop all SE. columns
G_comb_raw <- G_comb_raw_SE[, -grep(c("SE_"), colnames(G_comb_raw_SE))]
names(G_comb_raw) <- c("line","fruits","flowers","red_nodules","choice",
                       "total_nodules","leaves","survival","shoot_biomass")

## effect of line within environments
sum_shoot_GE.raw$line_env <- do.call(paste, c(sum_shoot_GE.raw[c("line","env")], sep = "-"))
sum_surv_GE.raw$line_env <- do.call(paste, c(sum_surv_GE.raw[c("line","env")], sep = "-"))
sum_leaf_GE.raw$line_env <- do.call(paste, c(sum_leaf_GE.raw[c("line","env")], sep = "-"))
sum_nod_GE.raw$line_env <- do.call(paste, c(sum_nod_GE.raw[c("line","env")], sep = "-"))
sum_flo_GE.raw$line_env <- do.call(paste, c(sum_flo_GE.raw[c("line","env")], sep = "-"))
sum_fru_GE.raw$line_env <- do.call(paste, c(sum_fru_GE.raw[c("line","env")], sep = "-"))
sum_choice_G.raw$env <- "GH"
sum_choice_G.raw$line_env <- do.call(paste, c(sum_choice_G.raw[c("line","env")], sep = "-"))
sum_totalred_G.raw$env <- "GH"
sum_totalred_G.raw$line_env <- do.call(paste, c(sum_totalred_G.raw[c("line","env")], sep = "-"))

GE_comb_raw_SE1 <- merge(y=sum_shoot_GE.raw, x=sum_surv_GE.raw, by = "line_env", all = TRUE)
GE_comb_raw_SE2 <- merge(y=GE_comb_raw_SE1, x=sum_leaf_GE.raw, by = "line_env", all = TRUE)
GE_comb_raw_SE3 <- merge(y=GE_comb_raw_SE2, x=sum_nod_GE.raw, by = "line_env", all = TRUE)
GE_comb_raw_SE4 <- merge(y=GE_comb_raw_SE3, x=sum_flo_GE.raw, by = "line_env", all = TRUE)
GE_comb_raw_SE5 <- merge(y=GE_comb_raw_SE4, x=sum_fru_GE.raw, by = "line_env", all = TRUE)
GE_comb_raw_SE6 <- merge(y=GE_comb_raw_SE5, x=sum_choice_G.raw, by = "line_env", all = TRUE)
GE_comb_raw_SE <- merge(y=GE_comb_raw_SE6, x=sum_totalred_G.raw, by = "line_env", all = TRUE)

# drop all SE. columns
GE_comb_raw1 <- GE_comb_raw_SE[, -grep(c("SE_"), colnames(GE_comb_raw_SE))]
GE_comb_raw2 <- GE_comb_raw1[, -grep(c("x"), colnames(GE_comb_raw1))]
GE_comb_raw3 <- GE_comb_raw2[, -grep(c("y"), colnames(GE_comb_raw2))]

GE_comb_raw <- GE_comb_raw3 %>% 
  separate(line_env, c('line','env'), sep = '-')

names(GE_comb_raw) <- c("line","env","totalred","choice","fruit","flower",
                       "nod","leaf","surv","shoot")

# dcast for env (long to wide format)
# reshape from long to wide (for each env)
GE_comb_raw.w <- reshape(GE_comb_raw, idvar = "line", timevar = "env", direction = "wide")
```

Save raw means
--------------

``` r
# mean across environments
save(G_comb_raw, file = "./G_combined_raw.Rdata")
## mean within each environment:
save(GE_comb_raw, file = "./GxE_combined_raw.Rdata")
```

Reaction norm plots (same traits across env)
--------------------------------------------

``` r
plot_colours <- c('green4','firebrick3', 'darkblue','darkorchid', 'darkgoldenrod')
names(plot_colours) <- levels(F_GH_ds$env)
colScale3 <- scale_fill_manual(name = "Environment", 
                               values = plot_colours, 
                                breaks = c("GH", "plot_1", "plot_2","plot_3","plot_4"), 
                                labels = c("Green- \n house", "Plot 1", "Plot 2","Plot 3","Plot 4"))

# for x-axis labels and colors
env_names <- list(
  'GH'="Green- \n house",
  'plot_1'="Plot 1",
  'plot_2'="Plot 2",
  'plot_3'="Plot 3",
  'plot_4'="Plot 4"
)

env_labeller <- function(variable,value){
  return(env_names[value])
}

# rxn norm plots (raw means)

# shoot
(plot.sum_shoot <- ggplot(GE_comb_raw, aes(x=env, y=shoot, group=line)) + 
  geom_point(size=3, position = position_dodge(0.3)) + 
  geom_line(position = position_dodge(0.3)) +
  annotate("text", x = 1, y = 1500, label ="A", fontface="bold") +
  annotate("text", x = 2, y = 1500, label = "B", fontface="bold") +
  annotate("text", x = 3, y = 1500, label = "C", fontface="bold") +
  annotate("text", x = 4, y = 1500, label = "B", fontface="bold") +
  annotate("text", x = 5, y = 1500, label = "A", fontface="bold") +  
  annotate("text", x = 1.4, y = 1000, label =  'bold("Environment***")', parse = TRUE) + 
  annotate("text", x = 1.4, y = 950, label =  'bold("Line***")', parse = TRUE) +  
  annotate("text", x = 1.4, y = 900, label =  'bold("Environment x Line***")', parse = TRUE) +   
  theme_bw() +
  xlab(NULL) + 
  ylab("Shoot biomass (mg)") +
  theme(axis.title.y = element_text(colour = "black", size = 18), 
        axis.text.y = element_text(size=16), 
        axis.title.x = element_text(size=18), 
        axis.text.x = element_blank(),
        legend.position="none",
        legend.title = element_text(colour="black", size=16, face="bold"),
        legend.text = element_text(colour="black", size=12),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()))
```

![](data_analyses_raw_means_files/figure-markdown_github/rxn_norms-1.png)

``` r
# surv
(plot.sum_surv <- ggplot(GE_comb_raw, aes(x=env, y=surv, group=line)) + 
  geom_point(size=3, position = position_dodge(0.3)) + 
  geom_line(position = position_dodge(0.3)) +
  annotate("text", x = 1, y = 1.05, label ="A", fontface="bold") +
  annotate("text", x = 2, y = 1.05, label = "A", fontface="bold") +
  annotate("text", x = 3, y = 1.05, label = "A", fontface="bold") +
  annotate("text", x = 4, y = 1.05, label = "A", fontface="bold") +
  annotate("text", x = 5, y = 1.05, label = "A", fontface="bold") +  
  annotate("text", x = 1.4, y = 0.18, label =  'bold("Environment***")', parse = TRUE) + 
  annotate("text", x = 1.4, y = 0.14, label =  'bold("Line*")', parse = TRUE) +  
  annotate("text", x = 1.4, y = 0.1, label =  'bold("Environment x Line***")', parse = TRUE) +   
  theme_bw() +
  xlab(NULL) + 
  ylab("Survival (%)") +
  theme(axis.title.y = element_text(colour = "black", size = 18), 
        axis.text.y = element_text(size=16), 
        axis.title.x = element_text(size=18), 
        axis.text.x = element_blank(),
        legend.position="none",
        legend.title = element_text(colour="black", size=16, face="bold"),
        legend.text = element_text(colour="black", size=12),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()))
```

![](data_analyses_raw_means_files/figure-markdown_github/rxn_norms-2.png)

``` r
# leaf
(plot.sum_leaf <- ggplot(GE_comb_raw, aes(x=env, y=leaf, 
                                      group=line)) + 
  geom_point(size=3, position = position_dodge(0.3)) + 
  geom_line(position = position_dodge(0.3)) +
  annotate("text", x = 1, y = 210, label = "A", fontface="bold") +
  annotate("text", x = 2, y = 210, label = "B", fontface="bold") +
  annotate("text", x = 3, y = 210, label = "C", fontface="bold") +
  annotate("text", x = 4, y = 210, label = "B", fontface="bold") +
  annotate("text", x = 5, y = 210, label = "A,B,C", fontface="bold") +   
  annotate("text", x = 4.7, y = 170, label =  'bold("Environment***")', parse = TRUE) + 
  annotate("text", x = 4.7, y = 160, label =  'bold("Line***")', parse = TRUE) +  
  annotate("text", x = 4.7, y = 150, label =  'bold("Environment x Line***")', parse = TRUE) +     
  theme_bw() +
  xlab(NULL) + 
  ylab("Leaves (no.)") +
  scale_x_discrete(labels=env_labeller)  +
  theme(axis.title.y = element_text(colour = "black", size = 18), 
        axis.text.y = element_text(size=16), 
        axis.title.x = element_text(size=18), 
        axis.text.x = element_text(size=16, colour = c('green4','darkred', 
                                                       'darkblue','darkmagenta', 
                                                       'darkgoldenrod')), 
        legend.position="none",
        legend.title = element_text(colour="black", size=16, face="bold"),
        legend.text = element_text(colour="black", size=12),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()))
```

![](data_analyses_raw_means_files/figure-markdown_github/rxn_norms-3.png)

``` r
# nodules
(plot.sum_nod <- ggplot(GE_comb_raw, aes(x=env, y=nod, 
                                      group=line)) + 
  geom_point(size=3, position = position_dodge(0.3)) + 
  geom_line(position = position_dodge(0.3)) +
  annotate("text", x = 1, y = 330, label = "C", fontface="bold") +
  annotate("text", x = 2, y = 330, label = "B", fontface="bold") +
  annotate("text", x = 3, y = 330, label = "B", fontface="bold") +
  annotate("text", x = 4, y = 330, label = "A", fontface="bold") +
  annotate("text", x = 5, y = 330, label = "A", fontface="bold") +   
  annotate("text", x = 4.6, y = 250, label =  'bold("Environment***")', parse = TRUE) + 
  annotate("text", x = 4.6, y = 238, label =  'bold("Line***")', parse = TRUE) +  
  annotate("text", x = 4.6, y = 226, label =  'bold("Environment x Line**")', parse=TRUE) +  
  theme_bw() +
  xlab(NULL) + 
  ylab("Nodules (no.)") +
  scale_x_discrete(labels=env_labeller)  +
  theme(axis.title.y = element_text(size = 18), 
        axis.text.y = element_text(size=16), 
        axis.title.x = element_text(size=18), 
        axis.text.x = element_text(size=16, colour = c('green4','darkred', 
                                                       'darkblue','darkmagenta', 
                                                       'darkgoldenrod')), 
        legend.position="none",
        legend.title = element_text(colour="black", size=16, face="bold"),
        legend.text = element_text(colour="black", size=12),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()))
```

![](data_analyses_raw_means_files/figure-markdown_github/rxn_norms-4.png)

``` r
# rxn norm plot for field and GH-only data

# label for x-axis
plot_names3 <- list(
  '1'="Plot 1",
  '2'="Plot 2",
  '3'="Plot 3",
  '4'="Plot 4"
)

plot_label3 <- function(variable,value){
  return(plot_names3[value])
}

# fruit
(plot.sum_fru <- ggplot(subset(GE_comb_raw, ! env %in% c("GH","plot_4")), aes(x=env, y=fruit, 
                                      group=line)) + 
  geom_point(size=3, position = position_dodge(0.3)) + 
  geom_line(position = position_dodge(0.3)) +
  theme_bw() +
  annotate("text", x = 1, y = 30, label = "A", fontface="bold") +
  annotate("text", x = 2, y = 30, label = "B", fontface="bold") +
  annotate("text", x = 3, y = 30, label = "A", fontface="bold") +
  annotate("text", x = 3, y = 26, label =  'bold("Environment***")', parse = TRUE) + 
  annotate("text", x = 3, y = 24.5, label =  'bold("Line***")', parse = TRUE) +  
  annotate("text", x = 3, y = 23, label =  'bold("Environment x Line***")', parse = TRUE) +   
  xlab(NULL) + 
  ylab("Fruits (no.)") +
  scale_x_discrete(labels=plot_label3)  +
  theme(axis.title.y = element_text(colour = "black", size = 18), 
        axis.text.y = element_text(size=16), 
        axis.title.x = element_text(size=18), 
        axis.text.x = element_text(size=16, colour = c('darkred', 'darkblue','darkmagenta', 'darkgoldenrod')),  
        legend.position="none",
        legend.title = element_text(colour="black", size=16, face="bold"),
        legend.text = element_text(colour="black", size=12),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()))
```

![](data_analyses_raw_means_files/figure-markdown_github/rxn_norms-5.png)

``` r
# flowers
(plot.sum_flo <- ggplot(subset(GE_comb_raw, ! env %in% c("GH","plot_4")), aes(x=env, y=flower, 
                                      group=line)) + 
  geom_point(size=3, position = position_dodge(0.3)) + 
  geom_line(position = position_dodge(0.3)) +
  annotate("text", x = 1, y = 5, label = "A", fontface="bold") +
  annotate("text", x = 2, y = 5, label = "B", fontface="bold") +
  annotate("text", x = 3, y = 5, label = "A", fontface="bold") +
  annotate("text", x = 3, y = 4.0, label =  'bold("Environment***")', parse = TRUE) + 
  annotate("text", x = 3, y = 3.7, label =  'bold("Line***")', parse = TRUE) +  
  annotate("text", x = 3, y = 3.5, label =  expression("Environment x Line"^'NS')) +   
  theme_bw() +
  xlab(NULL) + 
  ylab("Flowers (no.)") +
  scale_x_discrete(labels=plot_label3)  +
  theme(axis.title.y = element_text(colour = "black", size = 18), 
        axis.text.y = element_text(size=16), 
        axis.title.x = element_text(size=18), 
        axis.text.x = element_text(size=16, colour = c('darkred', 'darkblue','darkmagenta', 'darkgoldenrod')),  
        legend.position="none",
        legend.title = element_text(colour="black", size=16, face="bold"),
        legend.text = element_text(colour="black", size=12),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()))
```

![](data_analyses_raw_means_files/figure-markdown_github/rxn_norms-6.png)

``` r
# cowplots

# put all four plots into one
fig_base1 <- plot_grid(plot.sum_surv, plot.sum_shoot, plot.sum_leaf, plot.sum_nod, plot.sum_flo, plot.sum_fru,
          ncol = 2,
          nrow = 3,
          align = "hv",
          labels = c("AUTO"))

fig1 <- add_sub(fig_base1, "Environment", size = 20, hjust = 0.5)

save_plot("rxn_norms.pdf", fig1,
          ncol = 2, # we're saving a grid plot of 2 columns
          nrow = 3, # and 3 rows
          # each individual subplot should have an aspect ratio of 1.3
          base_aspect_ratio = 1.3
          )
```

Corrplot of raw trait values over environments
----------------------------------------------

``` r
# set line as row-names
G_comb_raw.r <- G_comb_raw[,-1]
rownames(G_comb_raw.r) <- G_comb_raw[,1]

# set label colours
traitcolors1 <- rep(NA,8)
names(traitcolors1) <- names(G_comb_raw.r)
traitcolors1[c(3:4)]   <- 'green4' # greenhouse-only
traitcolors1[c(1:2)]   <- 'darkblue'   # field-only
traitcolors1[c(5:8)]   <- 'gold3'   # combined field and greenhouse

# compute correlation matrix
G_comb_raw.r_mat <- cor(G_comb_raw.r, use="pairwise.complete.obs")

# compute the matrix of the p-value
source('corr_test.R')
library(psych)
```

    ## Warning: package 'psych' was built under R version 3.5.3

    ## 
    ## Attaching package: 'psych'

    ## The following objects are masked from 'package:ggplot2':
    ## 
    ##     %+%, alpha

``` r
library(corrplot)

p.mat1 <- cor.mtest(G_comb_raw.r)
p.mat1.c <- psych::corr.test(G_comb_raw.r, adjust="bonferroni")$p

# get order of columns
# ord1 <- corrMatOrder(G_comb_raw.r_mat, order="hclust")
ord1 <- c(3,4,1,2,5,6,7,8)

# order colours and variables by ord
traitcolors1.ro <- traitcolors1[ord1]

G_comb_raw.r_mat.o <- G_comb_raw.r_mat[ord1 , ord1]
p.mat1.o <- p.mat1[ord1 , ord1]
p.mat1.c.o <- p.mat1.c[ord1 , ord1]

## corrplot figure

sig_level <- 0.05/28

# upper
bg_colors_upper1 <- p.mat1.c.o < sig_level
bg_colors_upper1[bg_colors_upper1 == T] <- "grey"
bg_colors_upper1[bg_colors_upper1 == F] <- "white"
diag(bg_colors_upper1) <- "white" # if the diagonal values shall be white
# select colors from lower/upper1 part of matrix:
bg_colors_upper1 <- bg_colors_upper1[upper.tri(bg_colors_upper1, diag=T)]

### and the plot

pdf('corrplot_over_env.pdf', width = 10, height = 10, pointsize = 14)

par(xpd = TRUE)

corrplot(G_comb_raw.r_mat.o, method = "ellipse", type="upper",
         #order="hclust", 
         addCoef.col = "black",
         tl.pos = "tl",
         tl.col = traitcolors1.ro,
         tl.srt=45,
         col = rev(brewer.pal(n = 8, name = "RdYlBu")),
         # p.mat = p.mat1.o, sig.level = 0.1, insig = "p-value",
         number.digits = 3,
         bg = bg_colors_upper1,
         diag = TRUE,
         mar=c(0,0,2,3))
         
corrplot(G_comb_raw.r_mat.o, method = "ellipse", type="lower",
         #order="hclust", 
         # number.cex = 1,
         col = rev(brewer.pal(n = 8, name = "RdYlBu")),
         add=T, 
         tl.pos="n", 
         cl.pos="n",
         #bg = bg_colors_upper1,
         #addCoef.col = "black",
         p.mat = p.mat1.c.o, sig.level = -1, insig = "p-value",
         number.digits = 3,
         #tl.col = newcolours,
         #bg = bg_colors_lower,
         diag = FALSE)

dev.off()
```

    ## png 
    ##   2

``` r
## result: shoot/leaf and red_nod/total_nod are correlated no matter the environment
```

Corrplot btw traits calculated within environments
--------------------------------------------------

``` r
############ corrplot overview

## make line rownames
GE_comb_raw.r <- GE_comb_raw.w[ , -1]
rownames(GE_comb_raw.r) <- GE_comb_raw.w[ , 1]

# select columns of interest
GE_comb_raw.rs <- GE_comb_raw.r[,c(1:2,5:8,11:16,19:24,27:32,37:40)]
# reorder cols
GE_comb_raw.rs <- GE_comb_raw.rs[,c(6,5,4,3,2,1,
                                    12,11,10,9,8,7,
                                    18,17,16,15,14,13,
                                    24,23,22,21,20,19,
                                    28,27,26,25)]

# rename cols
names(GE_comb_raw.rs) <- c("GH_shoot","GH_survival","GH_leaf","GH_nod","GH_choice","GH_red_nod",
                          "p1_shoot","p1_survival","p1_leaf","p1_nod","p1_flower","p1_fruit",
                          "p2_shoot","p2_survival","p2_leaf","p2_nod","p2_flower","p2_fruit",
                          "p3_shoot","p3_survival","p3_leaf","p3_nod","p3_flower","p3_fruit",
                          "p4_shoot","p4_survival","p4_leaf","p4_nod")

# set label colours
traitcolors2 <- rep(NA,28)
names(traitcolors2) <- names(GE_comb_raw.rs)
traitcolors2[c(1:6)]   <- 'green4' # greenhouse
traitcolors2[c(7:12)]   <- 'darkred'   # field plot 1
traitcolors2[c(13:18)]   <- 'darkblue'   # field plot 2
traitcolors2[c(19:24)]   <- 'darkmagenta'   # field plot 3
traitcolors2[c(25:28)]   <- 'darkgoldenrod'   # field plot 4

# compute correlation matrix
GE_comb_raw.r_mat <- cor(GE_comb_raw.rs, use="pairwise.complete.obs")

# compute the matrix of the p-value
source('corr_test.R')

p.mat2 <- cor.mtest(GE_comb_raw.rs)
p.mat2.c <- psych::corr.test(GE_comb_raw.rs, adjust="bonferroni")$p

# get order of columns
ord2 <- corrMatOrder(GE_comb_raw.r_mat, order="alphabet")

# ord2 colours and variables by ord
traitcolors2.ro <- traitcolors2[ord2]

GE_comb_raw.r_mat.o <- GE_comb_raw.r_mat[ord2 , ord2]
p.mat2.o <- p.mat2[ord2 , ord2]
p.mat2.c.o <- p.mat2.c[ord2 , ord2]

## corrplot figure

sig_level <- 0.05

# upper
bg_colors_upper2 <- p.mat2.c.o < sig_level
bg_colors_upper2[bg_colors_upper2 == T] <- "grey"
bg_colors_upper2[bg_colors_upper2 == F] <- "white"
diag(bg_colors_upper2) <- "white" ## if the diagonal values shall be white
# select colors from lower/upper2 part of matrix:
bg_colors_upper2 <- bg_colors_upper2[upper.tri(bg_colors_upper2, diag=T)]

pdf('corrplot_p-adjust_all.pdf', width = 16, height = 16, pointsize = 15)

cex.before <- par("cex")
par(cex = 0.7)
corrplot(GE_comb_raw.r_mat.o, method = "ellipse", type="upper",
         #ord2="hclust", 
         addCoef.col = "black",
         tl.pos = "td",
         tl.col = traitcolors2.ro,
         tl.srt=45,
         tl.cex = 1/par("cex"),
         cl.cex = 1/par("cex"),
         col = rev(brewer.pal(n = 8, name = "RdYlBu")),
         # p.mat2 = p.mat23.o, sig.level = 0.05, insig = "p-value",
         number.digits = 3,
         bg = bg_colors_upper2,
         diag = TRUE)
par(cex = cex.before)

dev.off()
```

    ## png 
    ##   2

Resulting corrplots
-------------------

<img src="./corrplot_over_env.pdf" alt="Correlation plot of traits averaged across environments" width="1\linewidth" />
<p class="caption">
Correlation plot of traits averaged across environments
</p>

<img src="./corrplot_p-adjust_all.pdf" alt="Correlation plot of traits averaged within each environments" width="1\linewidth" />
<p class="caption">
Correlation plot of traits averaged within each environments
</p>
