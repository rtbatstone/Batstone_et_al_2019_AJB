data\_analyses\_Cockerham
================
Rebecca Batstone
2019-09-01

Load packages
-------------

``` r
# packages
library("tidyverse") ## includes ggplot2, dplyr, readr, stringr
```

Spreadsheets
------------

``` r
data = read.csv(paste(getwd(),"/Cockerhams_method/RB_Cockerhamdata.csv", sep="/"))
```

Functions
---------

``` r
# for imperfect correlation (page 88, Cockerham 1963)
crossing = function(Vg1,Vg2,Rg){
  
  out = 2*sqrt(Vg1)*sqrt(Vg2)*(1-Rg)
  
  return(out)
  
}

# for heterogeneous variances (page 88, Cockerham 1963)
hetero = function(Vg1,Vg2){
  
  out = ((sqrt(Vg1)-sqrt(Vg2))^2)
  
  return(out)
  
}

# Calculate imperfect correlation and heterogenenous variance values for each row in data frame
data$crossing2 = sapply(1:nrow(data), FUN = function(r){
  Vg1 = data$Vg1[r]
  Vg2 = data$Vg2[r]
  Rg = data$Rg[r]
  
  crossing.val = crossing(Vg1,Vg2,Rg)
  
  return(crossing.val)
})

data$hetero2 = sapply(1:nrow(data), FUN = function(r){
  Vg1 = data$Vg1[r]
  Vg2 = data$Vg2[r]
  
  hetero.val = hetero(Vg1,Vg2)
  
  return(hetero.val)
})
```

Calculations
------------

``` r
# Group by trait and calculate sum of imperfect correlation values (and divide by # environments*(# environments - 1) as per Cockerham 1963)
dataSum = data %>% 
  group_by(trait) %>% 
  summarise(crossingSum = sum(crossing2)/(5*(5-1)))

# Group by trait and calculate sum of heterogeneous variances values (and divide by # environments*(# environments - 1) as per Cockerham 1963)
dataSum2 = data %>% 
  group_by(trait) %>% 
  summarise(heteroSum = sum(hetero2)/(5*(5-1)))

# Join tibble tables together
dataSum = left_join(dataSum,dataSum2,"trait")

# Calculate for each trait the percent of the total variance that is due to imperfect correlation (i.e., rank changes)
dataSum  = dataSum %>% 
  mutate(.,Sum=crossingSum+heteroSum) %>% 
  mutate(.,perCross = 100*crossingSum/Sum)

write.csv(dataSum, "./Cockerhams_method/cockerham_res_01Sep2019.csv", row.names = FALSE)
```
