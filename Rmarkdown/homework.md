---
title: 'GG 606: Homework Weeks 1-2'
author: "Author: Julia K"
date: "Date: 2024-01-30"
output: 
  html_document:
    keep_md: true
    toc: true
    toc_float: true
    toc_depth: 4  
  collapsed: no 
---



### **Loading Necessary Items**

```r
library(tidyverse)
```

```
## Warning: package 'tidyverse' was built under R version 4.3.2
```

```
## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
## ✔ dplyr     1.1.3     ✔ readr     2.1.4
## ✔ forcats   1.0.0     ✔ stringr   1.5.0
## ✔ ggplot2   3.4.3     ✔ tibble    3.2.1
## ✔ lubridate 1.9.2     ✔ tidyr     1.3.0
## ✔ purrr     1.0.2     
## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
## ✖ dplyr::filter() masks stats::filter()
## ✖ dplyr::lag()    masks stats::lag()
## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors
```

```r
library(janitor)
```

```
## 
## Attaching package: 'janitor'
## 
## The following objects are masked from 'package:stats':
## 
##     chisq.test, fisher.test
```

```r
library(broom)
library(ggthemes)
```

```
## Warning: package 'ggthemes' was built under R version 4.3.2
```

```r
library(RColorBrewer)

library(here)                        #set wd through project file (week 2 hw)
```

```
## here() starts at C:/Users/jkoza/Documents/GRADUATE SCHOOL/2. COURSEWORK/GG606/1. GG606_R_project
```

```r
library(palmerpenguins)
```

```
## Warning: package 'palmerpenguins' was built under R version 4.3.2
```

```r
#getwd() #loads rmarkdown folder, set through knit > project as directory 
```


```r
source(here("functions", "theme_juicy.R")) #load from 'functions' folder
theme_set(theme_juicy())                   #auto-apply as theme for plots
```

```
## Warning: The `size` argument of `element_line()` is deprecated as of ggplot2 3.4.0.
## ℹ Please use the `linewidth` argument instead.
## This warning is displayed once every 8 hours.
## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
## generated.
```
Did nothing about that warning that prints above.


```r
penguins=penguins %>%         #created the dataset as a callable object
  glimpse()
```

```
## Rows: 344
## Columns: 8
## $ species           <fct> Adelie, Adelie, Adelie, Adelie, Adelie, Adelie, Adel…
## $ island            <fct> Torgersen, Torgersen, Torgersen, Torgersen, Torgerse…
## $ bill_length_mm    <dbl> 39.1, 39.5, 40.3, NA, 36.7, 39.3, 38.9, 39.2, 34.1, …
## $ bill_depth_mm     <dbl> 18.7, 17.4, 18.0, NA, 19.3, 20.6, 17.8, 19.6, 18.1, …
## $ flipper_length_mm <int> 181, 186, 195, NA, 193, 190, 181, 195, 193, 190, 186…
## $ body_mass_g       <int> 3750, 3800, 3250, NA, 3450, 3650, 3625, 4675, 3475, …
## $ sex               <fct> male, female, female, NA, female, male, female, male…
## $ year              <int> 2007, 2007, 2007, 2007, 2007, 2007, 2007, 2007, 2007…
```
### **Manipulating the Dataset** 

**What is it that we would want to know? What plot types can show us this?** \

I would want to know if penguin body mass (M/F) among species varies depending 
on the island that they're on (lots of different floating variables here).

```r
#Figure out how many different islands there are for each species (filter or tally)
penguins %>%                         #did not create a new object in env. 
dplyr::select (species, island) %>%  #create subset w/ only these two cols
distinct(species, island) %>%        #create combination of species and island
  group_by(species) %>%              #tally of islands (n) per species
  count() %>%
print()                              #three spp. only Adelie has three islands
```

```
## # A tibble: 3 × 2
## # Groups:   species [3]
##   species       n
##   <fct>     <int>
## 1 Adelie        3
## 2 Chinstrap     1
## 3 Gentoo        1
```
 Can only plot Adelie body mass per island because it's the only species that has
data from more than one island. But because I'm curious...  

***Adelie:*** three islands (Biscoe, Dream, Torgersen) n (appears) = 44, 56, 52 \
***Chinstrap:*** only one island (Dream) n= 68 \
***Gentoo:*** only one island (Biscoe) n= 124 \

**Don't know if M/F for any given n, nor the year**
_______________________________________________________________________________

Okay... *now* what do I wanna know? 

Differences in body mass index of both male and females among islands. Is there 
a correlation between body mass index and flipper length (i.e., if you're bigger
do you have larger flippers)? Do these variables vary a lot throughout the years? \
Would have to breakdown groupings by: i. Year ii. Island iii. Gender
  

```r
peng_adelie=penguins %>%                         #created adelie penguins subset 
filter(species == "Adelie") %>%
 na.omit() %>%                                  #this removes ALL rows with 1+ NA
  mutate(body_mass_lbs=(body_mass_g / 453.6)) %>% #new col convert grams to lbs
  #mutate(`sum`()) before brackets would put the total sum within the new col lol
  mutate(across(c('body_mass_lbs'), round, 2)) %>% #round new col 2 decimal places

# Want to add the total number of penguins measured per year:
# mutate(
# distinct(body_mass_lbs, flipper_length_mm, sex, year) #specific penguin
# group_by(year)               
# mutate(count()) %>%
#  ) %>% 
  print() 
```

```
## Warning: There was 1 warning in `mutate()`.
## ℹ In argument: `across(c("body_mass_lbs"), round, 2)`.
## Caused by warning:
## ! The `...` argument of `across()` is deprecated as of dplyr 1.1.0.
## Supply arguments directly to `.fns` through an anonymous function instead.
## 
##   # Previously
##   across(a:b, mean, na.rm = TRUE)
## 
##   # Now
##   across(a:b, \(x) mean(x, na.rm = TRUE))
```

```
## # A tibble: 146 × 9
##    species island    bill_length_mm bill_depth_mm flipper_length_mm body_mass_g
##    <fct>   <fct>              <dbl>         <dbl>             <int>       <int>
##  1 Adelie  Torgersen           39.1          18.7               181        3750
##  2 Adelie  Torgersen           39.5          17.4               186        3800
##  3 Adelie  Torgersen           40.3          18                 195        3250
##  4 Adelie  Torgersen           36.7          19.3               193        3450
##  5 Adelie  Torgersen           39.3          20.6               190        3650
##  6 Adelie  Torgersen           38.9          17.8               181        3625
##  7 Adelie  Torgersen           39.2          19.6               195        4675
##  8 Adelie  Torgersen           41.1          17.6               182        3200
##  9 Adelie  Torgersen           38.6          21.2               191        3800
## 10 Adelie  Torgersen           34.6          21.1               198        4400
## # ℹ 136 more rows
## # ℹ 3 more variables: sex <fct>, year <int>, body_mass_lbs <dbl>
```

Could not easily figure out how to add the `count()` summary function to the
data table... so, manually computed them then will write in for figure... 
*Whomp whomp :(*

```r
peng_adelie %>%
distinct(body_mass_lbs, flipper_length_mm, sex, year) %>%   #specific penguin
  group_by(year) %>%                   #tally of penguins (n) per year
  count() %>%                         
print()                                #these data counts check out (verified)       
```

```
## # A tibble: 3 × 2
## # Groups:   year [3]
##    year     n
##   <int> <int>
## 1  2007    44
## 2  2008    48
## 3  2009    50
```

Plot with manually positioned text labels:

```r
n_counts=data.frame (                 #create a subset dataframe of these values
  label=c("n= 44", "n= 48", "n= 50"), #text you want to add
  year  =c(2007, 2008, 2009),         #col name facet_wrapped by
  x    =c(7, 7, 7),                   #exact position on the x-axis per grid
  y    =c(207.5, 207.5, 207.5))       #exact position on the y-axis per grid            
```


### **Ploting the Data**

```r
peng_adelie_figure=peng_adelie %>%          #create figure as a callable object
ggplot() + 
  facet_wrap(vars(year), ncol=3) +               #create a grid w/ 3 columns 
  geom_point(aes(x=body_mass_lbs, y=flipper_length_mm, colour=island, shape=sex), 
  size=2) +
  geom_smooth(aes(x=body_mass_lbs, y=flipper_length_mm, colour=island), method = "lm", se=FALSE) +
  
scale_colour_brewer(name= "Island", palette="YlOrRd") +  #ColorRBrewer package

  
labs(title="Adelie Penguin Survey Samples (2007-2009)") +
  labs(x=expression(atop("Body Mass (lbs)")),  #not technically coding right?
         y=expression("Flipper Length (mm)")) +

theme(legend.position="top") +                    #select where the legend rests
theme(legend.key.size=unit(0.35, 'cm')) +         #decrease legend size
theme(legend.margin=margin(t=0.25, unit='cm')) +  #reduce legend margin

geom_text(data=n_counts, 
          mapping=aes(x=x, y=y, label=label), colour="white")#add n text to each plot
          #don't have the skillset on how to generate background square for text rn
  
print(peng_adelie_figure)
```

```
## `geom_smooth()` using formula = 'y ~ x'
```

![](homework_files/figure-html/WEEK ONE: INTRODUCTION + penguins; plotting data-1.png)<!-- -->

Graph gives a... positive linear correlation? But linear regression stats do not
return any significant values (?) \
Data seems to show an overall increase in flipper length, while body mass remains 
similar. 

```r
body_mass_x_flipper_length=peng_adelie %>%      #create new object from dataframe
  group_by(year)                                #group analysis by year
  do(body_mass_x_flipper_length,                #take new object
     glance(                                    #look at the linear regression 
       lm(flipper_length_mm ~ body_mass_lbs, data=.))) #target x predictor
```

```
## # A tibble: 3 × 13
## # Groups:   year [3]
##    year r.squared adj.r.squared sigma statistic p.value    df logLik   AIC   BIC
##   <int>     <dbl>         <dbl> <dbl>     <dbl>   <dbl> <dbl>  <dbl> <dbl> <dbl>
## 1  2007     0.197         0.177  5.95      10.3 2.58e-3     1  -140.  286.  291.
## 2  2008     0.330         0.316  4.71      23.6 1.29e-5     1  -147.  301.  307.
## 3  2009     0.255         0.241  5.42      17.2 1.33e-4     1  -161.  327.  333.
## # ℹ 3 more variables: deviance <dbl>, df.residual <int>, nobs <int>
```

```r
summary(body_mass_x_flipper_length)
```

```
##       species          island   bill_length_mm  bill_depth_mm  
##  Adelie   :146   Biscoe   :44   Min.   :32.10   Min.   :15.50  
##  Chinstrap:  0   Dream    :55   1st Qu.:36.73   1st Qu.:17.50  
##  Gentoo   :  0   Torgersen:47   Median :38.85   Median :18.40  
##                                 Mean   :38.82   Mean   :18.35  
##                                 3rd Qu.:40.77   3rd Qu.:19.00  
##                                 Max.   :46.00   Max.   :21.50  
##  flipper_length_mm  body_mass_g       sex          year      body_mass_lbs   
##  Min.   :172.0     Min.   :2850   female:73   Min.   :2007   Min.   : 6.280  
##  1st Qu.:186.0     1st Qu.:3362   male  :73   1st Qu.:2007   1st Qu.: 7.418  
##  Median :190.0     Median :3700               Median :2008   Median : 8.160  
##  Mean   :190.1     Mean   :3706               Mean   :2008   Mean   : 8.171  
##  3rd Qu.:195.0     3rd Qu.:4000               3rd Qu.:2009   3rd Qu.: 8.820  
##  Max.   :210.0     Max.   :4775               Max.   :2009   Max.   :10.530
```


### **Saving the Data**

```r
ggsave(here("figures", "adelie_penguin_plot.png"), peng_adelie_figure,
       width = 190, height = 120, units = "mm")  
```

```
## `geom_smooth()` using formula = 'y ~ x'
```

