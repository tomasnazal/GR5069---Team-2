---
title: "Untitled"
author: "Elka (Junxuan) Mao"
date: "2017/4/12"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

```{r, warning=FALSE, message=FALSE}
library(dplyr)
library(ggplot2)
library(VIM)
library(vcd)
require(car)
library(tabplot)
library(mice)
library(PerformanceAnalytics)
library(MASS)
library(glmnet)

```



```{r, warning=FALSE}
setwd("/Users/elka/Desktop/Applied data science/project")

imdb <- read.csv("/Users/elka/Desktop/Applied data science/project/movie_metadata.csv")
imdb <- filter(imdb, title_year > 1995)

imdb$ROI <- round((imdb$gross / imdb$budget), 2)


imdb$category <- as.factor(ifelse(imdb$ROI >= quantile(imdb$ROI, .8, na.rm = T) &
                                        imdb$imdb_score >= quantile(imdb$imdb_score, .7, na.rm = T), "HH",
                                      ifelse(imdb$ROI >= quantile(imdb$ROI, .8, na.rm = T) &
                                               imdb$imdb_score <= quantile(imdb$imdb_score, .3, na.rm = T), "HL",
                                             ifelse(imdb$ROI <= quantile(imdb$ROI, .2, na.rm = T) &
                                                      imdb$imdb_score >= quantile(imdb$imdb_score, .7, na.rm = T), "LH",
                                                    ifelse(imdb$ROI <= quantile(imdb$ROI, .2, na.rm = T) &
                                                             imdb$imdb_score <= quantile(imdb$imdb_score, .3, na.rm = T), "LL", "MID")))))

##dummies for category
for(n in unique(imdb$category)) {
  imdb[paste(" ", n, sep= "")] <- ifelse(imdb$category == n, 1, 0)
}


nums <- sapply(imdb, is.numeric)
imdb_num <- imdb[ ,nums]

```


```{r}
HH_fit <- glm(` HH` ~ . - ` HL` - ` LH` - ` LL` - ` MID`- ROI - gross - imdb_score, family = binomial(link = "logit"), data = imdb_num)

HL_fit <- glm(` HL` ~ . - ` HH` - ` LH` - ` LL` - ` MID`- ROI - gross - imdb_score, family = binomial(link = "logit"), data = imdb_num)

LH_fit <- glm(` LH` ~ . - ` HH` - ` HL` - ` LL` - ` MID`- ROI - gross - imdb_score, family = binomial(link = "logit"), data = imdb_num)

LL_fit <- glm(` LL` ~ . - ` HH` - ` HL` - ` LH` - ` MID`- ROI - gross - imdb_score, family = binomial(link = "logit"), data = imdb_num)

stargazer::stargazer(HH_fit, HL_fit, LH_fit, LL_fit,
                     dep.var.labels=c("HH", "HL", "LH", "LL"),
                     type = "text")
```

