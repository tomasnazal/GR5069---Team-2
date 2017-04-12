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

    HH_fit <- glm(` HH` ~ . - ` HL` - ` LH` - ` LL` - ` MID`- ROI - gross - imdb_score, family = binomial(link = "logit"), data = imdb_num)

    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

    HL_fit <- glm(` HL` ~ . - ` HH` - ` LH` - ` LL` - ` MID`- ROI - gross - imdb_score, family = binomial(link = "logit"), data = imdb_num)

    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

    LH_fit <- glm(` LH` ~ . - ` HH` - ` HL` - ` LL` - ` MID`- ROI - gross - imdb_score, family = binomial(link = "logit"), data = imdb_num)

    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

    LL_fit <- glm(` LL` ~ . - ` HH` - ` HL` - ` LH` - ` MID`- ROI - gross - imdb_score, family = binomial(link = "logit"), data = imdb_num)

    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

    stargazer::stargazer(HH_fit, HL_fit, LH_fit, LL_fit,
                         dep.var.labels=c("HH", "HL", "LH", "LL"),
                         type = "text")

    ## 
    ## =======================================================================
    ##                                        Dependent variable:             
    ##                           ---------------------------------------------
    ##                               HH          HL         LH         LL     
    ##                               (1)         (2)        (3)        (4)    
    ## -----------------------------------------------------------------------
    ## num_critic_for_reviews     0.007***    -0.003**    -0.001    -0.004**  
    ##                             (0.001)     (0.001)    (0.001)    (0.002)  
    ##                                                                        
    ## duration                    0.010**    -0.039***  0.022***    -0.007   
    ##                             (0.004)     (0.007)    (0.004)    (0.005)  
    ##                                                                        
    ## director_facebook_likes   -0.0001***    -0.001    -0.00003   -0.00000  
    ##                            (0.00003)    (0.001)   (0.00004)  (0.0001)  
    ##                                                                        
    ## actor_3_facebook_likes     -0.0002**   -0.0004**  0.001***    0.0002   
    ##                            (0.0001)    (0.0002)   (0.0003)   (0.0001)  
    ##                                                                        
    ## actor_1_facebook_likes      -0.0001   -0.0004***  0.001***    0.0001   
    ##                            (0.0001)    (0.0001)   (0.0002)   (0.0001)  
    ##                                                                        
    ## num_voted_users           0.00001***  -0.00001**  -0.00000  -0.00004***
    ##                            (0.00000)   (0.00000)  (0.00000)  (0.00001) 
    ##                                                                        
    ## cast_total_facebook_likes   0.0001*    0.0004***  -0.001***   -0.0001  
    ##                            (0.0001)    (0.0001)   (0.0002)   (0.0001)  
    ##                                                                        
    ## facenumber_in_poster        -0.004       0.033     -0.092*    0.058*   
    ##                             (0.029)     (0.038)    (0.050)    (0.035)  
    ##                                                                        
    ## num_user_for_reviews        -0.0004    0.002***    -0.001*   0.002***  
    ##                            (0.0003)    (0.0004)    (0.001)    (0.001)  
    ##                                                                        
    ## budget                    -0.00000*** -0.00000***  0.000**     0.000   
    ##                             (0.000)     (0.000)    (0.000)    (0.000)  
    ##                                                                        
    ## title_year                 -0.085***   0.104***     0.029    0.073***  
    ##                             (0.021)     (0.021)    (0.019)    (0.017)  
    ##                                                                        
    ## actor_2_facebook_likes      -0.0001   -0.0004***  0.001***    0.0001   
    ##                            (0.0001)    (0.0001)   (0.0002)   (0.0001)  
    ##                                                                        
    ## aspect_ratio               -1.235***   -0.934**     0.008      0.089   
    ##                             (0.291)     (0.366)    (0.174)    (0.132)  
    ##                                                                        
    ## movie_facebook_likes       -0.00000    0.00001*   0.00001*    0.00001  
    ##                            (0.00000)   (0.00001)  (0.00001)  (0.00001) 
    ##                                                                        
    ## Constant                  168.776***  -204.728***  -62.511  -146.688***
    ##                            (41.996)    (42.185)   (38.708)   (34.733)  
    ##                                                                        
    ## -----------------------------------------------------------------------
    ## Observations                 3,238       3,238      3,238      3,238   
    ## Log Likelihood             -697.565    -498.443   -586.673   -598.209  
    ## Akaike Inf. Crit.          1,425.130   1,026.887  1,203.346  1,226.417 
    ## =======================================================================
    ## Note:                                       *p<0.1; **p<0.05; ***p<0.01
