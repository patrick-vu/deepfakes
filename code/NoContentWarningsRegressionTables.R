#=================================================================================================
# Experiment 1: analysis
#=================================================================================================
# Purpose: generate tables and graphs for experiment 1
#=================================================================================================
#-------------------------------------------------------------------------------------------------
# Housekeeping
#-------------------------------------------------------------------------------------------------
rm(list = ls())

# packages
library(ggplot2)
library(ggpubr)
library(stargazer)
library(tidyverse)
library(dplyr)
library(reshape2)
library(rstudioapi)
library(stringr)
library(stringi)
library(sandwich)

# import data
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # set to current directory
load('../data/dfexp1.Rda')
           
#-------------------------------------------------------------------------------------------------
# (1) regression: out of the ordinary?
#-------------------------------------------------------------------------------------------------
# share and number out of the ordinary by control and treatment
Table = df %>%
    group_by(treatment, out_of_ordinary) %>%
    summarise(Count = n()) %>% 
    mutate(Share = Count/sum(Count))

# models
m1 = lm(out_of_ordinary ~ treatment, df)
m2 = lm(out_of_ordinary ~ treatment + female + I(qage/10) + aware_df + proficient_dummy + internet_use_dummy + familiar_tc, df)

# adjust standard errors
afRobustSEs_m1 = sqrt(diag(vcovHC(m1, "HC1")))
afRobustSEs_m2 = sqrt(diag(vcovHC(m2, "HC1")))

# latex output
stargazer(m1, m2,
          header = F,
          type = 'text',
          covariate.labels = 
              c("Treatment (T1)", "Female", "Age x 1/10", "Aware of deepfakes",
                "Proficient with social media", "High level of internet use", "Familiar with actor (0-10)"),
          se=list(afRobustSEs_m1, afRobustSEs_m2),
          dep.var.labels = 'Out of the ordinary',
          omit.stat = c("ll","rsq", "ser", "f"),
          out = '../submission/tables/table2_detection_no_warning.doc'
          )



#-------------------------------------------------------------------------------------------------
# (2) regression with heterogeneity
#-------------------------------------------------------------------------------------------------
# interact treatment dummy with dummy of various measures of heterogeneity
m1 = lm(out_of_ordinary ~ aware_df + treatment:aware_df, df)
m2 = lm(out_of_ordinary ~ age_dummy + treatment:age_dummy, df)

# adjust standard errors
afRobustSEs_m1 = sqrt(diag(vcovHC(m1, "HC1")))
afRobustSEs_m2 = sqrt(diag(vcovHC(m2, "HC1")))

# latex output
stargazer(m1, m2,
          type = 'text',
          header = F,
          covariate.labels = 
              c('Aware of deepfakes', 'Aware of deepfakes x Treatment (T1)',
                'Below median age', 'Below median age x Treatment (T1)'),
          se=list(afRobustSEs_m1, afRobustSEs_m2),
          dep.var.labels = 'Out of the ordinary',
          omit.stat = c("ll","rsq", "ser", "f"),
          out = '../submission/tables/table3_detection_no_warning_heterogeneity.doc'
          )







