#=================================================================================================
# Experiment 2: analysis
#=================================================================================================
# Purpose: generate tables and graphs for experiment 2
#=================================================================================================
#-------------------------------------------------------------------------------------------------
# Housekeeping
#-------------------------------------------------------------------------------------------------
rm(list = ls())

# packages
library(ggplot2)
library(ggpubr)
library(stargazer)
library(miceadds)
library(magrittr)
library(yaml)
library(tidyverse)
library(dplyr)
library(sandwich)

# import data
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # set to current directory
load('../data/dfexp2_reg.Rda')

#-------------------------------------------------------------------------------------------------
# Main regressions
#-------------------------------------------------------------------------------------------------
regDetectFull = lm(correct ~ treatment, df) 
summary(regDetectFull)

# adjust standard errors
vcov         = vcovHC(regDetectFull, "HC1")
robust_se_1  = sqrt(diag(vcov))

regDetectSub = lm(correct ~ treatment, filter(df, out_of_ordinary == 1 | is.na(out_of_ordinary))) 
summary(regDetectSub)

vcov         = vcovHC(regDetectSub, "HC1")
robust_se_2  = sqrt(diag(vcov))

stargazer(regDetectFull, regDetectSub,
          type = 'text',
          header = F,
          se=list(robust_se_1, robust_se_2),
          omit.stat = c('rsq', 'ser', 'f'),
          covariate.labels = c('Treatment',
                               'Constant'),
          dep.var.labels   = "Detection")


#-------------------------------------------------------------------------------------------------
# heterogeneity analysis
#-------------------------------------------------------------------------------------------------
# import data (only includes those in T2, who are asked additional questions)
load('../data/dfexp2.Rda')

# sureness
m1 = lm(exp2correct ~ as.factor(exp2dfobv), df)
vcov         = vcovHC(m1, "HC1")
robust_se_1  = sqrt(diag(vcov))

# confidence
m2 = lm(exp2correct ~ as.factor(Confidence), df)
vcov         = vcovHC(m2, "HC1")
robust_se_2  = sqrt(diag(vcov))

# gender
m3 = lm(exp2correct ~ as.factor(Gender), df)
vcov         = vcovHC(m3, "HC1")
robust_se_3  = sqrt(diag(vcov))

# aware of df
m4 = lm(exp2correct ~ aware_df, df)
vcov         = vcovHC(m4, "HC1")
robust_se_4  = sqrt(diag(vcov))

# age
m5 = lm(exp2correct ~ I(qage/10), df)
vcov         = vcovHC(m5, "HC1")
robust_se_5  = sqrt(diag(vcov))

# proficient
m6 = lm(exp2correct ~ proficient_dummy, df)
vcov         = vcovHC(m6, "HC1")
robust_se_6  = sqrt(diag(vcov))

# internet use
m7 = lm(exp2correct ~ internet_use_dummy, df)
vcov         = vcovHC(m7, "HC1")
robust_se_7  = sqrt(diag(vcov))

# aware of tc
m8 = lm(exp2correct ~ familiar_tc_dummy, df)
vcov         = vcovHC(m8, "HC1")
robust_se_8  = sqrt(diag(vcov))

# combined
m9 = lm(exp2correct ~ as.factor(exp2dfobv) + as.factor(Confidence) + as.factor(Gender)
        + aware_df + I(qage/10) + proficient_dummy + internet_use_dummy + familiar_tc_dummy
        , df)
vcov         = vcovHC(m9, "HC1")
robust_se_9  = sqrt(diag(vcov))

stargazer(m1, m2, m3, m4, m5, m6, m7, m8, m9,
          type = 'text',
          header = F,
          omit.stat = c('rsq', 'ser', 'f'),
          covariate.labels = c('Obvious: not',
                               "Obvious: don't know",
                               'Confidence: low',
                               'Confidence: medium',
                               'Male',
                               'Aware of deepfakes',
                               'Age x 1/10',
                               'Proficient with social media',
                               'High level of internet use',
                               'Familiar with actor',
                               'Constant'),
          se=list(robust_se_1, robust_se_2, robust_se_3,
                  robust_se_4, robust_se_5, robust_se_6,
                  robust_se_7, robust_se_8, robust_se_9
          ),
          dep.var.labels   = "Detection")








