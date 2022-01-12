#=================================================================================================
# Descriptive statistics
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
dfexp2 = df
load('../data/dfexp1.Rda')
dfexp1 = df
remove(df)


#-------------------------------------------------------------------------------------------------
# Combine dataframes
#-------------------------------------------------------------------------------------------------
dfexp1$treatment_group = ifelse(dfexp1$treatment == 1, 'T1', 'C1')
dfexp2$treatment_group = ifelse(dfexp2$treatment == 1, 'T2', 'T1')

dfexp1 = dfexp1 %>% 
  select(treatment_group, qage, female, aware_df, proficient_dummy, internet_use_dummy, familiar_tc)

dfexp2 = dfexp2 %>% 
  select(treatment_group, qage, female, aware_df, proficient_dummy, internet_use_dummy, familiar_tc) %>%
  filter(treatment_group == 'T2')

df = rbind.data.frame(dfexp1, dfexp2)
dfC1T1 = filter(df, treatment_group != 'T2')
dfC1T2 = filter(df, treatment_group != 'T1')
remove(dfexp1, dfexp2)
  

#-------------------------------------------------------------------------------------------------
# summary stats + randomization check
#-------------------------------------------------------------------------------------------------
# run regressions
regAge         = lm(qage ~ as.factor(treatment_group), dfC1T1)
regFemale      = lm(female ~ as.factor(treatment_group), dfC1T1) 
regAwareDf     = lm(aware_df ~ as.factor(treatment_group), dfC1T1) 
regProficient  = lm(proficient_dummy ~ as.factor(treatment_group), dfC1T1)
regInternetUse = lm(internet_use_dummy ~ as.factor(treatment_group), dfC1T1)
regFamiliar    = lm(familiar_tc ~ as.factor(treatment_group), dfC1T1)

regAge2         = lm(qage ~ as.factor(treatment_group), dfC1T2)
regFemale2      = lm(female ~ as.factor(treatment_group), dfC1T2) 
regAwareDf2     = lm(aware_df ~ as.factor(treatment_group), dfC1T2) 
regProficient2  = lm(proficient_dummy ~ as.factor(treatment_group), dfC1T2)
regInternetUse2 = lm(internet_use_dummy ~ as.factor(treatment_group), dfC1T2)
regFamiliar2    = lm(familiar_tc ~ as.factor(treatment_group), dfC1T2)

# populate table with results
ExtractData = function(rg){
  afCoefficients = rg$coefficients
  
  # adjust standard errors
  mVarCov     = vcovHC(rg, "HC1")
  afRobustSE = sqrt(diag(mVarCov))
  
  # round figures for table
  output = cbind(afCoefficients, afCoefficients/afRobustSE) %>% round(3) %>% t()
  
  # t-values in brackets
  output[2,] = str_c('[', output[2,], ']')
  
  return(output)
  }

TableColumnsOneAndTwo = rbind(ExtractData(regAge),
                              ExtractData(regFemale),
                              ExtractData(regAwareDf),
                              ExtractData(regProficient),
                              ExtractData(regInternetUse),
                              ExtractData(regFamiliar))

TableColumn3 = rbind(ExtractData(regAge2),
                     ExtractData(regFemale2),
                     ExtractData(regAwareDf2),
                     ExtractData(regProficient2),
                     ExtractData(regInternetUse2),
                     ExtractData(regFamiliar2)) %>% .[,2]

Table = cbind(TableColumnsOneAndTwo, TableColumn3)

# add number of observations
temp = df %>%
  group_by(treatment_group) %>%
  summarise(Observations = n())
Table = rbind(Table, temp$Observations)

# column and row names
colnames(Table) = c('C1 mean', 'T1 vs. C1', 'T2 vs. C1')
rownames(Table) = c('Age', '',
                    'Female', ' ',
                    'Aware of deepfakes', ' ',
                    'Proficient with social media', ' ',
                    'High level of internet use', ' ',
                    'Familiar with actor', ' ',
                    'Observations'
)

LatexOutput = stargazer(Table, header = F)

InsertLineLatexTable = function(LatexObject, acString, nRow){
  x = str_c(acString, collapse = ' & ')
  # x = str_c(x, ' \\\\')
  output = c(LatexObject[1:nRow], x, LatexObject[(nRow+1):length(LatexObject)])
  return(output)
}

LatexOutput = InsertLineLatexTable(LatexOutput, '\\hline', 21)
LatexOutput = InsertLineLatexTable(LatexOutput, '\\hline', 24)

cat(str_c(LatexOutput, collapse = '\n'))


