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
# Experiment 1 graph
#-------------------------------------------------------------------------------------------------
# model + adjust standard errors
m1 = lm(out_of_ordinary ~ treatment, df)
afRobustSEs = sqrt(diag(vcovHC(m1, "HC1")))

df1 = data.frame(Group = as.factor(c('No deepfake \n (Control)', 'Deepfake \n (Treatment)')),
                 Share = c(m1$coefficients[1], m1$coefficients[1] + m1$coefficients[2])*100,
                 Error = c(0, 100*1.96*afRobustSEs[2]))
df1$Group = ordered(df1$Group, levels = rev(levels(df1$Group)))

g1 = ggplot(df1, aes(x=Group, y=Share)) + 
  geom_bar(position=position_dodge(), stat="identity", colour="black", fill = 'lightsteelblue2', size = 0.65) +
  geom_errorbar(aes(ymin=Share-Error, ymax=Share+Error),
                width=.15,                 
                position=position_dodge(.9)) +
  ylim(c(0, 100)) +
  ylab('Share who spot something \n out of the ordinary (%)') +
  ggtitle('Experiment 1: Spotting Something \nOut of the Ordinary') +
  theme_bw() +
  theme(axis.title.x = element_blank())


#-------------------------------------------------------------------------------------------------
# Experiment 2 graph
#-------------------------------------------------------------------------------------------------
load('../data/dfexp2_reg.Rda')
m2 = lm(correct ~ treatment, df) 
afRobustSEs = sqrt(diag(vcovHC(m2, "HC1")))

df2 = data.frame(Group = as.factor(c('No content warning \n (Control)', 'Content warning \n (Treatment)')),
                 Share = c(m2$coefficients[1], m2$coefficients[1] + m2$coefficients[2])*100,
                 Error = c(0, 100*1.96*afRobustSEs[2]))
df2$Group = ordered(df2$Group, levels = rev(levels(df2$Group)))

g2 = ggplot(df2, aes(x=Group, y=Share)) + 
  geom_bar(position=position_dodge(), stat="identity", colour="black", fill = 'lightsteelblue2', size = 0.65) +
  geom_errorbar(aes(ymin=Share-Error, ymax=Share+Error),
                width=.15,                 
                position=position_dodge(.9)) +
  ylim(c(0, 100)) +
  ylab('Share who correctly identify \nthe deepfake (%)') +
  ggtitle('Experiment 2: Manual Detection with \nContent Warnings') +
  theme_bw() +
  theme(axis.title.x = element_blank())


#-------------------------------------------------------------------------------------------------
# Combine
#-------------------------------------------------------------------------------------------------
ggarrange(g1, g2, 
          nrow=1)

ggsave('../../../Overleaf/Deepfakes/Bar_Summary.png')


