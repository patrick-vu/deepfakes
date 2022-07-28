#=================================================================================================
# Graphical summary
#=================================================================================================
# Purpose: generate graphs summarizing main results
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
# Results: No Content Warnings
#-------------------------------------------------------------------------------------------------
# model + adjust standard errors
m1 = lm(out_of_ordinary ~ treatment, df)
afRobustSEs = sqrt(diag(vcovHC(m1, "HC1")))

df1 = data.frame(Group = as.factor(c('No deepfake (C1)', 'Deepfake (T1)')),
                 Share = c(m1$coefficients[1], m1$coefficients[1] + m1$coefficients[2])*100,
                 Error = c(0, 100*1.96*afRobustSEs[2]))
df1$Group = ordered(df1$Group, levels = rev(levels(df1$Group)))

ggplot(df1, aes(x=Group, y=Share)) + 
  geom_bar(position=position_dodge(), stat="identity", colour="black", fill = 'lightsteelblue2', size = 0.65) +
  geom_errorbar(aes(ymin=Share-Error, ymax=Share+Error),
                width=.15,                 
                position=position_dodge(.9)) +
  ylim(c(0, 100)) +
  ylab('Share who spot something \n out of the ordinary (%)') +
  # ggtitle('Manual Detection in a Natural Setting') +
  theme_bw() +
  theme(axis.title.x = element_blank())

ggsave('../submission/figures/treatment1_bar.pdf', width = 5, height=5.5)

#-------------------------------------------------------------------------------------------------
# Results: Content Warnings 
#-------------------------------------------------------------------------------------------------
load('../data/dfexp2_het.Rda')

# distirbution of those selecting each video or 
df$selection = NA
for(i in 1:nrow(df)){
   if(df$onlyonechoice[i] == 1){
      df$selection[i] = ifelse(df$exp2dfthumb_1[i]==1, "1", df$selection[i])
      df$selection[i] = ifelse(df$exp2dfthumb_2[i]==1, "2", df$selection[i])
      df$selection[i] = ifelse(df$exp2dfthumb_3[i]==1, "3", df$selection[i])
      df$selection[i] = ifelse(df$exp2dfthumb_4[i]==1, "4 (DF)", df$selection[i])
      df$selection[i] = ifelse(df$exp2dfthumb_5[i]==1, 5, df$selection[i])
   } else{
      df$selection[i] = 'more than\none choice'
   }
}

ggplot(df, aes(x=selection)) +
   geom_bar(aes(y = 100*(..count..)/sum(..count..)), fill="cornflowerblue", color='black') +
   # ggtitle('Manual Detection With Content Warnings') +
   xlab('Choice of which video was a deepfake') + ylab('Percent (%)') +
   ylim(c(0, 100)) +
   theme_bw() 

ggsave('../submission/figures/treatment2_hist.pdf', width = 5, height=5)


