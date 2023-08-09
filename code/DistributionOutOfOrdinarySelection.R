#=================================================================================================
# Experiment 1: other graphs
#=================================================================================================
# Purpose: generate tables and graphs not used in the paper
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
library(tidyr)
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
# historgram of video choice distribution for control and treatment who selected out of ordinary
#-------------------------------------------------------------------------------------------------
dfHist = df %>%
  filter(exp1chooseone == 1, out_of_ordinary == 1) %>%
  # filter(out_of_ordinary == 1) %>%
  select(treatment, 
         treatordinarythumb_1, treatordinarythumb_2, treatordinarythumb_3, treatordinarythumb_4, treatordinarythumb_5,
         contordinarythumb_1, contordinarythumb_2, contordinarythumb_3, contordinarythumb_4, contordinarythumb_5,
  ) %>%
  group_by(treatment) %>%
  summarise(One   = sum(treatordinarythumb_1, contordinarythumb_1),
            Two   = sum(treatordinarythumb_2, contordinarythumb_2),
            Three = sum(treatordinarythumb_3, contordinarythumb_3),
            Four  = sum(treatordinarythumb_4, contordinarythumb_4),
            Five  = sum(treatordinarythumb_5, contordinarythumb_5)
  )

dfHist = melt(data.frame(dfHist), id = c('treatment')) %>%
  arrange(treatment) %>%
  group_by(treatment) %>%
  mutate(share = value/sum(value))
# dfHist$treatment = ifelse(dfHist$treatment == 0, 'control', 'treatment')

g = ggplot(dfHist, aes(x = variable, y= share, fill = as.factor(treatment))) +
  geom_bar(stat="identity", width=.75, position = "dodge") +
  xlab('Video') + ylab('Share') +
  ylim(c(0,0.4)) +
  theme_bw() +
  theme(legend.position="bottom",
        legend.title = element_blank()) +
  scale_fill_manual(values = c("royalblue2", "coral2")) 
g
ggsave('../figures/Distribution_OutOfOrdinary_VideoSelection.png')

