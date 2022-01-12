#=================================================================================================
# Clean data
#=================================================================================================
# Purpose: imports raw experimental data and cleans it
#=================================================================================================
#-------------------------------------------------------------------------------------------------
# Housekeeping
#-------------------------------------------------------------------------------------------------
rm(list = ls())

## packages ##
library(ggplot2)
library(stargazer)
library(tidyverse)
library(dplyr)
library(vroom)

#-------------------------------------------------------------------------------------------------
# Import data
#-------------------------------------------------------------------------------------------------
# set work directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # set to current directory
setwd('../data/')

# import raw data
dfraw = vroom("dfrawfinal.csv")
dfraw = dfraw[3:nrow(dfraw),] # drop first two rows (qualtrics labels)


#-------------------------------------------------------------------------------------------------
# Filter data
#-------------------------------------------------------------------------------------------------
# keep only good completes 
dfraw = filter(dfraw, as.numeric(gc) == 1)

# remove those who have seen the video before
dfraw = filter(dfraw, is.na(treatseenthumb_4) & is.na(exp2seenbeforethumb_4))


#-------------------------------------------------------------------------------------------------
# Create variable to distinguish experiments
#-------------------------------------------------------------------------------------------------
dfraw$experiment = NA
dfraw = dfraw %>%
    mutate(experiment = ifelse( !is.na(contordinary) | !is.na(treatordinary), '1', experiment),
           experiment = ifelse( !is.na(exp2dfobv), '2', experiment)) %>%
    relocate(experiment)

#-------------------------------------------------------------------------------------------------
# Experiment 1
#-------------------------------------------------------------------------------------------------
# convert classes
dfraw$qage       = as.numeric(dfraw$qage)

dfraw$contordinary        = as.numeric(dfraw$contordinary)
dfraw$contordinarythumb_1 = as.numeric(dfraw$contordinarythumb_1)
dfraw$contordinarythumb_1 = ifelse(is.na(dfraw$contordinarythumb_1), 0, 1)
dfraw$contordinarythumb_2 = as.numeric(dfraw$contordinarythumb_2)
dfraw$contordinarythumb_2 = ifelse(is.na(dfraw$contordinarythumb_2), 0, 1)
dfraw$contordinarythumb_3 = as.numeric(dfraw$contordinarythumb_3)
dfraw$contordinarythumb_3 = ifelse(is.na(dfraw$contordinarythumb_3), 0, 1)
dfraw$contordinarythumb_4 = as.numeric(dfraw$contordinarythumb_4)
dfraw$contordinarythumb_4 = ifelse(is.na(dfraw$contordinarythumb_4), 0, 1)
dfraw$contordinarythumb_5 = as.numeric(dfraw$contordinarythumb_5)
dfraw$contordinarythumb_5 = ifelse(is.na(dfraw$contordinarythumb_5), 0, 1)

dfraw$contaltered        = as.numeric(dfraw$contaltered)
dfraw$contalteredthumb_1 = as.numeric(dfraw$contalteredthumb_1)
dfraw$contalteredthumb_1 = ifelse(is.na(dfraw$contalteredthumb_1), 0, 1)
dfraw$contalteredthumb_2 = as.numeric(dfraw$contalteredthumb_2)
dfraw$contalteredthumb_2 = ifelse(is.na(dfraw$contalteredthumb_2), 0, 1)
dfraw$contalteredthumb_3 = as.numeric(dfraw$contalteredthumb_3)
dfraw$contalteredthumb_3 = ifelse(is.na(dfraw$contalteredthumb_3), 0, 1)
dfraw$contalteredthumb_4 = as.numeric(dfraw$contalteredthumb_4)
dfraw$contalteredthumb_4 = ifelse(is.na(dfraw$contalteredthumb_4), 0, 1)
dfraw$contalteredthumb_5 = as.numeric(dfraw$contalteredthumb_5)
dfraw$contalteredthumb_5 = ifelse(is.na(dfraw$contalteredthumb_5), 0, 1)

dfraw$contseenbefore      = as.numeric(dfraw$contseenbefore)
dfraw$contseenbefore      = ifelse(is.na(dfraw$contseenbefore), 0, 1)
dfraw$contfamvideothumb_1 = as.numeric(dfraw$contfamvideothumb_1)
dfraw$contfamvideothumb_1 = ifelse(is.na(dfraw$contfamvideothumb_1), 0, 1)
dfraw$contfamvideothumb_2 = as.numeric(dfraw$contfamvideothumb_2)
dfraw$contfamvideothumb_2 = ifelse(is.na(dfraw$contfamvideothumb_2), 0, 1)
dfraw$contfamvideothumb_3 = as.numeric(dfraw$contfamvideothumb_3)
dfraw$contfamvideothumb_3 = ifelse(is.na(dfraw$contfamvideothumb_3), 0, 1)
dfraw$contfamvideothumb_4 = as.numeric(dfraw$contfamvideothumb_4)
dfraw$contfamvideothumb_4 = ifelse(is.na(dfraw$contfamvideothumb_4), 0, 1)
dfraw$contfamvideothumb_4 = as.numeric(dfraw$contfamvideothumb_4)
dfraw$contfamvideothumb_5 = ifelse(is.na(dfraw$contfamvideothumb_5), 0, 1)

dfraw$contawaredf      = as.numeric(dfraw$contawaredf)	
dfraw$contdfslider_1   = as.numeric(dfraw$contdfslider_1)
dfraw$continternet_1   = as.numeric(dfraw$continternet_1)
dfraw$contproficient_1 = as.numeric(dfraw$contproficient_1)

dfraw$treatordinary        = as.numeric(dfraw$treatordinary)
dfraw$treatordinarythumb_1 = as.numeric(dfraw$treatordinarythumb_1)
dfraw$treatordinarythumb_1 = ifelse(is.na(dfraw$treatordinarythumb_1), 0, 1)
dfraw$treatordinarythumb_2 = as.numeric(dfraw$treatordinarythumb_2)
dfraw$treatordinarythumb_2 = ifelse(is.na(dfraw$treatordinarythumb_2), 0, 1)
dfraw$treatordinarythumb_3 = as.numeric(dfraw$treatordinarythumb_3)
dfraw$treatordinarythumb_3 = ifelse(is.na(dfraw$treatordinarythumb_3), 0, 1)
dfraw$treatordinarythumb_4 = as.numeric(dfraw$treatordinarythumb_4)
dfraw$treatordinarythumb_4 = ifelse(is.na(dfraw$treatordinarythumb_4), 0, 1)
dfraw$treatordinarythumb_5 = as.numeric(dfraw$treatordinarythumb_5)
dfraw$treatordinarythumb_5 = ifelse(is.na(dfraw$treatordinarythumb_5), 0, 1)

dfraw$treataltered        = as.numeric(dfraw$treataltered)
dfraw$treatalteredthumb_1 = as.numeric(dfraw$treatalteredthumb_1)
dfraw$treatalteredthumb_1 = ifelse(is.na(dfraw$treatalteredthumb_1), 0, 1)
dfraw$treatalteredthumb_2 = as.numeric(dfraw$treatalteredthumb_2)
dfraw$treatalteredthumb_2 = ifelse(is.na(dfraw$treatalteredthumb_2), 0, 1)
dfraw$treatalteredthumb_3 = as.numeric(dfraw$treatalteredthumb_3)
dfraw$treatalteredthumb_3 = ifelse(is.na(dfraw$treatalteredthumb_3), 0, 1)
dfraw$treatalteredthumb_4 = as.numeric(dfraw$treatalteredthumb_4)
dfraw$treatalteredthumb_4 = ifelse(is.na(dfraw$treatalteredthumb_4), 0, 1)
dfraw$treatalteredthumb_5 = as.numeric(dfraw$treatalteredthumb_5)
dfraw$treatalteredthumb_5 = ifelse(is.na(dfraw$treatalteredthumb_5), 0, 1)

dfraw$treatseenbefore  = as.numeric(dfraw$treatseenbefore)
dfraw$treatseenthumb_1 = as.numeric(dfraw$treatseenthumb_1)
dfraw$treatseenthumb_1 = ifelse(is.na(dfraw$treatseenthumb_1), 0, 1)
dfraw$treatseenthumb_2 = as.numeric(dfraw$treatseenthumb_2)
dfraw$treatseenthumb_2 = ifelse(is.na(dfraw$treatseenthumb_2), 0, 1)
dfraw$treatseenthumb_3 = as.numeric(dfraw$treatseenthumb_3)
dfraw$treatseenthumb_3 = ifelse(is.na(dfraw$treatseenthumb_3), 0, 1)
dfraw$treatseenthumb_4 = as.numeric(dfraw$treatseenthumb_4)
dfraw$treatseenthumb_4 = ifelse(is.na(dfraw$treatseenthumb_4), 0, 1)
dfraw$treatseenthumb_5 = as.numeric(dfraw$treatseenthumb_5)
dfraw$treatseenthumb_5 = ifelse(is.na(dfraw$treatseenthumb_5), 0, 1)

dfraw$treatawaredf      = as.numeric(dfraw$treatawaredf)	
dfraw$treatdfslider_1   = as.numeric(dfraw$treatdfslider_1)
dfraw$treatinternet_1   = as.numeric(dfraw$treatinternet_1)
dfraw$treatproficient_1 = as.numeric(dfraw$treatproficient_1)

# filter out those in experiment 2
dfexp1 = dfraw

# create variables: (i) correct detection variable; (ii) choosing df plus another video; (iii) number of false negatives
dfexp1$exp1correct     = NA
dfexp1$exp1choosedf    = NA
dfexp1$exp1c1onlyone   = NA
dfexp1$exp1t1onlyone   = NA
dfexp1$false_negatives = NA

for(i in 1:nrow(dfexp1)){
    # (i) correct detection variable
    dfexp1$exp1correct[i] = ifelse(dfexp1$treatordinarythumb_4[i] == 1 &
                                       sum(dfexp1$treatordinarythumb_1[i], dfexp1$treatordinarythumb_2[i], 
                                           dfexp1$treatordinarythumb_3[i] , dfexp1$treatordinarythumb_5[i]) == 0,
                                   1, 0)
    
    # (ii) selecting correct video + 1 or more other videos 
    dfexp1$exp1choosedf[i] = ifelse(dfexp1$treatordinarythumb_4[i] == 1, 1, 0)
    
    # (iii) selecting only 1 video
    dfexp1$exp1chooseone[i] = ifelse(sum(dfexp1$treatordinarythumb_1[i], dfexp1$treatordinarythumb_2[i], 
                                         dfexp1$treatordinarythumb_3[i], dfexp1$treatalteredthumb_4[i], dfexp1$treatordinarythumb_5[i]) == 1 |
                                         sum(dfexp1$contordinarythumb_1[i], dfexp1$contordinarythumb_2[i], 
                                             dfexp1$contordinarythumb_3[i], dfexp1$contalteredthumb_4[i], dfexp1$contordinarythumb_5[i]) == 1,
                                     1, 0)
    
    # (iv) correct altered video
    dfexp1$exp1altercorrect[i] = ifelse(dfexp1$treatalteredthumb_4[i] == 1 &
                                            sum(dfexp1$treatalteredthumb_1[i], dfexp1$treatalteredthumb_2[i], 
                                                dfexp1$treatalteredthumb_3[i] , dfexp1$treatalteredthumb_5[i]) == 0,
                                        1, 0)
    
    # (v) number of false negatives (true videos identified as false)
    # NB: sums across control and treatment columns since they are mutually exclusive
    dfexp1$false_negatives[i] = sum(# treatment
        dfexp1$treatordinarythumb_1[i], dfexp1$treatordinarythumb_2[i], 
        dfexp1$treatordinarythumb_3[i], dfexp1$treatordinarythumb_5[i],
        # control
        dfexp1$contordinarythumb_1[i], dfexp1$contordinarythumb_2[i], 
        dfexp1$contordinarythumb_3[i], dfexp1$contordinarythumb_4[i], 
        dfexp1$contordinarythumb_5[i],
        na.rm = T)
}

## create treatment column + single column variables
dfexp1$treatment       = NA
dfexp1$out_of_ordinary = NA
dfexp1$familiar_tc     = NA
dfexp1$aware_df        = NA
dfexp1$dfslider        = NA
dfexp1$internet_use    = NA
dfexp1$proficient      = NA

# convert (1=Yes) and (2=No) variables into a binary variable (1=Yes) and (0=No)
dfexp1$contordinary  = ifelse(dfexp1$contordinary == 2, 0, dfexp1$contordinary)
dfexp1$treatordinary = ifelse(dfexp1$treatordinary == 2, 0, dfexp1$treatordinary)
dfexp1$contawaredf   = ifelse(dfexp1$contawaredf == 2, 0, dfexp1$contawaredf)
dfexp1$treatawaredf  = ifelse(dfexp1$treatawaredf == 2, 0, dfexp1$treatawaredf)

for(i in 1:nrow(dfexp1)){
    bControl   = dfexp1$contordinary[i] 
    bTreatment = dfexp1$treatordinary[i] 
    
    if(!is.na(bControl) | !is.na(bTreatment)){ # observation is in experiment 1
        iType = which( !is.na(c(bControl, bTreatment)) )
        
        # construct combined out of ordinary column and treatment dummy variable
        if(iType == 1){ # control
            dfexp1$treatment[i]       = 0
            dfexp1$out_of_ordinary[i] = bControl
            dfexp1$familiar_tc[i]     = as.numeric(dfexp1$contfamiliar_1[i])
            dfexp1$aware_df[i]        = dfexp1$contawaredf[i]
            dfexp1$dfslider[i]        = dfexp1$contdfslider_1[i]
            dfexp1$internet_use[i]    = dfexp1$continternet_1[i]
            dfexp1$proficient[i]      = dfexp1$contproficient_1[i]
            
        } else if(iType == 2){ # treatment
            dfexp1$treatment[i]       = 1
            
            dfexp1$out_of_ordinary[i] = bTreatment
            dfexp1$familiar_tc[i]     = as.numeric(dfexp1$treatfamiliar_1[i])
            dfexp1$aware_df[i]        = dfexp1$treatawaredf[i]
            dfexp1$dfslider[i]        = dfexp1$treatdfslider_1[i]
            dfexp1$internet_use[i]    = dfexp1$treatinternet_1[i]
            dfexp1$proficient[i]      = dfexp1$treatproficient_1[i]
        }
    }
}

# filter out experiment 2 observations
dfexp1 = dfexp1[!is.na(dfexp1$out_of_ordinary),]

# crete gender variable
dfexp1$female = ifelse(dfexp1$qgender == 1, 0, 1)

# create indicator variables from sliding-scale variables
# define those (i) proficient navigating social media and (ii) internet use, as self-reports above the median
dfexp1$proficient_dummy = ifelse(dfexp1$proficient > median(dfexp1$proficient), 1, 0)
dfexp1$internet_use_dummy = ifelse(dfexp1$internet_use > median(dfexp1$internet_use), 1, 0)
dfexp1$familiar_tc_dummy = ifelse(dfexp1$familiar_tc > median(dfexp1$familiar_tc), 1, 0)
dfexp1$age_dummy = ifelse(dfexp1$qage < median(dfexp1$qage), 1, 0)

# select variables
dfexp1 = dfexp1 %>%
    select(experiment, out_of_ordinary, treatment, false_negatives,
           qage, female, aware_df, proficient_dummy, age_dummy, internet_use_dummy, familiar_tc, familiar_tc_dummy,
           exp1correct, exp1choosedf, exp1chooseone,
           treatordinarythumb_1, treatordinarythumb_2, treatordinarythumb_3, treatordinarythumb_4, treatordinarythumb_5,
           contordinarythumb_1, contordinarythumb_2, contordinarythumb_3, contordinarythumb_4, contordinarythumb_5,
           proficient)

#-------------------------------------------------------------------------------------------------
# Experiment 2: only T2
#-------------------------------------------------------------------------------------------------
# convert to correct class
dfraw$exp2dfobv       = as.numeric(dfraw$exp2dfobv)
dfraw$exp2dfthumb_1   = as.numeric(dfraw$exp2dfthumb_1)
dfraw$exp2dfthumb_1   = ifelse(is.na(dfraw$exp2dfthumb_1), 0, 1)
dfraw$exp2dfthumb_2   = as.numeric(dfraw$exp2dfthumb_2)
dfraw$exp2dfthumb_2   = ifelse(is.na(dfraw$exp2dfthumb_2), 0, 1)
dfraw$exp2dfthumb_3   = as.numeric(dfraw$exp2dfthumb_3)
dfraw$exp2dfthumb_3   = ifelse(is.na(dfraw$exp2dfthumb_3), 0, 1)
dfraw$exp2dfthumb_4   = as.numeric(dfraw$exp2dfthumb_4)
dfraw$exp2dfthumb_4   = ifelse(is.na(dfraw$exp2dfthumb_4), 0, 1)
dfraw$exp2dfthumb_5   = as.numeric(dfraw$exp2dfthumb_5)
dfraw$exp2dfthumb_5   = ifelse(is.na(dfraw$exp2dfthumb_5), 0, 1)
dfraw$exp2confident_1 = as.numeric(dfraw$exp2confident_1)

dfraw$exp2familiar_1   = as.numeric(dfraw$exp2familiar_1)
dfraw$exp2dfslider_1   = as.numeric(dfraw$exp2dfslider_1)
dfraw$exp2awaredf      = as.numeric(dfraw$exp2awaredf)
dfraw$exp2internet_1   = as.numeric(dfraw$exp2internet_1)
dfraw$exp2proficient_1 = as.numeric(dfraw$exp2proficient_1)
dfraw$exp2awaredf      = ifelse(dfraw$exp2awaredf == 2, 0, dfraw$exp2awaredf)

# select columns
dfexp2 = dfraw %>%
    select(qgender, qage,
           exp2dfobv, exp2dfthumb_1, exp2dfthumb_2, exp2dfthumb_3, exp2dfthumb_4, exp2dfthumb_5, exp2confident_1,
           exp2dfwords, exp2familiar_1, 
           exp2seenbefore, exp2seenbeforethumb_1, exp2seenbeforethumb_2, exp2seenbeforethumb_3, exp2seenbeforethumb_4, exp2seenbeforethumb_5,
           exp2awaredf, exp2dfslider_1, exp2internet_1, exp2proficient_1
    )

# create variables: (i) correct detection variable and (ii) dummy for only choosing one video
dfexp2$exp2correct     = NA
dfexp2$onlyonechoice   = NA
dfexp2$false_negatives = NA
for(i in 1:nrow(dfexp2)){
    # (i) correct detection variable (choose video 4 and no other videos)
    dfexp2$exp2correct[i] = ifelse(dfexp2$exp2dfthumb_4[i] == 1 &
                                   sum(dfexp2$exp2dfthumb_1[i], dfexp2$exp2dfthumb_2[i], 
                                       dfexp2$exp2dfthumb_3[i] , dfexp2$exp2dfthumb_5[i]) == 0, 1, 0)
    # (ii) dummy for only choosing one video
    dfexp2$onlyonechoice[i] = ifelse(sum(dfexp2$exp2dfthumb_1[i], dfexp2$exp2dfthumb_2[i], dfexp2$exp2dfthumb_3[i],
                                         dfexp2$exp2dfthumb_4[i], dfexp2$exp2dfthumb_5[i]) == 1, 1, 0)
    
    # (v) number of false negatives (true videos identified as false)
    dfexp2$false_negatives[i] = sum(# treatment
        dfexp2$exp2dfthumb_1[i], dfexp2$exp2dfthumb_2[i], 
        dfexp2$exp2dfthumb_3[i], dfexp2$exp2dfthumb_5[i],
        na.rm = T)
}

# create number of videos selected variable
dfexp2 = mutate(dfexp2, numberselected = dfexp2$exp2dfthumb_1 +  dfexp2$exp2dfthumb_2 + dfexp2$exp2dfthumb_3 + dfexp2$exp2dfthumb_4 + dfexp2$exp2dfthumb_5)

# crete gender variable
dfexp2$female = ifelse(dfexp2$qgender == 1, 0, 1)

# create confidence categorical variable
dfexp2$Confidence = NA
dfexp2$Confidence = ifelse(dfexp2$exp2confident_1 <= 2, 'Low', dfexp2$Confidence)
dfexp2$Confidence = ifelse(dfexp2$exp2confident_1 > 2 & dfexp2$exp2confident_1 < 8, 'Medium', dfexp2$Confidence)
dfexp2$Confidence = ifelse(dfexp2$exp2confident_1 >=8, 'High', dfexp2$Confidence)

# filter out those in experiment 1
dfexp2 = dfexp2[!is.na(dfexp2$exp2dfobv),] 

# rename columns
RenameColumn = function(cFrom, cTo, df){
    iColumn = grep(cFrom, names(df))
    colnames(df)[iColumn] = cTo
    return(df)
}

dfexp2 = RenameColumn('exp2familiar_1', 'familiar_tc', dfexp2)
dfexp2 = RenameColumn('exp2awaredf', 'aware_df', dfexp2)
dfexp2 = RenameColumn('exp2dfslider_1', 'dfslider', dfexp2)
dfexp2 = RenameColumn('exp2internet_1', 'internet_use', dfexp2)
dfexp2 = RenameColumn('exp2proficient_1', 'proficient', dfexp2)

# create indicator variables from sliding-scale variables
# define those (i) proficient navigating social media and (ii) internet use, as self-reports above the median
dfexp2$proficient_dummy = ifelse(dfexp2$proficient > median(dfexp2$proficient), 1, 0)
dfexp2$internet_use_dummy = ifelse(dfexp2$internet_use > median(dfexp2$internet_use), 1, 0)
dfexp2$familiar_tc_dummy = ifelse(dfexp2$familiar_tc > median(dfexp2$familiar_tc, na.rm = T), 1, 0)
dfexp2$age_dummy = as.numeric(ifelse(dfexp2$qage < median(dfexp2$qage, na.rm = T), 1, 0))


#-------------------------------------------------------------------------------------------------
# Experiment 2: T1 and T2
#-------------------------------------------------------------------------------------------------
temp1 = dfexp1 %>%
    filter(treatment == 1) %>%
    select(exp1correct, false_negatives, qage, aware_df, proficient_dummy, internet_use_dummy, familiar_tc, familiar_tc_dummy, age_dummy, female,
           out_of_ordinary)
temp1$treatment = 0
colnames(temp1)[1] = 'correct'

temp2 = dfexp2 %>%
    select(exp2correct, false_negatives, qage, aware_df, proficient_dummy, internet_use_dummy, familiar_tc, familiar_tc_dummy, age_dummy, female)
temp2$out_of_ordinary = NA
temp2$treatment = 1
colnames(temp2)[1] = 'correct'

dfexp2_reg = rbind.data.frame(temp1, temp2)


#-------------------------------------------------------------------------------------------------
# save cleaned data
#-------------------------------------------------------------------------------------------------
df = dfexp1
save(df, file = 'dfexp1.Rda')

df = dfexp2
save(df, file = 'dfexp2_het.Rda')

df = dfexp2_reg
save(df, file = 'dfexp2_reg.Rda')


