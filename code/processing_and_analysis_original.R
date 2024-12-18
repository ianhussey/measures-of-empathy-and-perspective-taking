
# author: Markus Dieterich

# Set working directory
setwd("D:/Studium/7. Semester/S Projektseminar MTI/Bachelorthesis/Timepoint 1 and 2 Qualtrics und R")
#
# Install needed packages
install.packages("dplyr")
library("dplyr")
install.packages("psych")
library("psych")
install.packages("irr")
library("irr")
install.packages("lme4")
library("lme4")
#
options(scipen=999)
#
# Read Data
timepoint1 <- read.csv("Test-Retest - 1 of 2_July 10, 2023_08.39.csv")
timepoint2 <- read.csv("Test-Retest - 2 of 2_July 20, 2023_23.39.csv")
#
dim(timepoint1)
names(timepoint1)
#
# Delete Tests and unnecessary rows
timepoint1 <- timepoint1[-c(1:7),]
timepoint2 <- timepoint2[-c(1:4),]
#
############## Filter variables of interest ###################
timepoint1.use <- timepoint1 %>% select(Consent_question,Age_1,Gender,Gender_3_TEXT,Prolific_ID_Entry,          
                                        SITES,IRI_1,IRI_2,IRI_3,IRI_4,IRI_5,IRI_6,IRI_7,IRI_8,IRI_9,IRI_10,IRI_11,IRI_12,IRI_13,IRI_14,IRI_15,           
                                        IRI_16,IRI_17,IRI_18,IRI_19,IRI_20,IRI_21,IRI_22,IRI_23,IRI_24,IRI_25,IRI_26,IRI_27,IRI_28,IRI_29,           
                                        EQ_1,EQ_2,EQ_3,EQ_4,EQ_5,EQ_6,EQ_7,EQ_8,EQ_9,EQ_10,EQ_11,EQ_12,EQ_13,EQ_14,EQ_15,EQ_16,EQ_17,EQ_18,
                                        EQ_19,EQ_20,EQ_21,EQ_22,EQ_23,EQ_24,EQ_25,EQ_26,EQ_27,EQ_28,EQ_29,EQ_30,EQ_31,EQ_32,EQ_33,EQ_34,EQ_35,
                                        EQ_36,EQ_37,EQ_38,EQ_39,EQ_40,EQ_41,EQ_42,EQ_43,EQ_44,EQ_45,EQ_46,EQ_47,EQ_48,EQ_49,EQ_50,EQ_51,EQ_52, 
                                        EQ_53,EQ_54,EQ_55,EQ_56,EQ_57,EQ_58,EQ_59,EQ_60,EQ_61,      
                                        Q2,Q4,Q6,Q8,Q10,Q12,Q14,  
                                        EYES_1,EYES_2,EYES_3,EYES_4,EYES_5,EYES_6,EYES_7,EYES_8,EYES_9,EYES_10,EYES_11,EYES_12,EYES_13,EYES_14,
                                        EYES_15,EYES_16,EYES_17,EYES_18,EYES_19,EYES_20,EYES_21,EYES_22,EYES_23,EYES_24,EYES_25,EYES_26,EYES_27,EYES_28,EYES_29,
                                        EYES_30,EYES_31,EYES_32,EYES_33,EYES_34,EYES_35,EYES_36,PROLIFIC_PID)
#
timepoint2.use <- timepoint2 %>% select(Consent_question,Age_1,Gender,Gender_3_TEXT,Prolific_ID_Entry,          
                                        SITES,IRI_1,IRI_2,IRI_3,IRI_4,IRI_5,IRI_6,IRI_7,IRI_8,IRI_9,IRI_10,IRI_11,IRI_12,IRI_13,IRI_14,IRI_15,           
                                        IRI_16,IRI_17,IRI_18,IRI_19,IRI_20,IRI_21,IRI_22,IRI_23,IRI_24,IRI_25,IRI_26,IRI_27,IRI_28,IRI_29,           
                                        EQ_1,EQ_2,EQ_3,EQ_4,EQ_5,EQ_6,EQ_7,EQ_8,EQ_9,EQ_10,EQ_11,EQ_12,EQ_13,EQ_14,EQ_15,EQ_16,EQ_17,EQ_18,
                                        EQ_19,EQ_20,EQ_21,EQ_22,EQ_23,EQ_24,EQ_25,EQ_26,EQ_27,EQ_28,EQ_29,EQ_30,EQ_31,EQ_32,EQ_33,EQ_34,EQ_35,
                                        EQ_36,EQ_37,EQ_38,EQ_39,EQ_40,EQ_41,EQ_42,EQ_43,EQ_44,EQ_45,EQ_46,EQ_47,EQ_48,EQ_49,EQ_50,EQ_51,EQ_52, 
                                        EQ_53,EQ_54,EQ_55,EQ_56,EQ_57,EQ_58,EQ_59,EQ_60,EQ_61,      
                                        Q2,Q4,Q6,Q8,Q10,Q12,Q14,  
                                        EYES_1,EYES_2,EYES_3,EYES_4,EYES_5,EYES_6,EYES_7,EYES_8,EYES_9,EYES_10,EYES_11,EYES_12,EYES_13,EYES_14,
                                        EYES_15,EYES_16,EYES_17,EYES_18,EYES_19,EYES_20,EYES_21,EYES_22,EYES_23,EYES_24,EYES_25,EYES_26,EYES_27,EYES_28,EYES_29,
                                        EYES_30,EYES_31,EYES_32,EYES_33,EYES_34,EYES_35,EYES_36,PROLIFIC_PID) 
#
########### Rename "Q" to PET, rename attention checks items #################
timepoint1.use <- timepoint1.use %>% rename(
  PET_1 = Q2,
  PET_2 = Q4,
  PET_3 = Q6,
  PET_4 = Q8,
  PET_5 = Q10,
  PET_6 = Q12,
  PET_7 = Q14,
  ATTENTION_CHECK_1 = IRI_24,
  # Because of the attention check item, all IRI_items after 24 have to be renamed  
  IRI_24 = IRI_25,
  IRI_25 = IRI_26,
  IRI_26 = IRI_27,
  IRI_27 = IRI_28,
  IRI_28 = IRI_29,
  ATTENTION_CHECK_2 = EQ_60,
  # Because of the attention check item, EQ Item 61 has to be renamed
  EQ_60 = EQ_61
)
timepoint2.use <- timepoint2.use %>% rename(
  PET_1 = Q2,
  PET_2 = Q4,
  PET_3 = Q6,
  PET_4 = Q8,
  PET_5 = Q10,
  PET_6 = Q12,
  PET_7 = Q14,
  ATTENTION_CHECK_1 = IRI_24,
  # Because of the attention check item, all IRI_items after 24 have to be renamed  
  IRI_24 = IRI_25,
  IRI_25 = IRI_26,
  IRI_26 = IRI_27,
  IRI_27 = IRI_28,
  IRI_28 = IRI_29,
  ATTENTION_CHECK_2 = EQ_60,
  # Because of the attention check item, EQ Item 61 has to be renamed
  EQ_60 = EQ_61
)
#
# Check Attention Checks
which(timepoint1.use$ATTENTION_CHECK_1 != "C")
which(timepoint1.use$ATTENTION_CHECK_2 != "strongly agree")
which(timepoint2.use$ATTENTION_CHECK_1 != "C")
which(timepoint2.use$ATTENTION_CHECK_2 != "strongly agree")
#
# Delete cases that failed the attention checks
timepoint1.use <- timepoint1.use[-c(88:88),]
timepoint2.use <- timepoint2.use[-c(27:27),]
#
############# Change all values to numeric values - Timepoint 1 ##############
# SITES
timepoint1.use <- timepoint1.use %>% mutate(
  SITES.n = recode(SITES,
                   '1 Not very true of me' = 1,
                   '2' = 2,
                   '3' = 3,
                   '4' = 4,
                   '5 Very true of me.' = 5,
                   .default = 0
  )
)
# Check if change worked
table(timepoint1.use$SITES, timepoint1.use$SITES.n)
#
# Interpersonal reactivity Index - normal scored items
timepoint1.use <- timepoint1.use %>% mutate(
  IRI_1.n = recode(IRI_1,
                   'A - DOES NOT DESCRIBE ME VERY WELL' = 0,
                   'B' = 1,
                   'C' = 2,
                   'D' = 3,
                   'E - DESCRIBES ME VERY WELL' = 4,
                   .default = 0),
  IRI_2.n = recode(IRI_2,
                   'A - DOES NOT DESCRIBE ME VERY WELL' = 0,
                   'B' = 1,
                   'C' = 2,
                   'D' = 3,
                   'E - DESCRIBES ME VERY WELL' = 4,
                   .default = 0),
  IRI_5.n = recode(IRI_5,
                   'A - DOES NOT DESCRIBE ME VERY WELL' = 0,
                   'B' = 1,
                   'C' = 2,
                   'D' = 3,
                   'E - DESCRIBES ME VERY WELL' = 4,
                   .default = 0),
  IRI_6.n = recode(IRI_6,
                   'A - DOES NOT DESCRIBE ME VERY WELL' = 0,
                   'B' = 1,
                   'C' = 2,
                   'D' = 3,
                   'E - DESCRIBES ME VERY WELL' = 4,
                   .default = 0),
  IRI_8.n = recode(IRI_8,
                   'A - DOES NOT DESCRIBE ME VERY WELL' = 0,
                   'B' = 1,
                   'C' = 2,
                   'D' = 3,
                   'E - DESCRIBES ME VERY WELL' = 4,
                   .default = 0),
  IRI_9.n = recode(IRI_9,
                   'A - DOES NOT DESCRIBE ME VERY WELL' = 0,
                   'B' = 1,
                   'C' = 2,
                   'D' = 3,
                   'E - DESCRIBES ME VERY WELL' = 4,
                   .default = 0),
  IRI_10.n = recode(IRI_10,
                    'A - DOES NOT DESCRIBE ME VERY WELL' = 0,
                    'B' = 1,
                    'C' = 2,
                    'D' = 3,
                    'E - DESCRIBES ME VERY WELL' = 4,
                    .default = 0),
  IRI_11.n = recode(IRI_11,
                    'A - DOES NOT DESCRIBE ME VERY WELL' = 0,
                    'B' = 1,
                    'C' = 2,
                    'D' = 3,
                    'E - DESCRIBES ME VERY WELL' = 4,
                    .default = 0),
  IRI_16.n = recode(IRI_16,
                    'A - DOES NOT DESCRIBE ME VERY WELL' = 0,
                    'B' = 1,
                    'C' = 2,
                    'D' = 3,
                    'E - DESCRIBES ME VERY WELL' = 4,
                    .default = 0),
  IRI_17.n = recode(IRI_17,
                    'A - DOES NOT DESCRIBE ME VERY WELL' = 0,
                    'B' = 1,
                    'C' = 2,
                    'D' = 3,
                    'E - DESCRIBES ME VERY WELL' = 4,
                    .default = 0),
  IRI_20.n = recode(IRI_20,
                    'A - DOES NOT DESCRIBE ME VERY WELL' = 0,
                    'B' = 1,
                    'C' = 2,
                    'D' = 3,
                    'E - DESCRIBES ME VERY WELL' = 4,
                    .default = 0),
  IRI_21.n = recode(IRI_21,
                    'A - DOES NOT DESCRIBE ME VERY WELL' = 0,
                    'B' = 1,
                    'C' = 2,
                    'D' = 3,
                    'E - DESCRIBES ME VERY WELL' = 4,
                    .default = 0),
  IRI_22.n = recode(IRI_22,
                    'A - DOES NOT DESCRIBE ME VERY WELL' = 0,
                    'B' = 1,
                    'C' = 2,
                    'D' = 3,
                    'E - DESCRIBES ME VERY WELL' = 4,
                    .default = 0),
  IRI_23.n = recode(IRI_23,
                    'A - DOES NOT DESCRIBE ME VERY WELL' = 0,
                    'B' = 1,
                    'C' = 2,
                    'D' = 3,
                    'E - DESCRIBES ME VERY WELL' = 4,
                    .default = 0),
  IRI_24.n = recode(IRI_24,
                    'A - DOES NOT DESCRIBE ME VERY WELL' = 0,
                    'B' = 1,
                    'C' = 2,
                    'D' = 3,
                    'E - DESCRIBES ME VERY WELL' = 4,
                    .default = 0),
  IRI_25.n = recode(IRI_25,
                    'A - DOES NOT DESCRIBE ME VERY WELL' = 0,
                    'B' = 1,
                    'C' = 2,
                    'D' = 3,
                    'E - DESCRIBES ME VERY WELL' = 4,
                    .default = 0),
  IRI_26.n = recode(IRI_26,
                    'A - DOES NOT DESCRIBE ME VERY WELL' = 0,
                    'B' = 1,
                    'C' = 2,
                    'D' = 3,
                    'E - DESCRIBES ME VERY WELL' = 4,
                    .default = 0),
  IRI_27.n = recode(IRI_27,
                    'A - DOES NOT DESCRIBE ME VERY WELL' = 0,
                    'B' = 1,
                    'C' = 2,
                    'D' = 3,
                    'E - DESCRIBES ME VERY WELL' = 4,
                    .default = 0),
  IRI_28.n = recode(IRI_28,
                    'A - DOES NOT DESCRIBE ME VERY WELL' = 0,
                    'B' = 1,
                    'C' = 2,
                    'D' = 3,
                    'E - DESCRIBES ME VERY WELL' = 4,
                    .default = 0),
)
table(timepoint1.use$IRI_20, timepoint1.use$IRI_20.n)
#
# Interpersonal reactivity Index - reverse scored items
timepoint1.use <- timepoint1.use %>% mutate(
  IRI_3.n = recode(IRI_3,
                   'A - DOES NOT DESCRIBE ME VERY WELL' = 4,
                   'B' = 3,
                   'C' = 2,
                   'D' = 1,
                   'E - DESCRIBES ME VERY WELL' = 0,
                   .default = 0),
  IRI_4.n = recode(IRI_4,
                   'A - DOES NOT DESCRIBE ME VERY WELL' = 4,
                   'B' = 3,
                   'C' = 2,
                   'D' = 1,
                   'E - DESCRIBES ME VERY WELL' = 0,
                   .default = 0),
  IRI_7.n = recode(IRI_7,
                   'A - DOES NOT DESCRIBE ME VERY WELL' = 4,
                   'B' = 3,
                   'C' = 2,
                   'D' = 1,
                   'E - DESCRIBES ME VERY WELL' = 0,
                   .default = 0),
  IRI_12.n = recode(IRI_12,
                    'A - DOES NOT DESCRIBE ME VERY WELL' = 4,
                    'B' = 3,
                    'C' = 2,
                    'D' = 1,
                    'E - DESCRIBES ME VERY WELL' = 0,
                    .default = 0),
  IRI_13.n = recode(IRI_13,
                    'A - DOES NOT DESCRIBE ME VERY WELL' = 4,
                    'B' = 3,
                    'C' = 2,
                    'D' = 1,
                    'E - DESCRIBES ME VERY WELL' = 0,
                    .default = 0),
  IRI_14.n = recode(IRI_14,
                    'A - DOES NOT DESCRIBE ME VERY WELL' = 4,
                    'B' = 3,
                    'C' = 2,
                    'D' = 1,
                    'E - DESCRIBES ME VERY WELL' = 0,
                    .default = 0),
  IRI_15.n = recode(IRI_15,
                    'A - DOES NOT DESCRIBE ME VERY WELL' = 4,
                    'B' = 3,
                    'C' = 2,
                    'D' = 1,
                    'E - DESCRIBES ME VERY WELL' = 0,
                    .default = 0),
  IRI_18.n = recode(IRI_18,
                    'A - DOES NOT DESCRIBE ME VERY WELL' = 4,
                    'B' = 3,
                    'C' = 2,
                    'D' = 1,
                    'E - DESCRIBES ME VERY WELL' = 0,
                    .default = 0),
  IRI_19.n = recode(IRI_19,
                    'A - DOES NOT DESCRIBE ME VERY WELL' = 4,
                    'B' = 3,
                    'C' = 2,
                    'D' = 1,
                    'E - DESCRIBES ME VERY WELL' = 0,
                    .default = 0),
  
)
table(timepoint1.use$IRI_18, timepoint1.use$IRI_18.n)
#
# Empathy Quotient
timepoint1.use <- timepoint1.use %>% mutate(
  EQ_1.n = recode(EQ_1,
                  'strongly agree' = 2,
                  'slightly agree' = 1,
                  'slightly disagree' = 0,
                  'strongly disagree' = 0,
                  .default = 0),
  EQ_4.n = recode(EQ_4,
                  'strongly agree' = 0,
                  'slightly agree' = 0,
                  'slightly disagree' = 1,
                  'strongly disagree' = 2,
                  .default = 0),
  EQ_6.n = recode(EQ_6,
                  'strongly agree' = 2,
                  'slightly agree' = 1,
                  'slightly disagree' = 0,
                  'strongly disagree' = 0,
                  .default = 0),
  EQ_8.n = recode(EQ_8,
                  'strongly agree' = 0,
                  'slightly agree' = 0,
                  'slightly disagree' = 1,
                  'strongly disagree' = 2,
                  .default = 0),
  EQ_10.n = recode(EQ_10,
                   'strongly agree' = 0,
                   'slightly agree' = 0,
                   'slightly disagree' = 1,
                   'strongly disagree' = 2,
                   .default = 0),
  EQ_11.n = recode(EQ_11,
                   'strongly agree' = 0,
                   'slightly agree' = 0,
                   'slightly disagree' = 1,
                   'strongly disagree' = 2,
                   .default = 0),
  EQ_12.n = recode(EQ_12,
                   'strongly agree' = 0,
                   'slightly agree' = 0,
                   'slightly disagree' = 1,
                   'strongly disagree' = 2,
                   .default = 0),
  EQ_14.n = recode(EQ_14,
                   'strongly agree' = 0,
                   'slightly agree' = 0,
                   'slightly disagree' = 1,
                   'strongly disagree' = 2,
                   .default = 0),
  EQ_15.n = recode(EQ_15,
                   'strongly agree' = 0,
                   'slightly agree' = 0,
                   'slightly disagree' = 1,
                   'strongly disagree' = 2,
                   .default = 0),
  EQ_18.n = recode(EQ_18,
                   'strongly agree' = 0,
                   'slightly agree' = 0,
                   'slightly disagree' = 1,
                   'strongly disagree' = 2,
                   .default = 0),
  EQ_19.n = recode(EQ_19,
                   'strongly agree' = 2,
                   'slightly agree' = 1,
                   'slightly disagree' = 0,
                   'strongly disagree' = 0,
                   .default = 0),
  EQ_21.n = recode(EQ_21,
                   'strongly agree' = 0,
                   'slightly agree' = 0,
                   'slightly disagree' = 1,
                   'strongly disagree' = 2,
                   .default = 0),
  EQ_22.n = recode(EQ_22,
                   'strongly agree' = 2,
                   'slightly agree' = 1,
                   'slightly disagree' = 0,
                   'strongly disagree' = 0,
                   .default = 0),
  EQ_25.n = recode(EQ_25,
                   'strongly agree' = 2,
                   'slightly agree' = 1,
                   'slightly disagree' = 0,
                   'strongly disagree' = 0,
                   .default = 0),
  EQ_26.n = recode(EQ_26,
                   'strongly agree' = 2,
                   'slightly agree' = 1,
                   'slightly disagree' = 0,
                   'strongly disagree' = 0,
                   .default = 0),
  EQ_27.n = recode(EQ_27,
                   'strongly agree' = 0,
                   'slightly agree' = 0,
                   'slightly disagree' = 1,
                   'strongly disagree' = 2,
                   .default = 0),
  EQ_28.n = recode(EQ_28,
                   'strongly agree' = 0,
                   'slightly agree' = 0,
                   'slightly disagree' = 1,
                   'strongly disagree' = 2,
                   .default = 0),
  EQ_29.n = recode(EQ_29,
                   'strongly agree' = 0,
                   'slightly agree' = 0,
                   'slightly disagree' = 1,
                   'strongly disagree' = 2,
                   .default = 0),
  EQ_32.n = recode(EQ_32,
                   'strongly agree' = 0,
                   'slightly agree' = 0,
                   'slightly disagree' = 1,
                   'strongly disagree' = 2,
                   .default = 0),
  EQ_34.n = recode(EQ_34,
                   'strongly agree' = 0,
                   'slightly agree' = 0,
                   'slightly disagree' = 1,
                   'strongly disagree' = 2,
                   .default = 0),
  EQ_35.n = recode(EQ_35,
                   'strongly agree' = 2,
                   'slightly agree' = 1,
                   'slightly disagree' = 0,
                   'strongly disagree' = 0,
                   .default = 0),
  EQ_36.n = recode(EQ_36,
                   'strongly agree' = 2,
                   'slightly agree' = 1,
                   'slightly disagree' = 0,
                   'strongly disagree' = 0,
                   .default = 0),
  EQ_37.n = recode(EQ_37,
                   'strongly agree' = 2,
                   'slightly agree' = 1,
                   'slightly disagree' = 0,
                   'strongly disagree' = 0,
                   .default = 0),
  EQ_38.n = recode(EQ_38,
                   'strongly agree' = 2,
                   'slightly agree' = 1,
                   'slightly disagree' = 0,
                   'strongly disagree' = 0,
                   .default = 0),
  EQ_39.n = recode(EQ_39,
                   'strongly agree' = 0,
                   'slightly agree' = 0,
                   'slightly disagree' = 1,
                   'strongly disagree' = 2,
                   .default = 0),
  EQ_41.n = recode(EQ_41,
                   'strongly agree' = 2,
                   'slightly agree' = 1,
                   'slightly disagree' = 0,
                   'strongly disagree' = 0,
                   .default = 0),
  EQ_42.n = recode(EQ_42,
                   'strongly agree' = 2,
                   'slightly agree' = 1,
                   'slightly disagree' = 0,
                   'strongly disagree' = 0,
                   .default = 0),
  EQ_43.n = recode(EQ_43,
                   'strongly agree' = 2,
                   'slightly agree' = 1,
                   'slightly disagree' = 0,
                   'strongly disagree' = 0,
                   .default = 0),
  EQ_44.n = recode(EQ_44,
                   'strongly agree' = 2,
                   'slightly agree' = 1,
                   'slightly disagree' = 0,
                   'strongly disagree' = 0,
                   .default = 0),
  EQ_46.n = recode(EQ_46,
                   'strongly agree' = 0,
                   'slightly agree' = 0,
                   'slightly disagree' = 1,
                   'strongly disagree' = 2,
                   .default = 0),
  EQ_48.n = recode(EQ_48,
                   'strongly agree' = 0,
                   'slightly agree' = 0,
                   'slightly disagree' = 1,
                   'strongly disagree' = 2,
                   .default = 0),
  EQ_49.n = recode(EQ_49,
                   'strongly agree' = 0,
                   'slightly agree' = 0,
                   'slightly disagree' = 1,
                   'strongly disagree' = 2,
                   .default = 0),
  EQ_50.n = recode(EQ_50,
                   'strongly agree' = 0,
                   'slightly agree' = 0,
                   'slightly disagree' = 1,
                   'strongly disagree' = 2,
                   .default = 0),
  EQ_52.n = recode(EQ_52,
                   'strongly agree' = 2,
                   'slightly agree' = 1,
                   'slightly disagree' = 0,
                   'strongly disagree' = 0,
                   .default = 0),
  EQ_54.n = recode(EQ_54,
                   'strongly agree' = 2,
                   'slightly agree' = 1,
                   'slightly disagree' = 0,
                   'strongly disagree' = 0,
                   .default = 0),
  EQ_55.n = recode(EQ_55,
                   'strongly agree' = 2,
                   'slightly agree' = 1,
                   'slightly disagree' = 0,
                   'strongly disagree' = 0,
                   .default = 0),
  EQ_57.n = recode(EQ_57,
                   'strongly agree' = 2,
                   'slightly agree' = 1,
                   'slightly disagree' = 0,
                   'strongly disagree' = 0,
                   .default = 0),
  EQ_58.n = recode(EQ_58,
                   'strongly agree' = 2,
                   'slightly agree' = 1,
                   'slightly disagree' = 0,
                   'strongly disagree' = 0,
                   .default = 0),
  EQ_59.n = recode(EQ_59,
                   'strongly agree' = 2,
                   'slightly agree' = 1,
                   'slightly disagree' = 0,
                   'strongly disagree' = 0,
                   .default = 0),
  EQ_60.n = recode(EQ_60,
                   'strongly agree' = 2,
                   'slightly agree' = 1,
                   'slightly disagree' = 0,
                   'strongly disagree' = 0,
                   .default = 0),
)
table(timepoint1.use$EQ_1, timepoint1.use$EQ_1.n)
table(timepoint1.use$EQ_50, timepoint1.use$EQ_50.n)
#
# Pictorial Empathy Test
timepoint1.use <- timepoint1.use %>% mutate(
  PET_1.n = recode(PET_1,
                   'not at all' = 1,
                   'a little bit' = 2,
                   'it arouses some feelings' = 3,
                   'quite a lot' = 4,
                   'very much' = 5,
                   .default = 0),
  PET_2.n = recode(PET_2,
                   'not at all' = 1,
                   'a little bit' = 2,
                   'it arouses some feelings' = 3,
                   'quite a lot' = 4,
                   'very much' = 5,
                   .default = 0),
  PET_3.n = recode(PET_3,
                   'not at all' = 1,
                   'a little bit' = 2,
                   'it arouses some feelings' = 3,
                   'quite a lot' = 4,
                   'very much' = 5,
                   .default = 0),
  PET_4.n = recode(PET_4,
                   'not at all' = 1,
                   'a little bit' = 2,
                   'it arouses some feelings' = 3,
                   'quite a lot' = 4,
                   'very much' = 5,
                   .default = 0),
  PET_5.n = recode(PET_5,
                   'not at all' = 1,
                   'a little bit' = 2,
                   'it arouses some feelings' = 3,
                   'quite a lot' = 4,
                   'very much' = 5,
                   .default = 0),
  PET_6.n = recode(PET_6,
                   'not at all' = 1,
                   'a little bit' = 2,
                   'it arouses some feelings' = 3,
                   'quite a lot' = 4,
                   'very much' = 5,
                   .default = 0),
  PET_7.n = recode(PET_7,
                   'not at all' = 1,
                   'a little bit' = 2,
                   'it arouses some feelings' = 3,
                   'quite a lot' = 4,
                   'very much' = 5,
                   .default = 0)
)
table(timepoint1.use$PET_6, timepoint1.use$PET_6.n)
#
# Reading the Mind in the Eyes
timepoint1.use <- timepoint1.use %>% mutate(
  EYES_1.n = recode(EYES_1,
                    'playful' = 1,
                    .default = 0),
  EYES_2.n = recode(EYES_2,
                    'upset' = 1,
                    .default = 0),
  EYES_3.n = recode(EYES_3,
                    'desire' = 1,
                    .default = 0),
  EYES_4.n = recode(EYES_4,
                    'insisting' = 1,
                    .default = 0),
  EYES_5.n = recode(EYES_5,
                    'worried' = 1,
                    .default = 0),
  EYES_6.n = recode(EYES_6,
                    'fantasizing' = 1,
                    .default = 0),
  EYES_7.n = recode(EYES_7,
                    'uneasy' = 1,
                    .default = 0),
  EYES_8.n = recode(EYES_8,
                    'despondent' = 1,
                    .default = 0),
  EYES_9.n = recode(EYES_9,
                    'preoccupied' = 1,
                    .default = 0),
  EYES_10.n = recode(EYES_10,
                     'cautious' = 1,
                     .default = 0),
  EYES_11.n = recode(EYES_11,
                     'regretful' = 1,
                     .default = 0),
  EYES_12.n = recode(EYES_12,
                     'sceptical' = 1,
                     .default = 0),
  EYES_13.n = recode(EYES_13,
                     'anticipating' = 1,
                     .default = 0),
  EYES_14.n = recode(EYES_14,
                     'accusing' = 1,
                     .default = 0),
  EYES_15.n = recode(EYES_15,
                     'contemplative' = 1,
                     .default = 0),
  EYES_16.n = recode(EYES_16,
                     'thoughtful' = 1,
                     .default = 0),
  EYES_17.n = recode(EYES_17,
                     'doubtful' = 1,
                     .default = 0),
  EYES_18.n = recode(EYES_18,
                     'decisive' = 1,
                     .default = 0),
  EYES_19.n = recode(EYES_19,
                     'tentative' = 1,
                     .default = 0),
  EYES_20.n = recode(EYES_20,
                     'friendly' = 1,
                     .default = 0),
  EYES_21.n = recode(EYES_21,
                     'fantasizing' = 1,
                     .default = 0),
  EYES_22.n = recode(EYES_22,
                     'preoccupied' = 1,
                     .default = 0),
  EYES_23.n = recode(EYES_23,
                     'defiant' = 1,
                     .default = 0),
  EYES_24.n = recode(EYES_24,
                     'pensive' = 1,
                     .default = 0),
  EYES_25.n = recode(EYES_25,
                     'interested' = 1,
                     .default = 0),
  EYES_26.n = recode(EYES_26,
                     'hostile' = 1,
                     .default = 0),
  EYES_27.n = recode(EYES_27,
                     'cautious' = 1,
                     .default = 0),
  EYES_28.n = recode(EYES_28,
                     'interested' = 1,
                     .default = 0),
  EYES_29.n = recode(EYES_29,
                     'reflective' = 1,
                     .default = 0),
  
  EYES_30.n = recode(EYES_30,
                     'flirtatious' = 1,
                     .default = 0),
  EYES_31.n = recode(EYES_31,
                     'confident' = 1,
                     .default = 0),
  EYES_32.n = recode(EYES_32,
                     'serious' = 1,
                     .default = 0),
  EYES_33.n = recode(EYES_33,
                     'concerned' = 1,
                     .default = 0),
  EYES_34.n = recode(EYES_34,
                     'distrustful' = 1,
                     .default = 0),
  EYES_35.n = recode(EYES_35,
                     'nervous' = 1,
                     .default = 0),
  EYES_36.n = recode(EYES_36,
                     'suspicious' = 1,
                     .default = 0)
)
table(timepoint1.use$EYES_27, timepoint1.use$EYES_27.n)
#
#
############# Change all values to numeric values - Timepoint 2 ##############
# SITES
timepoint2.use <- timepoint2.use %>% mutate(
  SITES.n = recode(SITES,
                   '1 Not very true of me' = 1,
                   '2' = 2,
                   '3' = 3,
                   '4' = 4,
                   '5 Very true of me.' = 5,
                   .default = 0
  )
)
# Check if change worked
table(timepoint2.use$SITES, timepoint2.use$SITES.n)
#
# Interpersonal reactivity Index - normal scored items
timepoint2.use <- timepoint2.use %>% mutate(
  IRI_1.n = recode(IRI_1,
                   'A - DOES NOT DESCRIBE ME VERY WELL' = 0,
                   'B' = 1,
                   'C' = 2,
                   'D' = 3,
                   'E - DESCRIBES ME VERY WELL' = 4,
                   .default = 0),
  IRI_2.n = recode(IRI_2,
                   'A - DOES NOT DESCRIBE ME VERY WELL' = 0,
                   'B' = 1,
                   'C' = 2,
                   'D' = 3,
                   'E - DESCRIBES ME VERY WELL' = 4,
                   .default = 0),
  IRI_5.n = recode(IRI_5,
                   'A - DOES NOT DESCRIBE ME VERY WELL' = 0,
                   'B' = 1,
                   'C' = 2,
                   'D' = 3,
                   'E - DESCRIBES ME VERY WELL' = 4,
                   .default = 0),
  IRI_6.n = recode(IRI_6,
                   'A - DOES NOT DESCRIBE ME VERY WELL' = 0,
                   'B' = 1,
                   'C' = 2,
                   'D' = 3,
                   'E - DESCRIBES ME VERY WELL' = 4,
                   .default = 0),
  IRI_8.n = recode(IRI_8,
                   'A - DOES NOT DESCRIBE ME VERY WELL' = 0,
                   'B' = 1,
                   'C' = 2,
                   'D' = 3,
                   'E - DESCRIBES ME VERY WELL' = 4,
                   .default = 0),
  IRI_9.n = recode(IRI_9,
                   'A - DOES NOT DESCRIBE ME VERY WELL' = 0,
                   'B' = 1,
                   'C' = 2,
                   'D' = 3,
                   'E - DESCRIBES ME VERY WELL' = 4,
                   .default = 0),
  IRI_10.n = recode(IRI_10,
                    'A - DOES NOT DESCRIBE ME VERY WELL' = 0,
                    'B' = 1,
                    'C' = 2,
                    'D' = 3,
                    'E - DESCRIBES ME VERY WELL' = 4,
                    .default = 0),
  IRI_11.n = recode(IRI_11,
                    'A - DOES NOT DESCRIBE ME VERY WELL' = 0,
                    'B' = 1,
                    'C' = 2,
                    'D' = 3,
                    'E - DESCRIBES ME VERY WELL' = 4,
                    .default = 0),
  IRI_16.n = recode(IRI_16,
                    'A - DOES NOT DESCRIBE ME VERY WELL' = 0,
                    'B' = 1,
                    'C' = 2,
                    'D' = 3,
                    'E - DESCRIBES ME VERY WELL' = 4,
                    .default = 0),
  IRI_17.n = recode(IRI_17,
                    'A - DOES NOT DESCRIBE ME VERY WELL' = 0,
                    'B' = 1,
                    'C' = 2,
                    'D' = 3,
                    'E - DESCRIBES ME VERY WELL' = 4,
                    .default = 0),
  IRI_20.n = recode(IRI_20,
                    'A - DOES NOT DESCRIBE ME VERY WELL' = 0,
                    'B' = 1,
                    'C' = 2,
                    'D' = 3,
                    'E - DESCRIBES ME VERY WELL' = 4,
                    .default = 0),
  IRI_21.n = recode(IRI_21,
                    'A - DOES NOT DESCRIBE ME VERY WELL' = 0,
                    'B' = 1,
                    'C' = 2,
                    'D' = 3,
                    'E - DESCRIBES ME VERY WELL' = 4,
                    .default = 0),
  IRI_22.n = recode(IRI_22,
                    'A - DOES NOT DESCRIBE ME VERY WELL' = 0,
                    'B' = 1,
                    'C' = 2,
                    'D' = 3,
                    'E - DESCRIBES ME VERY WELL' = 4,
                    .default = 0),
  IRI_23.n = recode(IRI_23,
                    'A - DOES NOT DESCRIBE ME VERY WELL' = 0,
                    'B' = 1,
                    'C' = 2,
                    'D' = 3,
                    'E - DESCRIBES ME VERY WELL' = 4,
                    .default = 0),
  IRI_24.n = recode(IRI_24,
                    'A - DOES NOT DESCRIBE ME VERY WELL' = 0,
                    'B' = 1,
                    'C' = 2,
                    'D' = 3,
                    'E - DESCRIBES ME VERY WELL' = 4,
                    .default = 0),
  IRI_25.n = recode(IRI_25,
                    'A - DOES NOT DESCRIBE ME VERY WELL' = 0,
                    'B' = 1,
                    'C' = 2,
                    'D' = 3,
                    'E - DESCRIBES ME VERY WELL' = 4,
                    .default = 0),
  IRI_26.n = recode(IRI_26,
                    'A - DOES NOT DESCRIBE ME VERY WELL' = 0,
                    'B' = 1,
                    'C' = 2,
                    'D' = 3,
                    'E - DESCRIBES ME VERY WELL' = 4,
                    .default = 0),
  IRI_27.n = recode(IRI_27,
                    'A - DOES NOT DESCRIBE ME VERY WELL' = 0,
                    'B' = 1,
                    'C' = 2,
                    'D' = 3,
                    'E - DESCRIBES ME VERY WELL' = 4,
                    .default = 0),
  IRI_28.n = recode(IRI_28,
                    'A - DOES NOT DESCRIBE ME VERY WELL' = 0,
                    'B' = 1,
                    'C' = 2,
                    'D' = 3,
                    'E - DESCRIBES ME VERY WELL' = 4,
                    .default = 0),
)
table(timepoint2.use$IRI_20, timepoint2.use$IRI_20.n)
#
# Interpersonal reactivity Index - reverse scored items
timepoint2.use <- timepoint2.use %>% mutate(
  IRI_3.n = recode(IRI_3,
                   'A - DOES NOT DESCRIBE ME VERY WELL' = 4,
                   'B' = 3,
                   'C' = 2,
                   'D' = 1,
                   'E - DESCRIBES ME VERY WELL' = 0,
                   .default = 0),
  IRI_4.n = recode(IRI_4,
                   'A - DOES NOT DESCRIBE ME VERY WELL' = 4,
                   'B' = 3,
                   'C' = 2,
                   'D' = 1,
                   'E - DESCRIBES ME VERY WELL' = 0,
                   .default = 0),
  IRI_7.n = recode(IRI_7,
                   'A - DOES NOT DESCRIBE ME VERY WELL' = 4,
                   'B' = 3,
                   'C' = 2,
                   'D' = 1,
                   'E - DESCRIBES ME VERY WELL' = 0,
                   .default = 0),
  IRI_12.n = recode(IRI_12,
                    'A - DOES NOT DESCRIBE ME VERY WELL' = 4,
                    'B' = 3,
                    'C' = 2,
                    'D' = 1,
                    'E - DESCRIBES ME VERY WELL' = 0,
                    .default = 0),
  IRI_13.n = recode(IRI_13,
                    'A - DOES NOT DESCRIBE ME VERY WELL' = 4,
                    'B' = 3,
                    'C' = 2,
                    'D' = 1,
                    'E - DESCRIBES ME VERY WELL' = 0,
                    .default = 0),
  IRI_14.n = recode(IRI_14,
                    'A - DOES NOT DESCRIBE ME VERY WELL' = 4,
                    'B' = 3,
                    'C' = 2,
                    'D' = 1,
                    'E - DESCRIBES ME VERY WELL' = 0,
                    .default = 0),
  IRI_15.n = recode(IRI_15,
                    'A - DOES NOT DESCRIBE ME VERY WELL' = 4,
                    'B' = 3,
                    'C' = 2,
                    'D' = 1,
                    'E - DESCRIBES ME VERY WELL' = 0,
                    .default = 0),
  IRI_18.n = recode(IRI_18,
                    'A - DOES NOT DESCRIBE ME VERY WELL' = 4,
                    'B' = 3,
                    'C' = 2,
                    'D' = 1,
                    'E - DESCRIBES ME VERY WELL' = 0,
                    .default = 0),
  IRI_19.n = recode(IRI_19,
                    'A - DOES NOT DESCRIBE ME VERY WELL' = 4,
                    'B' = 3,
                    'C' = 2,
                    'D' = 1,
                    'E - DESCRIBES ME VERY WELL' = 0,
                    .default = 0),
  
)
table(timepoint2.use$IRI_18, timepoint2.use$IRI_18.n)
#
# Empathy Quotient
timepoint2.use <- timepoint2.use %>% mutate(
  EQ_1.n = recode(EQ_1,
                  'strongly agree' = 2,
                  'slightly agree' = 1,
                  'slightly disagree' = 0,
                  'strongly disagree' = 0,
                  .default = 0),
  EQ_4.n = recode(EQ_4,
                  'strongly agree' = 0,
                  'slightly agree' = 0,
                  'slightly disagree' = 1,
                  'strongly disagree' = 2,
                  .default = 0),
  EQ_6.n = recode(EQ_6,
                  'strongly agree' = 2,
                  'slightly agree' = 1,
                  'slightly disagree' = 0,
                  'strongly disagree' = 0,
                  .default = 0),
  EQ_8.n = recode(EQ_8,
                  'strongly agree' = 0,
                  'slightly agree' = 0,
                  'slightly disagree' = 1,
                  'strongly disagree' = 2,
                  .default = 0),
  EQ_10.n = recode(EQ_10,
                   'strongly agree' = 0,
                   'slightly agree' = 0,
                   'slightly disagree' = 1,
                   'strongly disagree' = 2,
                   .default = 0),
  EQ_11.n = recode(EQ_11,
                   'strongly agree' = 0,
                   'slightly agree' = 0,
                   'slightly disagree' = 1,
                   'strongly disagree' = 2,
                   .default = 0),
  EQ_12.n = recode(EQ_12,
                   'strongly agree' = 0,
                   'slightly agree' = 0,
                   'slightly disagree' = 1,
                   'strongly disagree' = 2,
                   .default = 0),
  EQ_14.n = recode(EQ_14,
                   'strongly agree' = 0,
                   'slightly agree' = 0,
                   'slightly disagree' = 1,
                   'strongly disagree' = 2,
                   .default = 0),
  EQ_15.n = recode(EQ_15,
                   'strongly agree' = 0,
                   'slightly agree' = 0,
                   'slightly disagree' = 1,
                   'strongly disagree' = 2,
                   .default = 0),
  EQ_18.n = recode(EQ_18,
                   'strongly agree' = 0,
                   'slightly agree' = 0,
                   'slightly disagree' = 1,
                   'strongly disagree' = 2,
                   .default = 0),
  EQ_19.n = recode(EQ_19,
                   'strongly agree' = 2,
                   'slightly agree' = 1,
                   'slightly disagree' = 0,
                   'strongly disagree' = 0,
                   .default = 0),
  EQ_21.n = recode(EQ_21,
                   'strongly agree' = 0,
                   'slightly agree' = 0,
                   'slightly disagree' = 1,
                   'strongly disagree' = 2,
                   .default = 0),
  EQ_22.n = recode(EQ_22,
                   'strongly agree' = 2,
                   'slightly agree' = 1,
                   'slightly disagree' = 0,
                   'strongly disagree' = 0,
                   .default = 0),
  EQ_25.n = recode(EQ_25,
                   'strongly agree' = 2,
                   'slightly agree' = 1,
                   'slightly disagree' = 0,
                   'strongly disagree' = 0,
                   .default = 0),
  EQ_26.n = recode(EQ_26,
                   'strongly agree' = 2,
                   'slightly agree' = 1,
                   'slightly disagree' = 0,
                   'strongly disagree' = 0,
                   .default = 0),
  EQ_27.n = recode(EQ_27,
                   'strongly agree' = 0,
                   'slightly agree' = 0,
                   'slightly disagree' = 1,
                   'strongly disagree' = 2,
                   .default = 0),
  EQ_28.n = recode(EQ_28,
                   'strongly agree' = 0,
                   'slightly agree' = 0,
                   'slightly disagree' = 1,
                   'strongly disagree' = 2,
                   .default = 0),
  EQ_29.n = recode(EQ_29,
                   'strongly agree' = 0,
                   'slightly agree' = 0,
                   'slightly disagree' = 1,
                   'strongly disagree' = 2,
                   .default = 0),
  EQ_32.n = recode(EQ_32,
                   'strongly agree' = 0,
                   'slightly agree' = 0,
                   'slightly disagree' = 1,
                   'strongly disagree' = 2,
                   .default = 0),
  EQ_34.n = recode(EQ_34,
                   'strongly agree' = 0,
                   'slightly agree' = 0,
                   'slightly disagree' = 1,
                   'strongly disagree' = 2,
                   .default = 0),
  EQ_35.n = recode(EQ_35,
                   'strongly agree' = 2,
                   'slightly agree' = 1,
                   'slightly disagree' = 0,
                   'strongly disagree' = 0,
                   .default = 0),
  EQ_36.n = recode(EQ_36,
                   'strongly agree' = 2,
                   'slightly agree' = 1,
                   'slightly disagree' = 0,
                   'strongly disagree' = 0,
                   .default = 0),
  EQ_37.n = recode(EQ_37,
                   'strongly agree' = 2,
                   'slightly agree' = 1,
                   'slightly disagree' = 0,
                   'strongly disagree' = 0,
                   .default = 0),
  EQ_38.n = recode(EQ_38,
                   'strongly agree' = 2,
                   'slightly agree' = 1,
                   'slightly disagree' = 0,
                   'strongly disagree' = 0,
                   .default = 0),
  EQ_39.n = recode(EQ_39,
                   'strongly agree' = 0,
                   'slightly agree' = 0,
                   'slightly disagree' = 1,
                   'strongly disagree' = 2,
                   .default = 0),
  EQ_41.n = recode(EQ_41,
                   'strongly agree' = 2,
                   'slightly agree' = 1,
                   'slightly disagree' = 0,
                   'strongly disagree' = 0,
                   .default = 0),
  EQ_42.n = recode(EQ_42,
                   'strongly agree' = 2,
                   'slightly agree' = 1,
                   'slightly disagree' = 0,
                   'strongly disagree' = 0,
                   .default = 0),
  EQ_43.n = recode(EQ_43,
                   'strongly agree' = 2,
                   'slightly agree' = 1,
                   'slightly disagree' = 0,
                   'strongly disagree' = 0,
                   .default = 0),
  EQ_44.n = recode(EQ_44,
                   'strongly agree' = 2,
                   'slightly agree' = 1,
                   'slightly disagree' = 0,
                   'strongly disagree' = 0,
                   .default = 0),
  EQ_46.n = recode(EQ_46,
                   'strongly agree' = 0,
                   'slightly agree' = 0,
                   'slightly disagree' = 1,
                   'strongly disagree' = 2,
                   .default = 0),
  EQ_48.n = recode(EQ_48,
                   'strongly agree' = 0,
                   'slightly agree' = 0,
                   'slightly disagree' = 1,
                   'strongly disagree' = 2,
                   .default = 0),
  EQ_49.n = recode(EQ_49,
                   'strongly agree' = 0,
                   'slightly agree' = 0,
                   'slightly disagree' = 1,
                   'strongly disagree' = 2,
                   .default = 0),
  EQ_50.n = recode(EQ_50,
                   'strongly agree' = 0,
                   'slightly agree' = 0,
                   'slightly disagree' = 1,
                   'strongly disagree' = 2,
                   .default = 0),
  EQ_52.n = recode(EQ_52,
                   'strongly agree' = 2,
                   'slightly agree' = 1,
                   'slightly disagree' = 0,
                   'strongly disagree' = 0,
                   .default = 0),
  EQ_54.n = recode(EQ_54,
                   'strongly agree' = 2,
                   'slightly agree' = 1,
                   'slightly disagree' = 0,
                   'strongly disagree' = 0,
                   .default = 0),
  EQ_55.n = recode(EQ_55,
                   'strongly agree' = 2,
                   'slightly agree' = 1,
                   'slightly disagree' = 0,
                   'strongly disagree' = 0,
                   .default = 0),
  EQ_57.n = recode(EQ_57,
                   'strongly agree' = 2,
                   'slightly agree' = 1,
                   'slightly disagree' = 0,
                   'strongly disagree' = 0,
                   .default = 0),
  EQ_58.n = recode(EQ_58,
                   'strongly agree' = 2,
                   'slightly agree' = 1,
                   'slightly disagree' = 0,
                   'strongly disagree' = 0,
                   .default = 0),
  EQ_59.n = recode(EQ_59,
                   'strongly agree' = 2,
                   'slightly agree' = 1,
                   'slightly disagree' = 0,
                   'strongly disagree' = 0,
                   .default = 0),
  EQ_60.n = recode(EQ_60,
                   'strongly agree' = 2,
                   'slightly agree' = 1,
                   'slightly disagree' = 0,
                   'strongly disagree' = 0,
                   .default = 0),
)
table(timepoint2.use$EQ_1, timepoint2.use$EQ_1.n)
table(timepoint2.use$EQ_50, timepoint2.use$EQ_50.n)
#
# Pictorial Empathy Test
timepoint2.use <- timepoint2.use %>% mutate(
  PET_1.n = recode(PET_1,
                   'not at all' = 1,
                   'a little bit' = 2,
                   'it arouses some feelings' = 3,
                   'quite a lot' = 4,
                   'very much' = 5,
                   .default = 0),
  PET_2.n = recode(PET_2,
                   'not at all' = 1,
                   'a little bit' = 2,
                   'it arouses some feelings' = 3,
                   'quite a lot' = 4,
                   'very much' = 5,
                   .default = 0),
  PET_3.n = recode(PET_3,
                   'not at all' = 1,
                   'a little bit' = 2,
                   'it arouses some feelings' = 3,
                   'quite a lot' = 4,
                   'very much' = 5,
                   .default = 0),
  PET_4.n = recode(PET_4,
                   'not at all' = 1,
                   'a little bit' = 2,
                   'it arouses some feelings' = 3,
                   'quite a lot' = 4,
                   'very much' = 5,
                   .default = 0),
  PET_5.n = recode(PET_5,
                   'not at all' = 1,
                   'a little bit' = 2,
                   'it arouses some feelings' = 3,
                   'quite a lot' = 4,
                   'very much' = 5,
                   .default = 0),
  PET_6.n = recode(PET_6,
                   'not at all' = 1,
                   'a little bit' = 2,
                   'it arouses some feelings' = 3,
                   'quite a lot' = 4,
                   'very much' = 5,
                   .default = 0),
  PET_7.n = recode(PET_7,
                   'not at all' = 1,
                   'a little bit' = 2,
                   'it arouses some feelings' = 3,
                   'quite a lot' = 4,
                   'very much' = 5,
                   .default = 0)
)
table(timepoint2.use$PET_6, timepoint2.use$PET_6.n)
#
# Reading the Mind in the Eyes
timepoint2.use <- timepoint2.use %>% mutate(
  EYES_1.n = recode(EYES_1,
                    'playful' = 1,
                    .default = 0),
  EYES_2.n = recode(EYES_2,
                    'upset' = 1,
                    .default = 0),
  EYES_3.n = recode(EYES_3,
                    'desire' = 1,
                    .default = 0),
  EYES_4.n = recode(EYES_4,
                    'insisting' = 1,
                    .default = 0),
  EYES_5.n = recode(EYES_5,
                    'worried' = 1,
                    .default = 0),
  EYES_6.n = recode(EYES_6,
                    'fantasizing' = 1,
                    .default = 0),
  EYES_7.n = recode(EYES_7,
                    'uneasy' = 1,
                    .default = 0),
  EYES_8.n = recode(EYES_8,
                    'despondent' = 1,
                    .default = 0),
  EYES_9.n = recode(EYES_9,
                    'preoccupied' = 1,
                    .default = 0),
  EYES_10.n = recode(EYES_10,
                     'cautious' = 1,
                     .default = 0),
  EYES_11.n = recode(EYES_11,
                     'regretful' = 1,
                     .default = 0),
  EYES_12.n = recode(EYES_12,
                     'sceptical' = 1,
                     .default = 0),
  EYES_13.n = recode(EYES_13,
                     'anticipating' = 1,
                     .default = 0),
  EYES_14.n = recode(EYES_14,
                     'accusing' = 1,
                     .default = 0),
  EYES_15.n = recode(EYES_15,
                     'contemplative' = 1,
                     .default = 0),
  EYES_16.n = recode(EYES_16,
                     'thoughtful' = 1,
                     .default = 0),
  EYES_17.n = recode(EYES_17,
                     'doubtful' = 1,
                     .default = 0),
  EYES_18.n = recode(EYES_18,
                     'decisive' = 1,
                     .default = 0),
  EYES_19.n = recode(EYES_19,
                     'tentative' = 1,
                     .default = 0),
  EYES_20.n = recode(EYES_20,
                     'friendly' = 1,
                     .default = 0),
  EYES_21.n = recode(EYES_21,
                     'fantasizing' = 1,
                     .default = 0),
  EYES_22.n = recode(EYES_22,
                     'preoccupied' = 1,
                     .default = 0),
  EYES_23.n = recode(EYES_23,
                     'defiant' = 1,
                     .default = 0),
  EYES_24.n = recode(EYES_24,
                     'pensive' = 1,
                     .default = 0),
  EYES_25.n = recode(EYES_25,
                     'interested' = 1,
                     .default = 0),
  EYES_26.n = recode(EYES_26,
                     'hostile' = 1,
                     .default = 0),
  EYES_27.n = recode(EYES_27,
                     'cautious' = 1,
                     .default = 0),
  EYES_28.n = recode(EYES_28,
                     'interested' = 1,
                     .default = 0),
  EYES_29.n = recode(EYES_29,
                     'reflective' = 1,
                     .default = 0),
  
  EYES_30.n = recode(EYES_30,
                     'flirtatious' = 1,
                     .default = 0),
  EYES_31.n = recode(EYES_31,
                     'confident' = 1,
                     .default = 0),
  EYES_32.n = recode(EYES_32,
                     'serious' = 1,
                     .default = 0),
  EYES_33.n = recode(EYES_33,
                     'concerned' = 1,
                     .default = 0),
  EYES_34.n = recode(EYES_34,
                     'distrustful' = 1,
                     .default = 0),
  EYES_35.n = recode(EYES_35,
                     'nervous' = 1,
                     .default = 0),
  EYES_36.n = recode(EYES_36,
                     'suspicious' = 1,
                     .default = 0)
)
table(timepoint2.use$EYES_27, timepoint2.use$EYES_27.n)
#
#
############## Calculate total scores - Timepoint 1 #####################
# IRI
timepoint1.use$IRI_FS.n <- timepoint1.use$IRI_1.n + timepoint1.use$IRI_5.n + timepoint1.use$IRI_7.n +
  timepoint1.use$IRI_12.n + timepoint1.use$IRI_16.n + timepoint1.use$IRI_26.n + timepoint1.use$IRI_23.n
#
timepoint1.use$IRI_EC.n <- timepoint1.use$IRI_2.n + timepoint1.use$IRI_4.n + timepoint1.use$IRI_9.n + 
  timepoint1.use$IRI_14.n + timepoint1.use$IRI_18.n + timepoint1.use$IRI_20.n + timepoint1.use$IRI_22.n
#
timepoint1.use$IRI_PT.n <- timepoint1.use$IRI_3.n + timepoint1.use$IRI_8.n + timepoint1.use$IRI_11.n + 
  timepoint1.use$IRI_15.n + timepoint1.use$IRI_21.n + timepoint1.use$IRI_25.n + timepoint1.use$IRI_28.n
#
timepoint1.use$IRI_PD.n <- timepoint1.use$IRI_6.n + timepoint1.use$IRI_10.n + timepoint1.use$IRI_13.n + 
  timepoint1.use$IRI_17.n + timepoint1.use$IRI_19.n + timepoint1.use$IRI_24.n + timepoint1.use$IRI_27.n
#
timepoint1.use$IRI_TOTAL.n <- timepoint1.use$IRI_FS.n + timepoint1.use$IRI_EC.n +
  timepoint1.use$IRI_PT.n + timepoint1.use$IRI_PD.n
#
# EQ
timepoint1.use$EQ_TOTAL.n <- timepoint1.use$EQ_1.n  + timepoint1.use$EQ_4.n + timepoint1.use$EQ_6.n +
  timepoint1.use$EQ_8.n + timepoint1.use$EQ_10.n + timepoint1.use$EQ_11.n + timepoint1.use$EQ_12.n +
  timepoint1.use$EQ_14.n + timepoint1.use$EQ_15.n + timepoint1.use$EQ_18.n + timepoint1.use$EQ_19.n +          
  timepoint1.use$EQ_21.n + timepoint1.use$EQ_22.n + timepoint1.use$EQ_25.n + timepoint1.use$EQ_26.n +
  timepoint1.use$EQ_27.n +  timepoint1.use$EQ_28.n + timepoint1.use$EQ_29.n + timepoint1.use$EQ_32.n +
  timepoint1.use$EQ_34.n + timepoint1.use$EQ_35.n + timepoint1.use$EQ_36.n + timepoint1.use$EQ_37.n +      
  timepoint1.use$EQ_38.n + timepoint1.use$EQ_39.n + timepoint1.use$EQ_41.n + timepoint1.use$EQ_42.n +
  timepoint1.use$EQ_43.n + timepoint1.use$EQ_44.n + timepoint1.use$EQ_46.n + timepoint1.use$EQ_48.n +
  timepoint1.use$EQ_49.n + timepoint1.use$EQ_50.n + timepoint1.use$EQ_52.n + timepoint1.use$EQ_54.n +        
  timepoint1.use$EQ_55.n  + timepoint1.use$EQ_57.n + timepoint1.use$EQ_58.n + timepoint1.use$EQ_59.n +
  timepoint1.use$EQ_60.n 
#
# PET - the mean score is calculated
timepoint1.use$PET_TOTAL.n <- timepoint1.use$PET_1.n + timepoint1.use$PET_2.n + timepoint1.use$PET_3.n +
  timepoint1.use$PET_4.n + timepoint1.use$PET_5.n + timepoint1.use$PET_6.n + timepoint1.use$PET_7.n
timepoint1.use$PET_TOTAL.n <- timepoint1.use$PET_TOTAL.n / 7
#
# EYES
#
timepoint1.use$EYES_TOTAL.n <- timepoint1.use$EYES_1.n + timepoint1.use$EYES_2.n + timepoint1.use$EYES_3.n + 
  timepoint1.use$EYES_4.n + timepoint1.use$EYES_5.n + timepoint1.use$EYES_6.n + timepoint1.use$EYES_7.n  + 
  timepoint1.use$EYES_8.n + timepoint1.use$EYES_9.n + timepoint1.use$EYES_10.n + timepoint1.use$EYES_11.n  + 
  timepoint1.use$EYES_12.n + timepoint1.use$EYES_13.n + timepoint1.use$EYES_14.n + timepoint1.use$EYES_15.n + 
  timepoint1.use$EYES_16.n + timepoint1.use$EYES_17.n + timepoint1.use$EYES_18.n + timepoint1.use$EYES_19.n +
  timepoint1.use$EYES_20.n + timepoint1.use$EYES_21.n + timepoint1.use$EYES_22.n + timepoint1.use$EYES_23.n  +
  timepoint1.use$EYES_24.n + timepoint1.use$EYES_25.n + timepoint1.use$EYES_26.n + timepoint1.use$EYES_27.n + 
  timepoint1.use$EYES_28.n + timepoint1.use$EYES_29.n + timepoint1.use$EYES_30.n + timepoint1.use$EYES_31.n +
  timepoint1.use$EYES_32.n + timepoint1.use$EYES_33.n + timepoint1.use$EYES_34.n + timepoint1.use$EYES_35.n # + 
timepoint1.use$EYES_36n
#
############## Calculate total scores - Timepoint 2 #####################
# IRI
timepoint2.use$IRI_FS.n <- timepoint2.use$IRI_1.n + timepoint2.use$IRI_5.n + timepoint2.use$IRI_7.n +
  timepoint2.use$IRI_12.n + timepoint2.use$IRI_16.n + timepoint2.use$IRI_26.n + timepoint2.use$IRI_23.n
#
timepoint2.use$IRI_EC.n <- timepoint2.use$IRI_2.n + timepoint2.use$IRI_4.n + timepoint2.use$IRI_9.n + 
  timepoint2.use$IRI_14.n + timepoint2.use$IRI_18.n + timepoint2.use$IRI_20.n + timepoint2.use$IRI_22.n
#
timepoint2.use$IRI_PT.n <- timepoint2.use$IRI_3.n + timepoint2.use$IRI_8.n + timepoint2.use$IRI_11.n + 
  timepoint2.use$IRI_15.n + timepoint2.use$IRI_21.n + timepoint2.use$IRI_25.n + timepoint2.use$IRI_28.n
#
timepoint2.use$IRI_PD.n <- timepoint2.use$IRI_6.n + timepoint2.use$IRI_10.n + timepoint2.use$IRI_13.n + 
  timepoint2.use$IRI_17.n + timepoint2.use$IRI_19.n + timepoint2.use$IRI_24.n + timepoint2.use$IRI_27.n
#
timepoint2.use$IRI_TOTAL.n <- timepoint2.use$IRI_FS.n + timepoint2.use$IRI_EC.n +
  timepoint2.use$IRI_PT.n + timepoint2.use$IRI_PD.n
#
# EQ
timepoint2.use$EQ_TOTAL.n <- timepoint2.use$EQ_1.n  + timepoint2.use$EQ_4.n + timepoint2.use$EQ_6.n +
  timepoint2.use$EQ_8.n + timepoint2.use$EQ_10.n + timepoint2.use$EQ_11.n + timepoint2.use$EQ_12.n +
  timepoint2.use$EQ_14.n + timepoint2.use$EQ_15.n + timepoint2.use$EQ_18.n + timepoint2.use$EQ_19.n +          
  timepoint2.use$EQ_21.n + timepoint2.use$EQ_22.n + timepoint2.use$EQ_25.n + timepoint2.use$EQ_26.n +
  timepoint2.use$EQ_27.n +  timepoint2.use$EQ_28.n + timepoint2.use$EQ_29.n + timepoint2.use$EQ_32.n +
  timepoint2.use$EQ_34.n + timepoint2.use$EQ_35.n + timepoint2.use$EQ_36.n + timepoint2.use$EQ_37.n +      
  timepoint2.use$EQ_38.n + timepoint2.use$EQ_39.n + timepoint2.use$EQ_41.n + timepoint2.use$EQ_42.n +
  timepoint2.use$EQ_43.n + timepoint2.use$EQ_44.n + timepoint2.use$EQ_46.n + timepoint2.use$EQ_48.n +
  timepoint2.use$EQ_49.n + timepoint2.use$EQ_50.n + timepoint2.use$EQ_52.n + timepoint2.use$EQ_54.n +        
  timepoint2.use$EQ_55.n  + timepoint2.use$EQ_57.n + timepoint2.use$EQ_58.n + timepoint2.use$EQ_59.n +
  timepoint2.use$EQ_60.n 
#
# PET - the mean score is calculated
timepoint2.use$PET_TOTAL.n <- timepoint2.use$PET_1.n + timepoint2.use$PET_2.n + timepoint2.use$PET_3.n +
  timepoint2.use$PET_4.n + timepoint2.use$PET_5.n + timepoint2.use$PET_6.n + timepoint2.use$PET_7.n
timepoint2.use$PET_TOTAL.n <- timepoint2.use$PET_TOTAL.n / 7
#
# EYES
#
timepoint2.use$EYES_TOTAL.n <-  timepoint2.use$EYES_1.n + timepoint2.use$EYES_2.n + timepoint2.use$EYES_3.n + 
  timepoint2.use$EYES_4.n + timepoint2.use$EYES_5.n + timepoint2.use$EYES_6.n + timepoint2.use$EYES_7.n + 
  timepoint2.use$EYES_8.n + timepoint2.use$EYES_9.n + timepoint2.use$EYES_10.n + timepoint2.use$EYES_11.n + 
  timepoint2.use$EYES_12.n + timepoint2.use$EYES_13.n + timepoint2.use$EYES_14.n + timepoint2.use$EYES_15.n + 
  timepoint2.use$EYES_16.n + timepoint2.use$EYES_17.n + timepoint2.use$EYES_18.n + timepoint2.use$EYES_19.n +
  timepoint2.use$EYES_20.n + timepoint2.use$EYES_21.n + timepoint2.use$EYES_22.n + timepoint2.use$EYES_23.n +
  timepoint2.use$EYES_24.n + timepoint2.use$EYES_25.n + timepoint2.use$EYES_26.n + timepoint2.use$EYES_27.n + 
  timepoint2.use$EYES_28.n + timepoint2.use$EYES_29.n + timepoint2.use$EYES_30.n + timepoint2.use$EYES_31.n +
  timepoint2.use$EYES_32.n + timepoint2.use$EYES_33.n + timepoint2.use$EYES_34.n + timepoint2.use$EYES_35.n + 
  timepoint2.use$EYES_36.n
#
# Merge both datasets to contain only participants that completed both time points
timepointboth <- merge(timepoint1.use, timepoint2.use, by = "PROLIFIC_PID")
#
# Number of participants, age, gender
#
# N of both timepoints
nrow(timepointboth)
# Gender
table(timepointboth$Gender.y)
prop.table(table(timepointboth$Gender.y))
round(100 * prop.table(table(timepointboth$Gender.y)), digits= 1)
# Age
timepointboth$Age_1.y.n <- as.numeric(timepointboth$Age_1.y)
describe(timepointboth$Age_1.y.n)
range(timepointboth$Age_1.y.n)
median(timepointboth$Age_1.y.n)
#
# Create new data-sets for ICC calculation
sites.icc <- timepointboth %>% select(SITES.n.x, SITES.n.y)
iri_fs.icc <- timepointboth %>% select(IRI_FS.n.x, IRI_FS.n.y)
iri_ec.icc <- timepointboth %>% select(IRI_EC.n.x, IRI_EC.n.y)
iri_pt.icc <- timepointboth %>% select(IRI_PT.n.x, IRI_PT.n.y)
iri_pd.icc <- timepointboth %>% select(IRI_PD.n.x, IRI_PD.n.y)               
iri_total.icc <- timepointboth %>% select(IRI_TOTAL.n.x, IRI_TOTAL.n.y)
eq_total.icc <- timepointboth %>% select(EQ_TOTAL.n.x, EQ_TOTAL.n.y)  
pet_total.icc <- timepointboth %>% select(PET_TOTAL.n.x, PET_TOTAL.n.y)  
eyes_total.icc <- timepointboth %>% select(EYES_TOTAL.n.x, EYES_TOTAL.n.y)        
#
# Create new data-sets for Cronbach's Alpha calculation
# SITES has just one item, Cronbach's Alpha not possible
# Timepoint 1
iri_fs.alpha1 <- timepointboth %>% select(IRI_1.n.x, IRI_5.n.x,IRI_7.n.x,IRI_12.n.x,
                                          IRI_16.n.x, IRI_26.n.x, IRI_23.n.x)
iri_ec.alpha1 <- timepointboth %>% select(IRI_2.n.x,IRI_4.n.x, IRI_9.n.x, IRI_14.n.x, IRI_18.n.x,
                                          IRI_20.n.x, IRI_22.n.x)
iri_pt.alpha1 <- timepointboth %>% select(IRI_3.n.x, IRI_8.n.x, IRI_11.n.x, IRI_15.n.x,
                                          IRI_21.n.x,  IRI_25.n.x,  IRI_28.n.x)
iri_pd.alpha1 <- timepointboth %>% select(IRI_6.n.x, IRI_10.n.x, IRI_13.n.x, IRI_17.n.x,
                                          IRI_19.n.x,  IRI_24.n.x, IRI_27.n.x)
iri_total.alpha1 <- cbind(iri_fs.alpha, iri_ec.alpha, iri_pt.alpha, iri_pd.alpha)
eq.alpha1 <- timepointboth %>% select(EQ_1.n.x, EQ_6.n.x, EQ_4.n.x, EQ_8.n.x, EQ_10.n.x,
                                      EQ_11.n.x, EQ_12.n.x, EQ_14.n.x, EQ_15.n.x, EQ_18.n.x,
                                      EQ_19.n.x, EQ_21.n.x, EQ_22.n.x, EQ_25.n.x, EQ_26.n.x,
                                      EQ_27.n.x,EQ_28.n.x, EQ_29.n.x, EQ_32.n.x, EQ_34.n.x,
                                      EQ_35.n.x, EQ_36.n.x, EQ_37.n.x, EQ_38.n.x,EQ_39.n.x,
                                      EQ_41.n.x, EQ_42.n.x, EQ_43.n.x, EQ_44.n.x, EQ_46.n.x,
                                      EQ_48.n.x, EQ_49.n.x, EQ_50.n.x, EQ_52.n.x, EQ_54.n.x,
                                      EQ_55.n.x, EQ_57.n.x, EQ_58.n.x, EQ_59.n.x, EQ_60.n.x)
pet.alpha1 <- timepointboth %>% select(PET_1.n.x, PET_2.n.x, PET_3.n.x, PET_4.n.x,
                                       PET_5.n.x, PET_6.n.x, PET_7.n.x)
eyes.alpha1 <- timepointboth %>% select(EYES_1.n.x, EYES_2.n.x, EYES_3.n.x, EYES_4.n.x,
                                        EYES_5.n.x, EYES_6.n.x, EYES_7.n.x,EYES_8.n.x,
                                        EYES_9.n.x, EYES_10.n.x, EYES_11.n.x, EYES_12.n.x,
                                        EYES_13.n.x, EYES_14.n.x, EYES_15.n.x, EYES_16.n.x,
                                        EYES_17.n.x, EYES_18.n.x, EYES_19.n.x, EYES_20.n.x,
                                        EYES_21.n.x, EYES_22.n.x, EYES_23.n.x, EYES_24.n.x,
                                        EYES_25.n.x, EYES_26.n.x, EYES_27.n.x, EYES_28.n.x,
                                        EYES_29.n.x, EYES_30.n.x, EYES_31.n.x, EYES_32.n.x,
                                        EYES_33.n.x, EYES_34.n.x, EYES_35.n.x, EYES_36.n.x)
# Timepoint 2
iri_fs.alpha <- timepointboth %>% select(IRI_1.n.y, IRI_5.n.y,IRI_7.n.y,IRI_12.n.y,
                                         IRI_16.n.y, IRI_26.n.y, IRI_23.n.y)
iri_ec.alpha <- timepointboth %>% select(IRI_2.n.y,IRI_4.n.y, IRI_9.n.y, IRI_14.n.y, IRI_18.n.y,
                                         IRI_20.n.y, IRI_22.n.y)
iri_pt.alpha <- timepointboth %>% select(IRI_3.n.y, IRI_8.n.y, IRI_11.n.y, IRI_15.n.y,
                                         IRI_21.n.y,  IRI_25.n.y,  IRI_28.n.y)
iri_pd.alpha <- timepointboth %>% select(IRI_6.n.y, IRI_10.n.y, IRI_13.n.y, IRI_17.n.y,
                                         IRI_19.n.y,  IRI_24.n.y, IRI_27.n.y)
iri_total.alpha <- cbind(iri_fs.alpha, iri_ec.alpha, iri_pt.alpha, iri_pd.alpha)
eq.alpha <- timepointboth %>% select(EQ_1.n.y, EQ_6.n.y, EQ_4.n.y, EQ_8.n.y, EQ_10.n.y,
                                     EQ_11.n.y, EQ_12.n.y, EQ_14.n.y, EQ_15.n.y, EQ_18.n.y,
                                     EQ_19.n.y, EQ_21.n.y, EQ_22.n.y, EQ_25.n.y, EQ_26.n.y,
                                     EQ_27.n.y,EQ_28.n.y, EQ_29.n.y, EQ_32.n.y, EQ_34.n.y,
                                     EQ_35.n.y, EQ_36.n.y, EQ_37.n.y, EQ_38.n.y,EQ_39.n.y,
                                     EQ_41.n.y, EQ_42.n.y, EQ_43.n.y, EQ_44.n.y, EQ_46.n.y,
                                     EQ_48.n.y, EQ_49.n.y, EQ_50.n.y, EQ_52.n.y, EQ_54.n.y,
                                     EQ_55.n.y, EQ_57.n.y, EQ_58.n.y, EQ_59.n.y, EQ_60.n.y)
pet.alpha <- timepointboth %>% select(PET_1.n.y, PET_2.n.y, PET_3.n.y, PET_4.n.y,
                                      PET_5.n.y, PET_6.n.y, PET_7.n.y)
eyes.alpha <- timepointboth %>% select(EYES_1.n.y, EYES_2.n.y, EYES_3.n.y, EYES_4.n.y,
                                       EYES_5.n.y, EYES_6.n.y, EYES_7.n.y,EYES_8.n.y,
                                       EYES_9.n.y, EYES_10.n.y, EYES_11.n.y, EYES_12.n.y,
                                       EYES_13.n.y, EYES_14.n.y, EYES_15.n.y, EYES_16.n.y,
                                       EYES_17.n.y, EYES_18.n.y, EYES_19.n.y, EYES_20.n.y,
                                       EYES_21.n.y, EYES_22.n.y, EYES_23.n.y, EYES_24.n.y,
                                       EYES_25.n.y, EYES_26.n.y, EYES_27.n.y, EYES_28.n.y,
                                       EYES_29.n.y, EYES_30.n.y, EYES_31.n.y, EYES_32.n.y,
                                       EYES_33.n.y, EYES_34.n.y, EYES_35.n.y, EYES_36.n.y)
#
# Analysis of each measure
#
### IRI ###
icc(iri_fs.icc, model = "twoway", type = "agreement", unit = "single",
    r0 = 0, conf.level = 0.95)
icc(iri_ec.icc, model = "twoway", type = "agreement", unit = "single",
    r0 = 0, conf.level = 0.95)
icc(iri_pt.icc, model = "twoway", type = "agreement", unit = "single",
    r0 = 0, conf.level = 0.95)
icc(iri_pd.icc, model = "twoway", type = "agreement", unit = "single",
    r0 = 0, conf.level = 0.95)
icc(iri_total.icc, model = "twoway", type = "agreement", unit = "single",
    r0 = 0, conf.level = 0.95)
# Alpha timepoint 1
alpha(iri_fs.alpha1)
alpha(iri_ec.alpha1)
alpha(iri_pt.alpha1)
alpha(iri_pd.alpha1)
alpha(iri_total.alpha1)
# Alpha timepoint 2
alpha(iri_fs.alpha)
alpha(iri_ec.alpha)
alpha(iri_pt.alpha)
alpha(iri_pd.alpha)
alpha(iri_total.alpha)
describe(iri_fs.icc)
describe(iri_ec.icc)
describe(iri_pt.icc)
describe(iri_pd.icc)
describe(iri_total.icc)
### EQ ###
icc(eq_total.icc, model = "twoway", type = "agreement", unit = "single",
    r0 = 0, conf.level = 0.95)
alpha(eq.alpha1) # timepoint 1
alpha(eq.alpha) # timepoint 2
describe(eq_total.icc)
#
### PET ###
icc(pet_total.icc, model = "twoway", type = "agreement", unit = "single",
    r0 = 0, conf.level = 0.95)
alpha(pet.alpha1) # timepoint 1
alpha(pet.alpha) # timepoint 2
describe(pet_total.icc)
#
### SITES ###
icc(sites.icc, model = "twoway", type = "agreement", unit = "single",
    r0 = 0, conf.level = 0.95)
# Cronbach's Alpha not possible for SITES; just one item
describe(sites.icc)
range(sites.icc)
which.max(table(sites.icc$SITES.n.y))
prop.table(table(sites.icc$SITES.n.y))
round(100 * prop.table(table(sites.icc$SITES.n.y)), digits= 1)
median(sites.icc$SITES.n.y)
#
### EYES ###
icc(eyes_total.icc, model = "twoway", type = "agreement", unit = "single",
    r0 = 0, conf.level = 0.95)
alpha(eyes.alpha1) # timepoint 1
alpha(eyes.alpha) # timepoint 2
describe(eyes_total.icc)
