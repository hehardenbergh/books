
################################

# ------------------------
# THE HIERARCHY OF GENDER - ADJUSTMENTS
# ------------------------
# Author: Eve Kraicer and Dr. Andrew Piper
# Date: August 2018

################################

# This script uses hand-coded validation tables compared to predicted values; 
# it calculates sensitivity and specificity for adjustments throughout.
# Four se/sp are calculated for the subsets of data (1p, 3p, all (m/w)).
# This needs to be run and loaded BEFORE running the script

### REQUIREMENTS
# packages
library(caret)
library(e1071)

# working directory
wd<-paste("~/Desktop/hierarchy/hierarchy_data")
# set working directory 
setwd(wd)

# SENSITIVITY AND SPECIFICITY FOR ALL CHARACTERS (men and women)
# read in df 
validation_2 <- data.frame(read.csv("validation_all.csv", sep=","))

# calculate for women
# change all 'M' labels to '?' to create a 2x2 matrix of women and not women
validation_2w <- data.frame(cbind(as.character(validation_2$GENDER), as.character(validation_2$VALIDATION)))
validation_2w$GENDER <- gsub('M', '?', validation_2w[,1])
validation_2w$CHECK <- gsub('M', '?', validation_2w[,2])
# make confusion matrix 
cm_2 <- confusionMatrix(as.factor(validation_2w$GENDER), as.factor(validation_2w$CHECK), positive = 'F')

# pull sensitivity and specificity from confusion matrix
se_2 <- cm_2$byClass[[1]]
sp_2 <- cm_2$byClass[[2]]

# calculate for men
# change all 'F' labels to '?' to create a 2x2 matrix of men and not men
validation_2m <- data.frame(cbind(as.character(validation_2$GENDER), as.character(validation_2$VALIDATION)))
validation_2m$GENDER <- gsub('F', '?', validation_2m[,1])
validation_2m$CHECK <- gsub('F', '?', validation_2m[,2])
# make confusion matrix 
cm_2m <- confusionMatrix(as.factor(validation_2m$GENDER), as.factor(validation_2m$CHECK), positive = 'M')

# pull sensitivity and specificity from confusion matrix
se_2m <-cm_2m$byClass[[1]]
sp_2m <- cm_2m$byClass[[2]]

### SENSITIVITY AND SPECIFICITY FOR THIRD PERSON 
# read in df and make confusion matrix
validation_1 <- data.frame(read.csv("validation_tp.csv"), row.names = NULL)
cm_1 <- confusionMatrix(validation_1$GENDER, validation_1$CHECK, positive = 'F')

# calculate sensitivity and specificity based on above values 
se_1 <- cm_1$byClass[[1]]
sp_1 <- cm_1$byClass[[2]]

# SENSITIVITY AND SPECIFICITY FOR FIRST PERSON 
# read in df, remove unspecified cases 
validation_1b <- data.frame(read.csv("validation_1p.csv"), row.names = NULL)
validation_1b <- validation_1b[validation_1b$GENDER != '?',]
validation_1b$GENDER <- factor(validation_1b$GENDER)
# make confusion matrix 
cm_1b <- confusionMatrix(validation_1b$GENDER, validation_1b$CHECK, positive = 'F')

# pull sensitivity and specificity from confusion matrix
se_1b <- cm_1b$byClass[[1]]
sp_1b <- cm_1b$byClass[[2]]
