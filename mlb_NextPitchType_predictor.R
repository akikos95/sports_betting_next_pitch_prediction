### ALEX KIKOS ###

library(readxl)
library(dplyr)
library(tidyverse)
library(readxl)
library(caret)
library(pROC)
library(gains)
library(adabag)

###MODEL CREATION PART 1###
###MODEL CREATION PART 1###
###MODEL CREATION PART 1###
##BOOSTING TREE FOR A SINGLE PITCHER (ID 434378)##

###DATA IMPORT AND MANAGEMENT###
rm(list=ls())

data <- read.csv("C:/Users/Alex/Documents/pitches.csv")

#pitcher 434378 threw the most pitches in the df, use for simpler model looking at his next pitch
data <- subset(data, pitcher_id == 434378)

#filter to keep relevant values that are available PRIOR to pitch and the pitch type (via metadata file)
data <- data[,c(7:15,23,32:33,63)]
data <- data %>% relocate(pitch_type)
#replace blank cells with NA
data <- data %>% mutate_all(na_if,"")
#uses only complete cases
data <- (data[complete.cases(data), ])


#changes the R/L values to numerics, R = 0, L = 1
data$stand <- ifelse(data$stand == "R",0,1)

data$pitch_type<- as.factor(data$pitch_type)

set.seed(1)
#60/40 training/validation split
myIndex <- createDataPartition(data$pitch_type, p=0.6, list=FALSE)
trainSet <- data[myIndex,]
validationSet <- data[-myIndex,]

#create 250 individual trees
set.seed(1)
boosting_tree <- boosting(pitch_type ~ ., 
                          data = trainSet, 
                          mfinal = 250)


##confusion matrix - throws error since sensitivity value is 0 for some pitches, cannot divide by 0.
prediction <- predict(boosting_tree, validationSet)
confusionMatrix(as.factor(prediction$class), 
                validationSet$pitch_type)

##data management on the validation set
validationSet$pitch_type <- unclass(validationSet$pitch_type)
##gains table
gains_table <- gains(validationSet$pitch_type, prediction$prob[,3])
gains_table
##lift chart
plot(c(0, gains_table$cume.pct.of.total*sum(validationSet$pitch_type)) ~ c(0, gains_table$cume.obs), 
     xlab = '# of cases', 
     ylab = "Cumulative", 
     type = "l")
lines(c(0, sum(validationSet$pitch_type))~c(0, dim(validationSet)[1]), 
      col="red", 
      lty=2)
##decile-wise lift chart
lines(c(0, sum(validationSet$pitch_type))~c(0, dim(validationSet)[1]), 
      col="red", 
      lty=2)
barplot(gains_table$mean.resp/mean(validationSet$pitch_type), 
        names.arg=gains_table$depth, 
        xlab="Percentile", 
        ylab="Lift", 
        ylim=c(0, 3.0), 
        main="Decile-Wise Lift Chart")

##creates the ROC curve
roc_object <- roc(validationSet$pitch_type, prediction$prob[,3])
plot.roc(roc_object)
auc(roc_object)


##model could then be used to predict next likely pitch by inputting current values for the situation
##i.e. ball, strikes, outs, runs...
# boost_score <- predict(boosting_tree, newdata=currentSituation_data)
# boost_score
# currentSituation_data$nextPitch <- boost_Score$class





###MODEL CREATION PART 2###
###MODEL CREATION PART 2###
###MODEL CREATION PART 2###
##BOOSTING TREE WITH ALL OBSERVATIONS##

##WILL TAKE A SIGNIFICANT AMOUNT OF TIME IF YOU WANT TO RUN ON YOUR OWN##

###DATA IMPORT AND MANAGEMENT###
# rm(list=ls())
# 
# data <- read.csv("C:/Users/Alex/Documents/pitches.csv")
# 
# #filter to keep relevant values that are available PRIOR to pitch and the pitch type (via metadata file)
# data <- data[,c(7:15,23,32:33,63)]
# data <- data %>% relocate(pitch_type)
# #replace blank cells with NA
# data <- data %>% mutate_all(na_if,"")
# #uses only complete cases
# data <- (data[complete.cases(data), ])
# 
# 
# #changes the R/L values to numerics, R = 0, L = 1
# data$stand <- ifelse(data$stand == "R",0,1)
# 
# data$pitch_type<- as.factor(data$pitch_type)
# 
# set.seed(1)
# #60/40 training/validation split
# myIndex <- createDataPartition(data$pitch_type, p=0.6, list=FALSE)
# trainSet <- data[myIndex,]
# validationSet <- data[-myIndex,]
# 
# #create 250 individual trees
# set.seed(1)
# boosting_tree <- boosting(pitch_type ~ ., 
#                           data = trainSet, 
#                           mfinal = 250)
# 
# 
# ##confusion matrix - throws error since sensitivity value is 0 for some pitches, cannot divide by 0.
# prediction <- predict(boosting_tree, validationSet)
# confusionMatrix(as.factor(prediction$class), 
#                 validationSet$pitch_type)
# 
# ##data management on the validation set
# validationSet$pitch_type <- unclass(validationSet$pitch_type)
# ##gains table
# gains_table <- gains(validationSet$pitch_type, prediction$prob[,3])
# gains_table
# ##lift chart
# plot(c(0, gains_table$cume.pct.of.total*sum(validationSet$pitch_type)) ~ c(0, gains_table$cume.obs), 
#      xlab = '# of cases', 
#      ylab = "Cumulative", 
#      type = "l")
# lines(c(0, sum(validationSet$pitch_type))~c(0, dim(validationSet)[1]), 
#       col="red", 
#       lty=2)
# ##decile-wise lift chart
# lines(c(0, sum(validationSet$pitch_type))~c(0, dim(validationSet)[1]), 
#       col="red", 
#       lty=2)
# barplot(gains_table$mean.resp/mean(validationSet$pitch_type), 
#         names.arg=gains_table$depth, 
#         xlab="Percentile", 
#         ylab="Lift", 
#         ylim=c(0, 3.0), 
#         main="Decile-Wise Lift Chart")
# 
# ##creates the ROC curve
# roc_object <- roc(validationSet$pitch_type, prediction$prob[,3])
# plot.roc(roc_object)
# auc(roc_object)

##model could then be used to predict next likely pitch by inputting current values for the situation
##i.e. ball, strikes, outs, runs...
# boost_score <- predict(boosting_tree, newdata=currentSituation_data)
# boost_score
# currentSituation_data$nextPitch <- boost_Score$class







###MODEL CREATION PART 3###
###MODEL CREATION PART 3###
###MODEL CREATION PART 3###
##LINEAR REGRESSION MODEL##

###DATA IMPORT AND MANAGEMENT###
rm(list=ls())

data <- read.csv("C:/Users/Alex/Documents/pitches.csv")

#pitcher 434378 threw the most pitches in the df, use for simpler model looking at his next pitch
#data <- subset(data, pitcher_id == 434378)

#filter to keep relevant values that are available PRIOR to pitch and the pitch type (via metadata file)
data <- data[,c(7:15,23,32:33,63)]
data <- data %>% relocate(pitch_type)
#replace blank cells with NA
data <- data %>% mutate_all(na_if,"")
#uses only complete cases
data <- (data[complete.cases(data), ])

#changes the R/L values to numerics, R = 0, L = 1
data$stand <- ifelse(data$stand == "R",0,1)
data$pitch_type<- as.factor(data$pitch_type)
data$pitch_type <- unclass(data$pitch_type)

linr1<-lm(pitch_type~., data=data)
summary(linr1)

linr2<-lm(pitch_type~inning+top+at_bat_num+pcount_pitcher+strikes+outs+stand+away_team_runs+home_team_runs, data=data)
summary(linr2)

#models would be compared using Adjusted R^2 values
#NOTE: I realize linr1 and linr2 adjusted R^2 are horrible (very low)
#and would further investigate/collect data to increase model accuracy


