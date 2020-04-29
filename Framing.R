getwd()

#Read data from CSV File
framingham = read.csv("framingham.csv")

str(framingham)
cor(framingham)

#Remove NAs from data
frm_model = na.omit(framingham)
cor(frm_model)
str(frm_model)

#Load library caTools
library(caTools)
library(MASS)
#Randomly split data into training and testing sets
set.seed(599)
split= sample.split(frm_model$TenYearCHD, SplitRatio = 0.65)

# Split the data using subset
frm_train= subset(frm_model, split==TRUE)
frm_test= subset(frm_model, split==FALSE)

                  # Logistic Regression Model


frm_complete= glm(TenYearCHD ~ ., data = frm_train, family=binomial)
summary(frm_complete) #1785.7 AIC


# Select Model by Automated Selection Model
frm_step = stepAIC(frm_complete, direction = "both", trace = FALSE)
summary(frm_step)


#Best Model by variable significance
frm_best= glm(TenYearCHD ~ male + age + cigsPerDay + prevalentStroke  + prevalentHyp + totChol + sysBP + glucose , data = frm_train, family = binomial)
summary(frm_best)   #1774  AIC


                          #Prediction on Complete Model

# Predictions on the test set
predictTest = predict(frm_complete, type="response", newdata=frm_test)

# Confusion matrix with threshold of 0.5
table(frm_test$TenYearCHD, predictTest > 0.5)

#Sensitivity & Specificity
16/195
1070/1085

# Accuracy
(1070+16)/(1070+15+179+16)

# Baseline accuracy
(1070+15)/(1070+15+179+16) 

# Confusion matrix with threshold of 0.2
table(frm_test$TenYearCHD, predictTest > 0.2)

#Sensitivity & Specificity
92/195
846/1085

# Accuracy
(846+92)/(846+239+103+92)


# Test set AUC

library(ROCR)
ROCpred1 = prediction(predictTest, frm_test$TenYearCHD)
as.numeric(performance(ROCpred1, "auc")@y.values)

# Performance function
ROCperf = performance(ROCpred1, "tpr", "fpr")

# Plot ROC curve
plot(ROCperf)

# Add colors
plot(ROCperf, colorize=TRUE)
# Add threshold labels 
plot(ROCperf, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7))


                #Prediction on best model

# Predictions on the test set
predictTest2 = predict(frm_best, type="response", newdata=frm_test)

# Confusion matrix with threshold of 0.5
table(frm_test$TenYearCHD, predictTest2 > 0.5)

#Sensitivity & Specificity
17/195
1073/1085

# Accuracy
(1073+17)/(1073+12+178+17)

# Baseline accuracy
(1073+12)/(1073+12+178+17) 

# Confusion matrix with threshold of 0.2
table(frm_test$TenYearCHD, predictTest2 > 0.2)

#Sensitivity & Specificity
93/195
842/1085

# Accuracy
(842+93)/(842+243+102+93)

# Baseline accuracy
(842+243)/(842+243+102+93) 


# Test set AUC 

ROCpred2 = prediction(predictTest2, frm_test$TenYearCHD)
as.numeric(performance(ROCpred2, "auc")@y.values)

# Performance function
ROCperf = performance(ROCpred2, "tpr", "fpr")

# Plot ROC curve
plot(ROCperf)

# Add colors
plot(ROCperf, colorize=TRUE)
# Add threshold labels 
plot(ROCperf, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7))
