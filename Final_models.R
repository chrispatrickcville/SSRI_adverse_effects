#####################
### CILATROPRAM   ###
#####################

###Lasso using multiTrain_CITALOPRAM as an example

#Training the 'LASSO'
multiTrain_CITALOPRAM.m <- as.matrix(multiTrain_CITALOPRAM)
View(multiTrain_CITALOPRAM.m)
multiTrain_CITALOPRAM.lasso <- glmnet(multiTrain_CITALOPRAM.m [,-c(56)], multiTrain_CITALOPRAM.m [,c(56)], alpha=1, family="multinomial")
plot(multiTrain_CITALOPRAM.lasso ,xvar="lambda",label=TRUE)

#predicting
dummy <- multiTest_CITALOPRAM
dummy$level <- NULL
dummy.m <- data.matrix(dummy)

#NOTE: Make sure to explore appropriate log(lambda) value in the 's' parameter
multiTest_CITALOPRAM$predict <- predict(multiTrain_CITALOPRAM.lasso, newx=dummy.m, s=exp(-5.9), family="multinomial", type="class")

#checking accuracy of predictions
sum(multiTest_CITALOPRAM$level==multiTest_CITALOPRAM$predict)/nrow(multiTest_CITALOPRAM)
table(multiTest_CITALOPRAM$predict)

#Accuracy for s=exp(-7.7) is 0.4337885
#accuracy for s=exp(-7.3) is 0.4339473*
#Accuracy for s=exp(-5.9) is 0.432677


#checking coefficients 
#NOTE: make sure to match the 's' parameter to that in line 19
coef(multiTrain_CITALOPRAM.lasso, s=exp(-7.3))

#NAIVE model predicts the most frequent level 
table(multiTrain_CITALOPRAM$level)
sum(multiTest_CITALOPRAM$level=='h')/nrow(multiTest_CITALOPRAM)
#0.035757 accuracy

#Precision and Recall for multiTrain_CITALOPRAM
# Get confusion matrix for multiclass predictor
lvls <- c('death','d','dl','h','hd','hdl','hl','l','other')
CITALOPRAM_mat <- table(factor(multiTest_CITALOPRAM$predict, levels=lvls), multiTest_CITALOPRAM$level)

# Get precision for each class 
CITALOPRAM_precision <- (precision <- diag(CITALOPRAM_mat) / rowSums(CITALOPRAM_mat))
CITALOPRAM_precision
#Death            h         other
#0.0259542    0.4462520     0.39287642

# Get recall for each class
CITALOPRAM_recall <- (diag(CITALOPRAM_mat) / colSums(CITALOPRAM_mat))
CITALOPRAM_recall
#d              h           other
#0.1611374    0.6212256     0.3616493

#Lasso using binaryTrain_CITALOPRAM
binaryTrain_CITALOPRAM.m <- as.matrix(binaryTrain_CITALOPRAM)

binaryTrain_CITALOPRAM.lasso <- glmnet(binaryTrain_CITALOPRAM.m [,-c(56)], 
                                       binaryTrain_CITALOPRAM.m [,c(56)], alpha=1, family="binomial")
plot(binaryTrain_CITALOPRAM.lasso ,xvar="lambda",label=TRUE)

lamdas_to_try <- c(-6.5,-5.5,-4.25)

#predicting
dummy <- binaryTest_CITALOPRAM
dummy$serious <- NULL
dummy.m <- data.matrix(dummy)

#NOTE: Make sure to explore appropriate log(lambda) value in the 's' parameter
s <- exp(-6.5)
binaryTest_CITALOPRAM$predict <- predict(binaryTrain_CITALOPRAM.lasso, newx=dummy.m, 
                                         s=s, family="binomial", type="class")

#checking accuracy of predictions
sum(binaryTest_CITALOPRAM$serious==binaryTest_CITALOPRAM$predict)/nrow(binaryTest_CITALOPRAM)
table(binaryTest_CITALOPRAM$predict)
table(binaryTrain_CITALOPRAM$serious)

#accuracy for Lambda = exp(-6.5), 0.7694373
#accuracy for Lambda = exp(-5.5), 0.7676065
#accuracy for Lambda = exp(-4.25), 0.7676065

#Naive Model Predicts
table(binaryTrain_CITALOPRAM$serious)
sum(binaryTest_CITALOPRAM$serious=='1')/nrow(binaryTest_CITALOPRAM)
#0.773099

#checking coefficients 
#NOTE: make sure to match the 's' parameter to that in line 19
coef(binaryTrain_CITALOPRAM.lasso, s=s)

#ROC curve for binaryTrain_CITALOPRAM
library(ROCR)
probs <- predict(binaryTrain_CITALOPRAM.lasso, newx=dummy.m, s=s, family="binomial", type = "response")
length(binaryTest_CITALOPRAM$serious)
length(probs)

# ROCR prediction object and extract ROC curve and AUC
prediction <- prediction(probs, binaryTest_CITALOPRAM$serious)
roc.curve <- performance(prediction,"tpr","fpr")
auc = performance(prediction, "auc")@y.values[[1]]

# plot
plot(roc.curve, main = paste("ROC (AUC=", round(auc,2), ")", sep=""))
abline(0, 1, lty="dashed")

#######################
### ESCILATROPRAM   ###
#######################

require(glmnet)
###Lasso using multiTrain_ESCITALOPRAM as an example

#Training the 'LASSO'
multiTrain_ESCITALOPRAM.m <- as.matrix(multiTrain_ESCITALOPRAM)
View(multiTrain_ESCITALOPRAM.m)
multiTrain_ESCITALOPRAM.lasso <- glmnet(multiTrain_ESCITALOPRAM.m [,-c(56)], multiTrain_ESCITALOPRAM.m [,c(56)], alpha=1, family="multinomial")
plot(multiTrain_ESCITALOPRAM.lasso ,xvar="lambda",label=TRUE)

#predicting
dummy <- multiTest_ESCITALOPRAM
dummy$level <- NULL
dummy.m <- data.matrix(dummy)

#NOTE: Make sure to explore appropriate log(lambda) value in the 's' parameter
multiTest_ESCITALOPRAM$predict <- predict(multiTrain_ESCITALOPRAM.lasso, newx=dummy.m, s=exp(-5.5), family="multinomial", type="class")

#checking accuracy of predictions
sum(multiTest_ESCITALOPRAM$level==multiTest_ESCITALOPRAM$predict)/nrow(multiTest_ESCITALOPRAM)
table(multiTest_ESCITALOPRAM$predict)

#Accuracy for s=exp(-8.5) is 0.4553
#accuracy for s=exp(-7) is 0.4559
#Accuracy for s=exp(-5.5) is 0.4559


#checking coefficients 
#NOTE: make sure to match the 's' parameter to that in line 19
coef(multiTrain_ESCITALOPRAM.lasso, s=exp(-5.5))

#NAIVE model predicts the most frequent level 
table(multiTrain_ESCITALOPRAM$level)
sum(multiTest_ESCITALOPRAM$level=='h')/nrow(multiTest_ESCITALOPRAM)
#0.4062168 accuracy

#Precision and Recall for multiTrain_ESCITALOPRAM
# Get confusion matrix for multiclass predictor
lvls <- c('death','d','dl','h','hd','hdl','hl','l','other')
ESCITALOPRAM_mat <- table(factor(multiTest_ESCITALOPRAM$predict, levels=lvls), multiTest_ESCITALOPRAM$level)

# Get precision for each class 
ESCITALOPRAM_precision <- (precision <- diag(ESCITALOPRAM_mat) / rowSums(ESCITALOPRAM_mat))
ESCITALOPRAM_precision
#h              other
#0.4602740     0.4560616

# Get recall for each class
ESCITALOPRAM_recall <- (diag(ESCITALOPRAM_mat) / colSums(ESCITALOPRAM_mat))
ESCITALOPRAM_recall
#h            other
#0.6592544    0.5049716 

#Lasso using binaryTrain_ESCITALOPRAM
binaryTrain_ESCITALOPRAM.m <- as.matrix(binaryTrain_ESCITALOPRAM)

binaryTrain_ESCITALOPRAM.lasso <- glmnet(binaryTrain_ESCITALOPRAM.m [,-c(56)], 
                                         binaryTrain_ESCITALOPRAM.m [,c(56)], alpha=1, family="binomial")
plot(binaryTrain_ESCITALOPRAM.lasso ,xvar="lambda",label=TRUE)

lamdas_to_try <- c(-6.5,-5.5,-4.25)

#predicting
dummy <- binaryTest_ESCITALOPRAM
dummy$serious <- NULL
dummy.m <- data.matrix(dummy)

#NOTE: Make sure to explore appropriate log(lambda) value in the 's' parameter
s <- exp(-5.5)
binaryTest_ESCITALOPRAM$predict <- predict(binaryTrain_ESCITALOPRAM.lasso, newx=dummy.m, 
                                           s=s, family="binomial", type="class")

#checking accuracy of predictions
sum(binaryTest_ESCITALOPRAM$serious==binaryTest_ESCITALOPRAM$predict)/nrow(binaryTest_ESCITALOPRAM)
table(binaryTest_ESCITALOPRAM$predict)
table(binaryTrain_ESCITALOPRAM$serious)

#accuracy for Lambda = exp(-6.5), 0.6976744
#accuracy for Lambda = exp(-5.5), 0.6980407**
#accuracy for Lambda = exp(-4.25),0.6973082

#Naive Model Predicts
table(binaryTrain_ESCITALOPRAM$serious)
sum(binaryTest_ESCITALOPRAM$serious=='1')/nrow(binaryTest_ESCITALOPRAM)
#0.6947446

#checking coefficients 
#NOTE: make sure to match the 's' parameter to that in line 19
coef(binaryTrain_ESCITALOPRAM.lasso, s=s)

#ROC curve for binaryTrain_ESCITALOPRAM
library(ROCR)
probs <- predict(binaryTrain_ESCITALOPRAM.lasso, newx=dummy.m, s=s, family="binomial", type = "response")
length(binaryTest_ESCITALOPRAM$serious)
length(probs)

# ROCR prediction object and extract ROC curve and AUC
prediction <- prediction(probs, binaryTest_ESCITALOPRAM$serious)
roc.curve <- performance(prediction,"tpr","fpr")
auc = performance(prediction, "auc")@y.values[[1]]

# plot
plot(roc.curve, main = paste("ROC (AUC=", round(auc,2), ")", sep=""))
abline(0, 1, lty="dashed")

#######################
###   SERTRALINE    ###
#######################

library(glmnet)
library(ROCR)

# Lasso using binaryTrain_SERTRALINE
binaryTrain_SERTRALINE.m <- data.matrix(binaryTrain_SERTRALINE)

binaryTrain_SERTRALINE.lasso <- glmnet(binaryTrain_SERTRALINE.m [,-c(56)], 
                                       binaryTrain_SERTRALINE.m [,c(56)], alpha=1, family="binomial")
plot(binaryTrain_SERTRALINE.lasso ,xvar="lambda",label=TRUE)
title("LASSO Trace Plot: Sertraline", line=3)

lamdas_to_try <- c(-6.5,-5.5,-4.25)

#predicting
dummy <- binaryTest_SERTRALINE
dummy$serious <- NULL
dummy$predict <- NULL
dummy.m <- data.matrix(dummy)

#NOTE: Make sure to explore appropriate log(lambda) value in the 's' parameter
s <- exp(-4.25)
binaryTest_SERTRALINE$predict <- predict(binaryTrain_SERTRALINE.lasso, newx=dummy.m, 
                                         s=s, family="binomial", type="class")

#checking accuracy of predictions
sum(binaryTest_SERTRALINE$serious==binaryTest_SERTRALINE$predict)/nrow(binaryTest_SERTRALINE)
table(binaryTest_SERTRALINE$predict)
table(binaryTrain_SERTRALINE$serious)

#checking coefficients 
#NOTE: make sure to match the 's' parameter to that in line 19
coef(binaryTrain_SERTRALINE.lasso, s=s)

# Lambda = exp(-6.5), performance 0.6884905
# Lambda = exp(-5.5), performance 0.6885998 ** best performance
# Lambda = exp(-4.8), performance 0.6863045 

#NAIVE model predicts the most frequent level  = 0.6863045
table(binaryTrain_SERTRALINE$serious)
sum(binaryTest_SERTRALINE$serious=='1')/nrow(binaryTest_SERTRALINE)

# model with best lambda
s <- exp(lamdas_to_try[2])
probs <- predict(binaryTrain_SERTRALINE.lasso, newx=dummy.m, s=s, family="binomial", type = "response")
length(binaryTest_SERTRALINE$serious)
length(probs)

sort(as.numeric(probs))

# ROCR prediction object and extract ROC curve and AUC
prediction <- prediction(probs, binaryTest_SERTRALINE$serious)
roc.curve <- performance(prediction,"tpr","fpr")
auc = performance(prediction, "auc")@y.values[[1]]

# plot
plot(roc.curve, main = paste("ROC for 'Serious' Predictive Model: Sertraline \n(AUC=", round(auc,2), ")", sep=""))
abline(0, 1, lty="dashed")

######################################################################

###Lasso using multiTrain_SERTRALINE 
#Training the 'LASSO'
multiTrain_SERTRALINE.m <- as.matrix(multiTrain_SERTRALINE)

multiTrain_SERTRALINE.lasso <- glmnet(multiTrain_SERTRALINE.m [,-c(56)], 
                                      multiTrain_SERTRALINE.m [,c(56)], alpha=1, family="multinomial")
plot(multiTrain_SERTRALINE.lasso ,xvar="lambda",label=TRUE)

lamdas_to_try <- c(-10.5,-6.5,-4.8)

#predicting
dummy <- multiTest_SERTRALINE
dummy$predict <- NULL
dummy$level <- NULL
dummy.m <- data.matrix(dummy)

#NOTE: Make sure to explore appropriate log(lambda) value in the 's' parameter
s <- exp(-6.5)
multiTest_SERTRALINE$predict <- predict(multiTrain_SERTRALINE.lasso, newx=dummy.m, 
                                        s=s, family="multinomial", type="class")

#checking accuracy of predictions
sum(multiTest_SERTRALINE$level==multiTest_SERTRALINE$predict)/nrow(multiTest_SERTRALINE)
table(multiTest_SERTRALINE$predict)
table(multiTrain_SERTRALINE$level)

#checking coefficients 
#NOTE: make sure to match the 's' parameter to that in line 19
options(scipen=999)
coef(multiTrain_SERTRALINE.lasso, s=s)$death

# Lambda = exp(-10.5), performance 0.4664532
# Lambda = exp(-6.5), performance 0.4675741 ** best
# Lambda = exp(-4.8), performance 0.4630905

#NAIVE model predicts the most frequent level  = 0.4131305
table(multiTrain_SERTRALINE$level)
sum(multiTest_SERTRALINE$level=='other')/nrow(multiTest_SERTRALINE)

####### 

# Get confusion matrix for multiclass predictor
lvls <- c('death','d','dl','h','hd','hdl','hl','l','other')
SERTRALINE_mat <- table(factor(multiTest_SERTRALINE$predict, levels=lvls), multiTest_SERTRALINE$level)

# Get precision for each class 
SERTRALINE_precision <- (precision <- diag(SERTRALINE_mat) / rowSums(SERTRALINE_mat))
SERTRALINE_precision

# Get recall for each class
SERTRALINE_recall <- (diag(SERTRALINE_mat) / colSums(SERTRALINE_mat))
SERTRALINE_recall

#######

#######################
###      PAXIL      ###
###  (PAROXETINE)   ### 
#######################



#######################
###   FLUOXETINE    ### 
#######################

############################ Binary  ############################
### Train the 'LASSO'
binaryTrain_FLUOXETINE.m <- as.matrix(binaryTrain_FLUOXETINE)
binaryTrain_FLUOXETINE.lasso <- glmnet(binaryTrain_FLUOXETINE.m [,-c(56)], binaryTrain_FLUOXETINE.m [,c(56)], alpha=1, family="binomial")
plot(binaryTrain_FLUOXETINE.lasso, xvar="lambda", label=TRUE)

### Predict on Testing Set 
# NAIVE model predicts the most frequent level 
table(binaryTrain_FLUOXETINE$serious)
sum(binaryTest_FLUOXETINE$serious=='1')/nrow(binaryTest_FLUOXETINE) # = 0.8190158
dummy <- binaryTest_FLUOXETINE
dummy$serious <- NULL
dummy$predict <- NULL
dummy.m <- data.matrix(dummy)

## Pick a lambda to explore
x <- -5 # Also looked at -7.5, -4.5, -4
# Predict (with s=x)
binaryTest_FLUOXETINE$predict <- predict(binaryTrain_FLUOXETINE.lasso, newx=dummy.m, s=exp(x), family="binomial", type="class")

# Check accuracy of predictions
sum( binaryTest_FLUOXETINE$serious == binaryTest_FLUOXETINE$predict ) / nrow( binaryTest_FLUOXETINE )
table(binaryTest_FLUOXETINE$predict)
table(binaryTest_FLUOXETINE$serious)

### Check coefficients 
coef(binaryTrain_FLUOXETINE.lasso, s=exp(x))

#######

# Model with best lambda
binaryTrain_FLUOXETINE.m <- as.matrix(binaryTrain_FLUOXETINE)
probs <- predict(binaryTrain_FLUOXETINE.lasso, newx=dummy.m, s=exp(x), family="binomial", type = "response")
length(binaryTest_FLUOXETINE$serious)
length(probs)
# ROCR prediction object and extract ROC curve and AUC
prediction <- prediction(probs, binaryTest_FLUOXETINE$serious)
roc.curve <- performance(prediction,"tpr","fpr")
auc = performance(prediction, "auc")@y.values[[1]]
# plot
plot(roc.curve, main = paste("ROC (AUC=", round(auc,2), ")", sep=""))
abline(0, 1, lty="dashed")

#######

## Lambda = -7.5, accuracy = 0.8128996, AUC = 0.71
## Lambda = -5, accuracy = 0.8154017, AUC = 0.71 ***
## Lambda = -4.5, accuracy = 0.8190158, , AUC = 0.7
## Lambda = -4, accuracy = 0.8190158, AUC = 0.68


############################ Multi  ############################
# Train the 'LASSO'
multiTrain_FLUOXETINE.m <- as.matrix(multiTrain_FLUOXETINE)
multiTrain_FLUOXETINE.lasso <- glmnet(multiTrain_FLUOXETINE.m [,-c(56)], multiTrain_FLUOXETINE.m [,c(56)], alpha=1, family="multinomial")
plot(multiTrain_FLUOXETINE.lasso ,xvar="lambda",label=TRUE)

### Predict on Testing Set
# NAIVE model predicts the most frequent level  
table(multiTrain_FLUOXETINE$level)
sum(multiTest_FLUOXETINE$level=='other')/nrow(multiTest_FLUOXETINE) # = 0.3487

dummy <- multiTest_FLUOXETINE
dummy$level <- NULL
dummy$predict <- NULL
dummy.m <- data.matrix(dummy)

## Pick a lambda to explore
x <- -8 # Also looked at -8.5, -7.5, -6, -5
multiTest_FLUOXETINE$predict <- predict(multiTrain_FLUOXETINE.lasso, newx=dummy.m, s=exp(x), family="multinomial", type="class")

# Check accuracy of predictions
sum(multiTest_FLUOXETINE$level==multiTest_FLUOXETINE$predict)/nrow(multiTest_FLUOXETINE)
table(multiTest_FLUOXETINE$predict)

## Lambda = -8.5, accuracy = 0.434976
## Lambda = -8, accuracy = 0.4366872 ***
## Lambda = -7.5, accuracy = 0.436345
## Lambda = -6, accuracy = 0.4315537
## Lambda = -5, accuracy = 0.426078

# Check coefficients 
options(scipen=999)
coef(multiTrain_FLUOXETINE.lasso, s=exp(x))$death

####### 

# Confusion matrix for multiclass predictor
lvls <- c('death','d','dl','h','hd','hdl','hl','l','other')
FLUOXETINE_mat <- table(factor(multiTest_FLUOXETINE$predict, levels=lvls), multiTest_FLUOXETINE$level)
# Precision for each class 
FLUOXETINE_precision <- (precision <- diag(FLUOXETINE_mat) / rowSums(FLUOXETINE_mat))
FLUOXETINE_precision
# Recall for each class
FLUOXETINE_recall <- (diag(FLUOXETINE_mat) / colSums(FLUOXETINE_mat))
FLUOXETINE_recall

#######
# Lasso usig binaryTrain_PAXIL
binaryTrain_PAXIL.m <- as.matrix(binaryTrain_PAXIL)

binaryTrain_PAXIL.lasso <- glmnet(binaryTrain_PAXIL.m [,-c(56)], 
                                       binaryTrain_PAXIL.m [,c(56)], alpha=1, family="binomial")
plot(binaryTrain_PAXIL.lasso ,xvar="lambda",label=TRUE)

lamdas_to_try <- c(-7,-6,-5,-4.5)

#predicting
dummy <- binaryTest_PAXIL
dummy$serious <- NULL
dummy.m <- data.matrix(dummy)

#NOTE: Make sure to explore appropriate log(lambda) value in the 's' parameter
s <- exp(lamdas_to_try[2])
binaryTest_PAXIL$predict <- predict(binaryTrain_PAXIL.lasso, newx=dummy.m, 
                                         s=s, family="binomial", type="class")

#checking accuracy of predictions
sum(binaryTest_PAXIL$serious==binaryTest_PAXIL$predict)/nrow(binaryTest_PAXIL)
table(binaryTest_PAXIL$predict)
table(binaryTest_PAXIL$serious)

#checking coefficients 
#NOTE: make sure to match the 's' parameter to that in line 19
options(scipen = 99)
coef(binaryTrain_PAXIL.lasso, s=s)

# Lambda = exp(-7), performance 0.6857143
# Lambda = exp(-6), performance 0.688417 ** best performance
# Lambda = exp(-5), performance 0.6864865 
# Lambda = exp(-4.5), performance 0.6861004 

#NAIVE model predicts the most frequent level  = 0.6853282
table(binaryTrain_PAXIL$serious)
sum(binaryTest_PAXIL$serious=='1')/nrow(binaryTest_PAXIL)

library(ROCR)
# model with best lambda
s <- exp(lamdas_to_try[2])
probs <- predict(binaryTrain_PAXIL.lasso, newx=dummy.m, s=s, family="binomial", type = "response")
length(binaryTest_PAXIL$serious)
length(probs)

# ROCR prediction object and extract ROC curve and AUC
prediction <- prediction(probs, binaryTest_PAXIL$serious)
roc.curve <- performance(prediction,"tpr","fpr")
auc = performance(prediction, "auc")@y.values[[1]]

# plot
plot(roc.curve, main = paste("ROC (AUC=", round(auc,2), ")", sep=""))
abline(0, 1, lty="dashed")

######################################################################

###Lasso using multiTrain_PAXIL 
#Training the 'LASSO'
multiTrain_PAXIL.m <- as.matrix(multiTrain_PAXIL)

multiTrain_PAXIL.lasso <- glmnet(multiTrain_PAXIL.m [,-c(56)], 
                                      multiTrain_PAXIL.m [,c(56)], alpha=1, family="multinomial")
plot(multiTrain_PAXIL.lasso ,xvar="lambda",label=TRUE)

lamdas_to_try <- c(-4,-5.7,-6,-6.2,-8)

#predicting
dummy <- multiTest_PAXIL
dummy$level <- NULL
dummy.m <- data.matrix(dummy)

#NOTE: Make sure to explore appropriate log(lambda) value in the 's' parameter
s <- exp(lamdas_to_try[4])
multiTest_PAXIL$predict <- predict(multiTrain_PAXIL.lasso, newx=dummy.m, 
                                        s=s, family="multinomial", type="class")

#checking accuracy of predictions
sum(multiTest_PAXIL$level==multiTest_PAXIL$predict)/nrow(multiTest_PAXIL)
table(multiTest_PAXIL$predict)
table(multiTrain_PAXIL$level)

#checking coefficients 
#NOTE: make sure to match the 's' parameter to that in line 19
coef(multiTrain_PAXIL.lasso, s=s)

# Lambda = exp(-4), performance 0.4386181
# Lambda = exp(-5), performance 0.4454041
# Lambda = exp(-6), performance 0.446021
# Lambda = exp(-6.2), performance 0.4466379 ** best
# Lambda = exp(-8), performance 0.4423196

#NAIVE model predicts the most frequent level  = 0.407773
table(multiTrain_PAXIL$level)
sum(multiTest_PAXIL$level=='other')/nrow(multiTest_PAXIL)

####### 

# Get confusion matrix for multiclass predictor
lvls <- c('death','d','dl','h','hd','hdl','hl','l','other')
PAXIL_mat <- table(factor(multiTest_PAXIL$predict, levels=lvls), multiTest_PAXIL$level)

# Get precision for each class 
PAXIL_precision <- (precision <- diag(PAXIL_mat) / rowSums(PAXIL_mat))
PAXIL_precision
# death         d        dl         h        hd       hdl        hl         l     other 
# 0.2666667       NaN       NaN 0.4240232 0.2500000       NaN       NaN       NaN 0.4676180 

# Get recall for each class
PAXIL_recall <- (diag(PAXIL_mat) / colSums(PAXIL_mat))
PAXIL_recall
#      death          d         dl          h         hd        hdl         hl          l 
# 0.02325581 0.00000000 0.00000000 0.45852895 0.02857143 0.00000000 0.00000000 0.00000000 
# other 
# 0.64447806 
