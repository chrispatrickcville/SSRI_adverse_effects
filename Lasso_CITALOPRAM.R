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
