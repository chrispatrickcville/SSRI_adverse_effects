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

