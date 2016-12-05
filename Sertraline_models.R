library(glmnet)

# Lasso usig binaryTrain_SERTRALINE
binaryTrain_SERTRALINE.m <- as.matrix(binaryTrain_SERTRALINE)

binaryTrain_SERTRALINE.lasso <- glmnet(binaryTrain_SERTRALINE.m [,-c(56)], 
                                      binaryTrain_SERTRALINE.m [,c(56)], alpha=1, family="binomial")
plot(binaryTrain_SERTRALINE.lasso ,xvar="lambda",label=TRUE)

lamdas_to_try <- c(-6.5,-5.5,-4.25)

#predicting
dummy <- binaryTest_SERTRALINE
dummy$serious <- NULL
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
coef(multiTrain_SERTRALINE.lasso, s=s)

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
