library(leaps)  # regsubsets
library(glmnet)  # glmnet for lasso and ridge
library(moments)  # skewness
library(corrplot)  # corrplot
library(pls)      

source('helper_fxn.R')

#set workspace
#setwd("C:\\Users\\mzedc\\Dropbox\\stat542\\Iowa housing data")

#load in data
train <- read.csv('train.csv',header=T);
test <- read.csv('test.csv',header=T);

###############################################################################
corr_thresh = 0;
skew_thresh = .75;
min_abs_corr = 0;

output <- transform(train,test,corr_thresh,skew_thresh,min_abs_corr);
train <- attr(output,"train");
test <- attr(output,"test");
highCor <- attr(output,"highCor");
length(highCor) == ncol(output) - 1;

###############################################################################
X = data.matrix(train[,-c(1,77)]) #remove columns ID and SalePrice

Y = data.matrix(train[77])
var.all= names(train)[-1]

fit = glmnet(X, Y)
#plot(fit)

cv.out = cv.glmnet(X, Y, alpha=1)  # lambda sequence set by glmnet
#plot(cv.out)

cv.out$lambda.min
cv.out$lambda.1se
mdl1min.coef=as.matrix(coef(cv.out, s="lambda.min"))
mdl1se.coef=as.matrix(coef(cv.out, s="lambda.1se"))

crit = 1e-8

#rownames(sort(mdl1min.coef[abs(mdl1min.coef)>crit]))
rslt=sort(mdl1se.coef[abs(mdl1se.coef)>crit],decreasing=TRUE,index.return=TRUE)
var=rownames(mdl1se.coef)[unlist(as.matrix(rslt)[2])]

# ######### Use my own lambda sequence #########
# lam.seq = exp(seq(-6,2,length=100))
# cv.out = cv.glmnet(X,Y,alpha=1,lambda=lam.seq)
# plot(cv.out)
# 
# cv.out$lambda.min
# cv.out$lambda.1se
# 
# mdl2min.coef=as.matrix(coef(cv.out, s="lambda.min"))
# mdl2se.coef=as.matrix(coef(cv.out, s="lambda.1se"))
# 
# mdl2min.coef[abs(mdl2min.coef)>crit];
# mdl2se.coef[abs(mdl2se.coef)>crit];
# 
# rslt=sort(mdl2se.coef[abs(mdl2se.coef)>crit],decreasing=TRUE,index.return=TRUE)
# var=rownames(mdl2se.coef)[unlist(as.matrix(rslt)[2])]

##### Make prediction on the test data ######
Xtest = data.matrix(test[,-1])
ytest = predict(cv.out,newx=Xtest,s="lambda.1se")

ytest = exp(ytest) - 1

submission = data.frame(test$Id); names(submission) = "Id";
submission$SalePrice = ytest
write.table(submission, 'mysubmission2.txt', row.names = FALSE, sep = ',')
