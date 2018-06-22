library(leaps)  # regsubsets
library(glmnet)  # glmnet for lasso and ridge
library(moments)  # skewness
library(corrplot)  # corrplot
library(pls)      

rm(list=ls())
source('helper_fxn.R')

#load in data
d <- read.csv('train.csv',header=T);

rmse = c();
time = list();

#10-fold cross validation
k = 10;
fold = sample(rep(1:k,each=nrow(d)/k),nrow(d),replace=F);

for (f in 1:k){
  
  ptm <- proc.time();
  
  train = d[fold != f,];
  test = d[fold == f,];

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
  plot(fit)
  
  cv.out = cv.glmnet(X, Y, alpha=1)  # lambda sequence set by glmnet
  plot(cv.out)
  
  cv.out$lambda.min
  cv.out$lambda.1se
  mdl1min.coef=as.matrix(coef(cv.out, s="lambda.min"))
  mdl1se.coef=as.matrix(coef(cv.out, s="lambda.1se"))
  
  crit = 1e-8
  
  #rownames(sort(mdl1min.coef[abs(mdl1min.coef)>crit]))
  rslt=sort(mdl1se.coef[abs(mdl1se.coef)>crit],decreasing=TRUE,index.return=TRUE)
  var=rownames(mdl1se.coef)[unlist(as.matrix(rslt)[2])]
  
  ##### Make prediction on the test data ######
  Xtest = data.matrix(test[,-c(1,77)])
  yHat = predict(cv.out,newx=Xtest,s="lambda.1se")
  y = test$SalePrice;
  
  yHat = exp(yHat) - 1;
  y = exp(test$SalePrice) - 1;
  
  rmse[f] = RMSE(y,yHat);
  time[[f]] = proc.time() -ptm;

}

str_c('mean rmse: ',mean(rmse))
str_c('sd rmse: ',sd(rmse))

elapsed_time <- c();
for(i in 1:k){
  elapsed_time[i] <- time[[i]][3];
}

str_c('mean time:',mean(elapsed_time))
str_c('sd time: ',sd(elapsed_time))

