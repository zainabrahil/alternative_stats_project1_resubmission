#######################################################
# For evaluation:
#######################################################
library(moments)  # skewness
library(corrplot)  # corrplot
library(stringr) # for output

rm(list=ls())
source('helper_fxn.R')

corr_thresh = .7;
skew_thresh = .75;
min_abs_corr = .3;

d = read.csv('train.csv');

rmse = c();
time = list();

#10-fold cross validation
k = 10;
fold = sample(rep(1:k,each=nrow(d)/k),nrow(d),replace=F);

for (f in 1:k){
  
  ptm <- proc.time();
  
  train = d[fold != f,];
  test = d[fold == f,];

  output <- transform(train,test,corr_thresh,skew_thresh,min_abs_corr);
  train <- attr(output,"train");
  test <- attr(output,"test");
  highCor <- attr(output,"highCor");
  
  mlr = lm(SalePrice ~ ., data = train[, names(train) %in% c('SalePrice',names(highCor))])
  
  yHat = predict(mlr, newdata = test);
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




