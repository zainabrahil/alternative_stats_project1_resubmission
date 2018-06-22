library(gbm)

rm(list=ls())
source("helper_fxn.R")

#load in data
d <- read.csv('train.csv',header=T);

rmse = c();
time = list();

#5-fold cross validation
k = 5;
fold = sample(rep(1:k,each=nrow(d)/k),nrow(d),replace=F);

for (f in 1:k){
  
  ptm <- proc.time();
  
  train = d[fold != f,];
  test = d[fold == f,];

  corr_thresh = 0;
  min_abs_corr = 0;
  skew_thresh = .75;
  
  output <- transform(train,test,corr_thresh,skew_thresh,min_abs_corr);
  train <- attr(output,"train");
  test <- attr(output,"test");
  
  current_model <- gbm(SalePrice ~., 
                           data = train[,-1], #remove ID column
                           distribution = "laplace",
                           shrinkage = 0.05,
                           interaction.depth = 5,
                           bag.fraction = 0.66,
                           n.minobsinnode = 1,
                           cv.folds = 100,
                           keep.data = F,
                           verbose = F,
                           n.trees = 1000);
  
  yHat = predict(current_model,newdata=test);
  
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