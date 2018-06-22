library(gbm)

source("helper_fxn.R")

#load in data
train <- read.csv('train.csv',header=T);
test <- read.csv('test.csv',header=T);
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

ytest = predict(current_model,newdata=test);

ytest = exp(ytest) - 1;

submission = data.frame(test$Id); names(submission) = "Id";
submission$SalePrice = ytest
write.table(submission, 'mysubmission3.txt', row.names = FALSE, sep = ',')