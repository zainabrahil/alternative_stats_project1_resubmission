#######################################################
# For submission:
#######################################################

source('helper_fxn.R')

train = read.csv('train.csv');
test = read.csv('test.csv');
corr_thresh = .7;
skew_thresh = .75;
min_abs_corr = .3;

output <- transform(train,test,corr_thresh,skew_thresh,min_abs_corr);
train <- attr(output,"train");
test <- attr(output,"test");
highCor <- attr(output,"highCor");

mlr = lm(SalePrice ~ ., data = train[, names(train) %in% c('SalePrice',names(highCor))])

yHat = predict(mlr, newdata = test);
yHat = exp(yHat) - 1;

submission = data.frame(test$Id); names(submission) = "Id";
submission$SalePrice = yHat
write.table(submission, 'mysubmission1.txt', row.names = FALSE, sep = ',')




