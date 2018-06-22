#helper function for transforming data:

#code below for preprocessing data adapted from:
#https://piazza-resources.s3.amazonaws.com/iwgts4iuaq41h1/iyf5s5pqnnpae/Iowa_housing_Jan27.html?AWSAccessKeyId=AKIAIEDNRLJ4AZKBW6HA&Expires=1487739124&Signature=j5tv%2B0Qbf%2B6TZbb%2BZcoFTEiioUk%3D
transform <- function(train,test,corr_thresh,skew_thresh,min_abs_corr){
  
  output <- list();
  
  # apply function: use the given function (is.na) for each column (use 2 for column, and use 1 for row) 
  # in the matrix train[, -c(1, 81)].
  numNA = colSums(apply(train[, -c(1, 81)], 2, is.na))
  number_of_missing = numNA[which(numNA != 0)]  # number of NA's
  data_type = sapply(train[,names(which(numNA != 0))], class)  # type of data
  cbind(number_of_missing, data_type)
  ## dropping data with the most empty information
  drops = c("Alley", "PoolQC", "Fence", "MiscFeature", "FireplaceQU")
  train = train[ , !(names(train) %in% drops)]
  test = test[ , !(names(test) %in% drops)]
  
  data.type = sapply(train[, -c(1, ncol(train))], class)  # as we've removed 4 variables
  cat_var = names(train)[which(c(NA, data.type, NA) == 'factor')]  # categorical variables
  numeric_var =  names(train)[which(c(NA, data.type, NA) == 'integer')]  # continuous variables
  for (j in cat_var){
    train[, j] = addNA(train[, j])  # addNA treat the NA's as a new level called '<NA>'
    test[, j] = addNA(test[, j])
  }
  
  tempVar = c('LotFrontage', 'MasVnrArea', 'GarageYrBlt')
  for (j in tempVar){
    na.id = is.na(train[, j])  # binary indicator: NA (1) or not (0)
    tempMedian = median(train[, j], na.rm = TRUE)  # find the median
    train[which(na.id), j] = tempMedian
  }
  
  for (j in numeric_var){
    na.id = is.na(test[, j])
    if (!any(na.id)){
      next
    }
    test[which(na.id), j] = median(train[, j])
  }
  
  train$SalePrice <- log(train$SalePrice + 1 )
  
  if("SalePrice" %in% names(test)){
    test$SalePrice <- log(test$SalePrice + 1 )
  }
  
  # for numeric feature with excessive skewness, perform log transformation
  # determine skew for each numeric feature
  skewed_feats = sapply(train[, numeric_var], skewness)
  # only log transform features that exceed a threshold = 0.75 for skewness
  skewed_feats = numeric_var[which(skewed_feats > skew_thresh)]
  for(j in skewed_feats) {
    train[, j] = log(train[, j] + 1)
    test[, j] = log(test[, j] + 1)
  }
  
  correlations = cor(train[, c(numeric_var, 'SalePrice')])  # correlation matrix
  
  #for those relatively large correlations (> 0.3)
  row_indic = apply(correlations, 1, function(x) sum(abs(x) > min_abs_corr) > 1)
  correlations = correlations[row_indic, row_indic]
  
  highCor = which(abs(correlations[, ncol(correlations)]) > corr_thresh)
  highCor = highCor[-length(highCor)]
  names(highCor)
  
  attr(output,'train') = train;
  attr(output,'test') = test;
  attr(output,'highCor') = highCor;
  
  return(output);
}


##compute root mean squared error between x and y
RMSE <- function(x,y){
  a <- sqrt(sum((log(x)-log(y))^2)/length(y))
  return(a)
}
