# This script contains our 
# Regularized logistic regression (discriminative linear classifer)
 
#This function uses cross validation to return a sequence of models that user can choose from.
# cv.glmnet returns a cv.glmnet object which is a list of all the cross-vaildation ingredients
# In K-flods cross validation the dataset is dividd in K parts, and K-1
# parts are used to predict the K-th part (this is done K times, using a different K part each time)
# lambda.min is the value of lamda that gives the mean cross-validated error.



train.logres <- function(train, train.labels){
  
  train.glmnet <- cv.glmnet(as.matrix(train),
                            train.labels,
                            family="binomial",
                            type.measure="class")
}

predict.logres <- function(model,test){
  
  train.logreg2.pred <- predict(model,
                                as.matrix(test),
                                s="lambda.min",
                                type="class")
  
}
