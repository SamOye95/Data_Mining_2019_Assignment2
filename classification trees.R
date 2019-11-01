library(rpart.plot)		

source("dataclean.R")
#This function trains our flexible classification tree
# Arugements: 
# dtm : 
# labels
# complexity: the cost-complexity pruning parameter (called CP in Rpart) 
# for classification trees,

train.class <- function(dtm, labels)
{
  dtm.frame <- data.frame(as.matrix(dtm), label=labels)
  dtm.tree <- rpart(label~., method="class", data = dtm.frame, control = rpart.control(xval=10, minsplit=4, cp = 0, maxdepth=10))
  # simple tree for plotting
  dev.new(width = 550, height = 330, unit = "px")
  #plot(dtm.tree)
  #Visualize the decision tree with rpart.plot
  #rpart.plot(dtm.tree, box.palette = "RdBu", shadow.col = "gray", nn=TRUE)
  
  #pruned tree
  dtm.tree.pruned <-prune(dtm.tree,cp= 0.0137)
  #Visualize the decision tree with rpart.plot
  rpart.plot(dtm.tree.pruned, box.palette = "RdBu", shadow.col = "gray", nn=TRUE)
  #plot(dtm.tree.pruned)
  
  return (dtm.tree.pruned)
}

predict.class <- function(model, dtm)
{
  test.frame <- data.frame(as.matrix(dtm))
  pred.tree <- predict(model, test.frame, type="class")
  return(class.pred <- as.numeric(pred.tree)-1)
}

#ct_unimodel <- train.class(Unigramdftrain[,row.names(data.frame( tail(sort(colSums(Unigramdftrain) ),1000)))], trainlabels)
#ct_unimodel$cptable
#plotcp(ct_unimodel)

bidata =Bigramdftrain[row.names(data.frame(tail(sort(colSums(Bigramdftrain) ),1000)))]

ct_bimodel <- train.class(data.frame(as.matrix(bidata)), trainlabels)

#result_ct_uni <- table(testlabels, predict.class(ct_unimodel, Unigramdftest))

result_ct_bi <- table(testlabels, predict.class(ct_bimodel, Bigramdftest))