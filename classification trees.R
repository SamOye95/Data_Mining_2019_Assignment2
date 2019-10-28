

#This function trains our flexible classification tree
# Arugements: 
# dtm : 
# labels
# complexity: the cost-complexity pruning parameter (called CP in Rpart) 
# for classification trees,

train.class <- function(dtm, labels, complexity){
  
  dtm.frame <- data.frame(as.matrix(dtm), label=labels)
  dtm.tree <- rpart(label~., method="class", data = dtm.frame, control = rpart.control(xval=10, minsplit=4, cp = 0, maxdepth=10))
  
  # simple tree for plotting
  dtm.tree.pruned <-prune(dtm.tree,cp= 1.37e-02)
  plot(dtm.tree.pruned)
  
  # tree with lowest cv error
  dtm.tree.pruned <-prune(dtm.tree,cp=0.001)
  
  # make predictions on the dataset
  dtm.tree.pred <- predict(dtm.tree.pruned,newdata=dtm.frame,type= "class")
}

