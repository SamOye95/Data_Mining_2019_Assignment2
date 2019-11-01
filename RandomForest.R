maketrees = function(){
    
    unidata = Unigramdftrain
    bidata = Bigramdftrain
    
    # bigram.deceptive.bestfeatures = row.names(data.frame( tail(sort(colSums(Bigramdftrain[1:320]) ),10000)))
    # bigram.truthful.bestfeatures = row.names(data.frame( tail(sort(colSums(Bigramdftrain[321:640]) ),10000)))
    # bidata = Bigramdftrain[ c(bigram.truthful.bestfeatures,bigram.deceptive.bestfeatures)]
  
  
  Bi_forest100_5 = randomForest(x= bidata, y=trainlabels, ntree = 100, mtry = 5) 
  cat('1 done \n')
  Bi_forest200_5 = randomForest(x= bidata, y=trainlabels, ntree = 200, mtry = 5)
  cat("2 done\n")
  Bi_forest300_5 = randomForest(x= bidata, y=trainlabels, ntree = 300, mtry = 5)
  cat("3 done\n")
  
  
  uni_forest100_5 = randomForest(x= unidata, y=trainlabels, ntree = 100, mtry = 5)
  cat('4 done')
  uni_forest200_5 = randomForest(x= unidata, y=trainlabels, ntree = 200, mtry = 5)
  cat("5 done")
  uni_forest300_5 = randomForest(x= unidata, y=trainlabels, ntree = 300, mtry = 5)
   cat("\014")

   result_uni_forest_100_5 <<-table(testlabels, predict(uni_forest100_5, newdata = Unigramdftest[colnames(unidata)]))
   result_uni_forest_200_5 <<-table(testlabels, predict(uni_forest200_5, newdata = Unigramdftest[colnames(unidata)]))
   result_uni_forest_300_5 <<-table(testlabels, predict(uni_forest300_5, newdata = Unigramdftest[colnames(unidata)]))
   
  result_Bi_forest_100_5 <<-table(testlabels, predict(Bi_forest100_5, newdata = Bigramdftest[colnames(bidata)]))
  result_Bi_forest_200_5 <<-table(testlabels, predict(Bi_forest200_5, newdata = Bigramdftest[colnames(bidata)]))
  result_Bi_forest_300_5 <<-table(testlabels, predict(Bi_forest300_5, newdata = Bigramdftest[colnames(bidata)]))
  
 
  
  
}


maketrees()
