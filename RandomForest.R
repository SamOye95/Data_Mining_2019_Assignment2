maketrees = function(){
  Bigramdata = Bigramdftrain[,gsub(" ", ".",  findFreqTerms(Bigram, 1.068), fixed = TRUE)]
  unigramdata= Unigramdftrain[,gsub(" ", ".",  findFreqTerms(Unigram, 3.2), fixed = TRUE)]
  
  Bi_forest100_5 <<- randomForest(x= data, y=trainlabels, ntree = 100, mtry = 5) 
  cat('1 done \n')
  Bi_forest200_5 <<- randomForest(x= data, y=trainlabels, ntree = 200, mtry = 5)
  cat("2 done\n")
  Bi_forest300_5 <<- randomForest(x= data, y=trainlabels, ntree = 300, mtry = 5)
  cat("3 done\n")
  
  
  uni_forest100_5 <<- randomForest(x= Unigramdftrain, y=trainlabels, ntree = 100, mtry = 5) 
  cat('4 done')
  uni_forest200_5 <<- randomForest(x= Unigramdftrain, y=trainlabels, ntree = 200, mtry = 5)
  cat("5 done")
  uni_forest300_5 <<- randomForest(x= Unigramdftrain, y=trainlabels, ntree = 300, mtry = 5)
   cat("\014")

  
  result_Bi_forest_100_5 =table(testlabels, predict(Bi_forest100_5, newdata = Unigramdftest))
  result_Bi_forest_200_5 =table(testlabels, predict(Bi_forest200_5, newdata = Unigramdftest))
  result_Bi_forest_300_5 =table(testlabels, predict(Bi_forest300_5, newdata = Unigramdftest))
  result_uni_forest_100_5 =table(testlabels, predict(uni_forest100_5, newdata = Unigramdftest))
  result_uni_forest_200_5 =table(testlabels, predict(uni_forest200_5, newdata = Unigramdftest))
  result_uni_forest_300_5 =table(testlabels, predict(uni_forest300_5, newdata = Unigramdftest))
  
  
}


maketrees()
