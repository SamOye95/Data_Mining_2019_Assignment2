#r packages
#install.packages(c("glmnet", "rpart", "randomForest","tm","readtext"))
library(rpart)
library(randomForest)
library(tm)
library(glmnet)
library(readtext)

# reading the data
setwd("D:/Documenten/Desktop/Data_Mining_2019_Assignment2")


# RDF =Read Files in Directory returns a datadrame of inputfiles given a directory
 
rdf = function(dir, label){
  setwd(dir)
  file_list = list.files()
  dataset <- data.frame()
  
  #had to specify columns to get rid of the total column
  for (i in 1:length(file_list)){
     content = readtext(file_list[i])
     dataset <- rbind(dataset, cbind(content,label))
  }
  setwd("D:/Documenten/Desktop/Data_Mining_2019_Assignment2")
  return(dataset)
}

read.all.data = function(){
  dat =data.frame()
  
  dat = rbind(dat, rdf("op_spam_v1.4/negative_polarity/deceptive_from_MTurk/fold1", "false negative"))
  dat = rbind(dat, rdf("op_spam_v1.4/negative_polarity/deceptive_from_MTurk/fold2", "false negative"))
  dat = rbind(dat, rdf("op_spam_v1.4/negative_polarity/deceptive_from_MTurk/fold3", "false negative"))
  dat = rbind(dat, rdf("op_spam_v1.4/negative_polarity/deceptive_from_MTurk/fold4", "false negative"))
  
  dat = rbind(dat, rdf("op_spam_v1.4/negative_polarity/truthful_from_Web/fold1", "true negative"))
  dat = rbind(dat, rdf("op_spam_v1.4/negative_polarity/truthful_from_Web/fold2", "true negative"))
  dat = rbind(dat, rdf("op_spam_v1.4/negative_polarity/truthful_from_Web/fold3", "true negative"))
  dat = rbind(dat, rdf("op_spam_v1.4/negative_polarity/truthful_from_Web/fold4", "true negative"))
  
  dat = rbind(dat, rdf("op_spam_v1.4/negative_polarity/deceptive_from_MTurk/fold5", "false negative"))
  dat = rbind(dat, rdf("op_spam_v1.4/negative_polarity/truthful_from_Web/fold5", "true negative"))
  
  
  #dat = rbind(dat, rdf("op_spam_v1.4/positive_polarity/deceptive_from_MTurk/fold1", "false positive"))
  #dat = rbind(dat, rdf("op_spam_v1.4/positive_polarity/deceptive_from_MTurk/fold2", "false positive"))
  #dat = rbind(dat, rdf("op_spam_v1.4/positive_polarity/deceptive_from_MTurk/fold3", "false positive"))
  #dat = rbind(dat, rdf("op_spam_v1.4/positive_polarity/deceptive_from_MTurk/fold4", "false positive"))
  
  #dat = rbind(dat, rdf("op_spam_v1.4/positive_polarity/truthful_from_TripAdvisor/fold1", "true positive"))
  #dat = rbind(dat, rdf("op_spam_v1.4/positive_polarity/truthful_from_TripAdvisor/fold2", "true positive"))
  #dat = rbind(dat, rdf("op_spam_v1.4/positive_polarity/truthful_from_TripAdvisor/fold3", "true positive"))
  #dat = rbind(dat, rdf("op_spam_v1.4/positive_polarity/truthful_from_TripAdvisor/fold4", "true positive"))
  return(dat)
}
#returns a cleaned document term matrix.
clean.data= function(data){
  CORP = Corpus(VectorSource(data))
  
  #cleaned corpus
  cl.co = tm_map(CORP, content_transformer(tolower))
  cl.co = tm_map(cl.co, removeNumbers)
  cl.co = tm_map(cl.co, removePunctuation)
  cl.co = tm_map(cl.co, removeWords, c("the", "and", stopwords("english")))
  cl.co = tm_map(cl.co, stripWhitespace)
  cl.co = as.matrix(DocumentTermMatrix(cl.co))
  
  return(cl.co)
}



