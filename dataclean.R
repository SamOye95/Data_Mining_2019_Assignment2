#r packages
#install.packages(c("glmnet", "rpart", "randomForest","tm","readtext", "SnowballC"))
library(rpart)
library(randomForest)
library(tm)
library(glmnet)
library(readtext)
library(SnowballC)
library(RWeka)



# reading the data
setwd(getwd())


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
  setwd("..")
  setwd("..")
  setwd("..")
  setwd("..")
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
clean.data= function(data,tokenizer){
  CORP = VCorpus(VectorSource(data))
  
  #cleaned corpus
  cl.co = tm_map(CORP, content_transformer(tolower))
  cl.co = tm_map(cl.co, removeNumbers)
  cl.co = tm_map(cl.co, removePunctuation)
  cl.co = tm_map(cl.co, removeWords, c("the", "and", stopwords("english")))
  cl.co = tm_map(cl.co, stripWhitespace)
  cl.co = tm_map(cl.co, stemDocument, language = "english")
 # cl.co = DocumentTermMatrix(cl.co)
  cl.co = DocumentTermMatrix(cl.co, control = list(tokenize = tokenizer,weighting = weightTfIdf))
  
  return(cl.co)
}
BigramTokenizer <-
  function(x)
    unlist(lapply(ngrams(words(x), 2), paste, collapse = " "), use.names = FALSE)

UnigramTokenizer <-
  function(x)
    unlist(lapply(ngrams(words(x), 1), paste, collapse = " "), use.names = FALSE)

dtm_alldata = function(){
  data = read.all.data()
  Unigram <<- clean.data(data$text, UnigramTokenizer)
  Bigram  <<- clean.data(data$text, BigramTokenizer)
  
  Unigramdf =data.frame(as.matrix(Unigram))
  Bigramdf  = data.frame(as.matrix(Bigram))
  
  Unigramdftrain <<- Unigramdf[1:640,]
  Unigramdftest  <<- Unigramdf[641:800,]
  
  Bigramdftrain <<-  Bigramdf[1:640,]
  Bigramdftest  <<-  Bigramdf[641:800,]
  
  trainlabels <<- data$label[1:640]
  testlabels  <<- data$label[641:800]
}

dtm_alldata()


