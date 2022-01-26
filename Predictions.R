
library(quanteda)
library(quanteda.textmodels)
library(caret)
reviews<-read.csv("tripadvisor_hotel_reviews.csv")
reviews <- reviews %>% filter(detect_language(Review) == 'en')
reviews$Rating[reviews$Rating ==1] <- 0
reviews$Rating[reviews$Rating ==2] <- 0
reviews$Rating[reviews$Rating ==3] <- 1
reviews$Rating[reviews$Rating ==4] <- 1
reviews$Rating[reviews$Rating ==5] <- 1

corpus<-iconv(reviews$Review)
corpus<-Corpus(VectorSource(corpus))
corpus<-tm_map(corpus,tolower)
corpus<-tm_map(corpus,removePunctuation)
corpus<-tm_map(corpus,removeNumbers)
corpus<-tm_map(corpus,removeWords,stopwords("english"))
corpus<-tm_map(corpus,stripWhitespace)
corp_review<-corpus(corpus)

summary(reviews)
docvars(corp_review, "Rating") <- reviews$Rating


set.seed(300)
id_train <- sample(1:20000, 15000, replace = FALSE)
head(id_train, 10)
# create docvar with ID
corp_review$id_numeric <- 1:ndoc(corp_review)

# tokenize texts
toks_review <- tokens(corp_review, remove_punct = TRUE, remove_number = TRUE) %>% 
  tokens_remove(pattern = stopwords("en")) %>% 
  tokens_wordstem()
dfmt_review <- dfm(toks_review)

# get training set
dfmat_training <- dfm_subset(dfmt_review, id_numeric %in% id_train)

# get test set (documents not in id_train)
dfmat_test <- dfm_subset(dfmt_review, !id_numeric %in% id_train)


####Modo del profesor#####


library(quanteda.textmodels)
library(caret)

nbPredictions<-function(dist){
  multi<-textmodel_nb(dfmat_training,
                      dfmat_training$Rating,
                      distribution = dist
  )
  pred<-predict(multi,
                newdata=dfmat_test)
  confM <- confusionMatrix(pred, as.factor(docvars(dfmat_test)$Rating))
  #Accuracy is the number of labels (strings) that match...
  my_acc_coincidences <- sum(as.character(pred) == as.character(docvars(dfmat_test)$Rating))
  #...divided by the total number of labels
  my_acc_total <- length(as.character(pred))
  my_acc <- my_acc_coincidences/my_acc_total
  my_acc #Sale 0.82876. Con "multinomial" era 0.81304
  #Precision
  precision <- confM$byClass['Pos Pred Value']
  precision #Sale 0.7951591 (antes 0.878327)
  #Recall
  recall <- confM$byClass['Sensitivity']
  recall #Sale 0.88568 (antes 0.8953488)
  list(acc = my_acc, p = precision, r = recall, mat=confM)
}
nbPredictions("Bernoulli")



set.seed(123)

svmPredictions <- function(x,weight){ #weight can be "uniform", "docfreq" or "termfreq".
  dfmat_train<-dfm_sample(dfmat_training,x)
  
  multi <- textmodel_svm(dfmat_train,
                         as.factor(dfmat_train$Rating),
                         weight = weight)
  pred <- predict(multi,
                  newdata = dfmat_test)
  confM <- confusionMatrix(pred, as.factor(docvars(dfmat_test)$Rating))
  my_acc_coincidences <- sum(as.character(pred) == as.character(docvars(dfmat_test)$Rating))
  my_acc_total <- length(as.character(pred))
  my_acc <- my_acc_coincidences/my_acc_total
  precision <- confM$byClass['Pos Pred Value']
  recall <- confM$byClass['Sensitivity']
  list(acc = my_acc, p = precision, r = recall, mat=confM)
}
svmPredictions(1000, "uniform")

svmPredictions(1000,"docfreq")

#####################################################################################
#####################################################################################



####Otro modo####





tmod_nb <- textmodel_nb(dfmat_training, dfmat_training$Rating)
summary(tmod_nb)
dfmat_matched <- dfm_match(dfmat_test, features = featnames(dfmat_training))
actual_class <- dfmat_matched$Rating
predicted_class <- predict(tmod_nb, newdata = dfmat_matched)
tab_class <- table(actual_class, predicted_class)
tab_class

