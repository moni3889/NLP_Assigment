library(sentimentr)
library(syuzhet)
library(dplyr)
library(cld3) # For detecting languages
library(tm) # For text mining
library(wordcloud) # For word cloud

reviews<-read.csv("tripadvisor_hotel_reviews.csv")
reviews <- reviews %>% filter(detect_language(Review) == 'en')
reviewssample<- sample(1:nrow(reviews),size=8000,replace=FALSE)
reviews<- reviews[reviewssample, ]
clean<-iconv(reviews$Review)
clean<-Corpus(VectorSource(clean))
clean<-tm_map(clean,tolower)
clean<-tm_map(clean,removePunctuation)
clean<-tm_map(clean,removeNumbers)
clean<-tm_map(clean,removeWords,stopwords("english"))
clean<-tm_map(clean,stripWhitespace)
clean<-tm_map(clean,removeWords, c("goes","got","hotel","hotels","amsterdam","looked","looks","dont"))
reviews_final<-clean
tdm<-TermDocumentMatrix(reviews_final)
tdm<-as.matrix(tdm)
tdm[1:10,1:8]
#view(tdm)
#Bar plot of words
w<-rowSums(tdm)
w<-subset(w,w>=2500)
barplot(w,las=2,col="blue")

#WordCloud

v <- sort(rowSums(tdm),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))
          
