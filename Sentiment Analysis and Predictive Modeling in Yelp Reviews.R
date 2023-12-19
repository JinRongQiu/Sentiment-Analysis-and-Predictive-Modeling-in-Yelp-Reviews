
rm(list = ls(all.names = TRUE)) 
library('tidyverse')
library(tidytext)
library(SnowballC)
library(textstem)
library(textdata)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(ranger)
library(rsample)
library(pROC)
library(e1071)
library(PerformanceAnalytics)

# Reading the reviews data
resReviewsData <- read_csv2('yelpRestaurantReviews_sample_s21b.csv')
glimpse(resReviewsData)
resReviewsData %>% group_by(starsReview) %>% count()
Star_count <- resReviewsData %>% group_by(starsReview) %>% count()
ggplot(resReviewsData, aes(x= funny, y=starsReview)) +geom_point()
ggplot(Star_count, aes(starsReview, n, fill = starsReview)) + geom_bar(stat = "identity") + ggtitle("Star rating distribution")
resReviewsData %>% group_by(state) %>% tally() %>% view()

#Converting star ratings into binomial classification with levels indicating 'positive' or 'negative'

#Looking at star rating distribution plot, we understand that there are high volume of rating for higher stars
#Hence, we considered the star ratings <=3 as negative and >3 as positive

Stars_binomial <- resReviewsData %>% select(review_id, starsReview)%>% mutate(Sentiment=ifelse(starsReview<=3, "Negative", "Positive"))

#Understanding the relation between star ratings and key words 'funny', 'cool', 'useful'

#plotting the star ratings w.r.t key word "funny"
ggplot(resReviewsData, aes(x= funny, y=starsReview)) +geom_point()

#plotting the star ratings w.r.t key word "Cool"
ggplot(resReviewsData, aes(x= cool, y=starsReview)) +geom_point()

#plotting the star ratings w.r.t key word "Useful"
ggplot(resReviewsData, aes(x= useful, y=starsReview)) +geom_point()

#Distribution of reviews across locations
resReviewsData %>%   group_by(state) %>% tally() %>% view()

#Distribution of reviews across postal-codes`
resReviewsData %>%   group_by(postal_code) %>% tally() %>% view()

#keeping only the those reviews from 5-digit postal-codes  
rrData <- resReviewsData %>% filter(str_detect(postal_code, "^[0-9]{1,5}"))

#tokenize the text of the reviews in the column named 'text' - keep only the reviewID, stars attribs
rrData <- resReviewsData %>% filter(str_detect(postal_code, "^[0-9]{1,5}"))
rrTokens <- rrData %>% select(review_id, starsReview, text ) %>% unnest_tokens(word, text)
dim(rrTokens)
head(rrTokens)
rrTokens %>% distinct(word) %>% dim()

#remove stopwords
rrTokens <- rrTokens %>% anti_join(stop_words)

#count the total occurrences of different words, & sort by most frequent
rrTokens %>% count(word, sort=TRUE) %>% top_n(10)

#remove the words which are not present in at least 10 reviews
rareWords <-rrTokens %>% count(word, sort=TRUE) %>% filter(n<10)
remove_rarew<-anti_join(rrTokens, rareWords)

#view words
remove_rarew %>% count(word, sort=TRUE) %>% view()

#Remove the terms containing digits?
remove_rarewd<- remove_rarew %>% filter(str_detect(word,"[0-9]")==FALSE)
rrTokens<- remove_rarewd

#How many distinct tokens remain ?
rrTokens %>% distinct(word) %>% dim()

#Analysis of words by star rating of reviews
#Check words by star rating of reviews
rrTokens %>% group_by(starsReview) %>% count(word, sort=TRUE)

#proportion of word occurrence by star ratings
ws <- rrTokens %>% group_by(starsReview) %>% count(word, sort=TRUE)
ws<- ws %>% group_by(starsReview) %>% mutate(prop=n/sum(n))

#check the proportion of 'love' among reviews with 1,2,..5 stars
ws %>% filter(word=='love')

#what are the most commonly used words by star rating
ws %>% group_by(starsReview) %>% arrange(starsReview, desc(prop)) %>% view()

#to see the top 20 words by star ratings
ws %>% group_by(starsReview) %>% arrange(starsReview, desc(prop)) %>% filter(row_number()<=20) %>% view()

#To plot this
ws %>% group_by(starsReview) %>% arrange(starsReview, desc(prop)) %>% filter(row_number()<=20) %>% ggplot(aes(word, prop))+geom_col()+coord_flip()+facet_wrap((~starsReview))

#plot without words like 'food', 'time',. which occurs across ratings
ws %>% filter(! word %in% c('food', 'time', 'restaurant', 'service'))%>% group_by(starsReview) %>% arrange(starsReview, desc(prop))%>% filter(row_number() <= 15)%>% ggplot(aes(word, prop))+geom_col()+coord_flip()+facet_wrap((~starsReview))

#Identifying the words which are related to higher/lower star ratings
xx<- ws %>% group_by(word) %>% summarise( totWS = sum(starsReview*prop))
xx %>% top_n(20)
xx %>% top_n(-20)


#Stemming and Lemmatization
rrTokens_stem <- rrTokens %>% mutate(word_stem = SnowballC::wordStem(word))
rrTokens_lemm <- rrTokens %>% mutate(word_lemma = textstem::lemmatize_words(word))

#Term frequency
#tokenize, remove stopwords, and lemmatize
rrTokens<-rrTokens %>% mutate(word = textstem::lemmatize_words(word))
#Or, can tokenize, remove stopwords, lemmatize i as
#rrTokens <- resReviewsData %>% select(review_id, stars, text )%>% unnest_tokens(word, text) %>% anti_join(stop_words)%>% mutate(word = textstem::lemmatize_words(word))

# filter out words with less than 3 characters more than 15 characters
rrTokens<-rrTokens %>% filter(str_length(word)<=3 | str_length(word)<=15)
rrTokens<- rrTokens %>% group_by(review_id, starsReview) %>% count(word)

#count total number of words by review, and add this in a column
totWords<-rrTokens %>% group_by(review_id)%>% count(word, sort=TRUE) %>% summarise(total=sum(n))

#add the column of counts
xx<-left_join(rrTokens, totWords)
# now n/total gives the tf values
xx<-xx %>% mutate(tf=n/total)
head(xx)

#We can use the bind_tfidf function to calculate the tf, idf and tfidf values
# (https://www.rdocumentation.org/packages/tidytext/versions/0.2.2/topics/bind_tf_idf)
rrTokens<-rrTokens %>% bind_tf_idf(word, review_id, n)
head(rrTokens)

#take a look at the words in the sentiment dictionaries – compare.
get_sentiments("bing") %>% view()
get_sentiments("nrc") %>% view()
get_sentiments("afinn")%>% view()

#---------------------------------------------with "bing" dictionary--------------------------------------
#get_sentiments("bing") , get_sentiments("nrc") %>% view(), get_sentiments("afinn")
#How many sentiment words? Positive/negative sentiment?
#ing dictionary
  #get sentiment of words in rrTokens – using join
rrSenti_bing<- rrTokens %>% left_join( get_sentiments("bing"), by="word")

#to retain only the words which match the sentiment dictionary, do an inner-join
rrSenti_bing<- rrTokens %>% inner_join( get_sentiments("bing"), by="word")

#count the occurrences of positive/negative sentiment words in the reviews
xx<-rrSenti_bing %>% group_by(word, sentiment) %>% summarise(totOcc=sum(n)) %>% arrange(sentiment, desc(totOcc))

#negate the counts for the negative sentiment words
xx<- xx %>% mutate (totOcc=ifelse(sentiment=="positive", totOcc, -totOcc))

# which are the most positive and most negative words in reviews
xx<-ungroup(xx)
xx %>% top_n(25)
xx %>% top_n(-25)

#You can plot these
rbind(top_n(xx, 25), top_n(xx, -25)) %>% ggplot(aes(word, totOcc, fill=sentiment)) +geom_col()+coord_flip()

#or, with a better reordering of words
rbind(top_n(xx, 25), top_n(xx, -25)) %>% mutate(word=reorder(word,totOcc)) %>% ggplot(aes(word, totOcc, fill=sentiment))+geom_col()+coord_flip()             

#Analysis by review sentiment (Bing). let's look into sentiment by review and see how that relates to review's star ratings
rrSenti_bing<- rrTokens %>% inner_join(get_sentiments("bing"), by="word") %>% view()

#summarise positive/negative sentiment words per review
revSenti_bing <- rrSenti_bing %>% group_by(review_id, starsReview) %>% summarise(nwords=n(),posSum=sum(sentiment=='positive'),negSum=sum(sentiment=='negative')) %>% view()

#calculate sentiment score based on proportion of positive, negative words
revSenti_bing<- revSenti_bing %>% mutate(posProp=posSum/nwords, negProp=negSum/nwords) 
revSenti_bing<- revSenti_bing%>% mutate(sentiScore=posProp-negProp) 

#Analysis by review sentiment
#Do review star ratings correspond to the positive/negative sentiment words
revSenti_bing %>% group_by(starsReview) %>%summarise(avgPos=mean(posProp), avgNeg=mean(negProp), avgSentiSc=mean(sentiScore))

#we can consider reviews with 1 to 2 stars as negative, and this with 4 to 5 stars as positive
revSenti_bing <- revSenti_bing %>% mutate(hiLo=ifelse(starsReview<=2,-1, ifelse(starsReview>=4, 1, 0 )))
revSenti_bing <- revSenti_bing %>% mutate(pred_hiLo=ifelse(sentiScore >0, 1, -1)) 
#filter out the reviews with 3 stars, and get the confusion matrix for hiLo vs pred_hiLo
xx<-revSenti_bing %>% filter(hiLo!=0)
table(actual=xx$hiLo, predicted=xx$pred_hiLo )
#prediction from the confusion matrix above (calculating the accuracy rate)
sum(diag(table(actual=xx$hiLo, predicted=xx$pred_hiLo )))/ sum(table(actual=xx$hiLo, predicted=xx$pred_hiLo ))


#---------------------------------------------with "nrc" dictionary--------------------------------------
rrSenti_nrc<-rrTokens %>% inner_join(get_sentiments("nrc"), by="word") %>%
  group_by (word, sentiment) %>% summarise(totOcc=sum(n)) %>%
  arrange(sentiment, desc(totOcc))

#How many words are there for the different sentiment categories
rrSenti_nrc %>% group_by(sentiment) %>% summarise(count=n(), sumn=sum(totOcc))             
             
#top few words for different sentiments
rrSenti_nrc %>% group_by(sentiment) %>% arrange(sentiment, desc(totOcc))%>% top_n(10) %>% view()             

#Suppose you want to consider {anger, disgust, fear sadness, negative} to denote 'bad' reviews,and {positive, joy, anticipation, trust} to denote 'good' reviews
xx<-rrSenti_nrc %>% mutate(goodBad=ifelse(sentiment %in% c('anger', 'disgust', 'fear', 'sadness', 'negative'), -totOcc,
                                          ifelse(sentiment %in% c('positive', 'joy', 'anticipation', 'trust'), totOcc, 0)))

xx<-ungroup(xx)
top_n(xx, -10)
top_n(xx, 10)

#
rbind(top_n(xx, 25), top_n(xx, -25)) %>% mutate(word=reorder(word,goodBad)) %>% ggplot(aes(word, goodBad, fill=goodBad)) +geom_col()+coord_flip()

rrSenti_nrc<- rrTokens %>% inner_join(get_sentiments("nrc"), by="word")%>% group_by(review_id, word)%>% slice_max(1)
revSenti_nrc <- rrSenti_nrc %>% mutate(goodBad=ifelse(sentiment %in% c('anger', 'disgust', 'fear', 'sadness', 'negative'), -1, ifelse(sentiment %in% c('positive', 'joy', 'anticipation', 'trust'), 1, 0))) %>%
  group_by(review_id, starsReview) %>% summarize(sentiSum = sum(goodBad))

revSenti_nrc <- revSenti_nrc %>% mutate(hiLo=ifelse(starsReview<=2,-1, ifelse(starsReview>=4, 1, 0 )))
revSenti_nrc <- revSenti_nrc %>% mutate(pred_hiLo=ifelse(sentiSum >0, 1, -1))
xx<-revSenti_nrc %>% filter(hiLo!=0)
table(actual=xx$hiLo, predicted=xx$pred_hiLo )
#prediction from the confusion matrix above (calcualting accuracy rate)
sum(diag(table(actual=xx$hiLo, predicted=xx$pred_hiLo )))/ sum(table(actual=xx$hiLo, predicted=xx$pred_hiLo ))

#-------------------------------------------------Afinn--------------------------------------------------
#Using AFINN dictionary words
#- AFINN assigns negative to positive sentiment value for words matching the dictionary
#- take the sum of sentiment value for words in a review?

rrSenti_afinn<- rrTokens %>% inner_join(get_sentiments("afinn"), by="word")
revSenti_afinn <- rrSenti_afinn %>% group_by(review_id, starsReview)%>% summarise(nwords=n(), sentiSum =sum(value))
revSenti_afinn %>% group_by(starsReview)%>% summarise(avgLen=mean(nwords), avgSenti=mean(sentiSum))

#considering reviews with 1 to 2 stars as negative, and this with 4 to 5 stars as positive
revSenti_afinn <- revSenti_afinn %>% mutate(hiLo = ifelse(starsReview <= 2, -1, ifelse(starsReview >=4, 1, 0 )))
revSenti_afinn <- revSenti_afinn %>% mutate(pred_hiLo=ifelse(sentiSum > 0, 1, -1))
#filter out the reviews with 3 stars, and get the confusion matrix for hiLo vs pred_hiLo
xx<-revSenti_afinn %>% filter(hiLo!=0)
table(actual=xx$hiLo, predicted=xx$pred_hiLo )

#prediction from the confusion matrix above (calculating the accuracy rate)
sum(diag(table(actual=xx$hiLo, predicted=xx$pred_hiLo )))/ sum(table(actual=xx$hiLo, predicted=xx$pred_hiLo ))


#-----------------------------------------Individual Part-------------------------------------------------

#afinn
#afinn
revDTM_sentiafinn <- rrSenti_afinn %>%  pivot_wider(id_cols = c(review_id,starsReview), names_from = word, values_from = tf_idf)  %>% ungroup()

#filter out the reviews with stars=3, and calculate hiLo sentiment 'class'
revDTM_sentiafinn <- revDTM_sentiafinn %>% filter(starsReview!=3) %>% mutate(hiLo=ifelse(starsReview<=2, -1, 1)) %>% select(-starsReview)

#how many review with 1, -1  'class'
revDTM_sentiafinn %>% group_by(hiLo) %>% tally()

#develop a random forest model to predict hiLo from the words in the reviews

#replace all the NAs with 0
revDTM_sentiafinn<-revDTM_sentiafinn %>% replace(., is.na(.), 0)

revDTM_sentiafinn$hiLo<- as.factor(revDTM_sentiafinn$hiLo)


revDTM_sentiafinn_split<- initial_split(revDTM_sentiafinn, 0.5)
revDTM_sentiafinn_trn<- training(revDTM_sentiafinn_split)
revDTM_sentiafinn_tst<- testing(revDTM_sentiafinn_split)

rfModel1A<-ranger(dependent.variable.name = "hiLo", data=revDTM_sentiafinn_trn %>% select(-review_id), num.trees = 100, importance='permutation', probability = TRUE)

rfModel1A

#which variables are important
importance(rfModel1A) %>% view()


#Obtain predictions, and calculate performance
revSentiafinn_predTrn<- predict(rfModel1A, revDTM_sentiafinn_trn %>% select(-review_id))$predictions

revSentiafinn_predTst<- predict(rfModel1A, revDTM_sentiafinn_tst %>% select(-review_id))$predictions

table(actual=revDTM_sentiafinn_trn$hiLo, preds=revSentiafinn_predTrn[,2]>0.5)
table(actual=revDTM_sentiafinn_tst$hiLo, preds=revSentiafinn_predTst[,2]>0.5)

rocTrnA <- roc(revDTM_sentiafinn_trn$hiLo, revSentiafinn_predTrn[,2], levels=c(-1, 1))
rocTstA <- roc(revDTM_sentiafinn_tst$hiLo, revSentiafinn_predTst[,2], levels=c(-1, 1))

plot.roc(rocTrnA, col='blue', legacy.axes = TRUE)
plot.roc(rocTstA, col='red', add=TRUE)
title(main = "afinn", 2.5)
legend("bottomright", legend=c("Training", "Test"),
       col=c("blue", "red"), lwd=2, cex=0.8, bty='n')


















#of matching terms for each dictionary 

#2902 matching terms; overall how many words we have per sentiment for nrc dictionary
rrTokens %>% inner_join(get_sentiments("nrc"), by="word") %>% group_by (word, sentiment) %>% summarise(totOcc=sum(n)) %>% arrange(sentiment, desc(totOcc))

#bing #960 matching terms
rrTokens %>% inner_join(get_sentiments("bing"), by="word") %>% group_by (word, sentiment) %>% summarise(totOcc=sum(n)) %>% arrange(sentiment, desc(totOcc))

#afinn #527
rrTokens %>% inner_join(get_sentiments("afinn"), by="word") %>% group_by (word) %>% summarise(totOcc=sum(n)) %>% arrange(word, desc(totOcc))


#SVM Model
# #Bing
# #develop a SVM model on the sentiment dictionary terms
svmM1B <- svm(as.factor(hiLo) ~., data = revDTM_sentiBing_trn %>%select(-review_id), kernel="radial", cost=1, scale=FALSE)
#
 revDTM_predTrn_svm1B<-predict(svmM1B, revDTM_sentiBing_trn)
 revDTM_predTst_svm1B<-predict(svmM1B, revDTM_sentiBing_tst)
 table(actual= revDTM_sentiBing_trn$hiLo, predicted= revDTM_predTrn_svm1B)
#
# # try different parameters -- rbf kernel gamma, and cost
 system.time( svmM2B <- svm(as.factor(hiLo) ~., data = revDTM_sentiBing_trn %>% select(-review_id), kernel="radial", cost=5, gamma=5, scale=FALSE) )
 revDTM_predTrn_svm2B<-predict(svmM2B, revDTM_sentiBing_trn)
 table(actual= revDTM_sentiBing_trn$hiLo, predicted= revDTM_predTrn_svm2B)
 revDTM_predTst_svm2B<-predict(svmM2B, revDTM_sentiBing_tst)
 table(actual= revDTM_sentiBing_tst$hiLo, predicted= revDTM_predTst_svm2B)

#
# #afinn
# #develop a SVM model on the sentiment dictionary terms
 svmM1A <- svm(as.factor(hiLo) ~., data = revDTM_sentiafinn_trn %>%select(-review_id), kernel="radial", cost=1, scale=FALSE)
#
 revDTM_predTrn_svm1A<-predict(svmM1A, revDTM_sentiafinn_trn)
 revDTM_predTst_svm1A<-predict(svmM1A, revDTM_sentiafinn_tst)
 table(actual= revDTM_sentiafinn_trn$hiLo, predicted= revDTM_predTrn_svm1A)
#
# # try different parameters -- rbf kernel gamma, and cost
# system.time( svmM2A <- svm(as.factor(hiLo) ~., data = revDTM_sentiafinn_trn %>% select(-review_id), kernel="radial", cost=5, gamma=5, scale=FALSE) )
# revDTM_predTrn_svm2A<-predict(svmM2A, revDTM_sentiafinn_trn)
# table(actual= revDTM_sentiafinn_trn$hiLo, predicted= revDTM_predTrn_svm2A)
# revDTM_predTst_svm2A<-predict(svmM2A, revDTM_sentiafinn_tst)
# table(actual= revDTM_sentiafinn_tst$hiLo, predicted= revDTM_predTst_svm2A)
#
# system.time(svmA_tune <- tune(svm, as.factor(hiLo) ~., data = revDTM_sentiafinn_trn %>% select(-review_id),
# kernel="radial", ranges = list( cost=c(0.1,1,10,50), gamma = c(0.5,1,2,5, 10))))
#
# #Check performance for different tuned parameters
# svmA_tune$performances
# #Best model
# svmA_tune$best.parameters
# svmA_tune$best.model
#
# #predictions from best model
# revDTM_predTrn_svmA_Best<-predict(svmA_tune$best.model, revDTM_sentiafinn_trn)
# table(actual= revDTM_sentiafinn_trn$hiLo, predicted= revDTM_predTrn_svmA_Best)
# revDTM_predTst_svmA_best<-predict(svmA_tune$best.model, revDTM_sentiafinn_tst)
# table(actual= revDTM_sentiafinn_tst$hiLo, predicted= revDTM_predTst_svmA_best)
#
# #nrc
# ##develop a SVM model on the sentiment dictionary terms
 svmM1N <- svm(as.factor(hiLo) ~., data = revDTM_sentinrc_trn %>%select(-review_id), kernel="radial", cost=1, scale=FALSE)
#
# revDTM_predTrn_svm1N<-predict(svmM1N, revDTM_sentinrc_trn)
# revDTM_predTst_svm1N<-predict(svmM1N, revDTM_sentinrc_tst)
# table(actual= revDTM_sentinrc_trn$hiLo, predicted= revDTM_predTrn_svm1N)
#
# ## try different parameters -- rbf kernel gamma, and cost
# system.time( svmM2N <- svm(as.factor(hiLo) ~., data = revDTM_sentinrc_trn %>% select(-review_id), kernel="radial", cost=5, gamma=5, scale=FALSE) )
# revDTM_predTrn_svm2N<-predict(svmM2N, revDTM_sentinrc_trn)
# table(actual= revDTM_sentinrc_trn$hiLo, predicted= revDTM_predTrn_svm2N)
# revDTM_predTst_svm2N<-predict(svmM2N, revDTM_sentinrc_tst)
# table(actual= revDTM_sentinrc_tst$hiLo, predicted= revDTM_predTst_svm2N)

# system.time(svmN_tune <- tune(svm, as.factor(hiLo) ~., data = revDTM_sentinrc_trn %>% select(-review_id),
# kernel="radial", ranges = list( cost=c(0.1,1,10,50), gamma = c(0.5,1,2,5, 10))))
#
# ##Check performance for different tuned parameters
# svmN_tune$performances
# ##Best model
# svmN_tune$best.parameters
# svmN_tune$best.model
#
# ##predictions from best model
# revDTM_predTrn_svmN_Best<-predict(svmN_tune$best.model, revDTM_sentinrc_trn)
# table(actual= revDTM_sentinrc_trn$hiLo, predicted= revDTM_predTrn_svmN_Best)
# revDTM_predTst_svmN_best<-predict(svmN_tune$best.model, revDTM_sentinrc_tst)
# table(actual= revDTM_sentinrc_tst$hiLo, predicted= revDTM_predTst_svmN_best)

```


















##########Not used
#considering reviews with 1 stars as negative, and this with 5 stars as positive
revSenti_afinn <- revSenti_afinn %>% mutate(hiLo=ifelse(starsReview<2,-1, ifelse(starsReview>4, 1, 0 )))
revSenti_afinn <- revSenti_afinn %>% mutate(pred_hiLo=ifelse(sentiSum >0, 1, -1))
xx<-revSenti_afinn %>% filter(hiLo!=0)
table(actual=xx$hiLo, predicted=xx$pred_hiLo )

#Learn a model to predict hiLo ratings, from words in reviews
#- considering only those words which match a sentiment dictionary (for eg. bing)
revDTM_sentiBing <- rrSenti_bing %>% pivot_wider(id_cols = c(review_id, starsReview), names_from = word, values_from = tf_idf) %>% ungroup()

#filter out the reviews with stars=3, and calculate hiLo sentiment 'class'
revDTM_sentiBing <- revDTM_sentiBing %>% filter(starsReview!=3) %>% mutate(hiLo=ifelse(starsReview<=2, -1, 1)) %>% select(-starsReview)

#how many review with 1, -1 'class'
revDTM_sentiBing %>% group_by(hiLo) %>% tally()

#develop a random forest model to predict hiLo from the words in the reviews
library(ranger)
#replace all the NAs with 0
revDTM_sentiBing <- revDTM_sentiBing %>% replace(., is.na(.), 0)
revDTM_sentiBing$hiLo <- as.factor(revDTM_sentiBing$hiLo)

library(rsample)
revDTM_sentiBing_split<- initial_split(revDTM_sentiBing, 0.5)
revDTM_sentiBing_trn<- training(revDTM_sentiBing_split)
revDTM_sentiBing_tst<- testing(revDTM_sentiBing_split)

#revDTM_sentiBing_trn2<- revDTM_sentiBing_trn2[order(revDTM_sentiBing_trn2$hiLo),]

rfModel1<-ranger(dependent.variable.name = "hiLo",
                 data=revDTM_sentiBing_trn %>% select(-review_id), num.trees = 100,
                 importance='permutation', probability = TRUE)

rfModel1

#which variables are important
importance(rfModel1) %>% view()

#Obtain predictions, and calculate performance
revSentiBing_predTrn<- predict(rfModel1, revDTM_sentiBing_trn %>% select(-review_id))$predictions

revSentiBing_predTst<- predict(rfModel1, revDTM_sentiBing_tst %>% select(-review_id))$predictions

table(actual=revDTM_sentiBing_trn$hiLo, preds=revSentiBing_predTrn[,2]>0.5)
table(actual=revDTM_sentiBing_tst$hiLo, preds=revSentiBing_predTst[,2]>0.5)



rocTrnB <- roc(revDTM_sentiBing_trn$hiLo, revSentiBing_predTrn[,2], levels=c(-1, 1))
rocTstB <- roc(revDTM_sentiBing_tst$hiLo, revSentiBing_predTst[,2], levels=c(-1, 1))

plot.roc(rocTrnB, col='blue', legacy.axes = TRUE)
plot.roc(rocTstB, col='red', add=TRUE)
title(main = "bing", 2.5)
legend("bottomright", legend=c("Training", "Test"),
       col=c("blue", "red"), lwd=2, cex=0.8, bty='n')

























#not used
#Obtain predictions, and calculate performance
revSentiBing_predTrn<- predict(rfModel1, revDTM_sentiBing_trn2 %>% select(-review_id))$predictions
revSentiBing_predTrn1<-revSentiBing_predTrn[,2]
revSentiBing_predTrn2 <- ifelse(revSentiBing_predTrn1>0.3,1,-1)
mean(revSentiBing_predTrn2 == revDTM_sentiBing_trn2$hiLo)
#accuracy is 0.92499

revSentiBing_predTst<- predict(rfModel1, revDTM_sentiBing_tst %>% select(-review_id))$predictions
revSentiBing_predTst1<-revSentiBing_predTst[,2]
revSentiBing_predTst2 <- ifelse(revSentiBing_predTst1>0.3,1,-1)
mean(revSentiBing_predTst2 == revDTM_sentiBing_tst$hiLo)
#accuracy is 0.9104709 

library(pROC)
#auc train
auc(as.numeric(revDTM_sentiBing_trn2$hiLo), revSentiBing_predTrn[,2])
#0.9919

#auc test
auc(as.numeric(revDTM_sentiBing_tst$hiLo), revSentiBing_predTst[,2])
#0.977


#Confusion matrix
table(actual=revDTM_sentiBing_trn2$hiLo, preds=revSentiBing_predTrn[,2]>0.3)
table(actual=revDTM_sentiBing_tst$hiLo, preds=revSentiBing_predTst[,2]>0.3)

library(pROC)
rocTrn <- roc(revDTM_sentiBing_trn$hiLo, revSentiBing_predTrn2[,2], levels=c(-1, 1))
rocTst <- roc(revDTM_sentiBing_tst$hiLo, revSentiBing_predTst[,2], levels=c(-1, 1))
plot.roc(rocTrn, col='blue')
plot.roc(rocTst, col='red', add=TRUE)
legend("bottomright", legend=c("Training", "Test"),col=c("blue", "red"), lwd=2, cex=0.8, bty='n')





#Developing SVM model on bing dictionary
library(e1071)
library(ROCR)

svmM1 <- svm(as.factor(hiLo) ~., data = revDTM_sentiBing_trn2 %>%select(-review_id),
             kernel="radial", cost=10,gamma=0.5, scale=FALSE)

revDTM_predTrn_svm1<-predict(svmM1, revDTM_sentiBing_trn2)
mean(revDTM_predTrn_svm1 == revDTM_sentiBing_trn2$hiLo)
#accuracy of training data is 0.9240124
table(actual= revDTM_sentiBing_trn2$hiLo, predicted= revDTM_predTrn_svm1)

revDTM_predTst_svm1<-predict(svmM1, revDTM_sentiBing_tst)
mean(revDTM_predTst_svm1 == revDTM_sentiBing_tst$hiLo)
#accuracy of testing data is 0.9153521
table(actual= revDTM_sentiBing_tst$hiLo, predicted= revDTM_predTst_svm1)
#auc train
auc(as.numeric(revDTM_sentiBing_trn2$hiLo), as.numeric(revDTM_predTrn_svm1))
#0.8794
#auc test
auc(as.numeric(revDTM_sentiBing_tst$hiLo), as.numeric(revDTM_predTst_svm1))
#0.8676
#building ROC curve
pred_rf_svm=prediction(as.numeric(revDTM_predTst_svm1),as.numeric(revDTM_sentiBing_tst$hiLo))
aucPerf_svm <-performance(pred_rf_svm, "tpr", "fpr")
plot(aucPerf_svm) + abline(a=0, b= 1)


#Building models on NRC dictionary
revDTM_sentinrc <- rrSenti_nrc %>%  pivot_wider(id_cols = c(review_id,starsReview), names_from = word, values_from = tf_idf)  %>% ungroup()

##filter out the reviews with stars=3, and calculate hiLo sentiment 'class'
revDTM_sentinrc <- revDTM_sentinrc %>% filter(starsReview!=3) %>% mutate(hiLo=ifelse(starsReview<=2, -1, 1)) %>% select(-starsReview)

##how many review with 1, -1  'class'
revDTM_sentinrc %>% group_by(hiLo) %>% tally()

##develop a random forest model to predict hiLo from the words in the reviews
library(ranger)
##replace all the NAs with 0
revDTM_sentinrc<-revDTM_sentinrc %>% replace(., is.na(.), 0)
revDTM_sentinrc$hiLo<- as.factor(revDTM_sentinrc$hiLo)


revDTM_sentinrc_split<- initial_split(revDTM_sentinrc, 0.5)
revDTM_sentinrc_trn<- training(revDTM_sentinrc_split)
revDTM_sentinrc_tst<- testing(revDTM_sentinrc_split)

rfModel1N<-ranger(dependent.variable.name = "hiLo", data=revDTM_sentinrc_trn %>% select(-review_id), num.trees = 500, importance='permutation', probability = TRUE)

rfModel1N
importance(rfModel1N) %>% view()

##Obtain predictions, and calculate performance
revSentinrc_predTrn<- predict(rfModel1N, revDTM_sentinrc_trn %>% select(-review_id))$predictions
revSentinrc_predTst<- predict(rfModel1N, revDTM_sentinrc_tst %>% select(-review_id))$predictions

table(actual=revDTM_sentinrc_trn$hiLo, preds=revSentinrc_predTrn[,2]>0.7)
table(actual=revDTM_sentinrc_tst$hiLo, preds=revSentinrc_predTst[,2]>0.7)

library(pROC)
rocTrnN <- roc(revDTM_sentinrc_trn$hiLo, revSentinrc_predTrn[,2], levels=c(-1, 1))
rocTstN <- roc(revDTM_sentinrc_tst$hiLo, revSentinrc_predTst[,2], levels=c(-1, 1))
plot.roc(rocTrnN, col='blue',add=TRUE)
plot.roc(rocTstN, col='red', add=TRUE) 
title(main = "nrc", 2.5)
legend("bottomright", legend=c("Training", "Test"),col=c("blue", "red"), lwd=2, cex=0.8, bty='n')



###stop here
#Building SVM on NRC dictionary
svmM1_nrc <- svm(as.factor(hiLo) ~., data = revDTM_senti_nrc_trn2 %>%select(-review_id),kernel="radial", cost=10,gamma=0.5, scale=FALSE)

#prediction
revDTM_predTrn_svm1_nrc<-predict(svmM1_nrc, revDTM_senti_nrc_trn2)
mean(revDTM_predTrn_svm1_nrc == revDTM_senti_nrc_trn2$hiLo)
#accuracy on training data is 0.9238
table(actual= revDTM_senti_nrc_trn2$hiLo, predicted= revDTM_predTrn_svm1_nrc)

revDTM_predTst_svm1_nrc<-predict(svmM1_nrc, revDTM_senti_nrc_tst)
mean(revDTM_predTst_svm1_nrc == revDTM_senti_nrc_tst$hiLo)
#accuracy on testing data is 0.8735
table(actual= revDTM_senti_nrc_tst$hiLo, predicted= revDTM_predTst_svm1_nrc)

#auc train
auc(as.numeric(revDTM_senti_nrc_trn2$hiLo), as.numeric(revDTM_predTrn_svm1_nrc))
#Auc of Traning data is 0.8648
#auc test
auc(as.numeric(revDTM_senti_nrc_tst$hiLo), as.numeric(revDTM_predTst_svm1_nrc))
#Auc of testing data is 0.7876

#building ROC curve
pred_nrc_svm1=prediction(as.numeric(revDTM_predTst_svm1_nrc),as.numeric(revDTM_senti_nrc_tst$hiLo))
aucPerf_svm1_nrc <-performance(pred_nrc_svm1, "tpr", "fpr")
plot(aucPerf_svm1_nrc) + abline(a=0, b= 1)

#Building SVM with different subset of training data
svmM2_nrc <- svm(as.factor(hiLo) ~., data = revDTM_senti_nrc_trn2a %>%select(-review_id),kernel="radial", cost=10,gamma=0.5, scale=FALSE)

revDTM_predTrn_svm2_nrc<-predict(svmM2_nrc, revDTM_senti_nrc_trn2a)
mean(revDTM_predTrn_svm2_nrc == revDTM_senti_nrc_trn2a$hiLo)
#accuracy on training data is 0.9266
table(actual= revDTM_senti_nrc_trn2a$hiLo, predicted= revDTM_predTrn_svm2_nrc)


revDTM_predTst_svm2_nrc<-predict(svmM2_nrc, revDTM_senti_nrc_tst)
mean(revDTM_predTst_svm2_nrc == revDTM_senti_nrc_tst$hiLo)
#accuracy on testing data is 0.8724
table(actual= revDTM_senti_nrc_tst$hiLo, predicted= revDTM_predTst_svm2_nrc)

#auc train
auc(as.numeric(revDTM_senti_nrc_trn2a$hiLo), as.numeric(revDTM_predTrn_svm2_nrc))
#Auc of training data is 0.8725
#auc test
auc(as.numeric(revDTM_senti_nrc_tst$hiLo), as.numeric(revDTM_predTst_svm2_nrc))
#Auc of testing data is 0.7897

#building ROC curve
pred_nrc_svm2=prediction(as.numeric(revDTM_predTst_svm2_nrc),as.numeric(revDTM_senti_nrc_tst$hiLo))
aucPerf_svm2_nrc <-performance(pred_nrc_svm2, "tpr", "fpr")
plot(aucPerf_svm2_nrc) + abline(a=0, b= 1)


######################################################################################################################
##############Building SVM with different subset of training data
svmM2 <- svm(as.factor(hiLo) ~., data = revDTM_sentiBing_trn2a %>%select(-review_id),
             kernel="radial", cost=10,gamma=0.5, scale=FALSE)

revDTM_predTrn_svm2<-predict(svmM2, revDTM_sentiBing_trn2a)
mean(revDTM_predTrn_svm2 == revDTM_sentiBing_trn2a$hiLo)
#accuracy train 0.9195
table(actual= revDTM_sentiBing_trn2a$hiLo, predicted= revDTM_predTrn_svm2)

revDTM_predTst_svm2<-predict(svmM2, revDTM_sentiBing_tst)
mean(revDTM_predTst_svm2 == revDTM_sentiBing_tst$hiLo)
#accuracy test 0.8902
table(actual= revDTM_sentiBing_tst$hiLo, predicted= revDTM_predTst_svm2)

auc(as.numeric(revDTM_sentiBing_trn2a$hiLo), as.numeric(revDTM_predTrn_svm2))
#0.8565
auc(as.numeric(revDTM_sentiBing_tst$hiLo), as.numeric(revDTM_predTst_svm2))
#0.8161

#building ROC curve
pred_rf_svm2=prediction(as.numeric(revDTM_predTst_svm2),as.numeric(revDTM_sentiBing_tst$hiLo))
aucPerf_svm2 <-performance(pred_rf_svm2, "tpr", "fpr")
plot(aucPerf_svm2) + abline(a=0, b= 1)



