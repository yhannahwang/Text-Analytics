install.packages("tidytext")
install.packages("dplyr")
install.packages("janeaustenr")
install.packages("stringr")
install.packages("tm")
install.packages("wordcloud")
install.packages('stringi')
install.packages("readtext")
install.packages("quanteda")
install.packages("tm")
install.packages("SnowballC")
library(tm)
library(readtext)
library(quanteda)
library(stringi)
library(SnowballC)
library(tidytext)
library(janeaustenr)
library(dplyr)
library(stringr)
library(wordcloud)

getwd()
setwd("~/Desktop/课程/3/ai/hotel data_review")
MBS <- read.csv("reviews for Marina Bay Sands.csv", header=TRUE,stringsAsFactors = FALSE,fileEncoding="latin1")

MBS_detail<- as.vector(MBS$content_detail)
MBS_toks<-tokens(MBS_detail)
MBS_toks1<-tokens_tolower(MBS_toks)
MBS_toks2<-tokens_wordstem(MBS_toks1)
sw<-stopwords("english")
MBS_toks3<-tokens_remove(MBS_toks2,sw)


#location
y1<-aggregate(MBS$score,list(nationality=MBS$nationality), FUN=mean)

#submitted ways
MBS_sub <- MBS[grepl("Submitted via mobile",MBS$tags),]
length(MBS_sub$X)
length(MBS$X)
mobile_rate =length(MBS_sub$X)/length(MBS$X)
mobile_rate

#content

MBS_con1<-MBS[,c(2,6)]
head(MBS_con1)
class(MBS_con1)

MBS_con1_word<-MBS_con1 %>%
  unnest_tokens(word, content)
head(MBS_con1_word)
data(stop_words)
MBS_con1_word <- MBS_con1_word %>%
  anti_join(stop_words)

word_counts1 <- MBS_con1_word %>%
  anti_join(stop_words) %>%
  count(word, sort = TRUE) %>%
  ungroup()
word_counts1[c(11:100),]
wordcloud(word_counts1$word,word_counts1$n,random.order=FALSE,colors=brewer.pal(8,"Dark2"))

###detail###
MBS_con2<-MBS[,c(2,7)]
head(MBS_con2)
class(MBS_con2)

MBS_con2_word<-MBS_con2 %>%
  unnest_tokens(word, content_detail)
head(MBS_con2_word)


MBS_con2_word <- MBS_con2_word %>%
  anti_join(stop_words)

word_counts2 <- MBS_con2_word %>%
  anti_join(stop_words) %>%
  count(word, sort = TRUE) %>%
  ungroup()
top100_2 <-word_counts2[c(1:100),]
wordcloud(top100_2$word,top100_2$n,random.order=FALSE,colors=brewer.pal(8,"Dark2"))
#score
mean(MBS$score)
#######score
low_MBS <- subset(MBS,MBS$score < 7 ,select=c("X","content_detail"))
low_MBS <- as.data.frame(low_MBS)
head(low_MBS)

MBS_con3_word<-low_MBS %>%
  unnest_tokens(word, content_detail)
head(MBS_con3_word)


MBS_con3_word <- MBS_con3_word %>%
  anti_join(stop_words)

word_counts3 <- MBS_con3_word %>%
  anti_join(stop_words) %>%
  count(word, sort = TRUE) %>%
  ungroup()
top100_3 <-word_counts3[c(1:100),]
wordcloud(top100_3$word,top100_3$n,random.order=FALSE,colors=brewer.pal(8,"Dark2"))






####################################
##CPCA
####################################
CPCA <- read.csv("reviews for Crowne Plaza Changi Airport.csv",header=TRUE,stringsAsFactors = FALSE,fileEncoding="latin1")
#location
aggregate(CPCA$score,list(nationality=CPCA$nationality), FUN=mean)
#submitted ways
CPCA_sub <- CPCA[grepl("Submitted via mobile",CPCA$tags),]
length(CPCA_sub$X)
length(CPCA$X)
mobile_rate =length(CPCA_sub$X)/length(CPCA$X)
mobile_rate
#score
mean(CPCA$score)

###detail###
CPCA_con<-CPCA[,c(2,7)]


CPCA_con_word<-CPCA_con %>%
  unnest_tokens(word, content_detail)
head(CPCA_con_word)


CPCA_con_word <- CPCA_con_word %>%
  anti_join(stop_words)

CPCA_word_counts <- CPCA_con_word %>%
  anti_join(stop_words) %>%
  count(word, sort = TRUE) %>%
  ungroup()
CPCA_top100 <-CPCA_word_counts[c(1:100),]
wordcloud(CPCA_top100$word,CPCA_top100$n,random.order=FALSE,colors=brewer.pal(8,"Dark2"))

###############
###PBR
###############
PBR <- read.csv("reviews for PARKROYAL on Beach Road.csv",header=TRUE,stringsAsFactors = FALSE,fileEncoding="latin1")
#location
aggregate(PBR$score,list(nationality=PBR$nationality), FUN=mean)
#submitted ways
PBR_sub <- PBR[grepl("Submitted via mobile",PBR$tags),]
length(PBR_sub$X)
length(PBR$X)
mobile_rate =length(PBR_sub$X)/length(PBR$X)
mobile_rate
#score
mean(PBR$score)

###detail###
PBR_con<-PBR[,c(2,7)]


PBR_con_word<-PBR_con %>%
  unnest_tokens(word, content_detail)
head(PBR_con_word)


PBR_con_word <- PBR_con_word %>%
  anti_join(stop_words)

PBR_word_counts <- PBR_con_word %>%
  anti_join(stop_words) %>%
  count(word, sort = TRUE) %>%
  ungroup()
PBR_top100 <-PBR_word_counts[c(1:100),]
wordcloud(PBR_top100$word,PBR_top100$n,random.order=FALSE,colors=brewer.pal(8,"Dark2"))

###################
####SBRS
###################

SBRS<- read.csv("reviews for Siloso Beach Resort Sentosa.csv",header=TRUE,stringsAsFactors = FALSE,fileEncoding="latin1")
#location
aggregate(SBRS$score,list(nationality=SBRS$nationality), FUN=mean)
#submitted ways
SBRS_sub <- SBRS[grepl("Submitted via mobile",SBRS$tags),]
length(SBRS_sub$X)
length(SBRS$X)
mobile_rate =length(SBRS_sub$X)/length(SBRS$X)
mobile_rate
#score
mean(SBRS$score)
###detail###
SBRS_con<-SBRS[,c(2,7)]


SBRS_con_word<-SBRS_con %>%
  unnest_tokens(word, content_detail)
head(SBRS_con_word)


SBRS_con_word <- SBRS_con_word %>%
  anti_join(stop_words)

SBRS_word_counts <- SBRS_con_word %>%
  anti_join(stop_words) %>%
  count(word, sort = TRUE) %>%
  ungroup()
SBRS_top100 <-SBRS_word_counts[c(1:100),]
wordcloud(SBRS_top100$word,SBRS_top100$n,random.order=FALSE,colors=brewer.pal(8,"Dark2"))


##################
##ISB
##################
ISB <- read.csv("reviews for Ibis Singapore on Bencoolen.csv",header=TRUE,stringsAsFactors = FALSE,fileEncoding="latin1")
#location
aggregate(ISB$score,list(nationality=ISB$nationality), FUN=mean)
#submitted ways
ISB_sub <- ISB[grepl("Couple",ISB$tags),]
length(ISB_sub$X)
length(ISB$X)
mobile_rate =length(ISB_sub$X)/length(ISB$X)
mobile_rate
#score
mean(ISB$score)
###detail###
ISB_con<-ISB[,c(2,7)]

ISB_con_word<-ISB_con %>%
  unnest_tokens(word, content_detail)
head(ISB_con_word)


ISB_con_word <- ISB_con_word %>%
  anti_join(stop_words)

ISB_word_counts <- ISB_con_word %>%
  anti_join(stop_words) %>%
  count(word, sort = TRUE) %>%
  ungroup()
ISB_top100 <-ISB_word_counts[c(1:100),]
wordcloud(ISB_top100$word,ISB_top100$n,random.order=FALSE,colors=brewer.pal(8,"Dark2"))



###############
###DSBR
###############
DSBR <- read.csv("reviews for Destination Singapore Beach Road.csv",header=TRUE,stringsAsFactors = FALSE,fileEncoding="latin1")
#location
aggregate(DSBR$score,list(nationality=DSBR$nationality), FUN=mean)
#submitted ways
DSBR_sub <- DSBR[grepl("Submitted via mobile",DSBR$tags),]
length(DSBR_sub$X)
length(DSBR$X)
mobile_rate =length(DSBR_sub$X)/length(DSBR$X)
mobile_rate
#score
mean(DSBR$score)
###detail###
DSBR_con<-DSBR[,c(2,7)]

DSBR_con_word<-DSBR_con %>%
  unnest_tokens(word, content_detail)
head(DSBR_con_word)


DSBR_con_word <- DSBR_con_word %>%
  anti_join(stop_words)

DSBR_word_counts <- DSBR_con_word %>%
  anti_join(stop_words) %>%
  count(word, sort = TRUE) %>%
  ungroup()
DSBR_top100 <-DSBR_word_counts[c(1:100),]
wordcloud(DSBR_top100$word,DSBR_top100$n,random.order=FALSE,colors=brewer.pal(8,"Dark2"))

################
##total
################

total <- rbind(MBS,CPCA,PBR,SBRS,ISB,DSBR)
#location
aggregate(total$score,list(nationality=total$nationality), FUN=mean)

#submitted ways
total_sub <- total[grepl("Submitted via mobile",total$tags),]
length(total_sub$X)
length(total$X)
mobile_rate =length(total_sub$X)/length(total$X)
mobile_rate

#content

CPCA_con1<-CPCA[,c(2,6)]
head(CPCA_con1)
class(CPCA_con1)

CPCA_con1_word<-CPCA_con1 %>%
  unnest_tokens(word, content_detail)
head(CPCA_con1_word)
data(stop_words)
CPCA_con1_word <- CPCA_con1_word %>%
  anti_join(stop_words)

CPCA_word_counts <-CPCA_con1_word %>%
  anti_join(stop_words) %>%
  count(word, sort = TRUE) %>%
  ungroup()

wordcloud(CPCA_word_counts$word,CPCA_word_counts$n,random.order=FALSE,colors=brewer.pal(8,"Dark2"))

###detail###
CPCA_con2<-CPCA[,c(2,7)]
head(CPCA_con2)
class(CPCA_con2)

CPCA_con2_word<-CPCA_con2 %>%
  unnest_tokens(word, content_detail)
head(CPCA_con2_word)


CPCA_con2_word <- CPCA_con2_word %>%
  anti_join(stop_words)

word_counts2 <- CPCA_con2_word %>%
  anti_join(stop_words) %>%
  count(word, sort = TRUE) %>%
  ungroup()
top100_2 <-word_counts2[c(1:100),]
wordcloud(top100_2$word,top100_2$n,random.order=FALSE,colors=brewer.pal(8,"Dark2"))

grep("Submitted via mobile",MBS$tags)
head(total)
head(MBS)
