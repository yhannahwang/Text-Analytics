library(NLP)
library(tm)
library(wordcloud)
library(SnowballC)
library(rJava)
library(Rwordseg)
library(plyr)
library(wordcloud2)
library(ggplot2)
library("scales")

# sentiment dictionary
pos <- read.csv("C:/Users/Saumya/MS in Analytics/02 Trimester II/01 Analytic Workshop/03 Text Mining/Sentiment Analysis/drive-download/positive-words.txt",header = FALSE,stringsAsFactors = FALSE)
weight<-rep(1,length(pos))
pos<-cbind(pos,weight)
head(pos)
neg <- read.csv("C:/Users/Saumya/MS in Analytics/02 Trimester II/01 Analytic Workshop/03 Text Mining/Sentiment Analysis/drive-download/negative-words.txt",header = FALSE,stringsAsFactors = FALSE)
neg <- as.data.frame(neg[51:nrow(neg),])
names(neg)[1]<-c("V1") 
weight<-rep(-1,length(neg))
neg<-cbind(neg,weight)
head(neg)
stopw <- read.csv("C:/Users/Saumya/MS in Analytics/02 Trimester II/01 Analytic Workshop/03 Text Mining/Sentiment Analysis/drive-download/Stopwords.txt",header = FALSE,stringsAsFactors = FALSE)
weight<-rep(0,length(stopw))
stopw<-cbind(stopw,weight)
head(stopw)
posneg <- rbind(pos,neg)
names(posneg) <- c("term", "weight") 
posneg <- posneg[!duplicated(posneg$term), ]
dict <- posneg[, "term"] 
insertWords(dict)
dict2 <- rbind(pos,neg,stopw)
names(dict2) <- c("term", "weight") 
dict2 <- dict2[!duplicated(dict2$term), ]

#load data
content_m <- read.csv("C:/Users/Saumya/MS in Analytics/02 Trimester II/01 Analytic Workshop/03 Text Mining/text mining/hotel data_review/reviews for Marina Bay Sands.csv",stringsAsFactors = FALSE)
head(content_m)
content_ID <- content_m$X
content_nat <- content_m$nationality
content_score <- content_m$score
content_usb <- content_m$content
content_usd <- content_m$content_detail
content_usd <- as.vector(content_usd)
content_usd1 <- as.vector(content_usd)

#remove the numbers/symbols, so on
content_usd1 <- gsub("\n","",content_usd1)
content_usd1 <- gsub("<U.*B209>","",content_usd1)
content_usd1 <- gsub("<U.*B207>"," ",content_usd1)
content_usd1 <- gsub("N/A","",content_usd1)
content_usd1 <- gsub("\\.", "", content_usd1) 
content_usd1 <- gsub("[[:digit:]]*", "", content_usd1) 
content_usd1 <- gsub("//.*/ "," ",fixed = FALSE,content_usd1)
head(content_usd)
head(content_usd1)

# split the words
system.time(x <- segmentCN(strwords = content_usd))
head(x)

#remove stopwords
x <- VCorpus(VectorSource(x)) 
x <- tm_map(x,content_transformer(tolower))
x <- tm_map(x, removeWords, stopwords("english")) 
x <- tm_map(x,stripWhitespace)
x <- lapply(x, as.character)
term <- unlist(x) 
b1 <- as.data.frame(table(term))
b1 <- b1[order(-b1$Freq),]
b1 <- b1[-1,]
wordcloud2(b1[b1$Freq>20,],size = 13,  color = "random-light", backgroundColor = 'white',minRotation = -pi/6, maxRotation = -pi/6, rotateRatio = 1 )
color <- c("tan4", "cyan4", "steelblue4","dark orange")
wordcloud(b1$term,b1$Freq,min.freq=15,random.order = FALSE,colors= color)

#join the dictionary with data
temp <- lapply(x, length)  
temp <- unlist(temp) 
ID <- rep(content_m[, "X"], temp) 
testterm_U <- as.data.frame(cbind(ID, term), stringsAsFactors = F) 
testterm_U <- join(testterm_U, dict2)
testsummary_U <- testterm_U
testterm_U <- testterm_U[!is.na(testterm_U$weight), ]  
head(testterm_U)

#Count the positive-negative score for each review
testsummary_U$pos <- as.numeric(testsummary_U$weight=="1")
testsummary_U$neg <- as.numeric(testsummary_U$weight=="-1")
testsummary_U$na <- as.numeric(is.na(testsummary_U$weight) || testsummary_U$weight=="0")
summary_1 <- aggregate(na ~ ID, data = testsummary_U, sum)
summary_2 <- aggregate(pos ~ ID, data = testsummary_U, sum)
summary_3 <- aggregate(neg ~ ID, data = testsummary_U, sum)
summary <- join(summary_3, summary_2)
summary$sentiment <- summary$pos - summary$neg
summary$sentiment <- summary$pos - summary$neg
summary$sentimentnormalized <- round(rescale(summary$sentiment, to=c(0,10)),2)
head(summary)

#caculate the sentiment score
dictresult_U <- aggregate(weight ~ ID, data = testterm_U, sum) 
dictresult_U$normalized <- round(rescale(dictresult_U$weight, to=c(0,10)),0)
dictlabel_U <- rep(-1, length(dictresult_U[, 1]))
dictlabel_U[dictresult_U$weight > 0] <- 1
dictresult_U1 <- as.data.frame(cbind(dictresult_U, dictlabel_U), stringsAsFactors = F) 
head(dictresult_U1)
dictresult2 <- as.data.frame(table(dictresult_U1$weight))
dictresult3 <- as.data.frame(table(dictresult_U1$normalized))

#show negtive
# find the negtive comments and draw wordcloud
negtive_U <- dictresult_U[dictresult_U$weight<0,]
negtive_1 <- data.frame()
for(j in 1:nrow(negtive_U)){
    for(i in 1:nrow(content_m)){
      if(negtive_U[j,]$ID ==content_m[i,]$X){
        negtive_1 <- rbind(negtive_1,content_m[i,])
        }
    }
}
#show wordcloud
color <- c("tan4", "cyan4", "steelblue4","dark orange")
system.time(x2 <- segmentCN(strwords = negtive_1$content_detail))
head(x2)
x2 <- VCorpus(VectorSource(x2)) 
x2 <- tm_map(x2, content_transformer(tolower))
x2 <- tm_map(x2, removeWords, stopwords("english"))
x2 <- lapply(x2, as.character)
term2 <- unlist(x2) 
b2 <- as.data.frame(table(term2))
b3 <- b2[-1,]
b3<- b3[!(b3$term2 %in% stopw$V1),]
b3 <- b3[order(-b3$Freq),]
wordcloud(b3$term2,b3$Freq,min.freq=1,random.order = FALSE,colors= color)

#show postive
# find the postive comments and draw wordcloud
postive_U <- dictresult_U[dictresult_U$weight>0,]
postive_1 <- data.frame()
for(j in 1:nrow(postive_U)){
  for(i in 1:nrow(content_m)){
    if(postive_U[j,]$ID ==content_m[i,]$X){
       postive_1 <- rbind(postive_1,content_m[i,])
    }
  }
}
#show wordcloud
system.time(y2 <- segmentCN(strwords = postive_1$content_detail))
head(y2)
y2 <- VCorpus(VectorSource(y2)) 
y2 <- tm_map(y2, content_transformer(tolower))
y2 <- tm_map(y2, removeWords, stopwords("english"))
y2 <- lapply(y2, as.character)
term3 <- unlist(y2) 
d2 <- as.data.frame(table(term3))
d3 <- d2[-1,]
d3<- d3[!(d3$term3 %in% stopw$V1),]
d3 <- d3[order(-d3$Freq),]
wordcloud(d3$term3,d3$Freq,min.freq=5,random.order = FALSE,colors= color)

# show sentiment score
dictresult2 <- as.data.frame(table(dictresult_U$weight))
ggplot(data = dictresult2,aes(x=dictresult2$Var1,y=dictresult2$Freq))+
  geom_col(fill="darkblue",alpha = 0.9)+
  labs(x="Sentiment score",y="Number of comments",title = "Sentiment Scores for comments about MB")+
  theme(axis.text.x = element_text(angle = 50,size=7.5),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.background = element_rect(fill = "gray97"),
        panel.border=element_rect(fill="transparent",color="light gray"),
        plot.title = element_text(lineheight = 610,colour = "gray9"))

#Compare with original Score
OriginalScore <- data.frame()
OriginalScore <- content_m[2:5]
OriginalScore <- OriginalScore[-2]
OriginalScore <- OriginalScore[-2]
OriginalScore$score <- round(OriginalScore$score,0)
names(OriginalScore) <- c("ID", "Original Score")
sentimentScore <- data.frame()
sentimentScore <- dictresult_U[-2]
sentimentScore$ID <- as.numeric(as.character(sentimentScore$ID))
names(sentimentScore) <- c("ID", "Sentiment Score")
ScoreCompare <- join(OriginalScore, sentimentScore)
ScoreCompare2 <- subset(ScoreCompare, ScoreCompare$`Sentiment Score` >= 0 )
ScoreCompare3 <- as.data.frame(table(OriginalScore$`Original Score`))
names(ScoreCompare3) <- c("Score", "Original")
ScoreCompare4 <- as.data.frame(table(sentimentScore$`Sentiment Score`))
names(ScoreCompare4) <- c("Score", "Sentiment")
Score <- c(0:10)
(ScoreCompare5 <-data.frame(Score))
names(ScoreCompare5) <- c("Score")
ScoreCompare5 <- merge(ScoreCompare5, ScoreCompare3, by.x = "Score", by.y = "Score", all.x = TRUE)
ScoreCompare5 <- merge(ScoreCompare5, ScoreCompare4, by.x = "Score", by.y = "Score", all.x = TRUE)
ScoreCompare5$Original[is.na(ScoreCompare5$Original)] <- 0
ScoreCompare5$Sentiment[is.na(ScoreCompare5$Sentiment)] <- 0

#Plot Comparison
ggplot(ScoreCompare5, aes(ScoreCompare5$Score)) +                    # basic graphical object
  geom_line(aes(y=ScoreCompare5$Original), colour="red") +  # first layer
  geom_line(aes(y=ScoreCompare5$Sentiment), colour="green") # second layer

 
#Exporting results
write.csv(summary, "summary2.csv")
write.csv(b3, "negative2.csv")
write.csv(d3, "positive2.csv")
