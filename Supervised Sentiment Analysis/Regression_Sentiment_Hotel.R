setwd("/Users/yangzhou/Desktop/Analytics Workshop/Text Mining/Analysis/")

Crown <- read_delim("~/Desktop/Analytics Workshop/Text Mining/hotel_review/Crown.csv", ",", escape_double = FALSE, trim_ws = TRUE)
Marina <- read_delim("~/Desktop/Analytics Workshop/Text Mining/hotel_review/Marina.csv", ",", escape_double = FALSE, trim_ws = TRUE)
Parkroyal <- read_delim("~/Desktop/Analytics Workshop/Text Mining/hotel_review/Parkroyal.csv", ",", escape_double = FALSE, trim_ws = TRUE)
Destination <- read_delim("~/Desktop/Analytics Workshop/Text Mining/hotel_review/Destination.csv", ",", escape_double = FALSE, trim_ws = TRUE)
Ibis <- read_delim("~/Desktop/Analytics Workshop/Text Mining/hotel_review/Ibis.csv", ",", escape_double = FALSE, trim_ws = TRUE)
Siloso <- read_delim("~/Desktop/Analytics Workshop/Text Mining/hotel_review/Siloso.csv", ",", escape_double = FALSE, trim_ws = TRUE)

train_data_df <- rbind(Crown[ ,c(5, 7)], Marina[ ,c(5, 7)], Parkroyal[ ,c(5, 7)], Destination[ ,c(5, 7)], Ibis[ ,c(5, 7)], Siloso[ ,c(5, 7)])

train_data_df$content_detail <- as.factor(train_data_df$content_detail)

head(train_data_df)

plot(table(train_data_df$score))

#Option1: convert to classification problem
for (i in 1:6300){
  if(train_data_df[i, 'score'] >= 8){train_data_df[i, 'score'] = 1}
  else {train_data_df[i, 'score'] = 0}
}
table(train_data_df$score)


#Option2: use scores as label directly
train_data_df$content_detail <- gsub("\n","",train_data_df$content_detail)
train_data_df$content_detail <- gsub("<U.*B209>","",train_data_df$content_detail)
train_data_df$content_detail <- gsub("<U.*B207>"," ",train_data_df$content_detail)
train_data_df$content_detail <- gsub("N/A","",train_data_df$content_detail)
train_data_df$content_detail <- gsub("\\.", "", train_data_df$content_detail) 
train_data_df$content_detail <- gsub("[[:digit:]]*", "", train_data_df$content_detail) 
train_data_df$content_detail <- gsub("//.*/ "," ",fixed = FALSE,train_data_df$content_detail)


train_data_df <- train_data_df[-70, ]
train_data_df <- train_data_df[-810, ]
train_data_df <- train_data_df[-1594, ]
train_data_df <- train_data_df[-1642, ]
train_data_df <- train_data_df[-1788,]
train_data_df <- train_data_df[-1826,]
train_data_df <- train_data_df[-1960,]
train_data_df <- train_data_df[-1990,]
train_data_df <- train_data_df[-3573,]
train_data_df <- train_data_df[-3970,]
train_data_df <- train_data_df[-4981,]
train_data_df <- train_data_df[-5089,]
train_data_df <- train_data_df[-5169,]
train_data_df <- train_data_df[-5837,]

library(tm)
corpus <- Corpus(VectorSource(train_data_df$content_detail))

corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeWords, stopwords("en"))
corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, stemDocument)


library(tm)
corpus <- Corpus(VectorSource(train_data_df$content_detail))
#put everything in lowercase. The second transformation is needed in order to have each 
#document in the format we will need later on. Then we remove punctuation, english stop 
#words, strip white spaces, and stem each word
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeWords, stopwords("english"))
corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, stemDocument)

corpus[1]$content

dtm <- DocumentTermMatrix(corpus)
dtm

sparse <- removeSparseTerms(dtm, 0.98)
sparse


#convert to data frame
important_words_df <- as.data.frame(as.matrix(sparse))
colnames(important_words_df) <- make.names(colnames(important_words_df))
import <- cbind(train_data_df, important_words_df)
import$content_detail <- NULL


library(caTools)
set.seed(1234)
# first we create an index with 80% True values based on Sentiment
spl <- sample.split(import$score, 2/3)
# now we use it to split our data into train and test
eval_train_data_df <- import[spl==T,]
eval_test_data_df <- import[spl==F,]
score <- eval_test_data_df[ ,1]
eval_test_data_df <- eval_test_data_df[ , -1]


# Build a CART classification regression tree model
#install.packages("rpart")
library(rpart)
CART <- rpart(score ~ ., data=eval_train_data_df, method='class')
predictCART <- predict(CART, newdata=eval_test_data_df, type='class')

table(score, predictCART)
(298+1037)/(298+547+213+1037)


# Try a random forest model.
library(randomForest)
set.seed(123)
RF <- randomForest(score ~ ., data=eval_train_data_df)
predictRF <- predict(RF, newdata=eval_test_data_df)


#Linear regression model
linear_model <- lm(score~., data=eval_train_data_df)
summary(linear_model)
pred <- predict(linear_model, newdata=eval_test_data_df)
pred <- (10/max(pred))*pred
plot(score, pred)
abline(line(score, pred))
cor(score, pred)









log_model <- glm(score~., data=eval_train_data_df, family=gaussian)
summary(log_model)

log_pred <- predict(log_model, newdata=eval_test_data_df, type="response")
log_pred2 <- (10/max(log_pred))*log_pred

plot(score, log_pred2)
abline(line(score, log_pred2))

cor(log_pred2,score)

















