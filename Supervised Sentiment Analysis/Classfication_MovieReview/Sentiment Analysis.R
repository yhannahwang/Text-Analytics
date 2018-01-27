#Step 2 每 exploring and preparing the data 
sa_raw <-read.csv("C:\\myprojectR\\sentiment labelled sentences\\ss1.csv",stringsAsFactors = FALSE)
sa_raw$type<- factor(sa_raw$type)
str(sa_raw$type)
table(sa_raw$type)

#Data preparation-cleaning and standardizing text data
library(NLP)
library(tm)
sa_corpus <- VCorpus(VectorSource(sa_raw$text))
print(sa_corpus)
inspect(sa_corpus[1:5]) 
as.character(sa_corpus[[1]])
#to apply as.character() to a subset of corpus elements is as follows:
lapply(sa_corpus[1:2], as.character) 

# removing numbers, stop words, punctuation, and blank space as well as performing stemming
sa_corpus_clean <- tm_map(sa_corpus,content_transformer(tolower))
as.character(sa_corpus_clean[[1]]) 
sa_corpus_clean <- tm_map(sa_corpus_clean, removeNumbers)
sa_corpus_clean <- tm_map(sa_corpus_clean, removeWords, stopwords()) 
sa_corpus_clean <- tm_map(sa_corpus_clean, removePunctuation) 
library(SnowballC)
sa_corpus_clean <- tm_map(sa_corpus_clean, stemDocument)
sa_corpus_clean <- tm_map(sa_corpus_clean, stripWhitespace) 
as.character(sa_corpus_clean[[1]])

#create a DTM sparse matrix 
sa_dtm <- DocumentTermMatrix(sa_corpus_clean)

# creating training and test datasets
order <- sample(6918,5188)
sa_dtm_train <- sa_dtm[order,]
sa_dtm_test <- sa_dtm[-order,]
sa_train_labels <- sa_raw[order, ]$type
sa_test_labels <- sa_raw[-order,  ]$type


#confirm that the subsets are representative of the complete set of sa data
prop.table(table(sa_train_labels))
prop.table(table(sa_test_labels))
#(Both the training data and test data contain about 50 percent 0. This suggests that the negative sentences were divided evenly between the two datasets)

#Visualizing text data 每 word clouds
library(RColorBrewer)
library(wordcloud)
b=c("tan4", "cyan4", "steelblue4","dark orange")
wordcloud(sa_corpus_clean, min.freq = 40, random.order = FALSE, random.color=TRUE, colors = b) 

negative <- subset(sa_raw, type == "0")
positive <- subset(sa_raw, type == "1")
wordcloud(positive$text,max.words = 60, scale = c(3, 0.5), random.color=TRUE, colors = b,random.order = FALSE)
wordcloud(negative$text, max.words = 60, scale = c(3, 0.5), random.color=TRUE, colors = b,random.order = FALSE)

# creating indicator features for frequent words 
findFreqTerms(sa_dtm_train, 4)
sa_freq_words <- findFreqTerms(sa_dtm_train, 4) # save our frequent words for later on
str(sa_freq_words)
sa_dtm_freq_train<- sa_dtm_train[ , sa_freq_words] 
sa_dtm_freq_test<- sa_dtm_test[ , sa_freq_words] 
#The Naive Bayes classifier is typically trained on data with categorical features. This poses a problem, since the cells in the sparse matrix are numeric and measure the number of times a word appears in a message. We need to change this to a categorical variable
convert_counts <- function(x) {    x <- ifelse(x > 0, "Yes", "No")  } 
sa_train <- apply(sa_dtm_freq_train, MARGIN = 2,convert_counts)
sa_test <- apply(sa_dtm_freq_test, MARGIN = 2, convert_counts) 

#Step 3 每 training a model on the data 
library(MASS)
library(klaR)
sa_classifier <- NaiveBayes(sa_train,sa_train_labels,laplace = 1)

#Step 4 每 evaluating model performance 
#The predict() function is used to make the predictions
sa_test_pred <- predict(sa_classifier, sa_test) 
library(gmodels)
CrossTable(sa_test_pred$class, sa_test_labels, prop.chisq = FALSE, prop.t = FALSE,  prop.r = FALSE,dnn = c('predicted', 'actual')) 
