library(gutenbergr)    # for loading books
library(topicmodels)   # for modeling topics
library(stringr)       # deal with string
library(dplyr)         # do operations on table or dataframe (can do multiple operations using "%>%")
library(wordcloud2)    # draw word cloud
library(ggplot2)       # draw pictures 
library(tidytext)      # tidying model objects, extract topic-related probabilities
library(tidyr)         # tidying model objects

#load the four books
titles <- c("Twenty Thousand Leagues under the Sea", "The War of the Worlds","Pride and Prejudice", "Great Expectations")
books <- gutenberg_works(title %in% titles) %>% gutenberg_download(meta_fields = "title")

# split into chapters (with no book titles)
chapters <-   books %>% group_by(title) %>%
              mutate(chapter = cumsum(str_detect(text, regex("^chapter ", ignore_case = TRUE)))) %>%
              ungroup() %>% filter(chapter > 0) %>%
              unite(document, title, chapter)

# split into words
chapters_word <- chapters %>% unnest_tokens(word, text)

# remove stop words & find document-word counts
wordcount <- chapters_word %>% anti_join(stop_words) %>%
             count(document, word, sort = TRUE) %>% ungroup()


###########################
#wordcloud for four books #
###########################

by_chapter1 <- books %>% group_by(title) %>% ungroup() %>% unite(document, title)

# split into words
by_chapter_word1 <- by_chapter1 %>%
                    unnest_tokens(word, text)

# ------------------------------------------- #
# find document-word counts& draw wordcloud   #
# ------------------------------------------- #

#1.TWW
word_countsTWW <- by_chapter_word1[by_chapter_word1$document=='The War of the Worlds',] 
rept_TWW<- anti_join(word_countsTWW,stop_words)
n <- as.data.frame(table(rept_TWW$word))
n <- n[order(-n$Freq),]
top100TWW <- n[c(1:100),]
#wordcloud
wordcloud2(top100TWW,color = "random-light", minRotation = 3/4, maxRotation = pi/4, shuffle = TRUE,
           rotateRatio = 0.4,backgroundColor = "white",ellipticity = 0.65,size = 0.7)
#top 5 keywords
top5TWW <- n[c(1:5),]

# 2. Twenty Thousand Leagues under the Sea
word_countsTTL <- by_chapter_word1[by_chapter_word1$document=='Twenty Thousand Leagues under the Sea',] 
rept_TTL<- anti_join(word_countsTTL,stop_words)
nTTL <- as.data.frame(table(rept_TTL$word))
nTTL <- nTTL[order(-nTTL$Freq),]
top100TTL <- nTTL[c(1:100),]
#wordcloud
wordcloud2(top100TTL, rotateRatio = 0,maxRotation = 3.3/4,size = 1)
#top 5 keywords
top5TTL <- nTTL[c(1:5),]

# 3. Pride and Prejudice
word_countsPAP <- by_chapter_word1[by_chapter_word1$document=='Pride and Prejudice',] 
rept_PAP<- anti_join(word_countsPAP,stop_words)
nPAP <- as.data.frame(table(rept_PAP$word))
nPAP <- nPAP[order(-nPAP$Freq),]
top100PAP <- nPAP[c(1:100),]
#wordcloud
wordcloud2(top100PAP, rotateRatio = 1,color= "random-light",minRotation = -3.3/4, maxRotation = 3.3/4,size = 1.3)
#top 5 keywords
top5PAP <- nPAP[c(1:5),]

# 4. Great Expectations
word_countsGE <- by_chapter_word1[by_chapter_word1$document=='Great Expectations',] 
rept_GE<- anti_join(word_countsGE,stop_words)
nGE <- as.data.frame(table(rept_GE$word))
nGE <- nGE[order(-nGE$Freq),]
top100GE <- nGE[c(1:100),]
#wordcloud
wordcloud2(top100GE, rotateRatio = 0.1,minRotation = -3.3/4, maxRotation = 3.3/4,size = 1.2)
#top 5 keywords
top5GE <- nGE[c(1:5),]


#################################################
## LDA on chapters (clustering into 4 topics)   #
#################################################

#create a DocumentTermMatrix
chapterDTM<- wordcount %>% cast_dtm(document, word, n)
# set seed to trace back
chapterLDA <- LDA(chapterDTM, k = 4, control = list(seed = 1234))
chapterLDA
# per-topic-per-word probabilities (beta)  _ one-topic-per-term-per-row format
chapter_topics <- tidy(chapterLDA, matrix = "beta")
chapter_topics

#find top-5 terms within each topic 
top_terms <- chapter_topics %>%
  group_by(topic) %>%
  top_n(5, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms

#plot TOP5 KEYWORDS for each topic
top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term,beta,fill=factor(topic))) +
  geom_col(show.legend = TRUE,alpha = 0.6) +
  labs(title = "Top5 Key words for 4 topics")+
  facet_wrap(~ topic, scales = "free") +
  coord_flip(expand = TRUE)+
  theme(axis.text.y = element_text(size=11),
        axis.text.x = element_text(size=10),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.background = element_rect(fill = "gray95"),
        panel.border=element_rect(fill="transparent",color="light gray"),
        plot.title = element_text(lineheight = 610,colour = "black",size = 15))

################################
#Per-document classification   #
################################  _ find the accuracy of our clustering result

# check per-document-per-topic probabilities (gamma)
chapters_gamma <- tidy(chapterLDA, matrix = "gamma")
chapters_gamma
chapters_gamma <- chapters_gamma %>%
  separate(document, c("title", "chapter"), sep = "_", convert = TRUE)

chapters_gamma

# plot the overall classfication results
chapters_gamma %>%
  mutate(title = reorder(title, gamma * topic)) %>%
  ggplot(aes(factor(topic),fill=factor(topic), gamma)) +
  labs(title = "Chapter Classification for 4 Topics")+
  theme(axis.text.y = element_text(size=11),
        axis.text.x = element_text(size=10),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.background = element_rect(fill = "gray95"),
        panel.border=element_rect(fill="transparent",color="light gray"),
        plot.title = element_text(lineheight = 610,colour = "black",size = 15))+
  geom_col(show.legend = TRUE,alpha = 0.6) +
  facet_wrap(~ title)


chapter_classifications <- chapters_gamma %>%
  group_by(title, chapter) %>%
  top_n(1, gamma) %>%
  ungroup()

chapter_classifications

# see chapters which is the most likey to be misclassified
book_topics <- chapter_classifications %>%
  count(title, topic) %>%
  group_by(title) %>%
  top_n(1, n) %>%
  ungroup() %>%
  transmute(consensus = title, topic)

#find the misclassified chapters
misclassifications <-  chapter_classifications %>% inner_join(book_topics, by = "topic") %>%
                       filter(title != consensus)

# find which words in each document were assigned to which topic
assignws <- augment(chapterLDA, data = chapterDTM)


#combine book_topics and table
assigntopic <- assignws %>%
               separate(document, c("title", "chapter"), sep = "_", convert = TRUE) %>%
               inner_join(book_topics, by = c(".topic" = "topic"))



# plot
assigntopic %>%
  count(title, consensus, wt = count) %>%
  group_by(title) %>%
  mutate(percent = n/sum(n) ) %>%
  ggplot(aes(consensus, title, fill = percent)) +
  geom_tile() +
  scale_fill_gradient2(high = "dark blue") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        panel.grid = element_blank()) +
  labs(x = "Book words assignment",
       y = "Book words came from",
       fill = "% of assignments")

#get wrong words
misclassified_word <- assigntopic %>% filter(title != consensus)
misclassified_word %>% count(title, consensus, term, wt = count) %>% ungroup() %>% arrange(desc(n))

