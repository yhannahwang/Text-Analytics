 # the-powerpuff-girls

our demo contains two parts: sentiment analysis and topic modeling.

for dictionary-based sentiment analysis, we mined reviews for 6 hotels from booking, and after some pre-processing on our data, we do the sentiment analysis based on the reviews, mainly try to find some interesting information(e.g.: for each hotel, what are the reviewers' favorite components, and what made the reviewers most uncomfortable about that hotel)

for supervised sentiment analysis,we try to capture the relationship between the real scores of each review and each review's sentiment score.
on the other side, in order to show the sentiment analysis on labeled dataset, we use a dataset of movie reviews(each review has a label to show whether that review is positive or negative), using Naive Bayes to train the model and do the classification to predict the label of the test dataset.

for topic modeling part, we used unlabeled chapters of 4 novels, using LDA, to clustering 4 topics, in which 1 should represent 1 novels respectively. in order to check the final result, we also put back the labeled chapters in to our clustering results and to find if there any chapters are misclassified.

Our file contains:
1. sentiment analysis
    1.1 dictionary-based sentiment analysis
        - raw data(uncleaned)/ cleaned data/ hotel basic infomation
        - dictionary used for sentiment analysis
        - sentiment output
        -R code:
           - mine data from booking
           - text preprocessing
           - basic analysis and word cloud 
           - dictionary-based sentiment analysis
     1.2 supervised sentiment analysis
        - classification based on movies' reviews
        - supervised learning on booking dataset

2. topic modeling
3. Slides
