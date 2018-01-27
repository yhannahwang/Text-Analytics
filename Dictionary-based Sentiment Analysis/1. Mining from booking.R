library(rvest)
library(xml2)
library(dplyr)
library(stringr)

#mine data from Booking
# for hotel's reviews 
##1. reviews for Marina Bay Sands_5 star
site1 <- "https://www.booking.com/reviews/sg/hotel/marina-bay-sands.en-gb.html?aid=397620;label=gog235jc-index-en-XX-XX-unspec-sg-com-L%3Aen-O%3AwindowsS10-B%3Achrome-N%3AXX-S%3Abo-U%3Ac-H%3As;sid=08f893084592e202d8893c3e06905168;customer_type=total;hp_nav=0;old_page=0;order=completed_desc;page="
site2 <- ";r_lang=en;rows=75&"
reviewers_inf <- data.frame()
i <- 1
for(i in 1:14){
site <- paste(site1,i,site2,sep="")
web<-read_html(site,encoding = "ISO-8859-1")
content<-web %>% html_nodes("div.review_item_header_content_container") %>% html_text() #brief content of the website
content <- gsub("\n","",content)
content_detail <- web %>% html_nodes("div.review_item_review_content")%>% html_text() #detail content of the website
content_detail <- gsub("\n","",content_detail)
nationality <- web %>% html_nodes("span.reviewer_country")%>% html_text() #nationality of the reviewer
nationality<- gsub("\n","",nationality)
score <- web %>% html_nodes("span.review-score-badge")%>% html_text() # each score ranked by the reviewers 
score <- score[-1]
score<- gsub("\n","",score)
tags <- web %>% html_nodes("ul.review_item_info_tags")%>% html_text() # the tag of the users
tags<- gsub("\n","",tags)
reviewers <-data_frame(nationality,tags,score,content,content_detail)
reviewers_inf<-rbind(reviewers_inf,reviewers)
}
setwd("C:\\Users\\Yhannah\\Desktop\\CLASS\\Analytics Workshop\\text mining\\booking")
write.csv(reviewers_inf,"reviews for Marina Bay Sands_total.csv")


# 2. Crowne Plaza Changi Airport_5 star
site1 <- "https://www.booking.com/reviews/sg/hotel/crowne-plaza-changi-airport.en-gb.html?aid=304142&label=gen173nr-1FCAEoggJCAlhYSDNiBW5vcmVmaMkBiAEBmAEyuAEHyAEN2AEB6AEB-AELkgIBeagCAw&sid=86590bba6f4b97b538fe1eaf742492ef&r_lang=en&customer_type=total&order=completed_desc;page="
site2 <- ";r_lang=en;rows=75&"
reviewers_inf <- data.frame()
i <- 1
for(i in 1:14){
  site <- paste(site1,i,site2,sep="")
  web<-read_html(site,encoding = "ISO-8859-1")
  content<-web %>% html_nodes("div.review_item_header_content_container") %>% html_text() #brief content of the website
  content <- gsub("\n","",content)
  content_detail <- web %>% html_nodes("div.review_item_review_content")%>% html_text() #detail content of the website
  content_detail <- gsub("\n","",content_detail)
  nationality <- web %>% html_nodes("span.reviewer_country")%>% html_text() #nationality of the reviewer
  nationality<- gsub("\n","",nationality)
  score <- web %>% html_nodes("span.review-score-badge")%>% html_text() # each score ranked by the reviewers 
  score <- score[-1]
  score<- gsub("\n","",score)
  tags <- web %>% html_nodes("ul.review_item_info_tags")%>% html_text() # the tag of the users
  tags<- gsub("\n","",tags)
  reviewers <-data_frame(nationality,tags,score,content,content_detail)
  reviewers_inf<-rbind(reviewers_inf,reviewers)
}
setwd("C:\\Users\\Yhannah\\Desktop\\CLASS\\Analytics Workshop\\text mining\\booking")
write.csv(reviewers_inf,"reviews for Crowne Plaza Changi Airport_total.csv")


# 3. PARKROYAL on Beach Road_4 star
site1 <- "https://www.booking.com/reviews/sg/hotel/parkroyal-on-beach-road.en-gb.html?aid=304142&label=gen173nr-1FCAEoggJCAlhYSDNiBW5vcmVmaMkBiAEBmAEyuAEHyAEN2AEB6AEB-AELkgIBeagCAw&sid=86590bba6f4b97b538fe1eaf742492ef&r_lang=en&customer_type=total&order=completed_desc;page="
site2 <- ";r_lang=en;rows=75&"
reviewers_inf <- data.frame()
i <- 1
for(i in 1:14){
  site <- paste(site1,i,site2,sep="")
  web<-read_html(site,encoding = "ISO-8859-1")
  content<-web %>% html_nodes("div.review_item_header_content_container") %>% html_text() #brief content of the website
  content <- gsub("\n","",content)
  content_detail <- web %>% html_nodes("div.review_item_review_content")%>% html_text() #detail content of the website
  content_detail <- gsub("\n","",content_detail)
  nationality <- web %>% html_nodes("span.reviewer_country")%>% html_text() #nationality of the reviewer
  nationality<- gsub("\n","",nationality)
  score <- web %>% html_nodes("span.review-score-badge")%>% html_text() # each score ranked by the reviewers 
  score <- score[-1]
  score<- gsub("\n","",score)
  tags <- web %>% html_nodes("ul.review_item_info_tags")%>% html_text() # the tag of the users
  tags<- gsub("\n","",tags)
  reviewers <-data_frame(nationality,tags,score,content,content_detail)
  reviewers_inf<-rbind(reviewers_inf,reviewers)
}
setwd("C:\\Users\\Yhannah\\Desktop\\CLASS\\Analytics Workshop\\text mining\\booking")
write.csv(reviewers_inf,"reviews for PARKROYAL on Beach Road_total.csv")


# 4. Siloso Beach Resort Sentosa_4 star
site1 <- "https://www.booking.com/reviews/sg/hotel/siloso-beach-resort.en-gb.html?aid=304142&label=gen173nr-1FCAEoggJCAlhYSDNiBW5vcmVmaMkBiAEBmAEyuAEHyAEN2AEB6AEB-AELkgIBeagCAw&sid=86590bba6f4b97b538fe1eaf742492ef&r_lang=en&customer_type=total&order=completed_desc;page="
site2 <- ";r_lang=en;rows=75&"
reviewers_inf <- data.frame()
i <- 1
for(i in 1:14){
  site <- paste(site1,i,site2,sep="")
  web<-read_html(site,encoding = "ISO-8859-1")
  content<-web %>% html_nodes("div.review_item_header_content_container") %>% html_text() #brief content of the website
  content <- gsub("\n","",content)
  content_detail <- web %>% html_nodes("div.review_item_review_content")%>% html_text() #detail content of the website
  content_detail <- gsub("\n","",content_detail)
  nationality <- web %>% html_nodes("span.reviewer_country")%>% html_text() #nationality of the reviewer
  nationality<- gsub("\n","",nationality)
  score <- web %>% html_nodes("span.review-score-badge")%>% html_text() # each score ranked by the reviewers 
  score <- score[-1]
  score<- gsub("\n","",score)
  tags <- web %>% html_nodes("ul.review_item_info_tags")%>% html_text() # the tag of the users
  tags<- gsub("\n","",tags)
  reviewers <-data_frame(nationality,tags,score,content,content_detail)
  reviewers_inf<-rbind(reviewers_inf,reviewers)
}
setwd("C:\\Users\\Yhannah\\Desktop\\CLASS\\Analytics Workshop\\text mining\\booking")
write.csv(reviewers_inf,"reviews for Siloso Beach Resort Sentosa_total.csv")


# 5. Ibis Singapore on Bencoolen_3 star
site1 <- "https://www.booking.com/reviews/sg/hotel/ibis-singapore-on-bencoolen.en-gb.html?aid=304142;label=gen173nr-1DCAEoggJCAlhYSDNYBGjJAYgBAZgBLsIBCndpbmRvd3MgMTDIARTYAQPoAQGSAgF5qAID;sid=aeb68474ab428022ac2392536398949e;customer_type=total;hp_nav=0;old_page=0;order=completed_desc;page="
site2 <- ";r_lang=en;rows=75&"
reviewers_inf <- data.frame()
i <- 1
for(i in 1:14){
  site <- paste(site1,i,site2,sep="")
  web<-read_html(site,encoding = "ISO-8859-1")
  content<-web %>% html_nodes("div.review_item_header_content_container") %>% html_text() #brief content of the website
  content <- gsub("\n","",content)
  content_detail <- web %>% html_nodes("div.review_item_review_content")%>% html_text() #detail content of the website
  content_detail <- gsub("\n","",content_detail)
  nationality <- web %>% html_nodes("span.reviewer_country")%>% html_text() #nationality of the reviewer
  nationality<- gsub("\n","",nationality)
  score <- web %>% html_nodes("span.review-score-badge")%>% html_text() # each score ranked by the reviewers 
  score <- score[-1]
  score<- gsub("\n","",score)
  tags <- web %>% html_nodes("ul.review_item_info_tags")%>% html_text() # the tag of the users
  tags<- gsub("\n","",tags)
  reviewers <-data_frame(nationality,tags,score,content,content_detail)
  reviewers_inf<-rbind(reviewers_inf,reviewers)
}
setwd("C:\\Users\\Yhannah\\Desktop\\CLASS\\Analytics Workshop\\text mining\\booking")
write.csv(reviewers_inf,"reviews for Ibis Singapore on Bencoolen_total.csv")


# 6.Destination Singapore Beach Road_3 star
site1 <- "https://www.booking.com/reviews/sg/hotel/destination-singapore-beach-road.en-gb.html?aid=304142;label=gen173nr-1DCAEoggJCAlhYSDNYBGjJAYgBAZgBLsIBCndpbmRvd3MgMTDIARTYAQPoAQGSAgF5qAID;sid=aeb68474ab428022ac2392536398949e;customer_type=total;hp_nav=0;old_page=0;order=completed_desc;page="
site2 <- ";r_lang=en;rows=75&"
reviewers_inf <- data.frame()
i <- 1
for(i in 1:14){
  site <- paste(site1,i,site2,sep="")
  web<-read_html(site,encoding = "ISO-8859-1")
  content<-web %>% html_nodes("div.review_item_header_content_container") %>% html_text() #brief content of the website
  content <- gsub("\n","",content)
  content_detail <- web %>% html_nodes("div.review_item_review_content")%>% html_text() #detail content of the website
  content_detail <- gsub("\n","",content_detail)
  nationality <- web %>% html_nodes("span.reviewer_country")%>% html_text() #nationality of the reviewer
  nationality<- gsub("\n","",nationality)
  score <- web %>% html_nodes("span.review-score-badge")%>% html_text() # each score ranked by the reviewers 
  score <- score[-1]
  score<- gsub("\n","",score)
  tags <- web %>% html_nodes("ul.review_item_info_tags")%>% html_text() # the tag of the users
  tags<- gsub("\n","",tags)
  reviewers <-data_frame(nationality,tags,score,content,content_detail)
  reviewers_inf<-rbind(reviewers_inf,reviewers)
}
setwd("C:\\Users\\Yhannah\\Desktop\\CLASS\\Analytics Workshop\\text mining\\booking")
write.csv(reviewers_inf,"reviews for Destination Singapore Beach Road_total.csv")




# for hotel information (scores)
##take Marina Bay Sands as example
web_MBS<-read_html("https://www.booking.com/reviews/sg/hotel/marina-bay-sands.en-gb.html?aid=397620;label=gog235jc-index-en-XX-XX-unspec-sg-com-L%3Aen-O%3AwindowsS10-B%3Achrome-N%3AXX-S%3Abo-U%3Ac-H%3As;sid=08f893084592e202d8893c3e06905168;customer_type=total;hp_nav=0;old_page=0;order=completed_desc;page=1;r_lang=en;rows=75&",encoding = "ISO-8859-1")
title <- web_MBS%>% html_nodes("p.review_score_name")%>% html_text() 
title[length(title)+1] <- "overall score"
score <-web_MBS %>% html_nodes("p.review_score_value")%>% html_text()
score_o <- (web_MBS %>% html_nodes("span.review-score-badge")%>% html_text())[1]
score_o <- gsub("\n","",score_o)
score[length(score)+1] <- score_o
score_total <- as.data.frame(rbind(title,score))
setwd("C:\\Users\\Yhannah\\Desktop\\CLASS\\Analytics Workshop\\TM")
write.csv(score_total,"Marina Bay Sands.csv")
