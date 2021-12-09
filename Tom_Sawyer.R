#Title: 01_Tom_Sawyer_Text_Analysis
#Author: Carolyn Wright


#Read in packages
library(gutenbergr)
library(magrittr)
library(dplyr)
library(tidytext)
library(stringr)
library(tidyr)
library(ggplot2)
library(wordcloud)
library(reshape2)

# library(devtools)
# devtools::install_github("lchiffon/wordcloud2")
library(wordcloud2)



#Download Tom Sawyer
Tom_Sawyer1 <- gutenberg_download(74)

#Clean up missing lines and remove preface information
Tom_Sawyer <- data.frame(line = 1:nrow(Tom_Sawyer1), text = Tom_Sawyer1$text) %>% filter(line>459)

#Create a word count and remove stop words
tidytext <- Tom_Sawyer%>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  count(word, sort = TRUE)

#Plot most commonly used words

# make word an ordered factor so that it does not sort the x-axis alphabetically
tidytext$word <- factor(tidytext$word, levels = tidytext$word)

common_words <- ggplot(data =head(tidytext,10)) + geom_bar(mapping = aes(x = word, y = n), stat = "identity", fill= "cornflowerblue") + 
  ylab("Frequency") + xlab("Word") + ggtitle("The Adventures of Tom Sawyer") +  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                                       panel.background = element_blank())

#Assign chapters

stop_words1 <- stop_words %>% rename(text = word)

tidy_tom <- Tom_Sawyer %>% anti_join(stop_words1) %>%
  mutate(
    linenumber = row_number(),
    chapter = cumsum(str_detect(text, 
                                regex("^chapter [\\divxlc]", 
                                      ignore_case = TRUE)))) %>%
  ungroup() %>%
  unnest_tokens(word, text)


#Create sentiment analysis using bing lexicon. 
#Note: It would have been better to use NRC however, this lexicon would not load
Tom_Sawyer_sentiment_BING <- tidy_tom %>%
  inner_join(get_sentiments("bing")) %>% group_by(chapter) %>%
  count( chapter = linenumber %/% 80, sentiment) %>% 
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>% 
  mutate(sentiment = positive - negative)

#Plot sentiment
BING_Plot <- ggplot(Tom_Sawyer_sentiment_BING, aes(chapter, sentiment)) +
  geom_col(show.legend = FALSE, fill = "tomato1") + ggtitle("Tom Sawyer - Sentiment Score (BING)") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank()) +  
  theme(title = element_text(size = 8))

#Look at most commonly used positive and negative words
bing_word_counts <- tidy_tom %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

word_count_plot_BING <- bing_word_counts %>%
  group_by(sentiment) %>%
  slice_max(n, n = 10) %>% 
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(x = "Contribution to sentiment(BING)",
       y = NULL) +  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                           panel.background = element_blank())


#Create sentiment analysis using bing lexicon. 
#Note: It would have been better to use NRC however, this lexicon would not load
Tom_Sawyer_sentiment_afinn <- tidy_tom %>%
  inner_join(get_sentiments("afinn")) %>% 
  group_by(chapter = linenumber %/% 80) %>% 
  summarise(sentiment = sum(value))

#Plot sentiment
AFINN_Plot <- ggplot(Tom_Sawyer_sentiment_afinn, aes(chapter, sentiment)) +
  geom_col(show.legend = FALSE, fill = "cornflowerblue") + ggtitle("Tom Sawyer - Sentiment Score (AFINN)") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank()) + 
  theme(title = element_text(size = 8))

#Look at most commonly used positive and negative words
afinn_word_counts <- tidy_tom %>%
  inner_join(get_sentiments("afinn")) 

afinn_word_counts$sentiment <- ifelse(afinn_word_counts$value <0,"Negative","Positive")

afinn_word_counts %<>% count(word, sentiment, sort = TRUE) %>%
  ungroup()

word_count_plot_AFINN <- afinn_word_counts %>%
  group_by(sentiment) %>%
  slice_max(n, n = 10) %>% 
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(x = "Contribution to sentiment (AFINN)",
       y = NULL) +  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                          panel.background = element_blank()) 


#WORD CLOUDS
# Word_Cloud_Prep <- tidy_tom %>%
#   anti_join(stop_words) %>%
#   count(word) 
# 
# Word_Cloud_Prep %<>% arrange(.,desc(n))
# 
# Word_Cloud_Prep %>% with(wordcloud(Word_Cloud_Prep$word, n, max.words = 100))




#Word cloud in shapes- chose to create a white fence.
# letterCloud(Word_Cloud_Prep, word = "TOM SAWYER",color = "white", background = "black", wordSize = 1)

# wordcloud2(Word_Cloud_Prep, figPath = "fence.png", size = 5,color = "white", background = "black")
# wordcloud2(head(Word_Cloud_Prep,100), figPath = "fence.png", size = 10,color = "white", background = "black")
# wordcloud2(Word_Cloud_Prep, figPath = "fence.png", size = .5,color = "white", background = "black")
# wordcloud2(Word_Cloud_Prep, figPath = "fence.png", size = 2,color = "white", background = "black")
# wordcloud2(Word_Cloud_Prep, figPath = "fence.png", size = 4,color = "white", background = "black")



#PART 3 - TRUENUMBERS
# devtools::install_github("Truenumbers/tnum/tnum")

# writeLines(Tom_Sawyer$text,"Tom_Sawyer_txt_RAW.txt")

# Tom_Sawyer_for_TN <- data.frame(readLines("Tom_Sawyer_txt.txt"))

# names(Tom_Sawyer_for_TN) <- "text"

library(tnum)

tnum.authorize("mssp1.bu.edu")

tnum.setSpace("test2")

# source("Book2TN-v6A-1.R")

# tnBooksFromLines(Tom_Sawyer_for_TN$text,"Twain/Tom_Sawyer2")

# tnum.getDBPathList(taxonomy = "subject", levels=2)


q1 <- tnum.query(query = "Twain/Tom_Sawyer2# has *", max = 19638)
df1 <- tnum.objectsToDf(q1)

df1 %<>% filter(property =="text")
df1 %<>% mutate(element_id = row_number())


library(sentimentr)

Sentence_Sentiment<-sentiment_by(df1$string.value, by = NULL)

Sentence_Sentiment_Full <- inner_join(df1,Sentence_Sentiment, by ="element_id" ) %>%
  mutate(sent_level = ifelse(ave_sentiment < 0.2, "Negative", ifelse(ave_sentiment > 0.2, "Positive","Neutral")))


Sentence_Sentiment_Full1<-Sentence_Sentiment_Full %>% separate(col=subject,
                                     into = c("Author", "Book","Chapter","Paragraph","Sentence"),
                                     sep = "/",
                                     fill = "right")
Sentence_Sentiment_Full1 %<>% filter(str_detect(Chapter,"heading",negate = FALSE)==FALSE)


Sentence_Sentiment_Full1 %>% ggplot() + geom_density(aes(ave_sentiment)) +facet_wrap(~Chapter)


Sentence_Sentiment_Full1$Chapter <- str_replace(Sentence_Sentiment_Full1$Chapter,"section","Chapter")
Sentence_Sentiment_Full1$Paragraph <- str_replace(Sentence_Sentiment_Full1$Paragraph,"paragraph:","")

Avg_Book_Sentiment <- mean(Sentence_Sentiment_Full1$ave_sentiment)

sentimentr_overall_book_pot<- Sentence_Sentiment_Full1 %>% ggplot() + geom_point(aes(x= Paragraph, y = ave_sentiment, color = ave_sentiment)) + 
  facet_wrap(~Chapter) + theme(legend.position = "none") + geom_hline(yintercept = Avg_Book_Sentiment)




#Character Analysis

Sentence_Sentiment_Full$Character_Subject <- ifelse(str_detect(Sentence_Sentiment_Full$string.value,"Tom", negate = FALSE)==TRUE | 
                                                      str_detect(Sentence_Sentiment_Full$string.value,"TOM", negate = FALSE)==TRUE, "TOM",
                                                    ifelse(str_detect(Sentence_Sentiment_Full$string.value,"Polly", negate = FALSE)==TRUE | 
                                                      str_detect(Sentence_Sentiment_Full$string.value,"POLLY", negate = FALSE)==TRUE, "POLLY",
                                                      ifelse(str_detect(Sentence_Sentiment_Full$string.value,"Becky", negate = FALSE)==TRUE | 
                                                               str_detect(Sentence_Sentiment_Full$string.value,"BECKY", negate = FALSE)==TRUE, "BECKY",
                                                             ifelse(str_detect(Sentence_Sentiment_Full$string.value,"Joe", negate = FALSE)==TRUE | 
                                                                      str_detect(Sentence_Sentiment_Full$string.value,"JOE", negate = FALSE)==TRUE, "JOE",
                                                                    ifelse(str_detect(Sentence_Sentiment_Full$string.value,"Huck", negate = FALSE)==TRUE | 
                                                                             str_detect(Sentence_Sentiment_Full$string.value,"HUCK", negate = FALSE)==TRUE, "HUCK",
                                                                           ifelse(str_detect(Sentence_Sentiment_Full$string.value,"Sid", negate = FALSE)==TRUE | 
                                                                                    str_detect(Sentence_Sentiment_Full$string.value,"SID", negate = FALSE)==TRUE, "SID",""))))))

Sentence_Sentiment_Full$Character_Subject <- ifelse(str_detect(Sentence_Sentiment_Full$string.value,"Tom said", negate = FALSE)==TRUE & Sentence_Sentiment_Full$Character_Subject =="TOM", 
                                                    "", Sentence_Sentiment_Full$Character_Subject )
Sentence_Sentiment_Full$Character_Subject <- ifelse(str_detect(Sentence_Sentiment_Full$string.value,"Polly said", negate = FALSE)==TRUE & Sentence_Sentiment_Full$Character_Subject =="POLLY", 
                                                    "", Sentence_Sentiment_Full$Character_Subject )
Sentence_Sentiment_Full$Character_Subject <- ifelse(str_detect(Sentence_Sentiment_Full$string.value,"Becky said", negate = FALSE)==TRUE & Sentence_Sentiment_Full$Character_Subject =="BECKY", 
                                                    "", Sentence_Sentiment_Full$Character_Subject )
Sentence_Sentiment_Full$Character_Subject <- ifelse(str_detect(Sentence_Sentiment_Full$string.value,"Joe said", negate = FALSE)==TRUE & Sentence_Sentiment_Full$Character_Subject =="JOE", 
                                                    "", Sentence_Sentiment_Full$Character_Subject )
Sentence_Sentiment_Full$Character_Subject <- ifelse(str_detect(Sentence_Sentiment_Full$string.value,"Huck said", negate = FALSE)==TRUE & Sentence_Sentiment_Full$Character_Subject =="HUCK", 
                                                    "", Sentence_Sentiment_Full$Character_Subject )
Sentence_Sentiment_Full$Character_Subject <- ifelse(str_detect(Sentence_Sentiment_Full$string.value,"Sid said", negate = FALSE)==TRUE & Sentence_Sentiment_Full$Character_Subject =="SID", 
                                                    "", Sentence_Sentiment_Full$Character_Subject )

Character_Analysis <- subset(Sentence_Sentiment_Full, Character_Subject != "")

Character_Analysis %>% 
  ggplot() + geom_boxplot(aes(y = Character_Subject, x = ave_sentiment))


Character_Analysis_plot<- Character_Analysis %>% ggplot() + geom_density(aes(ave_sentiment, color=Character_Subject)) +facet_wrap(~Character_Subject)

#Analyze Tom

Character_Analysis_TOM <- subset(Sentence_Sentiment_Full, Character_Subject == "TOM")

Character_Analysis_TOM %<>% separate(col=subject,
                    into = c("Author", "Book","Chapter","Paragraph","Sentence"),
                    sep = "/",
                    fill = "right")


Character_Analysis_TOM_CP <- Character_Analysis_TOM %>% group_by(Chapter, Paragraph) %>% summarise(avg_sent_CP = mean(ave_sentiment))

Character_Analysis_TOM_CP$Chapter <- str_replace(Character_Analysis_TOM_CP$Chapter,"section","Chapter")
Character_Analysis_TOM_CP$Paragraph <- str_replace(Character_Analysis_TOM_CP$Paragraph,"paragraph:","")

Character_Analysis_TOM_CP %>% ggplot() + geom_point(mapping = aes(x = Paragraph, y =avg_sent_CP, color = Chapter )) +
  facet_wrap(~Chapter) + theme(legend.position = "none") + scale_x_discrete(breaks=c("20","40","80","120","140","160")) + ggtitle("Tom")

#Analyze Huck

Character_Analysis_HUCK <- subset(Sentence_Sentiment_Full, Character_Subject == "HUCK")

Character_Analysis_HUCK %<>% separate(col=subject,
                                     into = c("Author", "Book","Chapter","Paragraph","Sentence"),
                                     sep = "/",
                                     fill = "right")


Character_Analysis_HUCK_CP <- Character_Analysis_HUCK %>% group_by(Chapter, Paragraph) %>% summarise(avg_sent_CP = mean(ave_sentiment))

Character_Analysis_HUCK_CP$Chapter <- str_replace(Character_Analysis_HUCK_CP$Chapter,"section","Chapter")
Character_Analysis_HUCK_CP$Paragraph <- str_replace(Character_Analysis_HUCK_CP$Paragraph,"paragraph:","")

Character_Analysis_HUCK_CP %>% ggplot() + geom_point(mapping = aes(x = Paragraph, y =avg_sent_CP, color = Chapter )) +
  facet_wrap(~Chapter) + theme(legend.position = "none") + scale_x_discrete(breaks=c("20","40","80","120","140","160")) + ggtitle("Huck")


#Analyze POLLY

Character_Analysis_Polly <- subset(Sentence_Sentiment_Full, Character_Subject == "POLLY")

Character_Analysis_Polly %<>% separate(col=subject,
                                      into = c("Author", "Book","Chapter","Paragraph","Sentence"),
                                      sep = "/",
                                      fill = "right")


Character_Analysis_POLLY_CP <- Character_Analysis_Polly %>% group_by(Chapter, Paragraph) %>% summarise(avg_sent_CP = mean(ave_sentiment))

Character_Analysis_POLLY_CP$Chapter <- str_replace(Character_Analysis_POLLY_CP$Chapter,"section","Chapter")
Character_Analysis_POLLY_CP$Paragraph <- str_replace(Character_Analysis_POLLY_CP$Paragraph,"paragraph:","")

Character_Analysis_POLLY_CP %>% ggplot() + geom_point(mapping = aes(x = Paragraph, y =avg_sent_CP, color = Chapter )) +
  facet_wrap(~Chapter) + theme(legend.position = "none") + scale_x_discrete(breaks=c("20","40","80","120","140","160")) + ggtitle("Polly")


#Analyze Tom and Huck

Character_Analysis_TOM_HUCK <- subset(Sentence_Sentiment_Full, Character_Subject %in% c("TOM","HUCK"))

Character_Analysis_TOM_HUCK %<>% separate(col=subject,
                                     into = c("Author", "Book","Chapter","Paragraph","Sentence"),
                                     sep = "/",
                                     fill = "right")


Character_Analysis_TOM_HUCK_CP <- Character_Analysis_TOM_HUCK %>% group_by(Chapter, Paragraph,Character_Subject ) %>% summarise(avg_sent_CP = mean(ave_sentiment))

Character_Analysis_TOM_HUCK_CP$Chapter <- str_replace(Character_Analysis_TOM_HUCK_CP$Chapter,"section","Chapter")
Character_Analysis_TOM_HUCK_CP$Paragraph <- str_replace(Character_Analysis_TOM_HUCK_CP$Paragraph,"paragraph:","")

Character_Analysis_TOM_HUCK_CP_plot<-Character_Analysis_TOM_HUCK_CP %>% ggplot() + geom_point(mapping = aes(x = Paragraph, y =avg_sent_CP, color = Character_Subject )) +
  facet_wrap(~Chapter) + ggtitle("Tom and Huck",subtitle = "Tom - Blue, Huck - Red") + geom_hline(yintercept = Avg_Book_Sentiment) + theme(legend.position = "none")


