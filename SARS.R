#Step 1: Load the required packages (including rtweet) in RStudio
library(rtweet)
library(textdata)
library(janeaustenr)
library(reshape2)
library(devtools)
library(widyr)
library(lubridate) # Date & Time
library(ggplot2)# Data Visualisation
library(dplyr) #Data Wrangling
library(tidytext) #Text Mining
library(tm) #Text Mining
library(wordcloud)
library(readr)
library(tidyr) #Tidy Text
library(RColorBrewer) #Data Visualisation
library(stringr) #String Manipulation
library(RSentiment) #Sentiment Analysis
library(cowplot) #Plot Arrange
library(ggthemes) #Data Visualisation
library(knitr)
library(kableExtra)
library(tm)
library(purrr)
library(twitteR)
library(httpuv)



#Step 2: Authenticate using your credentials to Twitter's API by creating an access token. Steps on getting Twitter access tokens:
create_token(app_name <- "Thinkkade",
             api_key <- "l5WNm9NYbu4Sw4qVp02DhNYju",
             api_secret <- "XvH85twt32MyU2LPnEQyCO9CfXLRgLRI91kH6Z1HIXNGV6YDen",
             access_token <- "415803231-O3SEOL1wMditnW44QGDDyRDWvasmEh3iZOelEsUA",
             access_token_secret <- "c0n2ANkjJKsUFU66ujXviyIrrRxNPddAgV2nAEPIEctgB")

#Step 3: search tweets
sars <- search_tweets('#EndSarsNow', n = 900, since='2020-10-04', until='2020-11-22', lang = 'en')
sars

sars <- apply(sars,2,as.character)
write.csv(sars, "sarsdata.csv")




# read in our data
f <- file.choose("sarsdata.csv")
sars <- read.csv(f)

#Step 4: Process each set of tweets into tidy text or corpus objects.
tweets.sars = sars %>% select(screen_name,text)
tweets.sars

#Step 5: Use pre-processing text transformations to clean up the tweets; this includes stemming words. An example of stemming is rolling the words "computer", "computational" and "computation" to the root "comput".
head(tweets.sars$text)
#Remove http elements manually
tweets.sars$stripped_text1 <- gsub("http\\s+","",tweets.sars$text)
#vuse the unnest_tokens() function to convert to lowercase,
#remove punctuation, and add id for each tweet
tweets.sars_stem <- tweets.sars %>%
  select(stripped_text1) %>%
  unnest_tokens(word, stripped_text1)
head(tweets.sars_stem)
# remove stop words from your list of words
cleaned_tweets.sars <- tweets.sars_stem %>%
  anti_join(stop_words)
head(cleaned_tweets.sars)




#Step6: The top 30 commonly used words in the set of tweets for Covid-19; this gives an overall picture of what people are most concerned about, and the extent to which they are engaged on the topic.#Top 10 words in #Covid_19 tweets
cleaned_tweets.sars %>%
  count(word, sort = TRUE) %>%
  top_n(30) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  theme_classic() +
  labs(x = "count",
       y = "Unique words",
       title = "Unique word counts found in #EndSarsNow")



#Step 7A: Perform sentiment analysis using the Bing lexicon and get_sentiments function from the tidytext package.
#bing sentiment analysis
bing_sars = cleaned_tweets.sars %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()
bing_sars


#Step 7B: Then to visually depict the word counts, you can filter and plot the words side-by-side to compare the positive vs negative emotion.
bing_sars %>%
  group_by(sentiment) %>%
  top_n(30) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(title = "Tweets containing #EndSarsNow",
       y = "contribution to sentiment",
       x = NULL) +
  coord_flip() + theme_bw()




#Step 9: Then to visually depict the word counts, you can filter and plot the words side-by-side to compare the positive vs negative emotion(7B Contd)
bing_sars %>%
  group_by(sentiment) %>%
  # Take the top 10 for each sentiment
  top_n(30) %>%
  ungroup() %>%
  # Make word a factor in order of n
  mutate(word = reorder(word, n)) %>%
  # Use aes() to put words on the x-axis and n on the y-axis
  ggplot(aes(word, n, fill = sentiment)) +
  # Make a bar chart with geom_col()
  geom_col(show.legend = FALSE) +
  geom_text (aes(label = n, hjust=1), size = 3.5, color = "black")+
  facet_wrap(~sentiment, scales = "free") +  
  coord_flip() +
  ggtitle("Most common positive and negative words in #EndSarsNow")


#Step 10: Bing overall sentiment.
bing_sars %>%
  group_by(sentiment) %>%
  summarise(word_count = n()) %>%
  ungroup() %>%
  mutate(sentiment = reorder(sentiment, word_count)) %>%
  ggplot(aes(sentiment, word_count, fill = sentiment)) +
  geom_col() +
  guides(fill = FALSE) +
  labs(x = NULL, y = "Word Count") +
  scale_y_continuous(limits = c(0, 1000)) +
  ggtitle("#EndSarsNow Overall Bing Sentiment") +
  coord_flip()




#Step 11: wordcloud plot the 50 most common words

pal <- brewer.pal(8,"Dark2")

tidy_sars = cleaned_tweets.sars %>%
  anti_join(stop_words) %>%
  count(word) %>%
  with(wordcloud(word, n, random.order = FALSE, max.words = 30, colors=pal))

#Sentiment word cloud: Classifying the words into different types of emotions also helps us understand how people are feeling towards a subject

#Wordcloud:the most common positive and negative words
tidy_sars = cleaned_tweets.sars %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("gray20", "gray80"),
                   max.words = 30)

head(sars)

cleaned_tweets.sars<- data.frame(txt = sars$text,
                                 stringsAsFactors = FALSE)


cleaned_tweets.sars %>%
  unnest_tokens(word, txt, token = "ngrams", n = 2) %>% 
  separate(word, c("word1", "word2"), sep = " ") %>% 
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>% 
  unite(word,word1, word2, sep = " ") %>% 
  count(word, sort = TRUE) %>% 
  slice(1:30) %>% 
  ggplot(x=word, y=n) + geom_bar(aes(word, n), stat = "identity", fill = "#de5833") +
  theme_minimal() +
  coord_flip() +
  labs(title = "Top Bigrams in #EndSarsNow")
cleaned_tweets.virus %>%
  anti_join(stop_words) %>%
  count(word) %>%
  
  
  
  
  cleaned_tweets.sars %>%
  unnest_tokens(word, txt, token = "ngrams", n = 3) %>% 
  separate(word, c("word1", "word2", "word3"), sep = " ") %>% 
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>% 
  filter(!word3 %in% stop_words$word) %>% 
  unite(word,word1, word2,word3, sep = " ") %>% 
  count(word, sort = TRUE) %>% 
  slice(1:30) %>% 
  ggplot(x=word, y=n) + geom_bar(aes(word, n), stat = "identity", fill = "#de5833") +
  theme_minimal() +
  coord_flip() +
  labs(title = "Top trigrams in #EndSarsNow")
  cleaned_tweets.virus %>%
  anti_join(stop_words) %>%
  count(word) %>%
  


cleaned_tweets.sars %>%
  unnest_tokens(word, txt, token = "ngrams", n = 4) %>% 
  separate(word, c("word1", "word2", "word3", "word4"), sep = " ") %>% 
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>% 
  filter(!word3 %in% stop_words$word) %>%
  filter(!word4 %in% stop_words$word) %>% 
  unite(word,word1, word2,word3,word4, sep = " ") %>% 
  count(word, sort = TRUE) %>% 
  slice(1:30) %>% 
  ggplot(x=word, y=n) + geom_bar(aes(word, n), stat = "identity", fill = "#de5833") +
  theme_minimal() +
  coord_flip() +
  labs(title = "Top 4-grams in #EndSarsNow")

cleaned_tweets.sars %>%
  unnest_tokens(word, txt, token = "ngrams", n = 5) %>% 
  separate(word, c("word1", "word2", "word3", "word4", "word5"), sep = " ") %>% 
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>% 
  filter(!word3 %in% stop_words$word) %>%
  filter(!word4 %in% stop_words$word) %>% 
  filter(!word5 %in% stop_words$word) %>% 
  unite(word,word1, word2,word3,word4, word5, sep = " ") %>% 
  count(word, sort = TRUE) %>% 
  slice(1:50) %>% 
  ggplot(x=word, y=n) + geom_bar(aes(word, n), stat = "identity", fill = "#de5833") +
  theme_minimal() +
  coord_flip() +
  labs(title = "Top 5grams in #EndSarsNow")


#Tokenizing character vector file 'tweets'.
token = data.frame(text=sars, stringsAsFactors = FALSE) %>% unnest_tokens(word, text)

#Matching sentiment words from the 'NRC' sentiment lexicon
senti = inner_join(token, get_sentiments("nrc")) %>%
  count(sentiment)
senti$percent = (senti$n/sum(senti$n))*100

#Plotting the sentiment summary 
ggplot(senti, aes(sentiment, percent)) +   
  geom_bar(aes(fill = sentiment), position = 'dodge', stat = 'identity')+ 
  ggtitle("Sentiment analysis based on lexicon: 'NRC'")+
  coord_flip() +
  theme(legend.position = 'none', plot.title = element_text(size=18, face = 'bold'),
        axis.text=element_text(size=16),
        axis.title=element_text(size=14,face="bold"))