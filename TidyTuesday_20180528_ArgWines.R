library(tidyverse)
library(ggplot2)
library(readr)
library(stringr)


# Loading Data
#Sys.setenv(http_proxy="http://proxy.ar.bsch:8080/")
wine_ratings <- readr::read_csv("winemag-data-130k-v2.csv")
wine_ratings2 <- readr::read_csv("winemag-data_first150k.csv")

#####################
# Some exploration

# Points vs price by Country
wine_ratings2 %>%
    ggplot(aes(x=price, y=points, color=as.factor(country))) +
    geom_point()

# Points vs price by Country
wine_ratings2 %>%
  filter(country == "Argentina", str_detect(variety,'Malbec')) %>%
  ggplot(aes(x=points, y=price, color=as.factor(variety))) +
  geom_jitter(alpha=0.3, size=1.5)

wine_ratings2 %>%
  filter(country == "Argentina", str_detect(variety,'Cabernet Sauvignon')) %>%
  ggplot(aes(x=points, y=price, color=as.factor(province))) +
  geom_jitter(alpha=0.3, size=1.5)



##########################
# Text mining on reviews
library(tm)
library(wordcloud)
library(RColorBrewer)
library(ggrepel)


# Keeping only wines from Argentina, below 88 points and above
wines_low <- wine_ratings %>% 
  filter(country == "Argentina", points < 88) %>% 
  select(description) %>%
  VectorSource() %>%
  VCorpus(readerControl = list(language="english"))

wines_high <- wine_ratings %>% 
  filter(country == "Argentina", points >= 88) %>% 
  select(description) %>%
  VectorSource() %>%
  VCorpus(readerControl = list(language="english"))


#########################
# Let's clean the data!
#########################
clean_corpus <- function(corpus){
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removeWords, stopwords("english"))
  corpus <- tm_map(corpus, stemDocument, language = "english") 
  #corpus <- tm_map(corpus, PlainTextDocument)
  #corpus <- tm_map(corpus, tokenize_ngrams, n=4, n_min=2, stopwords("spanish"))
  return(corpus)
}

clean_wines_low <- clean_corpus(wines_low)
clean_wines_high <- clean_corpus(wines_high)


######################
# One grams
dtm_low <- DocumentTermMatrix(clean_wines_low)
dtm_high <- DocumentTermMatrix(clean_wines_high)


# And now goes into a matrix
# colSums IF DocumentTermMatrix; rowSums IF TermDocumentMatrix !
term_freq_low <- sort(colSums(as.matrix(dtm_low)),decreasing=TRUE)
term_freq_high <- sort(colSums(as.matrix(dtm_high)),decreasing=TRUE)

nps_word_freqs_low <- data.frame(word = names(term_freq_low), freq=term_freq_low)
nps_word_freqs_high <- data.frame(word = names(term_freq_high), freq=term_freq_high)


nps_word_freqs_dual <- data.frame(inner_join(nps_word_freqs_low, 
                                             nps_word_freqs_high, by="word"))


#####################
# Basic Bag of Words
# Freq plot for words in all documents

ggplot(subset(nps_word_freqs_low, freq > 500), aes(x = reorder(word, -freq), y = freq)) +
  geom_bar(stat = "identity") + 
  theme(axis.text.x=element_text(angle=45, hjust=1))

ggplot(subset(nps_word_freqs_high, freq > 500), aes(x = reorder(word, -freq), y = freq)) +
  geom_bar(stat = "identity") + 
  theme(axis.text.x=element_text(angle=45, hjust=1))


ggplot(subset(nps_word_freqs_dual, freq.x > 200 & freq.y > 200), 
       aes(x = freq.x, y = freq.y, label=word, color=word)) +
  geom_point() + 
  geom_text_repel(angle=20) +
  xlab("Sentimientos Negativos") + ylab("Sentimientos Positivos") +
  theme(legend.position = "none")


#####################
# Cloud of Words :)
dark2 <- brewer.pal(6,"Dark2")

dev.new(width = 1000, height = 1000, unit = "px")
wordcloud(nps_word_freqs_high$word, nps_word_freqs_high$freq,
          # With random False then most important words in the centre
          random.order = F,
          rot.per = 0.2,
          scale=c(5,.2),
          #min.freq=40,
          max.words = 100,
          colors = dark2)

