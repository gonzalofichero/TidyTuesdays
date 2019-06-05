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


# How many reviews by region are there in Arg?
wine_ratings2 %>% 
  filter(country=="Argentina") %>%
  group_by(region_1) %>%
  summarise(n = n()) %>%
  arrange(desc(n))
# How many reviews by varietal are there in Arg?
wine_ratings2 %>% 
  filter(country=="Argentina") %>%
  group_by(variety) %>%
  summarise(n = n()) %>%
  arrange(desc(n))
# Top 10 varietales represent 87% of total reviews

# Selecting Arg wines of top 10 varietales
wine_arg <- wine_ratings2 %>%
              filter(country=="Argentina", 
                     variety %in% c("Malbec","Cabernet Sauvignon","Chardonnay",
                                    "Torrontés","Red Blend","Merlot","Bonarda",
                                    "Sauvignon Blanc","Syrah","Pinot Noir"))

# New feature: kind of wine c("Red","White")
wine_arg <- wine_arg %>%
              mutate(kind = case_when(variety %in% c("Chardonnay","Torrontés","auvignon Blanc") ~ "White",
                                      TRUE ~ "Red"))

wine_arg %>% 
  group_by(kind) %>%
  summarise(n = n()) %>%
  arrange(desc(n))
# 83.18% Red wine

wine_arg %>%
    ggplot(aes(x=kind, y=price)) + geom_boxplot()

wine_arg %>%
  ggplot(aes(x=kind, y=points)) + geom_boxplot()

wine_arg %>% 
  group_by(kind) %>%
  summarise(median_point = median(points),
            median_price = median(price, rm.na=T))


wine_arg %>%
  ggplot(aes(x=price, y=points, color=as.factor(kind))) +
  geom_point(alpha=0.2, size=2) +
  stat_smooth(method = 'lm', formula = y ~ log(x))


# Creating high vs low feature according to points = log(price) function
# Different classification for Red and White wines
red <- wine_arg %>%
        filter(kind == "Red")
red_class <- lm(points ~ log(price), data = red)

white <- wine_arg %>%
        filter(kind == "White")
white_class <- lm(points ~ log(price), data = white)

red$estimated_point <- predict(red_class, red)
white$estimated_point <- predict(white_class, white)

# Rejoining and class above or below expected
rm(wine_arg)
wine_arg <- rbind(red,white)

wine_arg <- wine_arg %>%
              mutate(expected_class = case_when(points >= estimated_point ~ "Above Expected",
                                                points < estimated_point ~ "Below Expected",
                                                TRUE ~ "No Price"))

wine_arg %>%
  ggplot(aes(x=price, y=points, color=as.factor(expected_class))) +
  geom_point(alpha=0.2, size=2)



##########################
# Text mining on reviews
library(tm)
library(wordcloud)
library(RColorBrewer)
library(ggrepel)


# Keeping only wines from Argentina, below 88 points and above
wines_low <- wine_arg %>% 
  filter(expected_class == "Below Expected") %>% 
  select(description) %>%
  VectorSource() %>%
  VCorpus(readerControl = list(language="english"))

wines_high <- wine_arg %>% 
  filter(expected_class == "Above Expected") %>% 
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



##########################
# Topic Modeling
library(topicmodels)
library(broom)


# Searching for 7 topics
lda <- LDA(dtm_high, k=3, control = list(seed = 31416))
term <- terms(lda, 10) #first 10 terms of every topic
term #checking

lda_low <- LDA(dtm_low, k=3, control = list(seed = 31416))
term_low <- terms(lda_low, 10) #first 10 terms of every topic
term_low #checking

topic_doc <- tidy(lda, matrix="beta")


################################
# Correlated words graph
library(tidyverse)
library(tidytext)
library(widyr)
library(igraph)
library(ggraph)
library(extrafont)

# Unnesting words from wine description
wine_words <- wine_arg %>%
  unnest_tokens(word, description) %>% 
  anti_join(stop_words, by = "word")

# Keeping top 40 words
top_wine_words <- wine_words %>%
  count(word, sort = TRUE) %>%
  head(40)

# Correlation matrix pairwise
wine_words_correlation <- wine_words %>%
  filter(word %in% top_wine_words$word) %>% 
  pairwise_cor(word, points, sort = TRUE)


# Plotting
set.seed(13)
#font_import()
#loadfonts(quiet = T)

wine_words_correlation %>%
  filter(correlation > 0.80) %>% 
  graph_from_data_frame() %>% 
  ggraph(layout = "fr") + 
  geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) +
  geom_node_point(color = "#C3272B", size = 3) +
  geom_node_text(aes(label = name), repel = TRUE, color="darkgreen", size=4) +
  labs(title = "Co-occuring Words in Argentine Wines' Descriptions",
       subtitle = "among the 40 most common words\n",
       caption = "\nSource: Kaggle.com
       Visualization @gonzalofichero") +
  set_graph_style(family = "Century Schoolbook") +
  theme_void()+ 
  theme(text = element_text(family = "Century Schoolbook"),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        plot.margin = margin(0.25, 0.25, 0.25, 0.25, "in")) 
