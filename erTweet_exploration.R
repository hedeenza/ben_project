# Loading Packages

install.packages(c("tm","RColorBrewer","wordcloud","wordcloud2"))
library(tm)
library(RColorBrewer)
library(wordcloud)
library(wordcloud2)
library(tidyverse)
library(stringr)

# Loading data
er_tweets <- read.csv("Data/elden_ring_tweets.csv", skip = 1)

# Creating a corpus from the tweets using tm
er_text <- er_tweets$text

er_removed <- 
  er_text |>
  str_remove_all("https\\S*") |>  
  str_remove_all("@\\S*") |>
  str_remove_all("amp") |>
  str_remove_all("[\r\n]") |>
  str_remove_all("[[:punct:]]")

er_docs <- Corpus(VectorSource(er_removed))

# Cleaning the data
er_docs_2 <- er_docs |>
  tm_map(removeNumbers) |>
  tm_map(removePunctuation) |>
  tm_map(stripWhitespace) |>
  tm_map(content_transformer(tolower)) |>
  tm_map(removeWords, stopwords(kind = "en"))

# Creating a "Term Document Matrix"
  # Don't panic that the matrix is GIANT
dtm <- TermDocumentMatrix(er_docs_2) 
matrix <- as.matrix(dtm) 
words <- sort(rowSums(matrix),decreasing=TRUE) 
df <- data.frame(word = names(words),freq=words)


# Create the word cloud
wordcloud(words = df$word, 
          freq = df$freq, 
          min.freq = 1,
          max.words=100, 
          random.order=F, 
          rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"),
          scale = c(3.5,0.7))



# Sentiment Analysis
library(syuzhet)
er_tweet_sentiment <- iconv(er_text)
er_sentiment_scores <- get_nrc_sentiment(er_tweet_sentiment)

# Tidying the Sentiment information for graphing
tidy_sentiment_er <- 
  colSums(er_sentiment_scores) |> 
  as.data.frame() |>
  rename("Score" = 1) |>
  mutate(Sentiment = c("anger", "anticipation", "disgust", "fear", "joy", "sadness" ,"surprise", "trust", "negative", "positive"),
         Percent = (Score/60003) * 100)

tidy_sentiment_er

# Graphing the sentiment data
er_sentiment_plot <- 
  tidy_sentiment_er |>
  mutate(Sentiment = fct_reorder(Sentiment, desc(Score))) |>
  ggplot(aes(x = Sentiment, y = Score, fill = Sentiment)) +
  geom_col() + 
  labs(title = "Elden Ring Tweet Sentiment Analysis",
       subtitle = "From ~30,000 Tweets") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        legend.position = "none")

er_sentiment_plot

ggsave(plot = er_sentiment_plot, "Graphs/EldenRing_Sentiment_Score.png", width = 12, height = 16, units = "cm", dpi = 300)
