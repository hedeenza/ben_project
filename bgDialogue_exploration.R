# Loading Packages
library(tm)
library(RColorBrewer)
library(wordcloud)
library(wordcloud2)
library(tidyverse)
library(stringr)

# Loading data
bg3_lines <- read.csv("Data/bg3_companions_lines.csv", skip = 0)

names(bg3_lines)

# Creating a corpus from the tweets using tm
bg3_text <- bg3_lines$line

# Removing common twitter elements
bg3_removed <- 
  bg3_text |>
  str_remove_all("https\\S*") |>  
  str_remove_all("@\\S*") |>
  str_remove_all("amp") |>
  str_remove_all("[\r\n]") |>
  str_remove_all("[[:punct:]]")



# Creating the corpus
bg3_docs <- Corpus(VectorSource(bg3_removed))

# Cleaning the data
bg3_docs_2 <- bg3_docs |>
  tm_map(removeNumbers) |>
  tm_map(removePunctuation) |>
  tm_map(stripWhitespace) |>
  tm_map(content_transformer(tolower)) |>
  tm_map(removeWords, stopwords(kind = "en"))

?TermDocumentMatrix

# Creating a "Term Document Matrix"
# Don't panic that the matrix is GIANT
length(bg3_docs_2) # 32,606
round(length(bg3_docs_2) * 0.98, 0) # What appears in 98% of the text samples

dtm <- TermDocumentMatrix(bg3_docs_2, control = list(global = c(1, round(length(bg3_docs_2) * 0.90, 0)))) 
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
          scale = c(3.3,0.4))

# Sentiment Analysis
library(syuzhet)
bg3_lines_sentiment <- iconv(bg3_text)
bg3_sentiment_scores <- get_nrc_sentiment(bg3_lines_sentiment)

glimpse(bg3_sentiment_scores)

# Tidying the Sentiment information for graphing
tidy_sentiment <- 
  colSums(bg3_sentiment_scores) |> 
  as.data.frame() |>
  rename("Score" = 1) |>
  mutate(Sentiment = c("anger", "anticipation", "disgust", "fear", "joy", "sadness" ,"surprise", "trust", "negative", "positive"))

tidy_sentiment

# Graphing the sentiment data
bg3_sentiment_plot <- 
  tidy_sentiment |>
  mutate(Sentiment = fct_reorder(Sentiment, desc(Score))) |>
  ggplot(aes(x = Sentiment, y = Score, fill = Sentiment)) +
  geom_col() + 
  labs(title = "Baldur's Gate 3 Sentiment Score",
       subtitle = "Companion Dialogue: \nShadowheart, Gale, Astarion, Karlach, Lae'zel, Minthara, \nHalsin, Minsc, Jaheira, Wyll") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        legend.position = "none")

bg3_sentiment_plot

ggsave(plot = bg3_sentiment_plot, "Graphs/BG3_CompanionLines_Sentiment_Score.png", width = 12, height = 16, units = "cm", dpi = 300)



