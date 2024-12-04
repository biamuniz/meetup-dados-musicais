library(stopwords)
library(tm)
library(tidytext)
library(wordcloud2)

count_words <- letras %>% 
  unnest_tokens(word, letra_musica) %>% 
  count(word) %>% 
  arrange(desc(n)) %>% 
  slice(1:30)

# Shows the most common words in songs from The Beatles


count_words2 <- count_words %>% 
  arrange(desc(n)) %>% 
  slice(1:30)

count_words2 %>% 
  ggplot(aes(word, n)) +
  geom_bar(stat = "identity", fill = "pink", colour = "black", 
           alpha = 0.7) +
  coord_flip() +
  theme_bw()


my_stopwords <- c(stopwords("pt"))

count_words <- count_words |>
  subset(!(word %in% my_stopwords))