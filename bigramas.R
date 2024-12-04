letras %>% 
  select(letra_musica) %>% 
  unnest_tokens(bigram, letra_musica, token = "ngrams", n = 2) %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% my_stopwords, 
         !is.na(word1), !is.na(word2),
         !word2 %in% my_stopwords) %>%
  count(word1, word2, sort = TRUE) %>% 
  mutate(word = paste(word1, word2)) %>% 
  filter(n < quantile(n, 0.999)) %>% 
  arrange(desc(n)) %>% 
  slice(1:30) %>%  
  ggplot(aes(reorder(word, n), n)) +
  geom_linerange(aes(ymin = min(n), ymax = n, x = reorder(word, n)),
                 position = position_dodge(width = 0.2), size = 1, 
                 colour = 'darksalmon') + 
  geom_point(colour = 'dodgerblue4', size = 3, alpha = 0.9) +
  coord_flip() +
  labs(x = 'Top 30 2-grams mais comuns', y = 'Contagem') +
  theme_bw(14)