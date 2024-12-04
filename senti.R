devtools::install_github("sillasgonzaga/lexiconPT")
# Buscando os sentimentos do pacote lexiconPT
sentiments_pt <- lexiconPT::oplexicon_v2.1 %>% 
  mutate(word = term) %>% 
  select(word, polarity)  
# Juntando os sentimentos com as palavras presentes nas músicas
add_sentiments <- letras %>% 
  group_by_all() %>% 
  slice(1) %>% 
  ungroup() %>% 
  unnest_tokens(word, letra_musica) %>% 
  #dplyr::anti_join(my_stopwords, by = c("word" = "word")) %>% 
  dplyr::inner_join(sentiments_pt, by = c("word" = "word"))

add_sentiments <- add_sentiments |>
  subset(!(word %in% my_stopwords))


add_sentiments %>% 
  group_by(polarity) %>% 
  count(word) %>% 
  filter(n < quantile(n, 0.999)) %>% 
  top_n(n = 15) %>% 
  ggplot(aes(reorder(word, n), n)) +
  geom_linerange(aes(ymin = min(n), ymax = n, x = reorder(word, n)),
                 position = position_dodge(width = 0.2), size = 1, 
                 colour = 'darksalmon') + 
  geom_point(colour = 'dodgerblue4', size = 3, alpha = 0.9) +
  facet_wrap(~polarity, scales = "free") +
  coord_flip() +
  labs(x = 'Top 15 palavras mais comuns', 
       y = 'Contagens', title = "Sentimentos") +
  theme_bw(14)



summ <- add_sentiments %>% 
  group_by(nome_musica) %>% 
  summarise(mean_pol = mean(polarity)) 
# 15 músicas mais negativas e mais positivas
summ %>% 
  arrange(desc(mean_pol)) %>% 
  slice(c(1:15, 121:135)) %>% 
  mutate(situation = rep(c('+positivas', '+negativas'), each = 15)) %>% 
  ggplot(aes(reorder(nome_musica, mean_pol), mean_pol)) +
  geom_linerange(aes(ymin = min(mean_pol), ymax = mean_pol, 
                     x = reorder(nome_musica, mean_pol)),
                 position = position_dodge(width = 0.2), size = 1, 
                 colour = 'darksalmon') + 
  geom_point(colour = 'dodgerblue4', size = 3, alpha = 0.9) +
  facet_wrap(~situation, scales = "free") +
  coord_flip() +
  labs(x = 'Músicas', y = 'Polaridades') +
  theme_bw(14)