# Carregando bibliotecas necessárias
library(httr2)       # Realiza requisições HTTP de forma robusta.
library(rvest)       # Extrai informações de páginas HTML para web scraping.
library(xml2)        # Manipula e analisa documentos HTML/XML.
library(purrr)       # Programação funcional para manipulação de listas e vetores.
library(glue)        # Cria strings interpoladas para formatação dinâmica.
library(dplyr)       # Manipulação de dados com verbos intuitivos (filter, mutate, etc.).
library(tidyr)       # Reorganiza e arruma dados em formato tidy.
library(stringr)     # Manipula strings com funções consistentes e expressões regulares.
library(pbapply)     # Aplica funções em paralelo com barra de progresso.
library(future)      # Configura e gerencia processamento paralelo.
library(tidytext)    # Processa e analisa textos em formato tidy.
library(stopwords)   # Fornece listas de stopwords para remoção em análises de texto.
library(tm)          # Ferramentas de processamento e manipulação de textos.
library(wordcloud2)  # Cria nuvens de palavras para visualização gráfica.
library(ggplot2)     # Cria gráficos estáticos e personalizáveis de alta qualidade.
library(lexiconPT)   # Fornece léxicos para análise de sentimentos em português.


# 1. Extração da lista de músicas
artistas <- tibble(
  url_artista = "https://www.vagalume.com.br/alcione",
  nome_artista = "Alcione"
)

# Função para coletar as músicas de cada artista
discografia <- function(url) {
  musLista <- url %>%
    read_html() %>%
    html_elements("#alfabetMusicList a")
  musLinks <- musLista %>% html_attr("href")
  musNomes <- musLista %>% html_text()
  
  musica <- tibble(
    url_musica = musLinks,
    nome_musica = musNomes
  ) %>%
    filter(!str_detect(url_musica, "#play$")) %>%
    mutate(url_musica = glue("https://www.vagalume.com.br{url_musica}"))
  
  Sys.sleep(1)
  return(musica)
}

discografia <- possibly(discografia)

musLista <- pblapply(
  artistas$url_artista, discografia, cl = future::availableCores() - 1
)

musicas <- artistas %>%
  mutate(data = musLista) %>%
  unnest(cols = data)

# 2. Extração dos dados das músicas
album <- function(url) {
  pagina <- url %>% read_html()
  
  letra <- pagina %>%
    html_element("#lyrics") %>%
    html_text2()
  
  estilo <- pagina %>%
    html_elements(".h14 a") %>%
    html_text()
  
  letra <- tibble(
    letra_musica = letra,
    estilo_musica = list(estilo)
  )
  
  Sys.sleep(0.5)
  return(letra)
}

album <- possibly(album)

letLista <- pblapply(
  musicas$url_musica, album, cl = future::availableCores() - 1
)

letras <- musicas %>%
  mutate(data = letLista) %>%
  unnest(cols = data)

# Salvando os dados extraídos
saveRDS(letras, "dados/letras-alcione.RDS")

# 3. Análise de frequência de palavras
count_words <- letras %>%
  unnest_tokens(word, letra_musica) %>%
  count(word) %>%
  arrange(desc(n)) %>%
  slice(1:30)

count_words %>%
  ggplot(aes(x = reorder(word, n), y = n)) +
  geom_bar(stat = "identity", fill = "pink", colour = "black", alpha = 0.7) +
  coord_flip() +
  labs(x = "Palavras", y = "Frequência", title = "Frequência das Palavras") +
  theme_bw()


# Filtrando palavras comuns (stopwords)
my_stopwords <- c(stopwords("pt"))
count_words <- count_words %>%
  filter(!word %in% my_stopwords)

# 4. Análise de bigramas
letras %>%
  select(letra_musica) %>%
  unnest_tokens(bigram, letra_musica, token = "ngrams", n = 2) %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% my_stopwords, !word2 %in% my_stopwords) %>%
  count(word1, word2, sort = TRUE) %>%
  mutate(word = paste(word1, word2)) %>%
  slice(1:30) %>%
  ggplot(aes(reorder(word, n), n)) +
  geom_linerange(aes(ymin = min(n), ymax = n), size = 1, colour = 'darksalmon') +
  geom_point(colour = 'dodgerblue4', size = 3, alpha = 0.9) +
  coord_flip() +
  labs(x = "Top 30 2-grams mais comuns", y = "Contagem") +
  theme_bw()

# Filtra as linhas que possuem o bigrama "chumbo trocado" na coluna `letra_musica`
exemplo_bigrama <- letras %>%
  filter(str_detect(letra_musica, "samba"))

# Printando as linhas correspondentes
exemplo_bigrama


# 5. Análise de sentimentos
sentiments_pt <- oplexicon_v2.1 %>%
  mutate(word = term) %>%
  select(word, polarity)

add_sentiments <- letras %>%
  unnest_tokens(word, letra_musica) %>%
  inner_join(sentiments_pt, by = "word") %>%
  filter(!word %in% my_stopwords)

add_sentiments %>%
  group_by(polarity) %>%
  count(word) %>%
  top_n(15) %>%
  ggplot(aes(reorder(word, n), n)) +
  geom_linerange(aes(ymin = min(n), ymax = n), size = 1, colour = 'darksalmon') +
  geom_point(colour = 'dodgerblue4', size = 3, alpha = 0.9) +
  facet_wrap(~polarity, scales = "free") +
  coord_flip() +
  labs(x = "Top 15 palavras mais comuns", y = "Contagens", title = "Sentimentos") +
  theme_bw()

# 6. Identificando músicas mais positivas e negativas
summ %>%
  arrange(desc(mean_pol)) %>%
  slice_head(n = 15) %>%                             # Seleciona as 15 mais positivas
  mutate(situation = "Positivas") %>%
  bind_rows(
    summ %>%
      arrange(mean_pol) %>%
      slice_head(n = 15) %>%                         # Seleciona as 15 mais negativas
      mutate(situation = "Negativas")
  ) %>%
  ggplot(aes(reorder(nome_musica, mean_pol), mean_pol)) +
  geom_linerange(aes(ymin = min(mean_pol), ymax = mean_pol), size = 1, colour = 'darksalmon') +
  geom_point(colour = 'dodgerblue4', size = 3, alpha = 0.9) +
  facet_wrap(~situation, scales = "free") +
  coord_flip() +
  labs(x = "Músicas", y = "Polaridades") +
  theme_bw()
