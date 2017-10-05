rm(list = ls())
library(tidyverse)
library(tidytext)
library(rvest)

# tidytext::get_sentiments()
# 
# get_sentiments(lexicon = c("afinn", "bing", "nrc", "loughran")[1]) %>% count(score)
# get_sentiments(lexicon = c("afinn", "bing", "nrc", "loughran")[2]) %>% count(sentiment)
# get_sentiments(lexicon = c("afinn", "bing", "nrc", "loughran")[3]) %>% count(sentiment)
# get_sentiments(lexicon = c("afinn", "bing", "nrc", "loughran")[4]) %>% count(sentiment)
# 

get_links_from_uni <- function(uni){
  
  message(uni)
  # file.path("asda", "asdasdasdasd")
  url <- read_html(file.path("https://www.reclamos.cl/empresa", uni))
  links <- url %>% 
    html_nodes("tbody") %>% 
    html_nodes("a") %>% 
    html_attr("href")
  return(links)
}


get_text_from_links <- function(links) {
  
  map(links, get_text_from_link)
  
}

get_text_from_link <- function(link) {
  
  message(link)
  
  text <- read_html(link) %>% 
    html_nodes("#main > div:nth-child(2) > div > div.node-content > span:nth-child(2)") %>% 
    html_text()
  
  return(text)
}


unis <- c("educacinycapacitacin/jardininfantil/vitamina",
          "universidad_santo_tom_s",
          "duoc", "inacap",
          "universidad_de_concepci_n",
          "universidad_del_mar",
          "universidad_de_playa_ancha",
          "instituto_profesional_los_leones")


# get_links_from_uni("universidad_adolfo_iba_ez")
# get_text_from_link("https://www.reclamos.cl/reclamo/universidad_adolfo_ib_ez_sin_t_tulo_por_gimnasio")

df <- data_frame(
uni = unis  
)

df

df <- df %>%
  mutate(links = map(uni, get_links_from_uni))


df <- df %>% 
  mutate(texts = map(links, get_text_from_links))

df

str(df$texts)

df <- df %>% 
  mutate(texts = map(texts, unlist),
         texts = map(texts, paste, collapse = " "),
         texts = unlist(texts))
df <- df %>% 
  mutate(texts = tolower(texts))


uni_words <- df %>%
  select(-links) %>% 
  unnest_tokens(word, texts) %>%
  count(uni, word, sort = TRUE) %>%
  ungroup()

uni_words <- uni_words %>%
  bind_tf_idf(word, uni, n) 

uni_words

plot_unis <- uni_words %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word))))

plot_unis %>%
  top_n(20) %>%
  ggplot(aes(word, tf_idf, fill = uni)) +
  geom_col() +
  labs(x = NULL, y = "tf-idf") +
  coord_flip()


plot_unis %>% 
  group_by(uni) %>% 
  top_n(15, tf_idf) %>% 
  ungroup() %>%
  mutate(word = reorder(word, tf_idf)) %>%
  ggplot(aes(word, tf_idf, fill = uni)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~uni, ncol = 2, scales = "free") +
  coord_flip()
