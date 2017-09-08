library(rvest)
library(tidytext)
library(stringr)



urls <- c(
  "https://es.wikipedia.org/wiki/Ingenier%C3%ADa",
  "https://es.wikipedia.org/wiki/Matem%C3%A1ticas",
  "https://es.wikipedia.org/wiki/Ciencia",
  "https://es.wikipedia.org/wiki/Juego_de_tronos",
  "https://es.wikipedia.org/wiki/Historia_de_Grecia"
)

textos <- urls %>% 
  map(read_html) %>% 
  # sacando parrafos
  map(html_nodes, "p") %>%  
  map(html_text) %>%
  # desenlistar
  unlist()

textos <- stringi::stri_trans_general(textos, "Latin-ASCII")

df <- data_frame(x = textos)
df

df %>% 
  unnest_tokens(w, x) %>% 
  count(w, sort = TRUE) %>% 
  filter(n == 1) %>% 
  sample_n(10)

 unnest_tokens(df, sentence, x, token = "sentences")

 df2 <- unnest_tokens(df, bigram, x, token = "ngrams", n = 2)
 
dfd <-  df2 %>% 
   separate(bigram, into = c("actual", "siguiente"), sep = " ") %>% 
   count(actual, siguiente, sort = TRUE) %>% 
   group_by(actual) %>% 
   mutate(p = n/sum(n))


n <- 50
palabras <- rep(NA, n)
palabras

a <- "la"
palabras[1] <- a

for(i in 2:n) {
  
  dfd2 <- dfd %>% filter(actual == a) 
  
  palabras[i] <- sample(dfd2$siguiente, size = 1, prob = dfd2$p)
  
  a <- palabras[i]
  
}

palabras






# que pasa oe? ------------------------------------------------------------
textos <- c("S N N N N S S N S N S N  N S N S N S S N N N S N S S S S S S S S S S S S N N N N N N  N N N")
df <- data_frame(x = textos)
df

df2 <- unnest_tokens(df, bigram, x, token = "ngrams", n = 2)

dfd <-  df2 %>% 
  separate(bigram, into = c("actual", "siguiente"), sep = " ") %>% 
  count(actual, siguiente, sort = TRUE) %>% 
  group_by(actual) %>% 
  mutate(p = n/sum(n))

dfd %>% 
  select(-n) %>% 
  spread(siguiente, p)



df2 <- unnest_tokens(df, trigram, x, token = "ngrams", n = 3)

dfd <-  ""

dfd <- df2 %>%
  mutate(
    actual = str_extract(trigram, "\\w{1} \\w{1}"),
    siguiente = str_extract(trigram, "\\w{1}$")
  ) %>% 
  select(-trigram) %>% 
  count(actual, siguiente, sort = TRUE) %>% 
  group_by(actual) %>% 
  mutate(p = n/sum(n))

dfd %>% 
  select(-n) %>% 
  spread(siguiente, p)
