library(tidyverse)
library(tidytext)
library(readr)
war_and_peace <- read_csv("~/Desktop/war-and-peace.txt", 
                          col_names = FALSE)

#total words
war_and_peace %>% 
  unnest_tokens(word, X1) %>% 
count()
#387,604

war_and_peace %>% 
  unnest_tokens(word, X1) %>% 
  anti_join(stop_words) -> wp_clean

wp_clean %>% 
  count(word, sort = TRUE)

wp_clean %>% 
  inner_join(get_sentiments('afinn')) %>% 
  count(word, value, sort = TRUE) %>% 
  head(20) %>% 
  ggplot(aes(reorder(word, n),n, fill = value)) + 
  geom_col() +
  coord_flip()

wp_clean %>% 
  inner_join(get_sentiments('bing')) %>% 
  count(word, sentiment, sort = TRUE) %>% 
  head(40) %>% 
  ggplot(aes(reorder(word, n),n, fill = sentiment)) + 
  geom_col() +
  facet_wrap(~sentiment, scales = "free_y") +
  coord_flip()

library(wordcloud2)

#positive words
wp_clean %>% 
  inner_join(get_sentiments('bing')) %>% 
  filter(sentiment %in% "positive") %>% 
  # count(): 8813
  count(word, sort = TRUE) %>% 
  wordcloud2()

#negative words
wp_clean %>% 
  inner_join(get_sentiments('bing')) %>% 
  filter(sentiment %in% "negative") %>% 
  count(word, sort = TRUE) %>% 
  #count(): 1,678
  wordcloud2()

#hmmmm...
wp_clean %>% 
  inner_join(get_sentiments('afinn')) %>% 
  count(word, value, sort = TRUE) %>% 
  spread(value, n, fill = 0) 
#n-grams
war_and_peace %>% 
  unnest_tokens(bigram, X1, token = "ngrams", n = 2) %>% 
  separate(bigram, c("word1", "word2"), sep = " ") %>% 
filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>% 
  count(word1, word2, sort = TRUE) -> wp_grams


wp_grams %>% 
  filter(word2 %in% "smile")
