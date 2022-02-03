library(tidyverse)
library(tidytext)
library(readr)
frank <- read_csv("~/Desktop/frank.txt")

colnames(frank)[1] <- "text"

frank %>% 
  unnest_tokens(word, text) -> frank_words

frank_words %>% 
count()

frank_words %>% 
  anti_join(stop_words) %>% 
  count(word) %>% 
  arrange(desc(n))

# sentiment

install.packages('textdata')
library(textdata)
get_sentiments("afinn")
# bing, nrc, afinn

# rbind, colnames, anti_join inner_join
frank_words %>% 
  anti_join(stop_words) %>% 
  count(word) %>% 
  inner_join(get_sentiments("afinn")) %>% 
  arrange(desc(n)) 

