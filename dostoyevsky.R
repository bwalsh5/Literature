library(tidyverse)
install.packages('gutenbergr')
# load packages
library(gutenbergr)
gutenberg_works(author == "Dostoyevsky, Fyodor") %>%  View()

library(tidyverse)
crime <-gutenberg_download(2554, mirror = "http://mirrors.xmission.com/gutenberg/") %>% mutate('title' = "Crime & Punishment")
brothers <-gutenberg_download(28054, mirror = "http://mirrors.xmission.com/gutenberg/") %>% mutate('title' = "The Brothers Karamazov")
notes <-gutenberg_download(600, mirror = "http://mirrors.xmission.com/gutenberg/") %>% mutate('title' = "Notes from the Underground")
idiot <-gutenberg_download(2638, mirror = "http://mirrors.xmission.com/gutenberg/") %>% mutate('title' = "The Idiot")
possessed <-gutenberg_download(8117, mirror = "http://mirrors.xmission.com/gutenberg/") %>% mutate('title' = "The Possessed")
gambler <-gutenberg_download(2197, mirror = "http://mirrors.xmission.com/gutenberg/") %>% mutate('title' = "The Gambler")
poor <-gutenberg_download(2302, mirror = "http://mirrors.xmission.com/gutenberg/") %>% mutate('title' = "Poor Folk")
white <-gutenberg_download(36034, mirror = "http://mirrors.xmission.com/gutenberg/") %>% mutate('title' = "White Nights and Other Stories")
house <-gutenberg_download(37536, mirror = "http://mirrors.xmission.com/gutenberg/") %>% mutate('title' = "The House of the Dead")
uncle <-gutenberg_download(38241, mirror = "http://mirrors.xmission.com/gutenberg/") %>% mutate('title' = "Uncle's Dream; and The Permanent Husband")
short <-gutenberg_download(40745, mirror = "http://mirrors.xmission.com/gutenberg/") %>% mutate('title' = "Short Stories")
grand <-gutenberg_download(8578, mirror = "http://mirrors.xmission.com/gutenberg/") %>% mutate('title' = "The Grand Inquisitor")
stavrogin <-gutenberg_download(57050, mirror = "http://mirrors.xmission.com/gutenberg/") %>% mutate('title' = "Stavrogin's Confession and The Plan of The Life of a Great Sinner")


dostoyevsky <- rbind(crime,brothers, notes, idiot, possessed, gambler, poor, white, house, uncle, short, grand, stavrogin)

library(tidytext)

dostoyevsky %>% 
  unnest_tokens(word, text) -> d_words

d_words %>% 
  group_by(title) %>% 
  count(word, sort = TRUE) %>% 
  head(20) %>% 
  knitr::kable()

d_words %>% 
  anti_join(stop_words) %>% 
  count(word, sort = TRUE)

dostoyevsky %>% 
  unnest_tokens(bigram, text, token="ngrams", n=2) %>% 
  separate(bigram, c("word1", "word2"), sep = " ") %>% 
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>% 
  filter(!word1 %in% NA) %>% 
  filter(!word2 %in% NA) %>%
  count(word1, word2, sort = TRUE) -> d_bigrams
  
d_bigrams %>%  
  filter(word2 == "fellow") 

d_bigrams %>% 
  head(40) %>% 
  View()


d_words %>% 
  anti_join(stop_words) %>% 
  inner_join(get_sentiments("bing")) %>% 
  group_by(sentiment) %>% 
  count()

d_words %>% 
  anti_join(stop_words) %>% 
  inner_join(get_sentiments("afinn")) %>% 
  group_by(value) %>% 
  count(word, sort=TRUE)

d_words %>% 
  anti_join(stop_words) %>% 
  inner_join(get_sentiments("afinn")) %>% 
  group_by(title) %>% 
  #count(word, value, sort=TRUE) %>% 
  summarize(average = mean(value)) %>% 
  ggplot(aes(reorder(title, -average), average, fill=average)) + geom_col() + coord_flip()

d_words %>% 
  anti_join(stop_words) %>% 
  inner_join(get_sentiments("bing")) %>% 
  group_by(title) %>% 
  count(sentiment, sort=TRUE)

library(textdata)
d_words %>% 
  anti_join(stop_words) %>% 
  inner_join(get_sentiments("nrc")) %>% 
  # group_by(title) %>% 
  count(sentiment,  sort=TRUE)


library(wordcloud2)

d_words %>% 
  anti_join(stop_words) %>% 
  count(word, sort = TRUE) %>% 
  wordcloud2()

#books

book_words <- dostoyevsky %>%
  unnest_tokens(word, text) %>%
  count(title, word, sort = TRUE)


total_words <- book_words %>% 
  group_by(title) %>% 
  summarize(total = sum(n))

book_words <- left_join(book_words, total_words)

View(book_words)

ggplot(book_words, aes(n/total, fill = title)) +
  geom_histogram(show.legend = FALSE) +
  xlim(NA, 0.0009) +
  facet_wrap(~title, ncol = 2, scales = "free_y")

freq_by_rank <- book_words %>% 
  group_by(title) %>% 
  mutate(rank = row_number(), 
         `term frequency` = n/total) %>%
  ungroup()

freq_by_rank %>% 
  ggplot(aes(rank, `term frequency`, color = title)) + 
  geom_line(size = 1.1, alpha = 0.8, show.legend = FALSE) + 
  scale_x_log10() +
  scale_y_log10()

rank_subset <- freq_by_rank %>% 
  filter(rank < 500,
         rank > 10)

lm(log10(`term frequency`) ~ log10(rank), data = rank_subset)

freq_by_rank %>% 
  ggplot(aes(rank, `term frequency`, color = title)) + 
  geom_abline(intercept = -0.6678, slope = -1.0982, 
              color = "gray50", linetype = 2) +
  geom_line(size = 1.1, alpha = 0.8, show.legend = FALSE) + 
  scale_x_log10() +
  scale_y_log10()

book_tf_idf <- book_words %>%
  bind_tf_idf(word, title, n)

book_tf_idf %>%
  select(-total) %>%
  arrange(desc(tf_idf))

library(forcats)

book_tf_idf %>%
  group_by(title) %>%
  slice_max(tf_idf, n = 15) %>%
  ungroup() %>%
  ggplot(aes(tf_idf, fct_reorder(word, tf_idf), fill = title)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~title, ncol = 2, scales = "free") +
  labs(x = "tf-idf", y = NULL)
