```
# Text mining of Twitter data for PhD
# Collecting tweets
# Create token to access Twitter API:
library(rtweet)
token = create_token(app = "xxxxxxxxxxx",
                     consumer_key = "xxxxxxxxxxxxxxxxxxxx",
                     consumer_secret = "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx",
                     access_token = "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx",
                     access_secret = " xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx")
                     
# Search for tweets:
translation_search1 = search_tweets2(c("\"translation\"",
                                  "#xl8 OR #1tn"), n = 50000, retryonratelimit = TRUE)
                                  
# Combine all searches and remove duplicate tweets:
translation_search = rbind(translation_search1, translation_search2) %>%
	distinct()
  
# Text mining
# Load packages:
library(tidytext)
library(dplyr)
library(ggplot2)
library(tidyr)
library(tm)
library(igraph)
library(ggraph)
library(widyr)
library(tidyverse)
library(stringr)

# Create dataframe for text mining:
tweets_df = data.frame(translation_search$user_id, translation_search$screen_name, translation_search$created_at,
                    translation_search$text, stringsAsFactors = FALSE)
names(tweet_df) = c(“user_id”, “screen_name”, “created_at”, “tweet_text”)

# Clean text, split into words and count:
word_count = tweet_df %>% 
  mutate(tweet_text = qdapRegex::rm_url(text_df$tweet)) %>% 
  dplyr::select(tweet_text) %>% 
  unnest_tokens(word, tweet_text) %>% 
  anti_join(stop_words) %>% 
  count(word, sort = TRUE) %>%
  mutate(word = reorder(word, n))
  
# Plot wordcloud with top 75 words:
word_count %>%
	top_n(75) %>%
	wordcloud2()
# Remove digits, dominant words and plot:
rm_words = c("xl8", "translator", "1nt", "t9n", "language", "languages", "translators", "translation", 
            "amp", "internationaltranslationday", "de", "day", "translating", "translations", "interpreting", 
            "l10n", "interpreters", "interpreter")
word_count2 = filter(word_count, !word %in% rm_words) %>%
	str_replace_all(word, “[:digit:]”, “”) %>%
	top_n(75) 
wordcloud2(word_count2, size = 0.5)

# Clean text to plot word association graph
# Select tweet text, remove URLs, pair words:
translation_tw_paired = tweets_df %>% 
  mutate(tweet_text = qdapRegex::rm_url(tweet_text)) %>% 
  dplyr::select(tweet_text) %>% 
  unnest_tokens(bigram, tweet_text, token = "ngrams", n = 2)

# Split pairs and tidy:
translation_paired_split = translation_tw_paired %>% 
  separate(bigram, c("word1", "word2"), sep = " ")

translation_paired_tidy = translation_paired_split %>%
  filter(!word1 %in% stop_words$word) %>% 
  filter(!word2 %in% stop_words$word) %>% 
  filter(!word1 %in% rm_words) %>% 
  filter(!word2 %in% rm_words))

# Count to plot:
translation_paired_count = translation_paired_tidy %>% 
  count(word1, word2, sort = TRUE)

# Plot word association network:
translation_paired_count %>%
        filter(n >= 50) %>%
        graph_from_data_frame() %>%
        ggraph(layout = "fr") +
        geom_edge_link(aes(edge_alpha = n, edge_width = n)) +
        geom_node_point(color = "darkslategray4", size = 3) +
        geom_node_text(aes(label = name), vjust = 1.8, size = 3) +
        theme_void()

