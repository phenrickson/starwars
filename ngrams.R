# load packages
library(tidyverse)
library(tidytext)
library(ggthemes)
library(textdata)
library(sentimentr)
library(ggrepel)
library(ggforce)
library(plotly)

# load and set ggplot theme
source("src/functions.R")

theme_set(
        theme_phil()+
                theme(
                        strip.text.x = element_text(size = 12)
                )
)

# tokenize and remove stopwords
load_stopwords()

stop_words = stop_words |>
        filter(lexicon == 'snowball')

# read in starwars text
starwars =
        load_data() |>
        clean_data()

# tokenize into ngrams
starwars_ngrams = 
        starwars |>
        unnest_tokens(ngram, dialogue, token = "ngrams", n = 2) |>
        separate(ngram, into = c("word1", "word2"), sep = " ") |>
        filter(if_any(starts_with("word"), ~ !is.na(.x))) |>
        filter(!word1 %in% stop_words$word) |>
        filter(!word2 %in% stop_words$word) 

# count
ngram_counts <-
        starwars_ngrams |>
        count(word1, word2, sort = TRUE)


# for making viz
library(igraph)
library(ggraph)

## calculate bigramsram
set.seed(1)
ngram_graph <-
        ngram_counts |>
        slice_max(order_by = n, n=200, with_ties = F) |>
        graph_from_data_frame()

ngram_graph |>
        ggraph(layout = "fr") +
        geom_edge_link() +
        geom_node_point() +
        geom_node_text(aes(label = name),
                       size = 3,
                       vjust = 1.5,
                       hjust = 1)+
        theme_void()