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

# read in starwars text
starwars =
        load_data() |>
        clean_data()

# use sentimentr
starwars_sentiment = 
        starwars |>
        get_sentences()  |>
        add_sentiment()

# plot sentiment for all characters
starwars_sentiment |>
        filter(document == 'return of the jedi') |>
        filter(character == 'EMPEROR') |>
        plot_character_lines_with_sentiment()

# plot sentiment for character by line
starwars_sentiment|>
        filter(document == 'a new hope') |>
        filter(character == 'LUKE') |>
        plot_character_lines_with_sentiment()

# overall sentiment by dialogue
starwars_sentiment |>
        plot_sentiment_by_line(
                combine_lines = 4,
                plotly = T
        )

# plot average sentiment by character
starwars |>
        plot_avg_sentiment(
                top_n = 25,
                scales = "free_y"
        )