# load packages
library(tidyverse)
library(tidytext)
library(ggthemes)
library(textdata)
library(sentimentr)
library(ggrepel)

# load and set ggplot theme
source('theme-phil.R')
theme_set(
        theme_phil()+
                theme(strip.text.x = element_text(size = 12)
                )
)

# functions
# load data
load_data = function(file = "data/starwars_text.csv", ...) {
        
        read_csv(file)
        
}

# function to clean up some pieces with the data
clean_data = function(data) {
        
        data |> 
                mutate(episode = case_when(document == 'a new hope' ~ 'iv',
                                           document == 'the empire strikes back' ~ 'v',
                                           document == 'return of the jedi' ~ 'vi')) |>
                mutate(character = case_when(character == 'BERU' ~ 'AUNT BERU',
                                             TRUE ~ character)) |>
                select(episode, everything())
}

# function to tokenize
get_tokens = function(data, ...) {
        
        data |>
                unnest_tokens(
                        word,
                        dialogue,
                        ...)
}

# strip stopwords
remove_stopwords = function(data, 
                            stopwords,
                            by = "word") {
        
        data |>
                anti_join(stopwords,
                          by = by)
        
}

# add sentiment
add_sentiment = function(data,
                         by = c('document', 'character', 'line_number')) {
        
        data |>
        sentiment_by(by = by)

}

# tokenize and remove stopwords
data("stop_words")

# read in starwars text
starwars =
        load_data() |>
        clean_data()

# use sentimentr
starwars_sentiment = 
        starwars |>
        get_sentences() |> 
        add_sentiment() |>
        as_tibble()

# sentiment by movie line
starwars_sentiment |>
        filter(document == 'a new hope') |>
        group_by(document,  index = line_number %/% 8) |>
        summarize(sentiment = sum(ave_sentiment)) |>
        ggplot(aes(x=index, y=sentiment, fill = sentiment))+
        geom_col()+
        scale_fill_gradient2_tableau(oob = scales::squish)+
        guides(fill = guide_colourbar(title = "sentiment",
                                      title.position = "top",
                                      barwidth=8,
                                      barheight=0.5))+
        ylab("character")+
        xlab("sentiment score")+
        facet_wrap(document~.)

# sentiment by character
starwars_sentiment |>
        filter(document == 'a new hope')  |>
        group_by(document, character) |>
        summarize(sentiment = sum(ave_sentiment),
                  word_count = sum(word_count)) |>
        slice_max(n = 40, order_by = word_count) |>
        ggplot(aes(y=reorder(character, sentiment), x=sentiment, fill = sentiment))+
        geom_col()+
        scale_fill_gradient2_tableau(oob = scales::squish)+
        guides(fill = guide_colourbar(title = "sentiment",
                                      title.position = "top",
                                      barwidth=8,
                                      barheight=0.5))+
        ylab("character")+
        xlab("sentiment score")+
        facet_wrap(document~.)

# why is this so different?
# plot running sentiment calculation for luke
starwars %>%
        filter(document == 'a new hope') |>
        filter(character== 'LUKE') |>
        get_sentences() |>
        add_sentiment(by = c('document', 'character', 'line_number', 'dialogue')) |>
        arrange(line_number) |>
        mutate(row_number = row_number()) |>
        mutate(dialogue = as.character(dialogue)) |>
        mutate(show= case_when(abs(ave_sentiment) > .35 ~ dialogue,
                               TRUE ~ "")) |>
        mutate(run_sentiment = cumsum(ave_sentiment)) |>
        ggplot(aes(x=row_number, y=run_sentiment, color = ave_sentiment, label = show)) +
        geom_line()+
        geom_label_repel(size=2.5, max.overlaps=30)+
        facet_wrap(document+character~.)+
        xlab("line_number")+
        ylab("running total of sentiment")+
        guides(color = 'none')+
        scale_color_gradient2_tableau(oob = scales::squish)