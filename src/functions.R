# functions

# visualization
theme_phil <- function () { 
        
        theme_fivethirtyeight() %+replace%
                theme(
                        axis.title = element_text(),
                        # strip.background = element_rect(fill="grey100"),
                        #strip.text = element_text(colour = 'black'),
                        #  strip.text.x = element_text(size = 7),
                        #  strip.text.y = element_text(size = 7),
                        legend.position = "top",
                        legend.title = element_blank(),
                        panel.grid.minor = element_blank(),
                        panel.spacing = unit(6.5, "mm"),
                        strip.text.y = element_text(family = "sans", colour = "#3C3C3C", size = 8),
                        strip.text.x = element_text(family = "sans", colour = "#3C3C3C", size = 8),
                        #    strip.background = element_rect(fill="grey80"),
                        panel.grid = element_line(colour = "grey80")
                        
                        # remove spacing between facets
                        
                        
                )
        
}

# load data
load_data = function(file = "data/starwars_text.csv",
                     show_col_types = F,
                     ...) {
        
        read_csv(file,
                 show_col_types = show_col_types,
                 ...)
        
}

load_stopwords = function() {
        
        data("stop_words")
}

# function to clean up some pieces with the data
clean_data = function(data) {
        
        data |> 
                mutate(episode = case_when(document == 'a new hope' ~ 'iv',
                                           document == 'the empire strikes back' ~ 'v',
                                           document == 'return of the jedi' ~ 'vi')) |>
                mutate(character = case_when(character == 'BERU' ~ 'AUNT BERU',
                                             character == 'LURE' ~ 'LUKE',                                            
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
                         by = c('episode', 'document', 'character', 'line_number')) {
        
        data |>
                sentiment_by(by = by) |>
                uncombine()
        
}

# plots
plot_character_lines_with_sentiment = function(data,
                                               character,
                                               cutoff = .35,
                                               ncol = 2) {
        
        data |>
                group_by(episode, document, character) |>
                arrange(line_number) |>
                mutate(row_number = row_number()) |>
                mutate(dialogue = as.character(dialogue)) |>
                mutate(show= case_when(abs(sentiment) > cutoff ~ dialogue,
                                       TRUE ~ "")) |>
                mutate(run_sentiment = cumsum(sentiment)) |>
                ggplot(aes(x=row_number, y=run_sentiment, color = sentiment, label = show)) +
                geom_step()+
                #  geom_point(size = 0.5) +
                ggrepel::geom_label_repel(size=2.5, max.overlaps=30)+
                facet_wrap(episode + document+character~.,
                           ncol = ncol)+
                xlab("line_number")+
                ylab("running total of sentiment")+
                guides(color = 'none')+
                scale_color_gradient2_tableau(oob = scales::squish)
        
}

plot_avg_sentiment = function(data,
                              by = c("episode", "document", "character"),
                              top_n = 30, 
                              ncol = 2,
                              scales = "free_x") {
        
        data |>
                get_sentences() |>
                sentiment_by(by = by) |>
                group_by(episode, document) |>
                slice_max(order_by = word_count,
                          n = top_n) |>
                ggplot(aes(x=reorder_within(character, ave_sentiment, document),
                           y=ave_sentiment,
                           color = ave_sentiment,
                           ymin = ave_sentiment - 1.96 * sd,
                           ymax = ave_sentiment + 1.96*sd))+
                geom_point(aes(size = word_count))+
                geom_pointrange()+
                coord_flip()+
                facet_wrap(episode +document ~., ncol = ncol,
                           scales = scales)+
                scale_color_gradient2_tableau(oob = scales::squish)+
                guides(color = 'none')+
                xlab('character')+
                theme(panel.grid.major = element_blank(),
                      legend.title = element_text(size = 10))+
                geom_hline(yintercept = 0, linetype = 'dotted')+
                scale_x_reordered()
        
}


plot_sentiment_by_line = function(data,
                                  combine_lines =2,
                                  plotly = F) {
        
        p = 
                data |>
                group_by(episode, document, index = line_number %/% combine_lines) |>
                summarize(sentiment = mean(sentiment),
                          dialogue = paste(dialogue, collapse = '\n'),
                          .groups = 'drop') |>
                ggplot(
                        aes(
                                x=index, 
                                y=sentiment,
                                color = sentiment,
                                text =
                                        paste('\n Average Sentiment: ', round(sentiment, 3),
                                              '\n Index: ', index,
                                              '\n',
                                              'Dialogue: \n',
                                              dialogue)
                        )
                )+
                geom_point()+
                scale_color_gradient2_tableau(oob = scales::squish)+
                xlab("dialogue")+
                ylab("sentiment score")+
                facet_wrap(episode + document~.,
                           ncol = 1,
                           scales = "free_x")+
                geom_hline(yintercept = 0, linetype = 'dotted')
        
        if (plotly == T) {
                suppressMessages({
                        ggplotly(p,
                                 tooltip = 'text')
                })
        }
        
        else {
                p +
                        guides(color = guide_colourbar(title = "sentiment",
                                                      title.position = "top",
                                                      barwidth=8,
                                                      barheight=0.5))
        }
                
}
