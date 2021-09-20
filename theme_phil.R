# create custom theme

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
