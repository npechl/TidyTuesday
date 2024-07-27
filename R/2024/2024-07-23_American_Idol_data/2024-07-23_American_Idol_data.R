


rm(list = ls())
gc()

# load libraries -----------------------------------

library(data.table)
library(stringr)

library(ggplot2)
library(ggtext)

library(ggforce)

library(ggnewscale)

library(paletteer)
library(colorspace)
library(extrafont)

my_font = "Jost"

my_col = paletteer_c("ggthemes::Sunset-Sunrise Diverging", 30) # c('#00429d', '#2b57a7', '#426cb0', '#5681b9', '#6997c2', '#7daeca', '#93c4d2', '#abdad9', '#caefdf', '#ffe2ca', '#ffc4b4', '#ffa59e', '#f98689', '#ed6976', '#dd4c65', '#ca2f55', '#b11346', '#93003a')

# input data frame ----------------------------------- 

auditions    <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-07-23/auditions.csv')
eliminations <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-07-23/eliminations.csv')
finalists    <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-07-23/finalists.csv')
ratings      <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-07-23/ratings.csv')
seasons      <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-07-23/seasons.csv')
songs        <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-07-23/songs.csv')



# --------------
ratings$season = paste0("S", ratings$season) |>
    factor(levels = paste0("S", seq_len(18)))

gr = ratings |>
    ggplot(aes(season, viewers_in_millions)) +
    
    geom_point(shape = 21, size = 3, stroke = .25,
               color = "grey90", aes(fill = season),
               position = position_jitternormal(sd_y = 0, sd_x = .1)) +
    
    scale_fill_manual(values = my_col) +
    
    geom_boxplot(width = .25, outlier.shape = NA, size = .25, fill = "grey90", aes(color = season)) +
    
    scale_color_manual(values = my_col |> darken(.35)) +
    
    scale_y_continuous(breaks = c(5, 10, 15, 20, 25, 30, 35)) +
    
    theme_minimal(base_family = my_font) +
    
    theme(
        legend.position = "none",
        
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(linewidth = .3, linetype = "dashed", lineend = "round", color = "grey87"),
        
        axis.title.x = element_text(margin = margin(t = 10)),
        axis.title.y = element_markdown(margin = margin(r = 10)),
        
        plot.title    = element_text(face = "bold", size = 26, margin = margin(b = 5), family = "Jost", hjust = .5),
        plot.subtitle = element_markdown(margin = margin(b = 25), size = 12, color = "grey25", family = "Jost", hjust = .5),
        plot.caption  = element_markdown(margin = margin(t = 25), size = 8, family = "Jost", hjust = .5),
        plot.background = element_rect(fill = "#f9fbfe", color = NA),
        plot.margin = margin(20, 20, 20, 20)
    ) +
    
    labs(
        x = "Season", y = "Number of viewers - *in millions*",
        
        title = "American Idol Data",
        
        subtitle = paste0(
            "Millions of viewers who watched each episode per season of American Idol"
        ),
        
        caption = paste0(
            "Source: <b>American Idol Data</b> | ",
            "Graphic: <b>Nikos Pechlivanis</b>"
        )
    )


ggsave(
    plot = gr, filename = "Rplot.png",
    width = 9, height = 7, units = "in", dpi = 600
)









