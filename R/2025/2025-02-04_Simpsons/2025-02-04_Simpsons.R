


rm(list = ls())
gc()

# load libraries -----------------------------------

library(data.table)
library(stringr)

library(ggplot2)
library(ggtext)

library(ggridges)

library(paletteer)
library(colorspace)
library(extrafont)

my_font = "Jost"

# load data ----------------------------------- 

d0 <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-02-04/simpsons_characters.csv')
d1 <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-02-04/simpsons_episodes.csv')
d2 <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-02-04/simpsons_locations.csv')
d3 <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-02-04/simpsons_script_lines.csv')

# remove columns ---------------------------------


d1$image_url <- NULL
d1$video_url <- NULL

d1$season <- paste0("*#", d1$season, "*")

gr <- d1 |>
    ggplot(aes(number_in_series, us_viewers_in_millions)) +
    geom_point(aes(fill = imdb_rating, colour = imdb_rating), shape = 21, stroke = .25) +
    geom_smooth(color = "#852c59", lineend = "round") +
    
    scale_fill_stepsn(breaks = c(5.5, 6.5, 7.5), colors = paletteer_c("ggthemes::Blue-Teal", 4) |>  as.character() |> lighten(.1), guide = guide_colorsteps(barwidth = unit(12, "lines"), barheight = unit(.5, "lines"))) +
    scale_color_stepsn(breaks = c(5.5, 6.5, 7.5), colors = paletteer_c("ggthemes::Blue-Teal", 4) |> as.character() |> darken(.25), guide = "none") +
    
    scale_x_continuous(breaks = seq(440, 600, by = 10), expand = c(0, 0)) +
    
    facet_grid(cols = vars(season), scales = "free_x", space = "free_x") +
    
    theme_minimal(base_family = my_font) +
    
    theme(
        legend.position = "bottom",
        legend.title.position = "top",
        legend.title = element_text(hjust = .5),
        
        panel.grid.major = element_line(color = "grey85", linewidth = .3, lineend = "round"),
        panel.grid.minor = element_line(color = "grey85", linewidth = .3, lineend = "round", linetype = "dashed"),
        
        # panel.background = element_rect(fill = NA, color = "grey", linewidth = .3),
        
        axis.ticks = element_line(color = "grey85", linewidth = .3, lineend = "round"),
        axis.text.x = element_text(size = 8),
        
        axis.title.x = element_text(margin = margin(t = 10)),
        axis.title.y = element_markdown(margin = margin(r = 10)),
        
        strip.text = element_markdown(face = "bold"),
        
        plot.title    = element_text(size = 24, family = my_font, face = "bold", hjust = .5),
        plot.subtitle = element_markdown(size = 10, family = my_font, margin = margin(b = 11), hjust = .5),
        plot.caption  = element_markdown(size = 6, family = my_font, margin = margin(t = 10), hjust = .5),
        
        plot.margin = margin(20, 20, 20, 20)
    ) +
    
    labs(
        fill = "IMDb rating for the episode",
        x = "Episode number within the series",
        y = "Number of viewers in the U.S. in **millions**",
        
        title = "Donuts, Data, and D'oh - A Deep Dive into The Simpsons",
        
        subtitle = paste0(
            "Number of viewers in the U.S. across seasons in The Simpsons dataset."
        ),
        
        caption = paste0(
            "Source: <b>Simpsons Dataset from Kaggle</b> | ",
            "Graphic: <b>Nikos Pechlivanis</b>"
        )
    )


ggsave(
    plot = gr, filename = "Rplot.png",
    width = 16, height = 6, units = "in", dpi = 600
)



