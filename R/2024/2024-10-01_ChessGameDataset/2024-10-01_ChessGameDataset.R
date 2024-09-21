


rm(list = ls())
gc()

# load libraries -----------------------------------

library(data.table)
library(stringr)

library(ggplot2)
library(ggtext)

library(ggpointdensity)
library(ggdensity)

library(paletteer)
library(colorspace)
library(extrafont)

my_font = "Jost"
my_col = c('#00429d', '#73a2c6', '#ffffe0', '#f4777f', '#93003a')

# 1 ----------------------------------- 

a <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-10-01/chess.csv')

a$moves <- NULL

# a$duration <- (a$end_time - a$start_time) |> as.integer() |> lubridate::as.duration()
a$rating_diff <- a$white_rating - a$black_rating

# 2 ---------------------------

gr <- a[which(turns < 300)] |>
    ggplot(aes(turns, rating_diff)) +
    
    geom_pointdensity(shape = 1, size = 1.5, stroke = .25) +
    
    geom_hdr_lines(linewidth = .5) +
    
    geom_hline(yintercept = 0, linetype = "dotted", lineend = "round", linewidth = .4, color = "#D61F33") +
    
    scale_color_gradient(low = "#5786C5", high = "#6B0077", 
                         guide = guide_colorbar(
                             barheight = unit(.25, "lines"), 
                             barwidth = unit(10, "lines")
                         )) +
    
    scale_y_continuous(
        breaks = c(-1000, 0, 1000),
        labels = c("**Black player's rating** is<br>higher than<br>White player's rating", 
                   "**Equal rating**",
                   "**White player's rating** is<br>higher than<br>Black player's rating")
    ) +
    
    theme_minimal(base_family = my_font) +
    
    theme(
        legend.position = "bottom",
        legend.justification = "left",
        legend.title.position = "top",
        
        panel.grid = element_line(linewidth = .3, linetype = "dashed", lineend = "round"),
        
        axis.text.y = element_markdown(),
        axis.title.y = element_text(margin = margin(r = 10)),
        axis.title.x = element_text(margin = margin(t = 10)),
        
        plot.title    = element_text(face = "bold", size = 26, margin = margin(b = 5), family = "Jost"),
        plot.subtitle = element_markdown(margin = margin(b = 15), size = 10, color = "grey25", family = "Jost"),
        plot.caption  = element_markdown(margin = margin(t = 20), size = 8, family = "Jost"),
        
        plot.title.position = "plot",
        plot.caption.position = "plot",
        
        plot.background = element_rect(fill = "#f9fbfe", color = NA),
        plot.margin = margin(20, 20, 20, 20)
    ) +
    
    labs(
        x = "Number of turns", y = "Rating difference", 
        color = "Density (no. of neighbors)", alpha = "Probabilities",
        
        title = "Chess Game Dataset (Lichess)",
        
        subtitle = paste0(
            "Game duration, in turns, based on white and black player ratings."
        ),
        
        caption = paste0(
            "Source: <b>Lichess.org</b> via <b>Kaggle/Mitchell J</b> | ",
            "Graphic: <b>Nikos Pechlivanis</b>"
        )
    )


ggsave(
    plot = gr, filename = "Rplot.png", dpi = 600,
    units = "in", width = 10, height = 9
)




