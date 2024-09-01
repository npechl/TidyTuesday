


rm(list = ls())
gc()

# load libraries -----------------------------------

library(data.table)
library(stringr)

library(ggplot2)
library(ggtext)

library(ggforce)
library(ggdist)

library(ggstar)

library(paletteer)
library(colorspace)
library(extrafont)

my_font = "Jost"

# 1 ----------------------------------- 

a1 <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-08-27/power_rangers_episodes.csv')
a2 <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-08-27/power_rangers_seasons.csv')

a1$desc <- NULL

df <- a1 |> merge(a2, by = "season_title", suffixes = c(".ep", ".se"))

rm(a1, a2)

# 2 ----------------------------

df <- df[order(season_num, air_date)]

df$season_title <- df$season_title |> factor(levels = df$season_title |> unique())

# 3 -------------------

df2 <- df[, c("season_title", "IMDB_rating.se"), with = FALSE] |> unique()

gr <- df |> 
    ggplot(aes(season_title, IMDB_rating.ep)) +
    
    geom_point(
        position = position_jitternormal(sd_y = 0, sd_x = .15),
        shape = 21, size = 3, stroke = .25,
        color = "grey96", aes(fill = season_title)
    ) +
    
    scale_fill_manual(values = paletteer_c("ggthemes::Sunset-Sunrise Diverging", 27) |> as.character() |> lighten(.25)) +
     
    stat_pointinterval(aes(color = season_title)) +
        
    scale_color_manual(values = paletteer_c("ggthemes::Sunset-Sunrise Diverging", 27) |> as.character() |> darken(.25)) +
    
    geom_star(
        data = df2, inherit.aes = FALSE,
        aes(season_title, IMDB_rating.se),
        color = "grey96", fill = "red", starstroke = .25, angle = 45,
        starshape = 'plus filled', size = 4
    ) +
    
    scale_shape_identity() +
    
    theme_minimal(base_family = my_font) +
    
    theme(
        legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1),
        
        panel.grid.major = element_line(linewidth = .35, linetype = "dashed", lineend = "round"),
        panel.grid.minor = element_line(linewidth = .35, linetype = "dotted", lineend = "round"),
        
        axis.line = element_line(linewidth = .35),
        axis.ticks = element_line(linewidth = .35),
        
        axis.title.y = element_markdown(margin = margin(r = 25)),
        axis.title.x = element_markdown(),
        
        plot.title    = element_text(face = "bold", size = 26, margin = margin(b = 5), family = "Jost", hjust = .5),
        plot.subtitle = element_markdown(margin = margin(b = 15), size = 10, color = "grey25", family = "Jost", hjust = .5),
        plot.caption  = element_markdown(margin = margin(t = 20), size = 8, family = "Jost", hjust = .5),
        
        
        plot.background = element_rect(fill = "#f9fbfe", color = NA),
        plot.margin = margin(20, 20, 20, 50)
    ) +
    
    labs(
        x = "**Title** of the overall **Season**",
        y = "**IMDb Rating**",
        
        title = "The Power Rangers Franchise",
        
        subtitle = paste0(
            "IMDB Ratings for Power Rangers Episodes. ",
            "Red Points indicate season ratings."
        ),
        
        caption = paste0(
            "Source: <b>Power Rangers Dataset</b> | ",
            "Graphic: <b>Nikos Pechlivanis</b>"
        )
    )




ggsave(
    plot = gr, filename = "Rplot.png",
    width = 10, height = 8, units = "in", dpi = 600
)
