


rm(list = ls())
gc()

# load libraries -----------------------------------

library(data.table)
library(stringr)

library(ggplot2)
library(ggtext)

library(ggh4x)
library(ggpointdensity)

library(paletteer)
library(colorspace)
library(extrafont)

my_font = "Jost"

# input data frame ----------------------------------- 

summer_movie_genres <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-07-30/summer_movie_genres.csv')
summer_movies       <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-07-30/summer_movies.csv')

summer_movies$genres <- NULL

df <- summer_movie_genres |> merge(summer_movies, by = "tconst")
df <- df[which(!is.na(genres))]
df <- df[which(year >= 1950)]

df[, by = .(genres), N := .N]

df$genres_ann = paste0("**", df$genres, "**<sup>", df$N, "</sup>")

# df[, by = .(genres_ann), c("year_median", "rating_median") := list(
#     year |> median(na.rm = TRUE),
#     average_rating |> median(na.rm = TRUE)
# )]


h <- df[which(N >= 10), by = .(genres_ann), .(
    year_median   = year |> median(na.rm = TRUE),
    rating_median = average_rating |> median(na.rm = TRUE)
)]

# h = h |> merge(df, by = "genres_ann")

gr = df |>
    ggplot(aes(year, average_rating)) +
    
    geom_pointdensity(shape = 1, stroke = .25) +
    
    geom_vline(data = h, aes(xintercept = year_median), color = "#00429d" |> darken(.5), linetype = "dashed", lineend = "round", linewidth = .45) +
    geom_hline(data = h, aes(yintercept = rating_median), color = "#00429d" |> darken(.5), linetype = "dashed", lineend = "round", linewidth = .45) +
        
    scale_y_continuous(limits = c(0, 10), breaks = c(0, 2.5, 5, 7.5, 10)) +
    scale_x_continuous(limits = c(1950, 2024), breaks = c(1960, 1980, 2000, 2020)) +
    
    scale_color_stepsn(
        colors =  paletteer_c("ggthemes::Red-Blue Diverging", 6, -1), 
        breaks = c(10, 15, 20, 25, 30),
        guide = guide_colorsteps(
            title = "Density",
            title.position = "left",
            title.theme = element_text(angle = 90),
            label.theme = element_text(size = 8),
            barwidth = unit(.5, "lines"),
            barheight = unit(10, "lines")
        )
    ) +
    
    facet_wrap(vars(genres_ann), nrow = 5, axes = "all_x") +
    
    theme_minimal(base_family = my_font) +
    
    theme(
        axis.text = element_text(size = 8),
        strip.text = element_markdown(),
        
        axis.title.x = element_markdown(margin = margin(t = 10)),
        axis.title.y = element_markdown(margin = margin(r = 10)),
        
        panel.grid.major = element_line(linewidth = .35, linetype = "dashed", lineend = "round"),
        panel.grid.minor = element_line(linewidth = .35, linetype = "dotted", lineend = "round"),
        
        panel.spacing = unit(1, "lines"),
        
        plot.title    = element_text(face = "bold", size = 26, margin = margin(b = 5), family = "Jost", hjust = .5),
        plot.subtitle = element_markdown(margin = margin(b = 25), size = 10, color = "grey25", family = "Jost", hjust = .5),
        plot.caption  = element_markdown(margin = margin(t = 25), size = 8, family = "Jost", hjust = .5),
        
        plot.background = element_rect(fill = "#f9fbfe", color = NA),
        plot.margin = margin(20, 20, 20, 20)
    ) +
    
    labs(
        y = "**Weighted Average** of all the Individual User Ratings **on IMDb**",
        x = "**Release Year** of a Movie/Video/tvMovie",
        
        title = "Summer Movies",
        
        subtitle = paste0(
            "Average IMDb ratings for each movie, video, or TV movie ",
            "from 1950 to 2024,<br>",
            "with <span style='color:#00235A'>horizontal</span> and <span style='color:#00235A'>vertical</span> lines ", 
            "<b style='color:#00235A'>--- --- ---</b> ",
            "representing the *median values*."
        ),
        
        caption = paste0(
            "Source: <b>Internet Movie Database</b> | ",
            "Graphic: <b>Nikos Pechlivanis</b>"
        )
    )


ggsave(
    plot = gr, filename = "Rplot.png", dpi = 600,
    width = 10, height = 12, units = "in"
)
