


rm(list = ls())
gc()

library(data.table)
library(stringr)

library(ggplot2)
library(ggtext)

library(ggstream)

library(extrafont)

holiday_movies <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-12-12/holiday_movies.csv')
holiday_movie_genres <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-12-12/holiday_movie_genres.csv')

index = match(holiday_movie_genres$tconst, holiday_movies$tconst)

holiday_movie_genres$year = holiday_movies[index]$year

x = holiday_movie_genres[, by = .(year, genres), .N]
x = x[which(year >= 1980 & !is.na(genres))]

x = x |> 
    dcast(year ~ genres, value.var = "N", fill = 0) |>
    melt(id.vars = "year", variable.factor = FALSE, value.factor = FALSE, variable.name = "genres", value.name = "N")

x$label = ifelse(
    x$genres %in% c("Drama", "Romance", "Comedy", "Family"),
    x$genres, NA
)

my_font = "Jost"

gr = ggplot(x, aes(year, N, fill = genres)) +
    
    geom_stream(color = "white", linewidth = .075, type = "ridge") +
    
    geom_vline(xintercept = seq(1990, 2025, by = 10), color = "grey75", linetype = "dotted", linewidth = .45) +
    geom_vline(xintercept = seq(1985, 2025, by = 10), color = "grey75", linetype = "dotted", linewidth = .35) +
    
    geom_hline(yintercept = c(100, 200, 300), color = "grey75", linetype = "dotted", linewidth = .35) +
    
    # geom_stream_label(aes(label = label), type = "ridge", color = "white", family = my_font, fontface = "bold", hjust = .5, vjust = .5) +
    
    annotate("text", x = 2019.5, y = 35, label = "Romance", color = "white", family = my_font, fontface = "bold") +
    annotate("text", x = 2020.5, y = 130, label = "Family", color = "white", family = my_font, fontface = "bold") +
    annotate("text", x = 2020, y = 170, label = "Drama", color = "white", family = my_font, fontface = "bold") +
    annotate("text", x = 2021, y = 250, label = "Comedy", color = "white", family = my_font, fontface = "bold") +
    
    scale_fill_viridis_d(option = "rocket", guide = guide_legend(nrow = 2, override.aes = list(color = "grey10"))) +
    
    scale_x_continuous(expand = c(0, 0)) +
    
    theme_minimal(base_family = my_font) +
    
    theme(
        legend.position = "bottom",
        legend.justification = "left",
        legend.title = element_blank(),
        
        axis.title.x = element_blank(),
        axis.text.x = element_text(face = "bold"),
        
        axis.line.y = element_line(linewidth = .3, color = "grey75"),
        axis.ticks.y = element_line(linewidth = .3, color = "grey75"),
        
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        
        plot.title    = element_text(face = "bold", size = 36, margin = margin(b = 5), family = my_font),
        plot.subtitle = element_markdown(margin = margin(b = 25), size = 11, color = "grey25", family = my_font),
        plot.caption  = element_markdown(margin = margin(t = 25), size = 8, family = my_font),
        
        plot.title.position = "plot",
        plot.caption.position = "plot",
        
        plot.background = element_rect(fill = "#fbf3f5", color = NA),
        plot.margin = margin(20, 20, 20, 20)
    ) +
    
    labs(
        y = "No. of Holiday movies",
        
        title = 'Holiday Movies',
        
        subtitle = paste0(
            "Distribution of Holiday Movies by Year and Genre. **Happy Holidays!**"
        ),
        
        caption = paste0(
            "Source: <b>Internet Movie Database</b> | ",
            "Graphic: <b>Nikolaos Pechlivanis</b>"
        )
    )


ggsave(
    "Rplot.jpeg", gr,
    width = 12, height = 10, units = "in", dpi = 600
)



