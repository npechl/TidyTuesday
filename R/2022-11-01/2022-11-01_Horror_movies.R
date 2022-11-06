


rm(list = ls())
gc()


library(data.table)
library(stringr)

library(ggplot2)
library(ggrepel)
library(ggtext)

library(extrafont)

horror_movies <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-11-01/horror_movies.csv')


horror_movies = horror_movies[which(horror_movies$original_language == "en"), ]

genre_names = horror_movies$genre_names

genre_names = str_split(genre_names, "\\, ")
genre_names = lapply(genre_names, sort)
genre_names = lapply(genre_names, paste, collapse = ", ")

horror_movies$genre_names = unlist(genre_names)

rm(genre_names)


y = horror_movies[which(!is.na(horror_movies$collection_name)), ]


y$collection_name = str_remove_all(y$collection_name, "\\ Collection")

stats = y[, by = collection_name, .(collection_vote_avg = mean(vote_average))]

stats = stats[order(collection_vote_avg), ]

y$collection_name = factor(
    y$collection_name,
    levels = stats$collection_name
)

y = y[order(-collection_name), ]

y = y[1:100, ]

# segs = y[order(-vote_average), ]
# segs = y[, by = collection_name, head(.SD, 1)]


segs = y[, by = collection_name, .(start = min(vote_average), end = max(vote_average))]


gr = ggplot(data = y, aes(y = collection_name, x = vote_average)) +
    
    geom_segment(data = segs, aes(x = start, xend = end,
                     y = collection_name, yend = collection_name),
                 color = "white", linetype = "dashed", size = 0.25) +
    
    geom_point(aes(color = vote_average), size = 1.5) +
    
    geom_text_repel(aes(label = title), 
                    max.overlaps = Inf, size = 2, 
                    min.segment.length = 0,
                    segment.linetype = "dotted",
                    segment.color = "white",
                    segment.size = .25,
                    color = "white",
                    family = "Treasure") +
    
    scale_color_gradient(
        low = "blue2", 
        high = "red2"
    ) +
    
    scale_x_continuous(
        expand = c(0, 0),
        breaks = seq(5, 10, by = 1),
        minor_breaks = seq(5.5, 9.5, by = 1)
    ) +
    
    scale_y_discrete(expand = c(0, 0)) +
    
    coord_cartesian(expand = TRUE, clip = "off") +
    
    theme_minimal(base_family = "Treasure") +
    
    theme(
        legend.position = "none",
        
        axis.title.y = element_blank(),
        
        axis.title.x = element_text(color = "white"),
        
        axis.text.x = element_text(color = "white", size = 8),
        axis.text.y = element_text(color = "white"),
        
        panel.grid.major.y = element_line(size = 0.25, linetype = 2, color = "#847a70"),
        panel.grid.major.x = element_line(size = 0.35, linetype = 2, color = "#847a70"),
        panel.grid.minor.x = element_line(size = 0.25, linetype = 2, color = "#847a70"),
        
        plot.title.position = "plot",
        plot.caption.position = "plot",
        
        plot.title = element_text(family = "Friday13SH", size = 34, color = "white"),
        plot.subtitle = element_markdown(margin = margin(b = 15), color = "white"),
        plot.caption = element_markdown(margin = margin(t = 15), color = "white"),
        
        plot.background = element_rect(fill = "#707084", color = "#707084"),
        
        plot.margin = margin(20, 20, 20, 20)
    ) +
    
    labs(
        x = "Vote Average",
        title = "Horror Movies",
        subtitle = paste0(
            "Top 100 Horror Movie Collections according to Vote Average"
        ),
        caption = paste0(
            "Source: **themoviedb.org** | ",
            "Graphic: **Nikolaos Pechlivanis**"
        )
    )

ggsave(
    plot = gr,
    filename = "Rplot.pdf",
    width = 10, height = 10,
    device = cairo_pdf,
    units = "in"
)


library(magick)


tmp = image_read_pdf("Rplot.pdf", density = 500)
image_write(tmp, "Rplot.png")

