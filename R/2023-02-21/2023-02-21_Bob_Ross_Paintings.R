


rm(list = ls())
gc()


library(data.table)
library(stringr)

library(ggplot2)
library(ggtext)
library(extrafont)

library(packcircles)
# library(ggforce)


df <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-02-21/bob_ross.csv')

df2 = df[, c("painting_index", "season", "episode", "color_hex"), with = FALSE]

df2$color_hex = str_remove_all(df2$color_hex, "\\[|\\]|\\'")


colors = str_split(df2$color_hex, "\\, ")
names(colors) = df2$painting_index

colors = lapply(colors, function(x) { data.table("hex" = x) })
colors = rbindlist(colors, idcol = "painting_index")

colors$painting_index = as.integer(colors$painting_index)

df2 = merge(colors, df2, by = "painting_index", all.x = TRUE)

df2$color_hex = NULL

df2 = df2[order(season, episode, hex), ]

df2$size = 1

df2$id = paste0(df2$season, ".", df2$episode)

df2 = split(df2, df2$id)

packing = lapply(df2, function(x) {
    
    out = circleProgressiveLayout(x$size) 
    out = circleLayoutVertices(out, npoints = 100)
    out = setDT(out)
    
    out$season  = x[out$id, ]$season
    out$episode = x[out$id, ]$episode
    out$hex     = x[out$id, ]$hex
    
    return(out)
})

# df2 = rbindlist(df2)
packing = rbindlist(packing)

# df2 = cbind(df2, packing)
# 
# df2$id = NULL

my_col = unique(packing$hex)
names(my_col) = my_col

packing = packing[order(season, episode), ]

packing$season = paste0("S", packing$season)
packing$episode = paste0("E", packing$episode)

packing$season = factor(packing$season, levels = unique(packing$season))
packing$episode = factor(packing$episode, levels = unique(packing$episode))

gr = ggplot(data = packing) +
    
    geom_polygon(
        aes(x, y, group = id, fill = hex),
        color = "grey95", linewidth = .1
    ) +
    
    scale_fill_manual(values = my_col) +
    scale_color_manual(values = my_col) +
    
    coord_equal() +
    
    facet_grid(
        rows = vars(episode),
        cols = vars(season)
    ) +
    
    theme_minimal(base_family = "Zilla Slab") +
    
    theme(
        legend.position = "none",
        
        axis.title = element_blank(),
        axis.text = element_blank(),
        
        strip.text = element_text(face = "bold", color = "white"),
        
        panel.grid = element_blank(),
        
        plot.title.position   = "plot",
        plot.caption.position = "plot",
        
        plot.title    = element_text(size = 32, color = "white", face = "bold"),
        plot.subtitle = element_markdown(margin = margin(b = 10), size = 8, color = "white", face = "bold"),
        plot.caption  = element_markdown(margin = margin(t = 20), size = 5, color = "white"),
        
        plot.background = element_rect(fill = "#4f1d38", color = NA),
        plot.margin = margin(10, 10, 10, 10)
    ) +
    
    labs(
        title = 'Bob Ross Paintings',
        
        subtitle = paste0(
            "Colors from the paintings of Bob Ross featured in the ",
            "TV Show *The Joy of Painting* across all seasons (columns) ",
            "and episodes (rows)"
        ),
        
        caption = paste0(
            "Source: <b>Bob Ross Colors data package</b> | ",
            "Graphic: <b>Nikolaos Pechlivanis</b>"
        )
    )

ggsave(
    plot = gr, filename = "Rplot.jpeg",
    width = 13, height = 7, units = "in"
)












