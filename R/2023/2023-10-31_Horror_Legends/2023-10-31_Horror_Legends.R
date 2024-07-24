


rm(list = ls())
gc()

library(data.table)
library(stringr)

library(ggplot2)
library(ggtext)

library(ggh4x)

library(extrafont)

library(packcircles)
library(paletteer)

library(ggrepel)


df <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-10-31/horror_articles.csv')


df$url = NULL
df$claim = NULL

df$rating |> unique()

df$year = df$published |> str_split("-") |> lapply(function(x) x[1]) |> unlist()



df = df |> 
    split(df$year) |>
    lapply(function(x) {
        
        x = x[order(rating)]
        
        packing <- circleProgressiveLayout( rep(1, nrow(x)) ) 
        dat.gg <- circleLayoutVertices(packing, npoints = 50) |> setDT()
        
        dat.gg$title = x[dat.gg$id]$title
        dat.gg$author = x[dat.gg$id]$author
        
        dat.gg$rating = x[dat.gg$id]$rating
        
        return(dat.gg)
        
    }) |>
    
    rbindlist(idcol = "year")

df2 = df[which(rating == "true")]

df2 = df2[, by = .(year, title), .(
    x = mean(x),
    y = mean(y)
)]



df$rating = df$rating |> factor(
    levels = c(
        "false",
        "mostly false",
        "probably false",
        "true",
        "partly true",
        "was true; now outdated",
        "mixture",    
        "unproven",
        "miscaptioned",
        "undetermined",
        "legend"
    )
)

my_font = "Jost"

gr = ggplot(data = df) +
    
    geom_polygon(aes(x, y, group = id, fill = rating), colour = "white", linewidth = .25) +
    
    # geom_point(data = df2, aes(x, y), color = "white", size = 1) +
    # 
    # geom_label_repel(data = df2, aes(x, y, label = title), size = 2) +
    
    facet_wrap2(
        vars(year),
        nrow = 4
    ) +
    
    scale_fill_manual(
        values = c(paletteer_d("ggsci::hallmarks_light_cosmic"), "#800000"),
        guide = guide_legend(
            nrow = 2,
            title.theme = element_text(angle = 90, hjust = 0, family = my_font)
        )
    ) +
    
    coord_equal(clip = "off", expand = TRUE) +
    
    theme_void(base_family = my_font) +
    
    theme(
        legend.position = "bottom",
        legend.justification = "left",
        
        panel.spacing = unit(1, "lines"),
        
        plot.title    = element_text(face = "bold", size = 36, margin = margin(b = 5), family = my_font),
        plot.subtitle = element_markdown(margin = margin(b = 25), size = 10, color = "grey20", family = my_font),
        plot.caption  = element_markdown(margin = margin(t = 25), size = 6, family = my_font),
        
        plot.title.position = "plot",
        plot.caption.position = "plot",
        
        plot.background = element_rect(fill = "#f5f5f5", color = NA),
        plot.margin = margin(20, 20, 20, 20)
    ) +
    
    labs(
        title = 'Horror Legends',
        
        subtitle = paste0(
            "The annual distribution of horror legend articles, ",
            "categorized by their corresponding ratings",
            "(*False*, *True*, or *In-Between*)."
        ),
        
        x = "Probability of the predicted 1-year risk of the corresponing profile",
        
        
        
        caption = paste0(
            "Source: <b>Snopes.com</b> | ",
            "Graphic: <b>Nikolaos Pechlivanis</b>"
        )
    )
    
ggsave(
    plot = gr, filename = "Rplot.jpeg",
    width = 10, height = 8, units = "in", dpi = 600
)
