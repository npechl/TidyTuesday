


rm(list = ls())
gc()


library(data.table)
library(stringr)

library(ggplot2)
library(extrafont)

library(ggtext)

library(rnaturalearth)

df <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-03-07/numbats.csv')


df$taxonConceptID = NULL
df$recordID = NULL

df = df[which(!is.na(decimalLatitude) & !is.na(decimalLongitude)), ]

world = ne_countries(scale = "medium", returnclass = "sf")

# world$ann = ifelse(
#     str_detect(world$formal_en, "Australia"),
#     "grey85", "white"
# )

au = world[which(str_detect(world$formal_en, "Australia")), ]

gr = ggplot() +
    
    geom_sf(data = au, 
            fill = "grey95",
            color = "grey10", linewidth = 0.1) +
    
    # geom_point(data = df, aes(x = decimalLongitude, y = decimalLatitude)) +
    
    geom_hex(
        data = df, 
        aes(x = decimalLongitude, y = decimalLatitude),
        color = "grey0", linewidth = 0.1,
        binwidth = c(.75, .75)
    ) +
    
    scale_fill_gradient2(
        low = "#497dc0",
        mid = "#f29b18",
        high = "#f3133d",
        
        midpoint = 125,
        
        guide = guide_colorbar(
            title.position = "top",
            barheight = unit(.35, "lines"),
            barwidth = unit(10, "lines"),
            
            title.theme = element_text(size = 7, family = "Zilla Slab", face = "bold"),
            label.theme = element_text(size = 6, family = "Zilla Slab")
        )
    ) +
    
    coord_sf(xlim = c(110, 155), ylim = c(-45, -8)) +
    
    facet_wrap(vars(scientificName), nrow = 1) +
    
    theme_minimal(base_family = "Zilla Slab") +
    
    theme(
        legend.position = "bottom",
        
        panel.spacing = unit(1, "lines"),
        
        strip.text = element_text(face = "bold"),
        
        panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        
        plot.title.position = "plot",
        plot.caption.position = "plot",
        
        plot.title    = element_text(face = "bold", size = 26),
        plot.subtitle = element_markdown(margin = margin(b = 20), size = 10, color = "grey20"),
        plot.caption  = element_markdown(margin = margin(t = 20), size = 6),
        
        plot.margin = margin(20, 20, 20, 20)
    ) +
    
    labs(
        
        title = 'Numbats in Australia',
        
        subtitle = paste0(
            "The maps summarize the distribution of numbats in Australia"
        ),
        
        caption = paste0(
            "Source: <b>Atlas of Living Australia</b> | ",
            "Graphic: <b>Nikolaos Pechlivanis</b>"
        ),
        
        fill = "Number of sightings"
    )


ggsave(
    plot = gr, filename = "Rplot.jpeg",
    width = 10, height = 6, units = "in"
)




















