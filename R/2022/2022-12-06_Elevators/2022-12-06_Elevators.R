


rm(list = ls())
gc()


library(data.table)
library(stringr)

library(ggplot2)
library(ggmap)
library(ggtext)
library(ggpointdensity)

library(extrafont)

# devtools::install_github("zachcp/nycmaps")
library(nycmaps)

df = fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-12-06/elevators.csv')
df = df[which(df$Borough == "Manhattan"), ]

nyc = map_data("nyc")
nyc = setDT(nyc)
nyc$subregion = str_squish(nyc$subregion)

manh = nyc[which(group == 15), ]

df = df[which(LATITUDE >= min(manh$lat) & LATITUDE <= max(manh$lat)), ]
df = df[which(LONGITUDE >= min(manh$long) & LONGITUDE <= max(manh$long)), ]

df$level = ifelse(
    df$`Device Type` %in% c("Handicap Lift (H)", "Manlift (M)", "Private Elevator (T)", "Public Elevator (L)"),
    "Other", df$`Device Type`
)

df = df[which(DV_DEVICE_STATUS_DESCRIPTION == "ACTIVE"), ]

s0 = df[, by = level, .N]
s0 = s0[order(-N), ]

df$level = factor(df$level, levels = s0$level)

gr = ggplot() +
    
    geom_polygon(
        data = manh, aes(x=long, y = lat, group = group),
        fill = "white" #, color = "grey10"
    ) +
    
    geom_pointdensity(
        data = df, aes(x = LONGITUDE, y = LATITUDE),
        adjust = .1, size = .5
    ) +
    
    scale_color_gradient(
        low = "#0edbff",
        high = "#bf1c00",
        guide = guide_colorbar(
            title.position = "top", title = "Density (No. of elevators)",
            title.theme = element_text(color = "white", face = "bold", size = 8, family = "Zilla Slab"),
            label.theme = element_text(color = "white", size = 8, family = "Zilla Slab"),
            barwidth = unit(10, "lines"), barheight = unit(.5, "lines")
        )
    ) +
    
    facet_wrap(
        vars(level)
    ) +
    
    coord_map(clip = "off") +
    
    theme_void(base_family = "Zilla Slab") +
    
    theme(
        
        legend.position = "bottom",
        
        plot.background = element_rect(fill = "#000e27", color = NA),
        
        strip.text = element_text(color = "white", face = "bold"),
        
        plot.title = element_markdown(color = "white", face = "bold", margin = margin(b = 5), size = 16),
        plot.subtitle = element_markdown(color = "white", margin = margin(b = 15), size = 10),
        plot.caption = element_markdown(color = "white", margin = margin(t = 15), size = 8),
        
        plot.title.position = "plot",
        plot.caption.position = "plot",
        
        plot.margin = margin(20, 20, 20, 20)
    ) +
    
    labs(
        title = 'Weather Forecast Accuracy',
        
        subtitle = paste0(
            "Density of registered (active) elevator devices in Manhattan, New York City"
        ),
        
        caption = paste0(
            "Source: <b>Elevators data package</b> | ",
            "Graphic: <b>Nikolaos Pechlivanis</b>"
        )
    )


ggsave(
    plot = gr, filename = "Rplot.jpeg",
    width = 6, height = 9, units = "in"
)






