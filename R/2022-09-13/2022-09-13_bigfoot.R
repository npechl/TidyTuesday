


rm(list = ls())
gc()


library(data.table)
library(stringr)


library(ggplot2)


bigfoot = fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-09-13/bigfoot.csv')

summary(bigfoot$date)


df = bigfoot[which(
   !is.na(bigfoot$longitude) & !is.na(bigfoot$latitude) 
), ]

df = df[which(df$state != "Alaska"), ]

df = df[which(longitude >= -130), ]

df = df[which(season != "Unknown"), ]

library(maps)

# county = map_data("county")
state = map_data("state")


df$season = factor(
    df$season,
    levels = c("Fall", "Winter", "Spring", "Summer")
)

sub = df[which(df$classification == "Class A"), ]



gr1 = ggplot() +
    
    geom_polygon(
        data = state, aes(x = long, y = lat, group = group), 
        color = "gray95", fill = "#2c656b", size = 0.1, alpha = .3
    ) +
    
    geom_point(
        data = sub, 
        aes(x = longitude, y = latitude, color = temperature_mid),
        shape = 19, alpha = .5, size = .8
    ) +
    
    scale_color_gradient(
        low = "#004d99", high = "#990000",
        guide = guide_colorbar(
            title = "Temperature mid (F)",
            title.position = "top",
            title.hjust = 1,
            barwidth = 12,
            barheight = .25
        )
    ) +
    
    facet_wrap(vars(season), nrow = 1) +
    
    theme_void(base_family = "Century Schoolbook") +
    
    theme(
        legend.position = "bottom",
        legend.justification = 1,
        strip.text = element_text(face = "bold", margin = margin(b = 5))
    ) +
    
    coord_map()


gr2 = ggplot() +
    
    geom_polygon(
        data = state, aes(x = long, y = lat, group = group), 
        color = "gray95", fill = "#2c656b", size = 0.1, alpha = .3
    ) +
    
    geom_point(
        data = sub, 
        aes(x = longitude, y = latitude, color = dew_point),
        shape = 19, alpha = .5, size = .8
    ) +
    
    scale_color_gradient(
        low = "#004d99", high = "#990000",
        guide = guide_colorbar(
            title = "Dew point (F)",
            title.position = "top",
            title.hjust = 1,
            barwidth = 12,
            barheight = .25
        )
    ) +
    
    facet_wrap(vars(season), nrow = 1) +
    
    theme_void(base_family = "Century Schoolbook") +
    
    theme(
        legend.position = "bottom",
        legend.justification = 1,
        strip.text = element_text(face = "bold", margin = margin(b = 5))
    ) +
    
    coord_map()





gr3 = ggplot() +
    
    geom_polygon(
        data = state, aes(x = long, y = lat, group = group), 
        color = "gray95", fill = "#2c656b", size = 0.1, alpha = .3
    ) +
    
    geom_point(
        data = sub, 
        aes(x = longitude, y = latitude, color = precip_probability),
        shape = 19, alpha = .5, size = .8
    ) +
    
    scale_color_gradient(
        low = "gray95", high = "#16748b",
        guide = guide_colorbar(
            title = "Chance of Precipitation (%)",
            title.position = "top",
            title.hjust = 1,
            barwidth = 12,
            barheight = .25
        )
    ) +
    
    facet_wrap(vars(season), nrow = 1) +
    
    theme_void(base_family = "Century Schoolbook") +
    
    theme(
        legend.position = "bottom",
        legend.justification = 1,
        strip.text = element_text(face = "bold", margin = margin(b = 5))
    ) +
    
    coord_map()


gr4 = ggplot() +
    
    geom_polygon(
        data = state, aes(x = long, y = lat, group = group), 
        color = "gray95", fill = "#2c656b", size = 0.1, alpha = .3
    ) +
    
    geom_point(
        data = sub, 
        aes(x = longitude, y = latitude, color = humidity),
        shape = 19, alpha = .5, size = .8
    ) +
    
    scale_color_gradient(
        low = "gray95", high = "#16748b",
        guide = guide_colorbar(
            title = "Humidity (%)",
            title.position = "top",
            title.hjust = 1,
            barwidth = 12,
            barheight = .25
        )
    ) +
    
    facet_wrap(vars(season), nrow = 1) +
    
    theme_void(base_family = "Century Schoolbook") +
    
    theme(
        legend.position = "bottom",
        legend.justification = 1,
        strip.text = element_text(face = "bold", margin = margin(b = 5))
    ) +
    
    coord_map()

gr5 = ggplot() +
    
    geom_polygon(
        data = state, aes(x = long, y = lat, group = group), 
        color = "gray95", fill = "#2c656b", size = 0.1, alpha = .3
    ) +
    
    geom_point(
        data = sub, 
        aes(x = longitude, y = latitude, color = cloud_cover),
        shape = 19, alpha = .5, size = .8
    ) +
    
    scale_color_gradient(
        low = "gray95", high = "gray10",
        guide = guide_colorbar(
            title = "Cloud cover (%)",
            title.position = "top",
            title.hjust = 1,
            barwidth = 12,
            barheight = .25
        )
    ) +
    
    facet_wrap(vars(season), nrow = 1) +
    
    theme_void(base_family = "Century Schoolbook") +
    
    theme(
        legend.position = "bottom",
        legend.justification = 1,
        strip.text = element_text(face = "bold", margin = margin(b = 5))
    ) +
    
    coord_map()



library(patchwork)
library(ggtext)
library(extrafont)

multi = gr1 / gr3 / gr4 / gr5 +
    
    plot_annotation(
        title = 'Finding Bigfoot',
        subtitle = 'Clear sightings (*Class A* classification) of Bigfoot across the United States from 1869 to 2021.',
        caption = paste0(
            "Source: **Bigfoot Field Researchers Organization (BFRO)** | ",
            "Graphic: **Nikolaos Pechlivanis**"
        )
    ) &
    
    theme(
        
        plot.caption = element_markdown(family = "Century Schoolbook", color = "#2c656b", margin = margin(t = 10)),
        plot.caption.position = "plot",
        
        plot.title = element_text(family = "Niagara Engraved", face = "bold", size = 40, color = "#070e11"),
        plot.title.position = "plot",
        
        plot.subtitle = element_markdown(family = "Century Schoolbook", color = "#2c656b", size = 12),
        
        plot.background = element_rect(fill = "#f5f5fa", color = "#f5f5fa"),
        
        plot.margin = margin(10, 10, 10, 10)
    )



ggsave(
    filename = "Rplot.pdf",
    plot = multi,
    width = 10, height = 10,
    device = cairo_pdf,
    units = "in"
)

ggsave(
    filename = "Rplot.png",
    plot = multi,
    width = 10, height = 10,
    
    units = "in"
)










    
















