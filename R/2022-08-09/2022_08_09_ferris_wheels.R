


rm(list = ls())
gc()

library(data.table)
library(stringr)

library(ggplot2)
library(measurements)

library(ggtext)

library(extrafont)

library(maps)   
library(ggforce)

wheels = fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-08-09/wheels.csv')
wheels = wheels[, 2:ncol(wheels)]

wheels = wheels[which(is.na(closed)), ]
wheels = wheels[which(!is.na(opened)), ]
    
usa = wheels[which(country == "USA"), ]

usa.cities = world.cities
usa.cities = setDT(usa.cities)
usa.cities = usa.cities[which(country.etc == "USA"), ]


index = c(
    "Chicago", "Orlando", "Santa Monica",  "Seattle", "Washington", 
    "Farmington", "Atlanta", "Atlantic City", "Dallas", "New York",
    "Las Vegas"
)

usa.cities = usa.cities[which(name %in% index), ]

out = list()

for(i in index) {
    
    
    tmp = usa[which(str_detect(usa$location, i)), ]
    
    tmp$lat = usa.cities[which(usa.cities$name == i), ]$lat
    tmp$long = usa.cities[which(usa.cities$name == i), ]$long
    
    out[[i]] = tmp
}

out = rbindlist(out)
out = out[, by = name, head(.SD, 1)]
    
usa.county <- map_data("county")
usa.state <- map_data("state")

gr = ggplot() +
    
    geom_polygon(
        data = usa.state, 
        aes(x = long, y = lat, group = group), 
        color = "black", size = .05,
        fill = "grey", alpha = 0.3
    ) +
    
    geom_mark_circle(
        data = out, 
        aes(x = long, y = lat, label = name, colour = name),
        show.legend = FALSE,
        con.colour = "#1d5aff",
        con.cap = 0,
        label.fill = NA,
        label.fontsize = 10,
        label.family = "Goudy Old Style"
    ) +
    
    geom_jitter(
        data = out,
        aes(x = long, y = lat, size = height),
        width = 0.1, height = 0.1,
        shape = 21, stroke = 0.25, color = "#ff1d5a", fill = "#ffc21d"
    ) +
    
    scale_size_binned(
        breaks = c(200, 300, 400, 500, 600),
        guide = guide_legend(
           title = "Height (in feet)", title.position = "top"
        ),
        range = c(1, 10)
    ) +
    
    scale_colour_manual(
        values = rep("#1d5aff", nrow(out))
    ) +
    
    theme_void(base_family = "Goudy Old Style") +
    
    coord_map() +
    
    theme(
        legend.position = "bottom",
        
        plot.title.position = "plot",
        plot.caption.position = "plot",
        
        plot.title = element_markdown(family = "Goudy Stout", margin = margin(b = 5)),
        plot.subtitle = element_markdown(face = "italic", margin = margin(b = 5)),
        plot.caption = element_markdown(margin = margin(t = 20), size = 11),
        
        plot.background = element_rect(
            fill = "#f5faf8", 
            color = "#f5faf8"
        ),
        
        plot.margin = margin(20, 20, 20, 20)
    ) +
    
    labs(
        title = "Ferris Wheels",
        
        subtitle = paste0(
            "Ferris Wheels across 11 cities in the United States"
        ),
        
        caption = paste0(
            "Source: <b><i>ferriswheels</i> package</b>",
            " | ",
            "Graphic: <b>Pechlivanis Nikolaos</b>"
            
        )
    )


ggsave(
    "Rplot.pdf",
    plot = gr,
    width = 14, height = 7,
    device = cairo_pdf,
    units = "in"
)

ggsave(
    "Rplot.png",
    plot = gr,
    width = 14, height = 7,
    units = "in"
)

    




