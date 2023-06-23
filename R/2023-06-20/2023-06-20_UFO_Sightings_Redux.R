
rm(list = ls())
gc()


library(data.table)
library(stringr)

ufo_sightings <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-06-20/ufo_sightings.csv')
places        <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-06-20/places.csv')
# day_parts_map <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-06-20/day_parts_map.csv')



ufo_sightings$summary = NULL
ufo_sightings |> head()
ufo_sightings$country_code |> unique()


places$alternate_city_names = NULL
places$country              = NULL

places = places |> unique()
places |> head()

# day_parts_map |> head()

ufo_sightings$key = paste(ufo_sightings$country_code, ufo_sightings$state, ufo_sightings$city, sep = "_")
places$key = paste(places$country_code, places$state, places$city, sep = "_")

ufo_sightings$city         = NULL
ufo_sightings$state        = NULL
ufo_sightings$country_code = NULL

df = merge(
    ufo_sightings, places, 
    by = "key", all.x = TRUE
)


df$key                    = NULL
df$elevation_m            = NULL
df$reported_date_time     = NULL
# df$reported_date_time_utc = NULL

library(ggplot2)
library(ggh4x)

library(ggtext)

library(extrafont)

library(maps)
library(sf)

world1 <- st_as_sf(map('world', plot = FALSE, fill = TRUE))

df = df[which( !is.na(df$day_part) & !is.na(df$shape))]

df$day_part = df$day_part |> str_to_title()

laea = st_crs("+proj=laea") # Lambert equal area
world2 <- st_transform(world1, laea)

my_font = "Zilla Slab"

gr = ggplot() + 
    
    geom_sf(data = world1, fill = "#00b888", color = NA) +
    
    geom_hex(data = df, aes(x = longitude, y = latitude),
             binwidth = c(2.5, 2.5), color = "grey96", linewidth = .01) +
    
    scale_fill_gradient2(
        low = "#29339B", mid = "#EEC643", high = "red4",
        midpoint = 1000, labels = scales::comma,
        guide = guide_colorbar(
            title = "No. of UFO Sightings",
            title.position = "left",
            title.theme = element_text(angle = 90, family = my_font),
            label.theme = element_text(family = my_font),
            barwidth = unit(.75, "lines"),
            barheight = unit(10, "lines")
        )
    ) +
    
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    
    facet_wrap2(
        vars(day_part)
    ) +
    
    theme_minimal() +
    
    theme(
        panel.grid = element_blank(),
        
        axis.title = element_blank(),
        axis.text = element_blank(),
        
        panel.spacing = unit(1.5, "lines"),
        
        strip.text = element_text(face = "bold", hjust = 0, family = my_font, size = 11),
        
        plot.title    = element_text(face = "bold", size = 32, family = "Rockwell"),
        plot.subtitle = element_markdown(margin = margin(b = 15), size = 11, color = "grey20", family = my_font),
        plot.caption  = element_markdown(margin = margin(t = 15), size = 6, family = my_font),
        
        plot.background = element_rect(fill = "grey96", color = NA),
        plot.margin = margin(20, 20, 20, 20)
    ) +
    
    labs(
        title = 'UFO Sightings Redux',
        
        subtitle = paste0(
            "Global UFO sightings categorized by the estimated time of day when the sightings occurred."
        ),
        
        caption = paste0(
            "Source: <b>National UFO Reporting Center</b> | ",
            "Graphic: <b>Nikolaos Pechlivanis</b>"
        )
    )
    
    
ggsave(
    plot = gr, filename = "Rplot.jpeg",
    width = 18, height = 10, units = "in", dpi = 600
)














