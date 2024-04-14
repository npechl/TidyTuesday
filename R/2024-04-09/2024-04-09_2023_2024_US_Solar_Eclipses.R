


rm(list = ls())
gc()

# load libraries -----------------------------------

library(data.table)
library(stringr)

library(ggplot2)
library(ggtext)

library(paletteer)
library(colorspace)
library(extrafont)

library(lubridate)

my_font = "Jost"

# input data frame -----------------------------------

# d0 <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-04-09/eclipse_annular_2023.csv')
d1 <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-04-09/eclipse_total_2024.csv')
# d2 <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-04-09/eclipse_partial_2023.csv')
# d3 <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-04-09/eclipse_partial_2024.csv')


d1 = d1 |> melt(id.vars = c("state", "name", "lat", "lon"), variable.factor = FALSE)

d1$timepoint = d1$value |> hms() |> as.numeric()
d1$timepoint2 = d1$value |> str_sub(1, -4)

d1$variable = d1$variable |>
    str_replace_all("eclipse_1", "Time at which the moon first contacts the sun in this location") |>
    str_replace_all("eclipse_2", "Time at which the eclipse is at 50% in this location") |>
    str_replace_all("eclipse_3", "Time at which totality begins in this location") |>
    str_replace_all("eclipse_4", "Time at which totality ends in this location") |>
    str_replace_all("eclipse_5", "Time at which the eclipse is back to 50% in this location") |>
    str_replace_all("eclipse_6", "Time at which the moon last contacts the sun in this location")
    
    
d1$variable = d1$variable |> factor(levels = d1$variable |> unique())



library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(rnaturalearthhires)

world <- ne_countries(scale = "large", returnclass = "sf") |>
    st_transform(crs = "+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs")

lakes <- ne_download(scale = "large", type = 'lakes', category = 'physical', returnclass = "sf") |>
    st_transform(crs = "+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs")

d1_sf = d1 |>
    st_as_sf(coords = c("lon", "lat"), crs = st_crs(4326)) |>
    st_transform(crs = "+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs")


gr = ggplot() +
    
    geom_sf(
        data = world, fill = "grey90",
        linewidth = .1, color = "grey25"
    ) +
    
    geom_sf(
        data = lakes, fill = "#eafdff",
        linewidth = .1, color = "grey25"
    ) +
    
    geom_sf(
        data = d1_sf, aes(fill = timepoint), color = "grey10",
        shape = 21, stroke = .05, size = .5 # alpha = .5
    ) +
    
    scale_fill_stepsn(
        colors = c('#00429d', '#4771b2', '#73a2c6', '#a5d5d8', '#ffffe0', '#ffbcaf', '#f4777f', '#cf3759', '#93003a'),
        labels = c("17:21", "18:03", "18:45", "19:26", "20:08"),
        
        guide = guide_colorbar(
            title = "Timepoint",
            label.theme = element_text(face = "italic"),
            barheight = unit(.5, "lines"),
            barwidth = unit(16, "lines")
        )
    ) +
    
    coord_sf(xlim = c(-20e5, 30e5), ylim = c(-20e5, 10e5)) +
    
    facet_wrap(
        vars(variable), nrow = 2
    ) +
    
    theme_minimal(base_family = my_font) +
    
    theme(
        legend.position = "top",
        legend.title.position = "top",

        axis.title = element_blank(),
        axis.text = element_blank(),
        
        strip.text = element_text(face = "bold"),
        
        panel.spacing = unit(1, "lines"),
        
        panel.grid = element_blank(),
        
        plot.title    = element_text(size = 26, family = my_font, face = "bold", hjust = .5),
        plot.subtitle = element_markdown(size = 9, family = my_font, margin = margin(b = 11), hjust = .5),
        plot.caption  = element_markdown(size = 6, family = my_font, margin = margin(t = 10), hjust = 0),
        
        plot.title.position = "plot",
        plot.caption.position = "plot",
        
        plot.background = element_rect(fill = "#f9fbfe", color = NA),
        plot.margin = margin(20, 20, 20, 20)
    ) +
    
    labs(
        title = "2024 US Solar Eclipses",
        
        subtitle = paste0(
            "From the moment the moon makes its initial contact with the sun ",
            "in a given location until the final moment of contact,<br>marking the ",
            "end of the **2024 path of totality across the United States**."
        ),
        
        caption = paste0(
            "Source: <b>NASA's Scientific Visualization Studio</b> | ",
            "Graphic: <b>Nikos Pechlivanis</b>"
        )
    )


ggsave(
    plot = gr, filename = "Rplot.jpeg",
    width = 16, height = 10, units = "in", dpi = 600
)


