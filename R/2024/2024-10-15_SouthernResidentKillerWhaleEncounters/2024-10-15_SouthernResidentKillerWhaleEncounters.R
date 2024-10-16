


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

my_font = "Jost"

# 1 ----------------------------------- 

d <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-10-15/orcas.csv')


d$observers         <- NULL
d$encounter_summary <- NULL
d$link              <- NULL

# 2 -----------------------------

index <- which(
    !is.na(d$year) & 
        !is.na(d$begin_latitude) & 
        !is.na(d$begin_longitude) & 
        !is.na(d$end_latitude) & 
        !is.na(d$end_longitude)
)


d1 <- d[index]


index <- which(
    d1$begin_latitude == d1$end_latitude &
        d1$begin_longitude == d1$end_longitude
)

d2 <- d1[index]
d1 <- d1[-index]


# world --------------------------

library(sf)
library(rnaturalearth)
library(rnaturalearthdata)

world <- ne_countries(scale = "large", returnclass = "sf") |>
    st_transform(crs = "+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs")

# convert coordinates system ----------------------

begin <- d1 |>
    st_as_sf(coords = c("begin_longitude", "begin_latitude"), crs = st_crs(4326)) |>
    st_transform(crs = "+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs") |>
    st_coordinates()

end <- d1 |>
    st_as_sf(coords = c("end_longitude", "end_latitude"), crs = st_crs(4326)) |>
    st_transform(crs = "+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs") |>
    st_coordinates()

d1$begin_longitude_t <- begin[, 1]
d1$begin_latitude_t  <- begin[, 2]

d1$end_longitude_t <- end[, 1]
d1$end_latitude_t  <- end[, 2]

# plot -------------------------

gr <- d1 |>
    ggplot() +
    
    geom_sf(
        data = world, fill = "grey90", 
        linewidth = .1, color = "grey"
    ) +
    
    coord_sf(xlim = c(-18.5e5, -16e5), ylim = c(5.5e5, 8e5)) +
        
    geom_curve(
        aes(x = begin_longitude_t, y = begin_latitude_t, xend = end_longitude_t, yend = end_latitude_t),
        arrow = arrow(length = unit(0.01, "inches"), type = "closed"),
        lineend = "round", curvature = 0.15, linewidth = .2,
        color = "#d81d45" |> darken(.5)
    ) +
    
    facet_wrap(vars(year), nrow = 2) +
    
    theme_minimal(base_family = my_font) +
    
    theme(
        legend.position = "none",
        
        axis.text = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_markdown(margin = margin(t = 10), color = "grey25", size = 10),
        
        strip.text = element_text(face = "bold"),
        
        panel.spacing = unit(1, "lines"),
        panel.background = element_rect(fill = "#a0daf2", color = NA),
        
        plot.title    = element_text(face = "bold", size = 20, margin = margin(b = 5), family = "Jost", hjust = .5),
        plot.subtitle = element_markdown(margin = margin(b = 15), size = 10, color = "grey25", family = "Jost", hjust = .5),
        plot.caption  = element_markdown(margin = margin(t = 20), size = 8, family = "Jost", hjust = .5),

        plot.background = element_rect(fill = "#f6fcfe", color = NA),
        plot.margin = margin(20, 20, 20, 20)
    ) +
    
    labs(     

        x = paste0(
            "An encounter refers to any time we observe killer whales (orcas), from one of CWR's research boats or land, ",
            "where at least one individual is identified and photographed.<br>Typically, 2 - 4 staff are involved in an encounter. ",
            "Once we come into contact with whales (ie. within distance of identifying individuals by sight) we have begun our encounter.<br>",
            "During an encounter, our main goal is to photograph every individual present from both the left and right side."
        ),

        title = "Southern Resident Killer Whale Encounters",
        
        subtitle = paste0(
            "Encounters of **Southern Resident Killer Whales** within their critical habitat **in the Pacific Northwest’s Salish Sea**."
        ),
        
        caption = paste0(
            "Source: <b>Center for Whale Research (CWR)</b> | ",
            "Graphic: <b>Nikos Pechlivanis</b>"
        )
    )


# save plot ------------------------------

ggsave(
    plot = gr, filename = "Rplot.png", dpi = 600,
    width = 16, height = 10, units = "in"
)


