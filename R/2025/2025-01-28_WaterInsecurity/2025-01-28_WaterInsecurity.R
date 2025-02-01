


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

# load data ----------------------------------- 

d0_2022 <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-01-28/water_insecurity_2022.csv')
d0_2023 <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-01-28/water_insecurity_2023.csv')

# geometry -------------------------------------------

library(sf)
library(rnaturalearth)

d0_2022$geometry <- NULL
d0_2023$geometry <- NULL


d0_2022$percent_lacking_plumbing <- d0_2022$plumbing / d0_2022$total_pop
d0_2023$percent_lacking_plumbing <- d0_2023$plumbing / d0_2023$total_pop

# # The geometry columns are saved as text with the code to reproduce them.
# d0_2022 <- d0_2022 |> dplyr::mutate(geometry = purrr::map(geometry, \(geo) { eval(parse(text = geo)) }))
# d0_2023 <- d0_2023 |> dplyr::mutate(geometry = purrr::map(geometry, \(geo) { eval(parse(text = geo)) }))

# d0 <- rbind(d0_2022, d0_2023)

# rm(d0_2022, d0_2023)

counties_2022 <- tigris::counties(year = 2022, class = "sf", resolution = "20m") |> st_as_sf(crs = 4269) |> st_transform(crs = "+proj=gs48")
counties_2023 <- tigris::counties(year = 2023, class = "sf", resolution = "20m") |> st_as_sf(crs = 4269) |> st_transform(crs = "+proj=gs48")

# extract county names -------------------------------------

d0_2022$county_name <- d0_2022$name |> str_split_i("\\,", 1)
d0_2023$county_name <- d0_2023$name |> str_split_i("\\,", 1)

d0_2022$state_name <- d0_2022$name |> str_split_i("\\,", 2) |> str_squish()
d0_2023$state_name <- d0_2023$name |> str_split_i("\\,", 2) |> str_squish()


d1_2022 <- d0_2022 |> merge(counties_2022, by.x = "county_name", by.y = "NAMELSAD") |> st_sf(sf_column_name = "geometry")
d1_2023 <- d0_2023 |> merge(counties_2023, by.x = "county_name", by.y = "NAMELSAD") |> st_sf(sf_column_name = "geometry")

states_to_exclude <- c("Alaska", "Hawaii", "Puerto Rico")

d1_2022 <- d1_2022[which(!(d1_2022$state_name %in% states_to_exclude)), ]
d1_2023 <- d1_2023[which(!(d1_2023$state_name %in% states_to_exclude)), ]

index_x <- which(colnames(d1_2022) %in% colnames(d1_2023))
index_y <- which(colnames(d1_2023) %in% colnames(d1_2022))

d1_2022 <- d1_2022[, index_x]
d1_2023 <- d1_2023[, index_y]

df <- rbind(d1_2022, d1_2023)

# clean environment ------------------------

rm(counties_2022, counties_2023, d0_2022, d0_2023, d1_2022, d1_2023)
gc()

usa <- ne_countries(scale = "large", country = "United States of America") |> st_transform(crs = "+proj=gs48")

# plot -------------------------

my_col <- c('#ffbeb2', '#fb927e', '#f26250', '#d43942', '#ae123a')

gr <- ggplot() +
    
    geom_sf(data = usa, fill = "grey90", color = "grey50", linewidth = .1) +
    
    geom_sf(data = df, aes(fill = percent_lacking_plumbing), linewidth = .1, color = "grey90") +
    
    scale_fill_stepsn(transform = scales::pseudo_log_trans(sigma = .0001, base = 10),
                      colors = my_col, # paletteer_c("ggthemes::Red", 5),
                      labels = scales::percent,
                      breaks = c(.00001, .0001, .001, .01),
                      guide = guide_colorsteps(
                          barwidth = unit(16, "lines"),
                          barheight = unit(.5, "lines")
                      )) +
    
    facet_wrap(vars(year), ncol = 2) +
    
    coord_sf(xlim = c(-2500000, 2200000), ylim = c(-1473267, 1300000)) +
    
    theme_minimal(base_family = my_font) +
    
    theme(
        legend.position = "bottom",
        legend.title.position = "top",
        legend.title = element_text(hjust = .5, size = 10),
        legend.text = element_text(size = 8),
        legend.margin = margin(t = 10),
        
        axis.text = element_text(size = 8),
        strip.text = element_text(face = "bold"),
        
        panel.grid = element_line(linewidth = .25, linetype = "dashed", lineend = "round", color = "grey75"),
        panel.spacing = unit(1, "lines"),
        
        plot.title    = element_text(size = 30, family = my_font, face = "bold", hjust = .5),
        plot.subtitle = element_markdown(size = 9, family = my_font, margin = margin(b = 11), hjust = .5),
        plot.caption  = element_markdown(size = 6, family = my_font, margin = margin(t = 10), hjust = .5),
        
        # plot.title.position = "plot",
        # plot.caption.position = "plot",
        
        plot.background = element_rect(fill = "#def3f6", color = NA),
        plot.margin = margin(20, 20, 20, 20)
    ) +
    
    labs(
        fill = "The percent of population lacking plumbing facilities",
        
        title = "Water Insecurity",
        
        subtitle = paste0(
            "The percentage of the U.S. population without access to plumbing facilities in 2022 and 2023."
        ),
        
        caption = paste0(
            "Source: <b>Mapping water insecurity in R with tidycensus</b> | ",
            "Graphic: <b>Nikos Pechlivanis</b>"
        )
    )



ggsave(
    plot = gr, filename = "Rplot.png",
    width = 16, height = 8, units = "in", dpi = 600
)




