


rm(list = ls())
gc()

# load libraries -----------------------------------

library(data.table)
library(stringr)

library(ggplot2)
library(ggtext)

library(colorspace)
library(extrafont)

# input data frame -----------------------------------

groundhogs <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-01-30/groundhogs.csv')
predictions <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-01-30/predictions.csv')

predictions$details           = NULL
groundhogs$source             = NULL
groundhogs$current_prediction = NULL
groundhogs$description        = NULL
groundhogs$image              = NULL

my_font = "Jost"


# plot 1 ----------------------

library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(rnaturalearthhires)

world <- ne_countries(scale = "large", returnclass = "sf") |>
    st_transform(crs = "+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs")

groundhogs_sf = groundhogs |>
    st_as_sf(
        coords = c("longitude", "latitude"),
        crs = st_crs(4326)
    ) |>
    st_transform(crs = "+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs")

# usa <- ne_countries(scale = "large", returnclass = "sf", country = "united states of america") |>
#     st_transform(crs = "+proj=gall")
# 
# canada <- ne_states(country = "Canada", returnclass = "sf") |>
#     st_transform(crs = "+proj=gall")

lakes <- ne_download(scale = "large", type = 'lakes', category = 'physical', returnclass = "sf") |>
    st_transform(crs = "+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs")

groundhogs_hl = groundhogs_sf[which.max(groundhogs_sf$predictions_count), ]
groundhogs_hl$label = "Pennsylvania's Punxsutawney Phil\nmade 128 predictions!"

library(ggrepel)

gr1 = ggplot() +
    
    geom_sf(
        data = world, fill = "grey96",
        linewidth = .1, color = "grey25"
    ) +
    
    geom_sf(
        data = lakes, fill = "#eafdff",
        linewidth = .1, color = "grey25"
    ) +
    
    geom_sf(
        data = groundhogs_sf, aes(size = predictions_count),
        shape = 21, stroke = .15,
        fill = "#832741" |> alpha(.75), 
        color = "#409ab9"
    ) +
    
    geom_text_repel(
        data = groundhogs_hl, 
        aes(label = label, geometry = geometry),
        stat = "sf_coordinates", 
        fontface = "bold", family = my_font,
        bg.color = "grey96", size = 3,
        box.padding = 1.5
    ) +
    
    scale_size_continuous(
        range = c(1, 8), 
        guide = guide_legend(
            title.position = "top",
            title = "Number of predictions"
        )
    ) +
    
    coord_sf(xlim = c(-35e5, 30e5), ylim = c(-20e5, 27e5)) +
    
    theme_minimal(base_family = my_font) +
    
    theme(
        panel.background = element_rect(fill = "#BFC9E9", color = NA),
        
        legend.position = "bottom",
        legend.justification = "left",
        
        axis.title = element_blank(),
        axis.text = element_blank(),
        
        plot.title    = element_text(face = "bold", size = 26, margin = margin(b = 5), family = "Jost"),
        plot.subtitle = element_markdown(margin = margin(b = 25), size = 10, color = "grey25", family = "Jost"),
        plot.caption  = element_markdown(margin = margin(t = 25), size = 7, family = "Jost"),
        
        plot.title.position = "plot",
        plot.caption.position = "plot",
        
        plot.background = element_rect(fill = "#f9fbfe", color = NA),
        plot.margin = margin(20, 20, 20, 20)
    ) +
    
    labs(
        title = 'Groundhog predictions',
        
        subtitle = paste0(
            "Groundhog locations in USA and Canada, along with their total predictions."
        ),
        
        caption = paste0(
            "Source: <b>groundhog-day.com</b> | ",
            "Graphic: <b>Nikos Pechlivanis</b>"
        )
    )



# plot 2 ---------------------------

library(ggh4x)

df = predictions |> merge(groundhogs, by = "id")

df = df[which(!is.na(shadow) & year >= 1980)]

df2 = df[, by = .(year, shadow), .N]

df2$cluster = ifelse(df2$shadow, "6 more weeks of winter", "spring will come early")

df3 = df2 |> dcast(year ~ cluster, value.var = "N", fill = 0)

df2 = df3 |> melt(id.vars = "year", variable.factor = FALSE, value.factor = FALSE)

gr2 = df2 |>
    ggplot(aes(year, value)) +
    
    stat_difference(
        data = df3, inherit.aes = FALSE,
        aes(x = year, ymax = `6 more weeks of winter`, ymin = `spring will come early`),
        alpha = .3, show.legend = FALSE
    ) +
    
    geom_line(aes(color = variable), linewidth = .55) +
    
    scale_color_manual(
        values = c(
            "spring will come early" = "#39a2bf",
            "6 more weeks of winter" = "#bf395f"
        )
    ) +
    
    theme_minimal(base_family = my_font) +
    
    theme(
        panel.grid = element_blank(),
        
        legend.position = c(.25, .75),
        legend.title = element_blank(),
        
        axis.title.x = element_blank(),
        axis.title.y = element_text(margin = margin(r = 10), face = "bold"),
        
        axis.line = element_line(linewidth = .35),
        axis.ticks = element_line(linewidth = .35)
    ) +
    
    labs(
        y = "Total number of predictions"
    )
    

# patchwork ---------------------

library(patchwork)
    
multi = gr1 + inset_element(gr2, .01, .01, .45, .35, align_to = "plot", clip = FALSE)


ggsave(
    plot = multi, filename = "Rplot.jpeg",
    width = 12, height = 11, units = "in", dpi = 600
)



