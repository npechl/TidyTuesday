
rm(list = ls())
gc()


library(data.table)
library(stringr)


library(ggplot2)
library(gganimate)
library(ggtext)

library(tidycensus)
library(maps)

library(transformr)

tuesdata = tidytuesdayR::tt_load('2022-06-14')

# drought      = setDT(tuesdata$drought)
drought_fips = setDT(tuesdata$`drought-fips`)

rm(tuesdata)


# part 1 -----------------------------------------

data("county.fips")

county.fips = setDT(county.fips)

county.fips$fips = as.character(county.fips$fips)
county.fips$fips = ifelse(
    str_length(county.fips$fips) == 5,
    county.fips$fips,
    paste0("0", county.fips$fips)
)

index                 = match(drought_fips$FIPS, county.fips$fips)
drought_fips$polyname = county.fips[index, ]$polyname

drought_fips = drought_fips[which(!is.na(drought_fips$polyname)), ]


# part 2 ----------------------------------

state  = map_data("state")
county = map_data("county")

state  = setDT(state)
county = setDT(county)

drought_fips$subregion = str_split(drought_fips$polyname, "\\,", simplify = TRUE)[,2]

county$polyname = paste0(county$region, ",", county$subregion)


rm(county.fips, drought, index)
gc()

drought_fips = merge(
    county, drought_fips, 
    by              = "polyname", 
    allow.cartesian = TRUE, 
    all             = TRUE
)

drought_fips = drought_fips[order(date), ]

drought_fips = drought_fips[which(!is.na(drought_fips$date)), ]
drought_fips = drought_fips[which(!is.na(drought_fips$long)), ]
drought_fips = drought_fips[which(!is.na(drought_fips$lat)), ]

drought_fips = drought_fips[, c(
    "long", 
    "lat",
    "group",
    "order",
    "region",
    "subregion.x",
    "DSCI",
    "date"
), with = FALSE]

colnames(drought_fips) = c(
    "long", 
    "lat",
    "group",
    "order",
    "region",
    "subregion",
    "DSCI",
    "date"
)

rm(county)
gc()

# plotting ------------------------------

# fwrite(drought_fips, "county-drought.tsv", 
#        row.names = FALSE, quote = FALSE, sep = "\t")
# 
# drought_fips = fread("county-drought.tsv")

drought_fips_sub = drought_fips[which(drought_fips$date >= "2018-01-01"), ]

static_plot = ggplot(data = drought_fips_sub,
                     aes(x = long, y = lat, group = group)) +
    
    geom_polygon(
        data  = drought_fips_sub, aes(fill = DSCI), 
        color = "gray80",
        alpha = 0.3,
    ) +
    
    geom_polygon(
        data  = state, aes(x = long, y = lat, group = group),
        fill  = NA,
        color = "black",
        alpha = 0.3,
    ) +
    
    scale_fill_gradientn(
        limits = c(0, 500),
        colors = c("gray95", "red4"),
        guide  = guide_colorbar(
            title.position = "top",
            barwidth       = 12,
            barheight      = 0.5
        )
    ) +
    
    labs(
        
        title = paste0(
            "<b>County Drought Severity and Coverage Index over time",
            " throughout the United States</b>"
        ),
        
        subtitle = "<b>{current_frame}</b>",
        caption  = paste0(
            "Source: <b>National Integrated Drought Information System</b>",
            " | ",
            "Graphic: <b>Nikolaos Pechlivanis</b>"
        )
    ) +
    
    theme_void() +
    
    theme(
        legend.position = "bottom",
        
        plot.title    = element_textbox(margin = margin(t = 10, l = 20, r = 20, b = 5)),
        plot.subtitle = element_textbox(margin = margin(t = 5, b = 20, l = 20, r = 20)),
        plot.caption  = element_textbox(margin = margin(t = 20, b = 10, l = 20, r = 20),
                                        hjust = 1),
        
        plot.title.position   = "plot",
        plot.caption.position = "plot",
        
        plot.margin = margin(25, 25, 25, 25)
    ) +

    coord_map(clip = "off")
    
animate_plot = static_plot + transition_manual(frames = date)

animate(animate_plot, height = 10, width = 12, units = "in", res = 150, fps = 20)
anim_save("DSCI_2018-2022.gif")


