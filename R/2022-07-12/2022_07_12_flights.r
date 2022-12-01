


rm(list = ls())
gc()


library(data.table)
library(stringr)



flights <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-07-12/flights.csv')


# library(sf)
# library(rnaturalearth)
# library(rnaturalearthdata)
# library(rgeos)

library(ggplot2)
library(ggsci)
library(ggtext)
library(tidytext)

library(extrafont)

# flights$evo = paste0(flights$STATE_NAME, ": ", flights$APT_NAME)

x = flights[, c("YEAR", "APT_NAME", "STATE_NAME", "FLT_DEP_1", "FLT_ARR_1"), with = FALSE]

colnames(x) = c("year", "airport", "state", "Departures", "Arrivals")

x = melt(x, id.vars = c("year", "airport", "state"), variable.factor = FALSE)

x = x[, by = .(year, state, variable), .(N = sum(value))]

x = x[order(year, N), ]

x = x[which(year >= 2017), ]

# x$evo = paste0(x$year, ".", x$state)
# 
# x$evo = factor(x$evo, levels = unique(x$evo))



gr = ggplot(data = x) +
    
    geom_col(aes(x = N, y = reorder_within(state, N, year), fill = variable),
             width = 0.75) +
    
    facet_wrap(vars(year), ncol = 3, scales = "free", dir = "h") +
    
    scale_x_continuous(
        expand = c(0, 0),
        limits = c(0, 2250000),
        labels = scales::comma_format(suffix = "M", scale = 1e-6),
        breaks = seq(500000, 2250000, by = 500000)
    ) +
    
    scale_y_reordered() +
    
    scale_fill_manual(values = c(
        "Departures" = "#bb0600",
        "Arrivals"   = "#00b5bb"
    )) +
    
    theme_minimal(base_family = "Century Gothic") +
    
    theme(
        legend.position = "bottom",
        legend.title = element_blank(),
        
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        
        panel.grid.major.x = element_line(color = "gray75", size = 0.2),
        panel.grid.minor.x = element_line(color = "gray80", size = 0.2),
        
        axis.title.y = element_blank(),
        axis.title.x = element_text(margin = margin(t = 10)),
        
        strip.text = element_text(face = "bold", size = 11),
        
        panel.spacing = unit(1, "lines"),
        
        plot.background = element_rect(fill = "#faf5f5", color = "#faf5f5"),
        
        plot.caption          = element_markdown(
            family = "Century Gothic",
            size = 11
        ),
        plot.caption.position = "plot",
        
        plot.title = element_markdown(
            margin = margin(t = 10, b = 10), 
            size = 17,
            family = "Century Gothic"
        ),
        plot.title.position = "plot",
        
        plot.subtitle = element_markdown(
            margin = margin(b = 10), 
            size = 14,
            color = "gray40", 
            family = "Century Gothic"
        ),
        
        plot.margin = margin(20, 20, 20, 20)
    ) +
    
    labs(
        title    = "<b>Commercial air transport in Europe over the past 6 years</b>",
        subtitle = "<i>number of flights per country</i>",
        x = "Total number of IFR movements",
        caption = paste0(
            "<i>",
            "Source: <b>Eurocontrol</b> | ",
            "Graphic: <b>Nikolaos Pechlivanis</b>",
            "</i>"
        )
    )


ggsave(
    "Rplot.pdf",
    plot = gr,
    width = 16, height = 13,
    device = cairo_pdf,
    units = "in"
)

ggsave(
    "Rplot.png",
    plot = gr,
    width = 16, height = 13,
    units = "in"
)



