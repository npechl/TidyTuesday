


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

my_font = "Roboto"

library(ggh4x)
library(ggdist)
library(ggforce)
library(shadowtext)

# load data ----------------------------------- 

d0 <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-07-29/movies.csv')
d1 <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-07-29/shows.csv')


for(i in names(d0)) if( is.character(d0[[i]]) ) d0[[i]] <- d0[[i]] |> str_squish()
for(i in names(d1)) if( is.character(d1[[i]]) ) d1[[i]] <- d1[[i]] |> str_squish()

d2 <- list("Netflix<br>**Movies**" = d0, "Netflix<br>**Shows**" = d1) |> rbindlist(idcol = "type") |> na.omit()

d2$available_globally <- ifelse(
    d2$available_globally == "Yes",
    "*Is the Movie/Show available globally?* **Yes**",
    "*Is the Movie/Show available globally?* **No**"
)

d2$year <- d2$release_date |> lubridate::year()

d3 <- d2[, by = .(available_globally, type), .(y = mean(views))]

gr <- d2 |> 
    ggplot(aes(as.character(year), views)) +
    
    geom_point(shape = 16, alpha = .5, color = "#4d69a9" |> lighten(.25), position = position_jitternormal(sd_y = 0, sd_x = .1)) +
    
    geom_hline(data = d3, aes(yintercept = y), linewidth = 1, color = "grey96", lineend = "round") +
    geom_hline(data = d3, aes(yintercept = y), linewidth = .5, color = "#a1b357", lineend = "round") +
    
    geom_boxplot(outlier.shape = NA, width = .25, color = "#5f4da9" |> darken(.5), fill = "grey90", lineend = "round", linewidth = .25) +
    
    geom_shadowtext(data = d3, 
                    aes(x = 1.5, y = y, label = paste0("avg ~ ", y |> round() |> scales::comma())),
                    hjust = 0, position = position_nudge(y = .1), family = "Jost",, fontface = "bold",
                    size = 2.5, color = "#5f4da9" |> darken(.5), bg.color = "white", bg.r = .075) +
    
    facet_grid2(cols = vars(available_globally), rows = vars(type), axes = "all") +
    
    scale_x_discrete(breaks = seq(2010, 2025, by = 5) |> as.character()) +
    
    scale_y_continuous(transform = "log10",
                       breaks = scales::trans_breaks("log10", function(x) 10^x),
                       labels = scales::trans_format("log10", scales::math_format(10^.x))) +
    
    theme_minimal(base_family = my_font) +
    
    theme(
        legend.position = "bottom",
        legend.title.position = "top",
        
        strip.text.x.top = element_markdown(margin = margin(b = 5), size = 10),
        strip.text.y.right = element_markdown(margin = margin(l = 5), size = 10, angle = 0, hjust = 0),
        
        # axis.line.x = element_line(lineend = "round"),
        # axis.ticks.x = element_line(lineend = "round"),
        
        axis.title.x.bottom = element_markdown(margin = margin(t = 10), size = 10),
        axis.title.y.left = element_markdown(margin = margin(r = 10), size = 10),
        
        axis.text = element_text(size = 8.5),
        
        panel.spacing = unit(2, "lines"),
        panel.grid.major = element_line(lineend = "round", color = "grey85"),
        panel.grid.minor = element_line(linetype = "dashed", lineend = "round", color = "grey85"),
        
        plot.title    = element_text(size = 30, family = "Roboto Black", face = "bold", hjust = 0),
        plot.subtitle = element_markdown(size = 11, family = "Roboto Light", margin = margin(b = 15, t = 5), hjust = 0),
        plot.caption  = element_markdown(size = 8, family = my_font, margin = margin(t = 15), hjust = 1),
        
        plot.title.position = "plot",
        
        plot.background = element_rect(fill = "#def3f6", color = NA),
        plot.margin = margin(20, 20, 20, 20)
    ) +
    
    labs(
        x = "**Release date**",
        y = "**Hours viewed** scaled to movie/show **runtime**",
        
        title = "What have we been watching on Netflix?",
        
        subtitle = paste0(
            "Trends in Netflix viewing hours from 2010 to 2025."
        ),
        
        caption = paste0(
            "Source: <b>Netflix Engagement Reports</b> | ",
            "Graphic: <b>Nikos Pechlivanis</b>"
        )
    )


ggsave(
    plot = gr, filename = "Rplot.png", dpi = 600, device = grDevices::png,
    width = 10, height = 10, units = "in"
)




