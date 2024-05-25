


rm(list = ls())
gc()

# load libraries -----------------------------------

library(data.table)
library(stringr)

library(ggplot2)
library(ggtext)

library(ggpointdensity)
library(ggdensity)
library(ggh4x)


library(paletteer)
library(colorspace)
library(extrafont)

my_font = "Jost"

# input data frame ----------------------------------- 

q <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-05-21/emissions.csv')



gr <- q |>
    ggplot(aes(year, total_emissions_MtCO2e)) +
    
    geom_pointdensity(shape = 19, size = .5) +
    
    geom_hdr_lines(linewidth = .25, color = "#636396") +
    
    scale_y_continuous(transform = "log10", 
                       # breaks = c(.0001, .001, .01, .1, 1, 10, 100, 1000, 10000), 
                       expand = c(0, 0),
                       breaks = scales::trans_breaks("log10", function(x) 10^x),
                       labels = scales::trans_format("log10", scales::math_format(10^.x))) +
    scale_x_continuous(expand = c(0, 0)) +
    
    scale_color_stepsn(
        colors = c('#089392', '#3ea88e', '#6bba8a', '#9acb89', '#e9b278', '#e59472', '#dd7775', '#cf597e'), # paletteer_c("grDevices::Temps", 8),
        breaks = c(50, 100, 150, 200, 250, 300, 350),
        guide = guide_colorsteps(
            barwidth = unit(16, "lines"),
            barheight = unit(.5, "lines")
        )
    ) +
    
    facet_wrap2(vars(commodity), nrow = 3, axes = "all") +
    
    theme_minimal(base_family = my_font) +
    
    theme(
        legend.position = "bottom",
        legend.justification = "left",
        legend.title.position = "top",
        
        panel.spacing = unit(1, "lines"),
        
        panel.grid.major = element_line(linewidth = .45, color = "grey85", lineend = "round"),
        panel.grid.minor = element_line(linewidth = .35, color = "grey85", linetype = "dotted", lineend = "round"),
        
        axis.title.x = element_blank(),
        axis.title.y = element_text(margin = margin(r = 10)),
        
        strip.text = element_text(face = "bold"),
        
        plot.title    = element_text(size = 26, family = my_font, face = "bold"),
        plot.subtitle = element_markdown(size = 9, family = my_font, margin = margin(b = 15)),
        plot.caption  = element_markdown(size = 7, family = my_font, margin = margin(t = 15)),

        plot.title.position = "plot",
        plot.caption.position = "plot",
        
        plot.background = element_rect(fill = "#f5f5f5", color = NA),
        plot.margin = margin(20, 20, 20, 20)
    ) +
    
    labs(
        y = "Total Emissions (MtCO2e)",
        color = "Number of neighboring points (entities)",
        alpha = "Probs",
        
        title = "Carbon Majors Emissions Data",
        subtitle = paste0(
            "Annual carbon emissions distribution by production commodity."
        ),
        
        caption = paste0(
            "Source: <b>Carbon Majors</b> | ",
            "Graphic: <b>Nikos Pechlivanis</b>"
        )
    )


ggsave(
    plot = gr, filename = "Rplot.png", 
    width = 9, height = 9, units = "in", dpi = 600
)
