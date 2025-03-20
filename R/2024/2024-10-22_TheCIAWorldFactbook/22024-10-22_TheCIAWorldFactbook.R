


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

d <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-10-22/cia_factbook.csv')

# 2 -------------

library(ggrepel)

gr = d |> 
    ggplot(aes(birth_rate, death_rate)) +
    
    geom_point(
        aes(fill = population_growth_rate, color = population_growth_rate), 
        shape = 21, size = 2, stroke = .25
    ) +
    
    geom_abline(intercept = 0, slope = 1, linewidth = .5, color = "#ffbf00",
                linetype = "dashed", lineend = "round") +
    
    annotate(
        "text", x = 15.5, y = 15, label = "Birth Rate higher than Death rate",
        angle = 45, family = "Anton", size = 2, hjust = .5, vjust = .5, color = "#93003a" |> darken(.25)
    ) +
    
    annotate(
        "text", x = 15.5, y = 16, label = "Death Rate higher than Birth rate",
        angle = 45, family = "Anton", size = 2, hjust = .5, vjust = .5, color = "#00429d" |> darken(.25)
    ) +
    
    geom_text_repel(
        aes(label = country), max.overlaps = Inf, 
        size = 2, segment.size = .15, segment.linetype = "dotted",
        color = "grey15", bg.color = "white", bg.r = .075,
        fontface = "bold", family = my_font
    ) +
    
    scale_color_stepsn(
        colors = c('#00429d', '#5681b9', '#93c4d2', '#ffa59e', '#dd4c65', '#93003a') |> darken(.35),
        breaks = c(-5, -2.5, 0, 2.5, 5),
        guide = "none"
    ) +
    
    scale_fill_stepsn(
        colors = c('#00429d', '#5681b9', '#93c4d2', '#ffa59e', '#dd4c65', '#93003a') |> darken(.15),
        breaks = c(-5, -2.5, 0, 2.5, 5),
        
        guide = guide_colorsteps(barwidth = unit(16, "lines"), barheight = unit(.35, "lines"))
    ) +
    
    scale_x_continuous(limits = c(5, 50)) +
    scale_y_continuous(limits = c(0, 20)) +
    
    coord_equal() +
    
    theme_minimal(base_family = my_font) +
    
    theme(
        legend.position = "top",
        legend.title.position = "top",
        legend.title = element_markdown(hjust = .5),
        
        panel.grid.major = element_line(linewidth = .45, linetype = "dashed", lineend = "round", color = "grey80"),
        panel.grid.minor = element_line(linewidth = .25, linetype = "dashed", lineend = "round", color = "grey80"),
        
        axis.title.x = element_markdown(margin = margin(t = 10)),
        axis.title.y = element_markdown(margin = margin(r = 10)),
        
        plot.title    = element_text(face = "bold", size = 20, margin = margin(b = 5), family = "Jost", hjust = .5),
        plot.subtitle = element_markdown(margin = margin(b = 15), size = 10, color = "grey25", family = "Jost", hjust = .5),
        plot.caption  = element_markdown(margin = margin(t = 20), size = 8, family = "Jost", hjust = .5),
        
        plot.background = element_rect(fill = "#f6fcfe", color = NA),
        plot.margin = margin(20, 20, 20, 20)
    ) +
    
    labs(
        x = "**Birth rate** (number of live births per 1,000 people)",
        y = "**Death rate** (number of deaths per 1,000 people)",
        
        fill = "**Population growth rate** (multiplier)",
        
        title = "The CIA World Factbook",
        
        subtitle = paste0(
            "Population growth rates, including birth and death rates, across 265 global entities, as reported by the CIA World Factbook."
        ),
        
        caption = paste0(
            "Source: <b>CIA Factbook, Country Comparisons, 2014</b> | ",
            "Graphic: <b>Nikos Pechlivanis</b>"
        )
    )


ggsave(
    plot = gr, filename = "Rplot.png",
    width = 16, height = 10, units = "in", dpi = 600
)







