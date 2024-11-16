


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

d0 <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-11-12/countries.csv')
d1 <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-11-12/country_subdivisions.csv')
# d2 <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-11-12/former_countries.csv')

# clean "Namibia" -------------

d0$alpha_2 <- ifelse(d0$name == "Namibia", "NA", d0$alpha_2)
d1$alpha_2 <- ifelse(is.na(d1$alpha_2), "NA", d1$alpha_2)

# merge data.tables ------------------------------------

d2 <- d0 |> merge(d1, by = "alpha_2", suffixes = c(".parent", ".child"))

# d2[, by = name.parent, N := name.child |> unique() |> length()]

d3 <- d2[, by = name.parent, .(N = name.child |> unique() |> length())]





library(treemapify)

my_col <- c('#33608c', '#a16e94', '#de8b7f', '#f6ba57', '#dd5c45', '#cb3e43', '#b81840')

d3$name2 <- paste0(d3$name.parent, "\n", d3$N)

gr <- d3 |>
    ggplot(aes(area = N)) +
    geom_treemap(aes(fill = N), layout = "squarified", color = "#f8f2f9", start = "topleft", radius = unit(2, "pt")) +
    
    geom_treemap_text(
        aes(label = name2), layout = "squarified", start = "topleft", 
        grow = FALSE, reflow = TRUE, min.size = .5, 
        family = "Bebas Neue", color = "#f8f2f9"
    ) +
    
    scale_fill_stepsn(
        colors = my_col, # paletteer_c("ggthemes::Sunset-Sunrise Diverging", 7), 
        breaks = c(4, 8, 16, 32, 64, 124), transform = "log2",
        guide = guide_colorsteps(barwidth = unit(16, "lines"), barheight = unit(.5, "lines"))
    ) +
    
    theme_void(base_family = my_font) +
    
    theme(
        legend.position = "bottom",
        legend.title.position = "top",
        legend.title = element_text(hjust = .5),
        
        plot.title    = element_text(face = "bold", size = 20, margin = margin(b = 5), family = "Jost", hjust = .5),
        plot.subtitle = element_markdown(margin = margin(b = 10), size = 11, color = "grey25", family = "Jost", hjust = .5),
        plot.caption  = element_markdown(margin = margin(t = 10), size = 8, family = "Jost", hjust = .5),
        
        plot.background = element_rect(fill = "#f8f2f9", color = NA),
        plot.margin = margin(20, 20, 20, 20)
    ) +
    
    labs(
        fill = "Number of subdivisions",
        title = "ISO Country Codes",
        
        subtitle = paste0(
            "Counts of country subdivisions (ISO 3166-2)"
        ),
        
        caption = "Source: **{ ISOcodes } R package** | Graphic: **Nikos Pechlivanis**"
    )


ggsave(
    plot = gr, filename = "Rplot.png",
    width = 9, height = 10, units = "in", dpi = 600
)






