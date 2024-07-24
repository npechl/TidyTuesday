


rm(list = ls())
gc()

library(data.table)
library(stringr)

library(ggplot2)
library(ggtext)
library(geomtextpath)

library(extrafont)
library(paletteer)

library(tidytext)

all_countries   <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-09-12/all_countries.csv')
country_regions <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-09-12/country_regions.csv')


all_countries |> head()
country_regions |> head()

all(all_countries$country_iso3 %in% country_regions$country_iso3)

index = match(all_countries$country_iso3, country_regions$country_iso3)

all_countries$region_name  = country_regions[index]$region_name
all_countries$country_name = country_regions[index]$country_name

df = all_countries[which(
    str_detect(all_countries$region_name, "Southern Europe") & 
        hoursPerDayCombined != 0
)]

df$ylabel = ifelse(df$hoursPerDayCombined < .05, " ", df$Subcategory)

df2 = df[, by = Subcategory, .(m = mean(hoursPerDayCombined))]
df2 = df2[order(m)]

df$Subcategory = df$Subcategory |> factor(df2$Subcategory)


# plot ------------------------------------

my_font = "Jost"

gr = ggplot(data = df) +
    
    geom_col(aes(x = hoursPerDayCombined, y = Subcategory, fill = Category)) +
    
    geom_textpath(
        aes(y = Subcategory, x = 23.5, label = ylabel),
        hjust = 0, size = 1.5, upright = TRUE, family = my_font
    ) +
    
    coord_polar(theta = "x") +
    
    facet_wrap(vars(country_name), nrow = 2) +
    
    scale_x_continuous(
        limits = c(0, 24), breaks = c(.25, .5, 1, 2, 3, 6, 12, 18, 24),
        labels = c(c("15min", "30min"), paste0(c(1, 2, 3, 6, 12, 18, 24), "h")),
        minor_breaks = c(3, 9, 15, 21), trans = "sqrt"
    ) +
    
    scale_fill_manual(
        values = paletteer_d("ggsci::planetexpress_futurama"),
        guide = guide_legend(title.position = "top")
    ) +
    
    theme_minimal(base_family = my_font) +
    
    theme(
        axis.text.y = element_blank(),
        axis.title = element_blank(),
        
        strip.text = element_text(face = "bold"),
        
        legend.position = "bottom",
        
        panel.spacing = unit(1, "lines"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(linewidth = .3, color = "grey87"),
        
        plot.title    = element_text(face = "bold", size = 30, family = my_font),
        plot.subtitle = element_markdown(margin = margin(b = 20), size = 12, color = "grey20", family = my_font),
        plot.caption  = element_markdown(margin = margin(t = 20), size = 8, family = my_font),
        
        plot.title.position = "plot",
        plot.caption.position = "plot",
        
        plot.background = element_rect(fill = "#f5f5f5", color = NA),
        plot.margin = margin(20, 20, 20, 20)
    ) +
    
    labs(
        title = 'The Global Human Day',
        
        subtitle = paste0(
            "An overview of people's daily routines in **Southern Europe** over a 24-hour period"
        ),
        
        caption = paste0(
            "Source: <b>The Human Chronome Project</b> | ",
            "Graphic: <b>Nikolaos Pechlivanis</b>"
        )
    )


ggsave(
    plot = gr, filename = "Rplot.jpeg",
    width = 20, height = 10, units = "in", dpi = 600
)


















