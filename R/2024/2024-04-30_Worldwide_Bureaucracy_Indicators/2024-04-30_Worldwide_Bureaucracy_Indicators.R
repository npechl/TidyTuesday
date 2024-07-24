


rm(list = ls())
gc()

# load libraries -----------------------------------

library(data.table)
library(stringr)

library(ggplot2)
library(ggtext)

library(ggh4x)


library(paletteer)
library(colorspace)
library(extrafont)

my_font = "Jost"

# input data frame -----------------------------------

wwbi_data = fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-04-30/wwbi_data.csv')
wwbi_series = fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-04-30/wwbi_series.csv')
wwbi_country = fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-04-30/wwbi_country.csv')


wwbi = wwbi_data |>
    merge(wwbi_series, by = "indicator_code") |>
    merge(wwbi_country, by = "country_code")

rm(wwbi_data, wwbi_country, wwbi_series)

wwbi$special_notes = NULL

wwbi$country_code |> unique() |> length()

t = wwbi[, by = .(year, indicator_code), .(N = country_code |> unique() |> length())]
t = t[order(year, -N)]
t = t[, by = year, head(.SD, 1)]
t$indicator_code |> unique()

wwbi$income_group = wwbi$income_group |>
    factor(
        levels = c(
            "High income",
            "Upper middle income",
            "Lower middle income",
            "Low income"
        )
    )

wwbi$region = wwbi$region |>
    factor(
        levels = c(
            "Europe & Central Asia",
            "Middle East & North Africa",
            "East Asia & Pacific",
            "South Asia",
            "Sub-Saharan Africa",
            "Latin America & Caribbean",
            "North America"
        )
    )

gr = wwbi[which(indicator_code == "BI.WAG.TOTL.GD.ZS" & !is.na(income_group))] |>
    ggplot(aes(year, value)) +
    geom_line(aes(group = country_code, color = income_group), linewidth = .25, alpha = .3) +
    
    geom_smooth(aes(color = income_group, fill = income_group), alpha = .1, linewidth = .75, lineend = "round") +
    
    scale_y_continuous(
        transform = "log2", limits = c(0.5, 46), expand = c(0, 0),
        breaks = c(4, 6, 8, 12, 16, 32)
    ) +
    
    scale_fill_manual(values = paletteer_d("ggsci::planetexpress_futurama")) +
    scale_color_manual(values = paletteer_d("ggsci::planetexpress_futurama")) +
    
    facet_wrap2(vars(region), nrow = 2, axes = "all") +
    
    coord_cartesian(expand = TRUE) +
    
    theme_minimal(base_family = my_font) +
    
    theme(
        legend.position = c(.9, .25),
        
        panel.spacing = unit(1, "lines"),
        
        panel.grid.major = element_line(linewidth = .35, color = "grey85"),
        panel.grid.minor = element_line(linewidth = .25, color = "grey85", linetype = "dashed"),
        
        strip.text = element_text(margin = margin(b = 10), face = "bold"),
        
        axis.title.x = element_blank(),
        axis.title.y = element_text(margin = margin(r = 10)),
        
        axis.text.x = element_text(size = 8),
        
        plot.title    = element_text(size = 26, family = my_font, face = "bold"),
        plot.subtitle = element_markdown(size = 9, family = my_font, margin = margin(b = 15)),
        plot.caption  = element_markdown(size = 6, family = my_font, margin = margin(t = 15)),
        
        plot.title.position = "plot",
        plot.caption.position = "plot",
        
        plot.background = element_rect(fill = "#f5f5f5", color = NA),
        plot.margin = margin(20, 20, 20, 20)
    ) +
    
    labs(
        fill = "Income group",
        color = "Income group",
        
        y = "Wage bill as a percentage of GDP",
        
        title = "Worldwide Bureaucracy Indicators",
        subtitle = paste0(
            "Percentage of GDP allocated to wages across regions and income groups from 2000 to 2020."
        ),
        
        caption = paste0(
            "Source: <b>NASA's Scientific Visualization Studio</b> | ",
            "Graphic: <b>Nikos Pechlivanis</b>"
        )
    )


ggsave(
    plot = gr, filename = "Rplot.jpeg",
    width = 10, height = 8, units = "in", dpi = 600
)















