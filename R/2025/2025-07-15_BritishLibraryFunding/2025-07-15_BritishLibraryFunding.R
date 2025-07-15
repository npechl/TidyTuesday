


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

d0 <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-07-15/bl_funding.csv')


d0$year_2000_gbp_millions     <- NULL
d0$percentage_of_y2000_income <- NULL
d0$gia_as_percent_of_peak_gia <- NULL


p1 <- d0 |>
    subset(select = c("year",
                      "gia_gbp_millions",
                      "voluntary_gbp_millions",       
                      "investment_gbp_millions", 
                      "services_gbp_millions", 
                      "other_gbp_millions")) |>
    melt(id.vars = "year", variable.factor = F, value.factor = F)

p2 <- d0 |>
    subset(select = c("year",
                      "gia_y2000_gbp_millions", 
                      "voluntary_y2000_gbp_millions", 
                      "investment_y2000_gbp_millions", 
                      "services_y2000_gbp_millions",
                      "other_y2000_gbp_millions")) |>
    melt(id.vars = "year", variable.factor = F, value.factor = F)

p2$variable <- p2$variable |> str_remove("_y2000")

p3 <- p1 |> merge(p2, by = c("year", "variable"), suffixes = c("", ".y2000"))

library(ggh4x)
library(ggstream)

# y2000 <- p2[year == "2000"]
# 
# y2000 <- y2000[order(-value)]
# 
# p2$variable <- p2$variable |> factor(levels = y2000$variable)


p2$variable <- p2$variable |>
    str_replace("gia_gbp_millions", "Grant-in-aid") |>
    str_replace("investment_gbp_millions", "Investment") |>
    str_replace("other_gbp_millions", "Other") |>
    str_replace("services_gbp_millions", "Services") |>
    str_replace("voluntary_gbp_millions", "Voluntary")

gr1 <- p2 |>
    ggplot(aes(x = year, value)) +
    geom_col(aes(fill = variable), width = .9, position = "stack", color = "grey25") +
    
    # scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0), limits = c(0, 150), breaks = seq(20, 150, by = 20)) +
    
    scale_fill_manual(values = paletteer_d("ggthemes::Tableau_10"),
                      guide = guide_legend(nrow = 2)) +
    
    theme_minimal(base_family = my_font) +
    
    theme(
        legend.title = element_markdown(angle = 0, hjust = 0),
        legend.title.position = "top",
        legend.direction = "horizontal",
        legend.position = "inside",
        legend.position.inside = c(.80, .90),
        legend.box.background = element_rect(fill = "white" |> alpha(.75), color = NA),
        
        axis.line.x = element_line(lineend = "round", linewidth = 1),
        axis.ticks.x = element_line(lineend = "round", linewidth = 1),
        
        axis.title.y = element_markdown(margin = margin(r = 10)),
        axis.title.x = element_blank(),
        axis.text.x = element_text(face = "bold"),
        
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_line(linetype = "dashed", lineend = "round", color = "grey"),
        panel.grid.major.y = element_line(lineend = "round", color = "grey")
    ) +
    
    labs(fill = "**Funding sources** in Y2000 GBP", y = "**Total reported funding** adjusted to Y2000 GBP")


gr2 <- p2 |>
    ggplot(aes(x = year, value)) +
    
    geom_area(aes(fill = variable), alpha = .35) +
    geom_line(aes(color = variable), linewidth = .75) +
    geom_segment(aes(x = year, xend = year, y = 0, yend = value, color = variable), linewidth = .75) +
    
    scale_x_continuous(expand = c(0, 0), breaks = c(2000, 2010, 2020)) +
    scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) + 
    
    scale_fill_manual(values = paletteer_d("ggthemes::Tableau_10")) +
    scale_color_manual(values = paletteer_d("ggthemes::Tableau_10")) +
    
    facet_wrap2(vars(variable), nrow = 1, scales = "free_y") +
    
    theme_minimal(base_family = my_font) +
    
    theme(
        legend.position = "none",
        
        strip.text = element_markdown(margin = margin(b = 10), face = "bold"),
        
        panel.grid.major = element_line(linetype = "dotted", lineend = "round", color = "grey80"),
        panel.grid.minor = element_line(linetype = "dotted", lineend = "round", color = "grey80"),
        
        axis.line = element_line(lineend = "round"),
        axis.ticks = element_line(lineend = "round"),
        
        axis.text = element_text(size = 8),
        axis.title.y = element_markdown(margin = margin(r = 10), size = 10),
        axis.title.x = element_text(face = "bold", margin = margin(t = 10), size = 10)
    ) +
    
    labs(x = "First year of the annual report", y = "**Total reported funding** adjusted to<br>Y2000 GBP per funding source")


library(patchwork)


multi <- (free(gr1) / gr2) +
    plot_layout(heights = c(3, 1)) +
    plot_annotation(
        title = "British Library Funding",
        
        subtitle = paste0(
            "Total reported funding (in millions of GBP), showing the decline in the British Library’s inflation-adjusted income from 1998 to 2023."
        ),
        
        caption = paste0(
            "Source: <b><i>@Andy Jackson</i> David Rosenthal's 2017 blog</b> | ",
            "Graphic: <b>Nikos Pechlivanis</b>"
        )
    ) &
    theme(
        plot.title    = element_text(size = 30, family = my_font, face = "bold", hjust = 0),
        plot.subtitle = element_markdown(size = 10, family = my_font, margin = margin(b = 10), hjust = 0),
        plot.caption  = element_markdown(size = 8, family = my_font, margin = margin(t = 10), hjust = 1),
        
        plot.background = element_rect(fill = "#def3f6", color = NA),
        plot.margin = margin(20, 20, 20, 20)
    )


ggsave(
    plot = multi, filename = "Rplot.png",
    width = 10, height = 10, units = "in", dpi = 600
)








