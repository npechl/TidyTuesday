


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

# d0 <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-11-26/cbp_resp.csv')
d0 <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-11-26/cbp_state.csv')

# clean columns ----------------

d0$month_grouping <- NULL

d1 <- d0[, by = .(fiscal_year, month_abbv, demographic), .(N = encounter_count |> sum())]


library(ggstream)


d1$month_abbv <- d1$month_abbv |> factor(levels = c("JAN", "FEB", "MAR", "APR", "MAY", "JUN", "JUL", "AUG", "SEP", "OCT", "NOV", "DEC"))

d1 <- d1[order(fiscal_year, month_abbv)]

d1$year_month <- paste0(d1$fiscal_year, " - ", d1$month_abbv)
d1$year_month <- d1$year_month |> factor(levels = d1$year_month |> unique())

d1$demographic2 <- d1$demographic |>
    str_replace("UC / Single Minors", "Unaccompanied Children") |>
    str_replace("Accompanied Minors", "Accompanied Children") |>
    str_replace("FMUA", "Individuals in a Family Unit")

gr <- ggplot() +
    
    geom_segment(aes(
        x = c("2020 - JAN", "2021 - JAN", "2022 - JAN", "2023 - JAN", "2024 - JAN"), 
        y = rep(-200000, 5), 
        xend = c("2020 - JAN", "2021 - JAN", "2022 - JAN", "2023 - JAN", "2024 - JAN"), 
        yend = rep(0, 5),
    ), linewidth = .35, inherit.aes = FALSE) +
    
    geom_stream(data = d1, aes(year_month, N, fill = demographic2), sorting = "inside_out", linewidth = .15, color = "#fbf2f4", bw = .5) +
    
    geom_vline(xintercept = c("2020 - JAN", "2021 - JAN", "2022 - JAN", "2023 - JAN", "2024 - JAN"), color = "#fbf2f4", linewidth = .35, linetype = "dashed", lineend = "round") +
    
    scale_fill_manual(values = c("#CD534C", "#003C67", "#8F7700", "#3B3B3B")) +
    
    scale_x_discrete(
        breaks = c("2020 - JAN", "2021 - JAN", "2022 - JAN", "2023 - JAN", "2024 - JAN"),
        labels = c("2020", "2021", "2022", "2023", "2024"),
        expand = c(0, 0)
    ) +
    
    scale_y_continuous(
        breaks = c(-150000, -100000, -50000, 0, 50000, 100000, 150000),
        minor_breaks = seq(-125000, 125000, by = 50000),
        labels = c("+150k", "+100k", "+50k", 0, "+50k", "+100k", "+150k")
    ) +
    
    coord_cartesian(clip = "off") +
    
    theme_minimal(base_family = my_font) +
    theme(
        legend.position = "top",
        legend.title = element_blank(),
        
        axis.ticks.y = element_line(color = "grey85", linewidth = .45),
        
        axis.title.x = element_text(margin = margin(t = 10)),
        axis.title.y = element_text(margin = margin(r = 10)),
        
        panel.grid.major.x = element_blank(),
        
        panel.grid.major.y = element_line(color = "grey85", linewidth = .45),
        panel.grid.minor.y = element_line(color = "grey85", linewidth = .35, linetype = "dotted", lineend = "round"),
        
        plot.title    = element_text(face = "bold", size = 18, margin = margin(b = 10), family = "Jost", hjust = .5),
        plot.subtitle = element_markdown(margin = margin(b = 10), size = 10, color = "grey25", family = "Jost", hjust = .5),
        plot.caption  = element_markdown(margin = margin(t = 10), size = 8, family = "Jost", hjust = .5),
        
        plot.background = element_rect(fill = "#fbf2f4", color = "#fbf2f4"),
        plot.margin = margin(20, 20, 20, 20)
    ) +
    
    labs(
        x = "The fiscal year the encounter took place",
        y = "Number of encounters",
        
        title = "U.S. Customs and Border Protection (CBP) Encounter Data",
        
        subtitle = paste0(
            "**The number of families migrating to the US continues to grow steadily over time**, reflecting ongoing migration patterns."
        ),
        
        caption = paste0(
            "Source: <b>U.S. Customs and Border Protection (CBP) encounter data</b> | ",
            "Graphic: <b>Nikos Pechlivanis</b>"
        )
    )




ggsave(
    plot = gr, filename = "Rplot.png",
    width = 10, height = 8, units = "in", dpi = 600
)















