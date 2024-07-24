


rm(list = ls())
gc()

# load libraries -----------------------------------

library(data.table)
library(stringr)

library(ggplot2)
library(ggtext)
library(ggh4x)

library(shadowtext)

library(paletteer)
library(colorspace)
library(extrafont)

my_font = "Jost"

# input data frame ----------------------------------- 

q <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-05-14/coffee_survey.csv')


q$age |> unique()
q$education_level |> unique()
q$strength |> unique()


q1 = q[, by = .(age, employment_status, strength), .N]

q1$age = q1$age |> factor(levels = c(
    "<18 years old",
    "18-24 years old",
    "25-34 years old",
    "35-44 years old",
    "45-54 years old",
    "55-64 years old",
    ">65 years old"
))

q1$employment_status = q1$employment_status |> factor(levels = c(
    "Employed full-time",
    "Employed part-time",
    "Homemaker",
    "Unemployed",
    "Student",
    "Retired"
))

q1$strength = q1$strength |> factor(levels = c(
    "Weak",
    "Somewhat light",
    "Medium",
    "Somewhat strong",
    "Very strong"
))

q1 = q1[which(
    !(is.na(age) | is.na(employment_status) | is.na(strength))
)]


gr = q1 |>
    ggplot(aes(N, age, fill = strength)) +
    
    geom_col(
        color = "grey96", linewidth = .25,
        position = "fill"
    ) +
    
    facet_wrap2(vars(employment_status), ncol = 3, axes = "x") +
    
    scale_x_continuous(
        expand = c(0, 0),
        labels = scales::percent,
        breaks = c(.05, .2, .4, .6, .8, .95)
    ) +
    
    scale_fill_manual(
        values = c('#00429d', '#497bb6', '#00c3b7', '#c75264', '#93003a')
    ) +
    
    theme_minimal(base_family = my_font) +
    
    theme(
        legend.position = "top",
        legend.title.position = "top",
        
        strip.text = element_text(face = "bold"),
        
        axis.title.y = element_blank(),
        axis.title.x = element_text(margin = margin(t = 10)),
        
        panel.spacing = unit(1, "lines"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(linewidth = .35, linetype = "dotted", lineend = "round", color = "grey75"),
        
        plot.title    = element_text(size = 26, family = my_font, face = "bold"),
        plot.subtitle = element_markdown(size = 9, family = my_font, margin = margin(b = 15)),
        plot.caption  = element_markdown(size = 7, family = my_font, margin = margin(t = 15)),
        
        plot.title.position = "plot",
        plot.caption.position = "plot",
        
        plot.background = element_rect(fill = "#f5f5f5", color = NA),
        plot.margin = margin(20, 20, 20, 20)
    ) +
    
    labs(
        x = "Frequency of observations",
        fill = "How strong do you like your coffee?",
        
        title = "The Great American Coffee Taste Test",
        subtitle = paste0(
            "Distribution of people based on their employment status and preferred coffee strength"
        ),
        
        caption = paste0(
            "Source: <b>Great American Coffee Taste Test</b> | ",
            "Graphic: <b>Nikos Pechlivanis</b>"
        )
    )


ggsave(
    plot = gr, filename = "Rplot.png",
    width = 10, height = 8, units = "in", dpi = 600
)




