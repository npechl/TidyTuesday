


rm(list = ls())
gc()

# load libraries -----------------------------------

library(data.table)
library(stringr)

library(ggplot2)
library(ggtext)

library(ggridges)
library(ggh4x)

library(paletteer)
library(colorspace)
library(extrafont)

my_font = "Jost"

# input data frame ----------------------------------- 

pride_index <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-06-11/pride_index.csv')
pride_index_tags <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-06-11/pride_index_tags.csv')

all(pride_index$campus_name == pride_index_tags$campus_name)

df = cbind(pride_index, pride_index_tags[, 3:ncol(pride_index_tags), with = FALSE])


df = df |>
    melt(
        id.vars = c("campus_name", "campus_location", "rating", "students", "community_type"),
        variable.factor = FALSE, value.factor = FALSE
    )

df = df[which(!is.na(value))]

df = df[which(!(variable %in% c("hbcu", "military", "other_minority_serving")))]

df$community_type2 = df$community_type |> 
    str_to_title() |>
    str_replace_all(fixed("Large Urban City"), "<b>Large Urban City</b> (500k+)") |>
    str_replace_all(fixed("Medium City"), "<b>Medium City</b> (100k to 500k)") |>
    str_replace_all(fixed("Small City"), "<b>Small City</b> (25k to 100k)") |>
    str_replace_all(fixed("Small Town"), "<b>Small Town</b> (10k to 25k)") |>
    str_replace_all(fixed("Very <b>Small Town</b> (10k to 25k)"), "<b>Very Small Town</b> (5k to 10k)") |>
    str_replace_all(fixed("Rural Community"), "<b>Rural Community</b> (<5k)") |>
    factor(
        levels = c(
            "<b>Large Urban City</b> (500k+)",
            "<b>Medium City</b> (100k to 500k)",
            "<b>Small City</b> (25k to 100k)",
            "<b>Small Town</b> (10k to 25k)",
            "<b>Very Small Town</b> (5k to 10k)",
            "<b>Rural Community</b> (<5k)"
        )
    )


df$variable = df$variable |> str_replace_all("_", " ") |> str_to_title()



gr = df |>
    ggplot(aes(rating, variable)) +
    
    geom_density_ridges(aes(fill = variable), color = "grey30", linewidth = .25, alpha = .9) +
    
    geom_vline(xintercept = c(2, 3, 4), linewidth = .3, linetype = "dotted", lineend = "round") +
    
    facet_wrap(vars(community_type2), axes = "all_x") +
    
    scale_fill_manual(values = paletteer_d("ggthemes::Tableau_20"), guide = "none") +
    scale_color_manual(values = paletteer_d("ggthemes::Tableau_20") |> darken(.25), guide = "none") +
    
    scale_x_continuous(breaks = seq(1, 5), limits = c(-1, 7), expand = c(0, 0)) +
    
    theme_minimal(base_family = my_font) +
    
    theme(
        panel.grid.major = element_line(linewidth = .3, lineend = "round"),
        panel.grid.minor = element_blank(),
        
        axis.title.x = element_markdown(margin = margin(t = 10)),
        axis.title.y = element_markdown(margin = margin(r = 10)),
        
        strip.text = element_markdown(),
        
        plot.title    = element_text(face = "bold", size = 26, margin = margin(b = 5), family = "Jost"),
        plot.subtitle = element_markdown(margin = margin(b = 25), size = 10, color = "grey25", family = "Jost"),
        plot.caption  = element_markdown(margin = margin(t = 25), size = 8, family = "Jost"),
        
        plot.title.position = "plot",
        plot.caption.position = "plot",
        
        plot.background = element_rect(fill = "#f9fbfe", color = NA),
        plot.margin = margin(20, 20, 20, 20)
    ) +
    
    labs(
        x = "**Campus Pride Index rating** ranging from 1 to 5 stars, including half-star increments",
        y = "**Tag** associated with the campus",
        
        title = "Campus Pride Index",
        
        subtitle = paste0(
            "**Density plots** illustrating the Campus Pride Index across various associated campus tags."
        ),
        
        caption = paste0(
            "Source: <b>Campus Pride Index</b> | ",
            "Graphic: <b>Nikos Pechlivanis</b>"
        )
    )


ggsave(
    plot = gr, filename = "Rplot.png",
    width = 7.5, height = 9, units = "in", dpi = 600
)    
    





    
    




