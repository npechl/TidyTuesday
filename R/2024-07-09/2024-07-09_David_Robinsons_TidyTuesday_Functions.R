


rm(list = ls())
gc()

# load libraries -----------------------------------

library(data.table)
library(stringr)

library(ggplot2)
library(ggtext)

library(ggforce)

library(paletteer)
library(colorspace)
library(extrafont)

my_font = "Jost"

# input data frame ----------------------------------- 

drob_funs <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-07-09/drob_funs.csv')

drob_funs$pkgs |> unique() |> sort() |> paste(collapse = "\n") |> message()

poi = c(
    "geosphere",
    "gganimate",
    "ggmap",
    "ggplot",
    "ggraph",
    "ggrepel",
    "ggridges",
    "ggthemes",
    "ggwordcloud",
    "graphics",
    "grid",
    "igraph",
    "magick",
    "malariaAtlas",
    "maps",
    "plotly",
    "sf",
    "shiny",
    "shinydashboard",
    "tidygraph",
    "survival"
)

tt = drob_funs[which(pkgs %in% poi), by = .(funs, contents), .N]

tt$contents = tt$contents |> str_remove_all("\\.Rmd")

my_col = c(
    '#00429d', 
    '#5681b9',
    '#93c4d2',
    '#ffa59e',
    '#dd4c65',
    '#93003a'
)

tt2 = tt[, by = funs, .(N = N |> sum())]

tt$funs = ifelse(
    tt$funs %in% tt2[which(N >= 20)]$funs, paste0("**", tt$funs, "**"),
    tt$funs
)

gr = tt |>
    ggplot(aes(funs, contents)) +
    geom_point(aes(size = N, fill = N), shape = 21, color = "grey96", stroke = .15) +
    
    scale_size_continuous(
        range = c(1, 4),
        guide = guide_legend(
            title = "No. of occurrences",
            title.theme = element_text(angle = 90, hjust = 0),
            title.position = "left",
            override.aes = list(color = "grey10")
        )
    ) +
    
    scale_fill_stepsn(
        colors = my_col, 
        guide = guide_colorsteps(
            title = "No. of occurrences",
            title.theme = element_text(angle = 90, hjust = 0),
            title.position = "left",
            barheight = unit(10, "lines"),
            barwidth = unit(.5, "lines")
        )
    ) +
    
    coord_cartesian() +
    
    theme_minimal(base_family = my_font) +
    
    theme(
        axis.text.x = element_markdown(angle = 90, hjust = 1, vjust = .5, size = 8),
        axis.text.y = element_text(size = 8),
        
        axis.title = element_markdown(),
        
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(linewidth = .35, linetype = "dotted", lineend = "round"),
        
        plot.title    = element_text(face = "bold", size = 26, margin = margin(b = 5), family = "Jost", hjust = .5),
        plot.subtitle = element_markdown(margin = margin(b = 25), size = 12, color = "grey25", family = "Jost", hjust = .5),
        plot.caption  = element_markdown(margin = margin(t = 25), size = 8, family = "Jost", hjust = .5),
        
        panel.background = element_rect(fill = "#f9fbfe" |> darken(.01), color = NA),
        plot.background = element_rect(fill = "#f9fbfe", color = NA),
        plot.margin = margin(20, 20, 20, 20)
    ) +
    
    labs(
        x = "*Plotting function names*", y = "*File in which the function is used*",
        
        title = "David Robinson's TidyTuesday Functions",
        
        subtitle = paste0(
            "David Robinson's most used plotting functions for the *TidyTuesday* project"
        ),
        
        caption = paste0(
            "Source: <b>{funspotr} package</b> | ",
            "Graphic: <b>Nikos Pechlivanis</b>"
        )
    )


ggsave(
    plot = gr, filename = "Rplot.png",
    width = 17, height = 13, units = "in", dpi = 600
    
)











