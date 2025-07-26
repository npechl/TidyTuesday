


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

d0 <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-07-22/mta_art.csv')
d1 <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-07-22/station_lines.csv')


# clean data ---------------------------

d0$art_description <- NULL
d0$art_image_link  <- NULL


p0 <- d0$art_material |> 
    str_split("\\,|\\;|\\/|\\&|\\-|\\ ") |>
    lapply(str_squish) |>
    lapply(str_to_lower) |>
    lapply(str_remove_all, c("\\(|\\)|\\.|[0-9]")) |>
    lapply(function(x) data.table("material" = x)) |>
    rbindlist(idcol = "id")

p0 <- cbind(d0[p0$id, 1:2], p0)

p0 <- p0[which(material != "")]

p1 <- p0[, by = .(agency, material), .(N = id |> unique() |> length())]

# p0 <- d0[, by = .(agency, station_name), .N]

library(ggwordcloud)

my_levels <- p1$agency |> table() |> sort(decreasing = TRUE) |> names()

p1$agency <- p1$agency |> factor(levels = my_levels)

words_to_exclude <- c(
    "\"\"nicopress\"\"",
    "and",
    "a",
    "absolute",
    "cor",
    "in",
    "faceted",
    "or"
)

p1 <- p1[which(!(material %in% words_to_exclude))]

p1$material <- p1$material |> str_to_title()

gr <- p1 |> 
    ggplot() +
    geom_text_wordcloud(
        aes(label = material, size = N, color = agency, x = agency), 
        family = "Anton", show.legend = TRUE
    ) +
    scale_size_area(max_size = 30) +
    scale_color_manual(values = c("NYCT" = "#1A476F", "LIRR" = "#C10534", "Metro-North" = "#E37E00",
                                  "B&T" = "#55752F", "MTA Bus Company" = "#9C8847", "SIR" = "#938DD2"),
                       guide = guide_legend(nrow = 1, override.aes = list(size = 4))) +
    guides(size = "none") +
    theme_void(base_family = my_font) +
    theme(
        legend.title.position = "top",
        legend.position = "bottom",
        legend.justification = "right",
        
        plot.title    = element_text(size = 30, family = "Jost", face = "bold", hjust = 0),
        plot.subtitle = element_markdown(size = 10, family = my_font, margin = margin(b = 10, t = 10), hjust = 0),
        plot.caption  = element_markdown(size = 8, family = my_font, margin = margin(t = 10), hjust = 1),
        
        plot.background = element_rect(fill = "#def3f6", color = NA),
        plot.margin = margin(20, 20, 20, 20)
    ) + 
    labs(
        title = "MTA Permanent Art Catalog",
        
        subtitle = paste0(
            "Materials used in artworks for each agency: ",
            "<b style='color:#1A476F'>NYCT</b> = New York City Transit, ",
            "<b style='color:#C10534'>LIRR</b> = Long Island Rail Road, ",
            "<b style='color:#E37E00'>MNR</b> = Metro-North Railroad."
        ),
        
        caption = paste0(
            "Source: <b>(New York) MTA Permanent Art Catalog</b> | ",
            "Graphic: <b>Nikos Pechlivanis</b>"
        ),
        
        color = "Agency"
    )


ggsave(
    plot = gr, filename = "Rplot.png", dpi = 600,
    width = 10, height = 8, units = "in"
)



# p0 |>
#     ggplot(aes(N, station_name)) +
#     geom_col(color = "grey", linewidth = .25) +
#     facet_grid(rows = vars(agency), scales = "free_y", space = "free_y")


















