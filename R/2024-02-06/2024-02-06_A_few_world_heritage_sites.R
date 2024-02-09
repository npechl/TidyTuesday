


rm(list = ls())
gc()

# load libraries -----------------------------------

library(data.table)
library(stringr)

library(ggplot2)
library(ggtext)
library(ggwordcloud)

library(colorspace)
library(extrafont)

# input data frame -----------------------------------

heritage <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-02-06/heritage.csv', header = TRUE)

heritage = heritage |> 
    melt(
        id.vars = "country", 
        variable.factor = FALSE, 
        value.factor = FALSE
    )

heritage$country2 = heritage$country |> str_to_upper()

my_base_family = "Figtree"

my_base_color = "#1f313d"
my_title_color = "#1f313d" |> darken(.5)

heritage = heritage |> split(by = "variable")


# plot A1 -----------------------------

a1 = heritage$`2004` |>
    ggplot() +
    geom_text_wordcloud(
        aes(
            label = country2, 
            label_content = sprintf(
                "%s<sub style = 'font-family:Jost;color:#515F6A'>(%g)</sub>", 
                country2, value
            ), size = value
        ), family = "Anton", color = my_base_color
    ) +
    
    scale_size_continuous(range = c(4, 12)) +
    
    coord_equal() +
    
    theme_void() +
    
    theme(
        plot.background = element_rect(fill = NA, color = NA)
    )

# plot A2 -----------------------------

a2 = heritage$`2022` |>
    ggplot() +
    geom_text_wordcloud(
        aes(
            label = country2, 
            label_content = sprintf(
                "%s<sub style = 'font-family:Jost;color:#515F6A'>(%g)</sub>", 
                country2, value
            ), size = value
        ), family = "Anton", color = my_base_color
    ) +
    
    scale_size_continuous(range = c(4, 12)) +
    
    coord_equal() +
    
    theme_void() +
    
    theme(
        plot.background = element_rect(fill = NA, color = NA)
    )

# gr = heritage |>
#     ggplot() +
#     geom_text_wordcloud(
#         aes(
#             label = country2, 
#             label_content = sprintf(
#                 "%s<sub style = 'font-family:Figtree;color:grey50'>(%g)</sub>", 
#                 country2, value
#             ), size = value
#         ), family = "Anton", color = my_base_color
#     ) +
#     
#     scale_size_continuous(range = c(4, 12)) +
#     
#     facet_wrap(vars(variable), nrow = 1) +
#     
#     coord_equal() +
#     
#     theme_minimal(base_family = "Figtree") +
#     
#     theme(
#         strip.text = element_text(face = "bold", size = 20, color = my_base_color),
#         
#         plot.title    = element_text(size = 26, margin = margin(b = 5), color = my_title_color, family = "Jost", face = "bold"),
#         plot.subtitle = element_markdown(size = 8, margin = margin(b = 50), color = my_base_color, family = "Jost"),
#         plot.caption  = element_markdown(size = 4,  margin = margin(t = 25), color = my_base_color, family = "Jost"),
#         
#         plot.title.position = "plot",
#         plot.caption.position = "plot",
#         
#         plot.background = element_rect(fill = "#f9fbfe", color = NA),
#         plot.margin = margin(20, 20, 20, 20)
#     ) +
#     
#     labs(
#         title = 'A few world heritage sites',
#         
#         subtitle = paste0(
#             "1 dataset, 100 visualizations project: **Number of UNESCO World Heritage Sites in 2004 and 2022**."
#         ),
#         
#         caption = paste0(
#             "Source: <b>UNESCO World Heritage Sites</b> | ",
#             "Graphic: <b>Nikos Pechlivanis</b>"
#         )
#     )
# 
# 
# ggsave(
#     plot = gr, filename = "Rplot.jpeg",
#     width = 8.43, height = 6.44, units = "in", dpi = 600
# )

# load libraries -------------------------

library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(rnaturalearthhires)
library(shadowtext)

s0 = ne_countries(
    scale = "large", returnclass = "sf",
    country = c("norway", "denmark", "sweden")
) |> st_transform(crs = "+proj=adams_ws1")

s0$country = s0$name_en


# plot B1 -----------------------

s0 = merge(s0, heritage$`2004`, all = TRUE)

b1 = ggplot() +

    geom_sf(
        data = s0, aes(fill = value),
        linewidth = .1, color = "grey85"
    ) +
    
    scale_fill_gradient(
        low = "#FFBEB2", 
        high = "#f06d8f" |> alpha(.5)
    ) +

    coord_sf(xlim = c(0, 15e5), ylim = c(35e5, 55e5)) +

    theme_void(base_family = "Anton") +

    theme(
        legend.position = "none",
        plot.title = element_text(face = "bold", size = 20, color = my_base_color, hjust = .5)
        # plot.margin = margin(20, 20, 20, 20)
    ) +
    
    labs(title = "2004")

# plot B1 -----------------------

s0 = ne_countries(
    scale = "large", returnclass = "sf",
    country = c("norway", "denmark", "sweden")
) |> st_transform(crs = "+proj=adams_ws1")

s0$country = s0$name_en

s0 = merge(s0, heritage$`2022`, all = TRUE)

b2 = ggplot() +
    
    geom_sf(
        data = s0, aes(fill = value),
        linewidth = .1, color = "grey85"
    ) +
    
    scale_fill_gradient(
        low = "#FFBEB2", 
        high = "#f06d8f" |> alpha(.5)
    ) +

    coord_sf(xlim = c(0, 15e5), ylim = c(35e5, 55e5)) +
    
    theme_void(base_family = "Anton") +
    
    theme(
        legend.position = "none",
        
        plot.title = element_text(face = "bold", size = 20, color = my_base_color, hjust = .5)
        
        # plot.margin = margin(20, 20, 20, 20)
    ) +
    
    labs(title = "2022")


# patchwork -----------------

library(patchwork)


gr1 = b1 + inset_element(a1, .15, .01, .85, .71)

gr2 = b2 + inset_element(a2, .15, .01, .85, .71)

multi = (gr1 | gr2) + 
    plot_annotation(
        title = 'A few world heritage sites',

        subtitle = paste0(
            "1 dataset, 100 visualizations project: **Number of UNESCO World Heritage Sites in 2004 and 2022**."
        ),

        caption = paste0(
            "Source: <b>UNESCO World Heritage Sites</b> | ",
            "Graphic: <b>Nikos Pechlivanis</b>"
        )
    ) &

    theme(
        plot.title    = element_text(size = 26, margin = margin(b = 5), color = my_base_color, family = "Jost", face = "bold"),
        plot.subtitle = element_markdown(size = 10, margin = margin(b = 25), color = my_base_color, family = "Jost"),
        plot.caption  = element_markdown(size = 6,  margin = margin(t = 25), color = my_base_color, family = "Jost"),

        plot.title.position = "plot",
        plot.caption.position = "plot",

        # panel.background = element_rect(fill = "#f9fbfe", color = NA),
        plot.margin = margin(20, 20, 20, 20)
    )


ggsave(
    plot = multi, filename = "Rplot.jpeg",
    width = 12, height = 8, units = "in", dpi = 600
)














