


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

# 1 ----------------------------------- 

d <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-10-08/most_visited_nps_species_data.csv')


d$CommonNames <- NULL
d$Synonyms    <- NULL


d1 <- d[, by = .(ParkName, CategoryName), .(
    N = SciName |> unique() |> length()
)]


mm = d1 |> dcast(CategoryName ~ ParkName, value.var = "N", fill = 0)
mm = mm[, -1] |> setDF(rownames = mm$CategoryName) |> as.matrix()

o_r <- mm |> dist("euclidean") |> hclust("ward.D2")
o_c <- mm |> t() |> dist("euclidean") |> hclust("ward.D2")

d1$ParkName <- d1$ParkName |> factor(levels = o_c$labels[o_c$order])
d1$CategoryName <- d1$CategoryName |> factor(levels = o_r$labels[o_r$order])

library(shadowtext)

gr <- d1 |>
    ggplot(aes(ParkName, CategoryName)) + 
    
    geom_vline(xintercept = seq(0.5, 15.5, by = 1), linewidth = .25, color = "grey") +
    geom_hline(yintercept = seq(0.5, 16.5, by = 1), linewidth = .25, color = "grey") +
    
    geom_tile(aes(fill = N), linewidth = .25, color = "grey20") +
    
    geom_shadowtext(
        aes(color = N, label = N |> scales::comma()), family = "Zilla Slab", fontface = "bold",
        bg.color = "grey96", bg.r = .1, size = 3
    ) +
    
    scale_fill_stepsn(
        colors = c("#00429d","#73a2c6","#ffffe0","#f4777f","#93003a") |> lighten(.1),
        breaks = c(10, 100, 1000, 10000),
        transform = "log10",
        labels = scales::comma,
        guide = guide_colorsteps(
            barheight = unit(16, "lines"), 
            barwidth = unit(.5, "lines")
        )
    ) +
    
    scale_color_stepsn(
        colors = c("#00429d","#73a2c6","#ffffe0","#f4777f","#93003a") |> darken(.75),
        breaks = c(10, 100, 1000, 10000),
        transform = "log10",
        guide = "none"
    ) +
    
    # facet_wrap(vars(Nativeness)) +
    
    scale_x_discrete(expand = c(0, 0)) +
    scale_y_discrete(expand = c(0, 0)) +
    
    theme_minimal(base_family = my_font) +
        
    theme(
        legend.title.position = "left",
        # legend.text.position = "left",
        legend.title = element_text(angle = 90, hjust = .5, face = "bold"),
        
        axis.title.y = element_text(face = "bold", margin = margin(r = 10)),
        axis.title.x = element_text(face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        
        panel.grid = element_blank(),
        panel.background = element_rect(fill = "grey95", color = NA),
        
        plot.title    = element_text(face = "bold", size = 20, margin = margin(b = 5), family = "Jost", hjust = .5),
        plot.subtitle = element_markdown(margin = margin(b = 15), size = 10, color = "grey25", family = "Jost", hjust = .5),
        plot.caption  = element_markdown(margin = margin(t = 20), size = 8, family = "Jost", hjust = .5),
        
        plot.background = element_rect(fill = "#f8f2f9", color = NA),
        plot.margin = margin(20, 20, 20, 20)
    ) +
    
    labs(
        fill = "No. of species", x = "National Park Full Name", y = "Species Category",
        
        title = "National Park Species",
        
        subtitle = paste0(
            "**Species distribution** by category across U.S. National Parks."
        ),
        
        caption = paste0(
            "Source: <b>NPSpecies</b> | ",
            "Graphic: <b>Nikos Pechlivanis</b>"
        )
    )


ggsave(
    plot = gr, filename = "Rplot.png",
    width = 9, height = 9, units = "in", dpi = 600
)


