


rm(list = ls())
gc()

# load libraries -----------------------------------

library(data.table)
library(stringr)

library(ggplot2)
library(ggtext)

library(geomtextpath)
library(ggnewscale)

library(paletteer)
library(colorspace)
library(extrafont)

my_font = "Jost"
my_col = c("#3DA873FF", "deepskyblue4", "tomato4", "#370335", "#2F509E")

# input data frame -----------------------------------

d <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-04-23/outer_space_objects.csv')


# plot 1 -----------------------------------

t = d[, by = .(Entity), .(N = mean(num_objects))]

t1 = t[which(N >= 10)]

d1 = d[which(Entity %in% t1$Entity)]
d2 = d[which(!(Entity %in% t1$Entity))]

d1$Entity = d1$Entity |> str_to_upper()

gr1 = d1 |>
    ggplot(aes(Year, num_objects)) +
    
    geom_line(aes(group = Entity, color = Entity), linewidth = .25) +
    
    geom_point(
        aes(color = Entity), fill = "grey96",
        shape = 21, size = 2, stroke = .25
    ) +
    
    scale_color_manual(values = my_col |> lighten(.5)) +
    
    new_scale_color() +
    
    geom_smooth(
        aes(group = Entity, color = Entity),
        linewidth = 3, se = FALSE, lineend = "round"
    ) +

    scale_color_manual(values = my_col |> alpha(.1)) +
    
    new_scale_color() +

    geom_textsmooth(
        aes(color = Entity, label = Entity),
        linewidth = .5, se = FALSE, lineend = "round",
        fontface = "bold", family = "Sitka Text"
    ) +

    scale_color_manual(values = my_col |> darken(.25)) +
    
    scale_y_continuous(transform = "log10", labels = scales::comma) +
    
    theme_minimal(base_family = my_font) +
    
    theme(
        legend.position = "none",
        
        panel.grid.major = element_line(linewidth = .45, color = "grey85"),
        panel.grid.minor = element_line(linewidth = .35, linetype = "dashed", color = "grey85"),
        
        axis.title = element_blank(),
        
        axis.text.x = element_text(face = "bold"),
        
        plot.title    = element_text(size = 26, family = my_font, face = "bold", color = "#2E2A2B"),
        plot.subtitle = element_markdown(size = 11, family = my_font, margin = margin(b = 10)),
        plot.caption  = element_markdown(size = 7, family = my_font, margin = margin(t = 10)),
        
        plot.title.position = "plot",
        plot.caption.position = "plot",
        
        plot.background = element_rect(fill = "#fbfbf8", color = NA),
        plot.margin = margin(20, 20, 20, 20)
    ) +
    
    labs(
        
        title = "Objects Launched into Space",
        
        subtitle = paste0(
            "Annual number of objects launched into outer space by the top 5 countries and globally"
        ),
        
        caption = paste0(
            "Source: <b>Online Index of Objects Launched into Outer Space</b> | ",
            "Graphic: <b>Nikos Pechlivanis</b>"
        )
    )

# plot 2 -----------------------------------

library(ggrepel)

d3 = d2[which(num_objects >= 15)]

gr2 = d2 |> 
    ggplot(aes(Year, num_objects)) +
    
    geom_point(
        fill = "grey96", color = "#97A1A7",
        shape = 21, size = 1.5, stroke = .25
    ) +
    
    geom_point(
        data = d3, aes(Year, num_objects), fill = "#358DB9", color = "#97A1A7",
        shape = 21, size = 1.5, stroke = .25
    ) +
    
    geom_text_repel(
        data = d3, aes(Year, num_objects, label = Entity),
        max.overlaps = Inf, family = "Bebas Neue",
        segment.size = .15
    ) +
    
    scale_y_continuous(limits = c(0, 30), expand = c(0, 0), breaks = c(10, 20, 30)) +
    scale_x_continuous(limits = c(1960, 2024), expand = c(0, 0)) +
    
    theme_minimal(base_family = my_font) +
    
    theme(
        panel.grid = element_blank(),
    
        axis.title = element_blank(),
        
        axis.line = element_line(linewidth = .45),
        axis.ticks = element_line(linewidth = .45),
    )


# patchwork -----------------------------

library(patchwork)


mgr = gr1 + inset_element(gr2, .025, .675, .525, .975)


# save plot -------------------------

ggsave(
    plot = mgr, filename = "Rplot.pdf", device = cairo_pdf,
    width = 10, height = 10, units = "in"
)


library(magick)


tmp = image_read_pdf("Rplot.pdf", density = 600)
image_write(tmp, "Rplot.jpeg")









