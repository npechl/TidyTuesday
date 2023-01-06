





rm(list = ls())
gc()

library(data.table)
library(stringr)

library(ggplot2)
library(ggrepel)
library(ggtext)

library(extrafont)

df = fread("Kalleidoscope.csv")

df$Description = NULL

my_col = df$Color
names(my_col) = df$Color

df$newTitle = paste0(
    "<span style='color:", "black", #df$ComplColor, 
    "'>**", df$Title,
    "**</span>"
)


x1 = df[which(Order %in% c(1, 5)), ]
x2 = df[which(Order %in% c(3, 7)), ]

y1 = df[which(Order %in% c(2, 6)), ]
y2 = df[which(Order %in% c(4, 8)), ]

gr1 = ggplot(data = df, aes(x = Order, y = 1)) +
    
    annotate("segment", x = x1$Order, xend = x1$Order, y = 1, yend = 1.2, color = x1$Color, linetype = "dashed") +
    annotate("segment", x = x2$Order, xend = x2$Order, y = 1, yend = 1.08, color = x2$Color, linetype = "dashed") +
    
    annotate("segment", x = y1$Order, xend = y1$Order, y = 1, yend = .8, color = y1$Color, linetype = "dashed") +
    annotate("segment", x = y2$Order, xend = y2$Order, y = 1, yend = .92, color = y2$Color, linetype = "dashed") +
    
    geom_line(linetype = "dashed", color = "grey50") +
    
    geom_point(aes(color = Color), size = 4) +
    
    geom_richtext(
        data = x1, 
        aes(x = Order, y = 1, label = newTitle, fill = Color),
        nudge_y = .2,
        label.size = NA,
        label.padding = unit(.35, "lines"),
        size = 3,
        family = "Bebas Neue"
    ) +
    
    geom_richtext(
        data = x2, 
        aes(x = Order, y = 1, label = newTitle, fill = Color),
        nudge_y = .08,
        label.size = NA,
        label.padding = unit(.35, "lines"),
        size = 3,
        family = "Bebas Neue"
    ) +
    
    geom_richtext(
        data = y1, 
        aes(x = Order, y = 1, label = newTitle, fill = Color),
        nudge_y = -.2,
        label.size = NA,
        label.padding = unit(.35, "lines"),
        size = 3,
        family = "Bebas Neue"
    ) +
    
    geom_richtext(
        data = y2, 
        aes(x = Order, y = 1, label = newTitle, fill = Color),
        nudge_y = -.08,
        label.size = NA,
        label.padding = unit(.35, "lines"),
        size = 3,
        family = "Bebas Neue"
    ) +
    
    scale_color_manual(values = my_col) +
    scale_fill_manual(values = my_col) +
    
    scale_x_continuous(limits = c(0, 9)) +
    
    # scale_y_continuous(limits = c(.5, 1.5)) +
    
    coord_cartesian(expand = TRUE, clip = "off") +
    
    theme_void() +
    
    theme(
        legend.position = "none",
        
        plot.background = element_rect(fill = "black", color = NA)
    )


df$Title = str_to_upper(df$Title)

df$episode = paste0(
    "<span style='font-size:25pt; color:white'>**The ", 
    "<span style='color:", df$Color, "'>", str_to_upper(df$Color), "</span>",
    " EPISODE**"
)

df$newTitle = paste0(
    "<span style='color:white; font-size:10pt'>**", df$Title, "**"
)

df$text = paste0(
    "<span style='font-size:8pt; color:white'>",
    "*directed by*: ", df$`Directed by`, "<br>",
    "*written by*: ", df$`Written by`, "</span>"
)


gr2 = ggplot() +
    
    geom_segment(
        data = df, aes(x = .95, xend = 1.05, y = 1, yend = 1, color = Color),
        linewidth = 2
    ) +
    
    geom_richtext(
        data = df, aes(x = .95, y = 1, label = episode),
        family = "Bebas Neue",
        label.size = NA, fill = NA,
        nudge_y = .5,
        label.padding = unit(.5, "lines"),
        hjust = 0
    ) +
    
    geom_richtext(
        data = df, aes(x = .95, y = 1, label = newTitle),
        family = "Bebas Neue",
        label.size = NA, fill = NA,
        nudge_y = .25,
        label.padding = unit(.5, "lines"),
        hjust = 0
    ) +
    
    geom_richtext(
        data = df, aes(x = 1.05, y = 1, label = text),
        family = "Bebas Neue",
        label.size = NA, fill = NA,
        nudge_y = -.25,
        label.padding = unit(.5, "lines"),
        hjust = 1
    ) +
    
    scale_y_continuous(limits = c(0, 2)) +
    
    scale_color_manual(values = my_col) +
    
    scale_fill_manual(values = my_col) +
    
    facet_wrap2(vars(Title), ncol = 4) +
    
    theme_void() +

    theme(
        legend.position = "none",
        strip.text = element_blank(),

        plot.background = element_rect(fill = "black", color = NA)
    )

library(patchwork)

multi = gr1 / gr2 + plot_layout(heights = c(1, 4)) +
    
    plot_annotation(
        title = 'Kaleidoscope episodes',
        
        subtitle = paste0(
            "***There Are 40,000 Ways to Watch Netflix's Kaleidoscope.***",
            " *Each episode is named after a different color, which means some ",
            "viewers may start on yellow,<br>while others might start with green. ",
            "Every viewer will eventually see all the episode colors, but the ",
            "order in which they watch them will affect their<br>viewpoint on the ",
            "story.*"
        ),
        
        caption = paste0(
            "Source: <b>Netflix</b> | ",
            "Graphic: <b>Nikolaos Pechlivanis</b>"
        )
    )  &
    theme(
        plot.background = element_rect(fill = "black", color = NA),
        
        plot.title.position = "plot",
        plot.caption.position = "plot",

        plot.title = element_text(face = "bold", size = 36, color = "white", family = "Bebas Neue"),
        plot.subtitle = element_markdown(margin = margin(b = 15), size = 9, color = "white", family = "Comfortaa"),
        plot.caption = element_markdown(margin = margin(t = 10), size = 8, color = "white", family = "Zilla Slab"),
        
        plot.margin = margin(20, 20, 20, 20)
    )

ggsave(
    plot = multi, filename = "Rplot.pdf",
    width = 12, height = 9, units = "in", device = cairo_pdf
)


library(magick)

tmp = image_read_pdf("Rplot.pdf", density = 500)
image_write(tmp, "Rplot.jpeg")

