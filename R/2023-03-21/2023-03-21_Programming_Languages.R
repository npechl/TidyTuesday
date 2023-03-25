


rm(list = ls())
gc()


library(data.table)
library(stringr)

library(ggplot2)
library(ggh4x)

library(ggrepel)
library(ggnewscale)

library(extrafont)

df <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-03-21/languages.csv')

df = df[which(appeared >= 1980), ]

df$description            = NULL
df$website                = NULL
df$domain_name            = NULL
df$domain_name_registered = NULL
df$reference              = NULL
df$wikipedia              = NULL
df$wikipedia_summary      = NULL
df$wikipedia_related      = NULL

my_font = "Source Code Pro"

# index = df$title |>
#     duplicated() |>
#     which()
# 
# index = df$title %in% df[index, ]$title |>
#     which()

x1 = df[which(number_of_jobs > 0)]

x1$col = ifelse(
    x1$type == "pl", "yes", "no"
)

gr1 = ggplot(data = x1, aes(x = appeared, y = number_of_jobs)) +
    
    geom_point(aes(size = number_of_users, fill = col), stroke = .15,
               shape = 21, color = "white") +
    
    scale_x_continuous(expand = c(0, 0), limits = c(1979, 2017)) +
    
    scale_y_continuous(
        trans = "log10",
        labels = scales::comma_format(scale = 0.001, suffix = "k")
    ) +
    
    scale_size_continuous(
        range = c(2, 10),
        breaks = c(10000000,
                   20000000,
                   30000000,
                   40000000,
                   50000000)
    ) +
    
    scale_fill_manual(
        values = c(
            "yes" = alpha("#2b83e2", alpha = 1),
            "no"  = alpha("#8a2be2", alpha = 1)
        )
    ) +
    
    facet_wrap2(vars(type), ncol = 4, axes = "all") +
    
    coord_cartesian(clip = "off", expand = TRUE) +
    
    theme_minimal(base_family = my_font) +
    
    theme(
        legend.position = "none",
        
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(linetype = "dashed", linewidth = .3, color = "#395b71"),
        
        axis.line = element_line(linewidth = .3, color = "white"),
        axis.ticks = element_line(linewidth = .3, color = "white"),
        
        strip.text = element_text(face = "bold", color = "white"),
        
        axis.text = element_text(size = 6, color = "white"),
        axis.title = element_blank()
    )


# ggsave(
#     plot = gr, filename = "Rplot.pdf",
#     width = 12, height = 6, units = "in"
# )


x2 = x1[which(type == "pl")]





gr2 = ggplot(data = x2, aes(x = appeared, y = number_of_jobs)) +
    
    geom_point(aes(size = number_of_users), stroke = .25,
               shape = 21, color = "white", fill = alpha("#2b83e2", alpha = 1)) +
    
    scale_size_continuous(
        range = c(2, 10),
        breaks = c(10000000,
                   20000000,
                   30000000,
                   40000000,
                   50000000)
    ) +
    
    new_scale("size") +
    
    geom_label_repel(
        aes(label = title, size = number_of_users),
        label.size = NA, fill = alpha("black", alpha = .3),
        family = "Courier New", color = "white",
        max.overlaps = Inf, fontface = "bold",
        
        segment.size = .3, segment.linetype = "dotted",
    ) +
    
    scale_size_continuous(
        range = c(2.5, 5)
    ) +
    
    scale_x_continuous(expand = c(0, 0), limits = c(1979, 2017)) +
    
    scale_y_continuous(
        trans = "log10",
        labels = scales::comma_format(scale = 0.001, suffix = "k")
    ) +
    
    facet_wrap(vars(type)) +
    
    theme_minimal(base_family = my_font) +
    
    theme(
        legend.position = "none",
        
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(linetype = "dashed", linewidth = .3, color = "#395b71"),
        
        strip.text = element_text(face = "bold", color = "white"),
        
        axis.line = element_line(linewidth = .3, color = "white"),
        axis.ticks = element_line(linewidth = .3, color = "white"),
        
        axis.text = element_text(color = "white"),
        axis.title.x = element_text(margin = margin(t = 10), color = "white", hjust = 0, family = "PT Serif"),
        axis.title.y = element_text(margin = margin(r = 10), color = "white", hjust = 0, family = "PT Serif")
    ) +
    
    labs(
        y = "The estimated number of job\nopenings for programmers in each language",
        x = "Year that the language was publicly released and/or announced"
    )


library(patchwork)
library(ggtext)

multi = (gr2 | gr1)  +
    
    plot_annotation(
        title = 'Programming Languages',
        
        subtitle = paste0(
            "Famous programming languages based on number of users - ",
            "*point size* - and number of job openings - *y-axis*"
        ),
        
        caption = paste0(
            "Source: <b>Programming Language DataBase</b> | ",
            "Graphic: <b>Nikolaos Pechlivanis</b>"
        )
    ) &
    
    theme(
        plot.background = element_rect(fill = "#1f313d", color = NA),
        
        plot.title.position = "plot",
        plot.caption.position = "plot",
        
        plot.title = element_text(face = "bold", size = 36, color = "white", family = my_font),
        plot.subtitle = element_markdown(margin = margin(b = 15), size = 9, color = "white", family = my_font),
        plot.caption = element_markdown(margin = margin(t = 10), size = 6, color = "white", family = my_font),
        
        plot.margin = margin(20, 20, 20, 20)
    )


ggsave(
    plot = multi, filename = "Rplot.jpeg",
    width = 16, height = 10, units = "in"
)






