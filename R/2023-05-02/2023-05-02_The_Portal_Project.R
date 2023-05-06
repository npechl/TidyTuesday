


rm(list = ls())
gc()

library(data.table)
library(stringr)

library(ggplot2)
library(ggtext)

library(ggh4x)

library(paletteer)

library(extrafont)


# plots   <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-05-02/plots.csv')
df <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-05-02/species.csv')
# surveys <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-05-02/surveys.csv')

# df = merge(surveys, species, by = "species")


rm(plots, species, surveys)
gc()


gr1 = ggplot(data = df, aes(y = scientificname)) +
    
    geom_segment(aes(x = minhfl, xend = maxhfl, yend = scientificname),
                 linetype = "dotted", linewidth = .5) +
    
    geom_point(aes(x = maxhfl), shape = 21, size = 3.5, stroke = .5, fill = "#320A28", color = "grey96") +
    geom_point(aes(x = meanhfl), shape = 21, size = 3.5, stroke = .5, fill = "#3E5641", color = "grey96") +
    geom_point(aes(x = minhfl), shape = 21, size = 3.5, stroke = .5, fill = "#EF3054", color = "grey96") +
    
    theme_minimal(base_family = "Zilla Slab") +
    
    theme(
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        
        axis.title.y = element_blank(),
        axis.title.x = element_text(face = "bold", hjust = 0, margin = margin(t = 10)),
        axis.text = element_text(face = "bold")
    ) +
    
    labs(x = "Hindfoot length")


gr2 = ggplot(data = df, aes(y = scientificname)) +
    
    geom_segment(aes(x = minwgt, xend = maxwgt, yend = scientificname),
                 linetype = "dotted", linewidth = .5) +
    
    geom_point(aes(x = maxwgt), shape = 21, size = 3.5, stroke = .5, fill = "#320A28", color = "grey96") +
    geom_point(aes(x = meanwgt), shape = 21, size = 3.5, stroke = .5, fill = "#3E5641", color = "grey96") +
    geom_point(aes(x = minwgt), shape = 21, size = 3.5, stroke = .5, fill = "#EF3054", color = "grey96") +
    
    theme_minimal(base_family = "Zilla Slab") +
    
    theme(
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        
        axis.title.y = element_blank(),
        axis.title.x = element_text(face = "bold", hjust = 0, margin = margin(t = 10)),
        # axis.text.y = element_blank(),
        axis.text = element_text(face = "bold")
    ) +
    
    labs(x = "Weight")

library(patchwork)


multi = (gr1 | gr2) +
    
    plot_annotation(
        title = 'The Portal Project',
        subtitle = paste0(
            "The graph depicts ",
            "<b style='color:#EF3054'>minimum</b>, ",
            "<b style='color:#3E5641'>mean</b>, and ",
            "<b style='color:#320A28'>maximum</b> **Hindfoot length**", 
            " and **Weight** for 21 species coming from the **Portal Project**"
        ),
        caption = paste0(
            "Source: <b>Portal Project</b> | ",
            "Graphic: <b>Nikolaos Pechlivanis</b>"
        )
    ) &
    
    theme(
        plot.title    = element_text(face = "bold", size = 26, family = "Zilla Slab"),
        plot.subtitle = element_markdown(margin = margin(b = 10), size = 10, color = "grey20", family = "Zilla Slab"),
        plot.caption  = element_markdown(margin = margin(t = 10), size = 6, family = "Zilla Slab"),
        
        plot.background = element_rect(color = NA, fill = "#f2f3f4"),
        plot.margin = margin(20, 20, 20, 20)
    )


ggsave(
    plot = multi, filename = "Rplot.jpeg",
    width = 16, height = 8, units = "in"
)






# 
# 
# gr = ggplot(data = df, aes(x = censusdate, y = meanhfl, group = commonname)) +
#     
#     geom_ribbon(aes(ymin = minhfl, ymax = maxhfl, fill = commonname), alpha = .5) +
#     
#     geom_line(aes(color = commonname), linewidth = 1) +
#     
#     scale_fill_manual(values = paletteer_d("ggsci::default_igv")) +
#     scale_color_manual(values = paletteer_d("ggsci::default_igv")) +
#     
#     # geom_point(aes(color = commonname)) +
#     
#     facet_wrap2(vars(commonname), nrow = 3, axes = "all") +
#     
#     theme_minimal(base_family = "Zilla Slab") +
#     
#     theme(
#         legend.position = "none",
#         
#         strip.text = element_text(face = "bold", hjust = 0),
#         
#         axis.text = element_text(size = 8),
#         
#         axis.title.y = element_markdown(hjust = 0, margin = margin(r = 10)),
#         
#         panel.grid.minor = element_blank(),
#         panel.grid.major = element_line(linetype = "dashed", linewidth = .3, color = "grey75"),
#         
#         panel.spacing = unit(1, "lines"),
#         
#         plot.background = element_rect(color = NA, fill = "#f2f3f4"),
#         
#         plot.margin = margin(20, 20, 20, 20)
#     ) +
#     
#     labs(
#         y = "Hindfoot Length (*minimum*, *mean*, and *maximum*)"
#     )


ggsave(
    plot = gr, filename = "Rplot.jpeg",
    width = 16, height = 8, units = "in"
)






