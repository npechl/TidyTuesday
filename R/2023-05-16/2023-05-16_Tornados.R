# 
# 
# 
# rm(list = ls())
# gc()
# 
# library(data.table)
# library(stringr)
# 
# tornados <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-05-16/tornados.csv')
# 
# tornados = tornados[order(om)]
# 
# tornados = tornados[which(!is.na(mag))]
# 
# 
# library(ggplot2)
# 
# gr = ggplot(data = tornados) +
#     
#     geom_point(aes(x = date, y = mag, size = as.character(mag), fill = as.character(mag)),
#                shape = 21, color = "white", stroke = .01) +
#     
#     scale_size_manual(
#         values = c(
#             "0" = 1,
#             "1" = 2,
#             "2" = 3,
#             "3" = 4,
#             "4" = 5,
#             "5" = 6
#         ),
#         
#         breaks = c("0", "1", "2", "3", "4", "5"),
#         labels = c(
#             "65–85 mph Light damage",
#             "86–110 mph Moderate damage",
#             "111–135 mph Considerable damage",
#             "136–165 mph Severe damage",
#             "166–200 mph Devastating damage",
#             ">200 mph Incredible damage"
#         )
#     ) +
#     
#     facet_wrap(vars(st), scales = "free_y", ncol = 3) +
#     
#     # facet_grid(
#     #     rows = vars(st),
#     #     
#     #     switch = "y",
#     #     
#     #     scales = "free_y"
#     # ) +
#     
#     theme_minimal() +
#     
#     theme(
#         legend.position = "bottom",
#         
#         panel.spacing.y = unit(1, "lines"),
#         axis.text.y = element_blank(),
#         
#         # strip.text.y.left = element_text(angle = 0, hjust = 1),
#         
#         plot.margin = margin(20, 20, 20 ,20)
#     )
# 
# 
# ggsave(
#     plot = gr, filename = "Rplot.jpeg",
#     width = 20, height = 20, units = "in"
# )


rm(list = ls())
gc()


library(data.table)
library(stringr)

library(ggplot2)
library(ggtext)



df <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-05-16/tornados.csv')
df = df[which(!is.na(mag))]


gr = ggplot(data = df) +
    
    geom_segment(
        aes(
            x = date, y = 0, xend = date, yend = 1,
            color = as.character(mag)
        )
    ) +
    
    scale_color_manual(
        values = c(
            "5" = "#142C5F",
            "4" = "#124E63",
            "3" = "#2E6A57",
            "2" = "#629f45",
            "1" = "#F6A895",
            "0" = "#FFBAD7"
        ),

        labels = c(
            "5" = "Incredible",
            "4" = "Devastating",
            "3" = "Severe",
            "2" = "Considerable",
            "1" = "Moderate",
            "0" = "Light"
        ),
        
        guide = guide_legend(override.aes = aes(linewidth = 2))
    ) +
    
    scale_x_date(expand = c(0, 0)) +
    
    facet_grid(rows = vars(st), switch = "y") +
    
    theme_minimal(base_family = "Zilla Slab") +
    
    theme(
        panel.spacing.y = unit(0, "lines"),
        strip.text.y.left = element_text(size = 10, angle = 0, hjust = 1),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        
        plot.title    = element_text(face = "bold", size = 36, family = "Zilla Slab"),
        plot.subtitle = element_markdown(margin = margin(b = 10), size = 12, color = "grey20", family = "Zilla Slab"),
        plot.caption  = element_markdown(margin = margin(t = 10), size = 6, family = "Zilla Slab"),
        
        plot.background = element_rect(color = NA, fill = "#fafaf9"),
        plot.margin = margin(20, 20, 20, 20)
    ) +
    
    labs(
        color = "Magnitude on the F scale",
        
        title = 'Tornados',
        
        subtitle = paste0(
            "Tornado magnitudes by state, 1950-2022"
        ),
        
        caption = paste0(
            "Source: <b>NOAA's National Weather Service Storm Prediction Center Severe Weather Maps, Graphics, and Data Page</b> | ",
            "Graphic: <b>Nikolaos Pechlivanis</b>"
        )
    )


ggsave(
    plot = gr, filename = "Rplot.jpeg",
    width = 16, height = 12, units = "in"
)





