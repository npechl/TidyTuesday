


rm(list = ls())
gc()


library(data.table)
library(stringr)


library(ggplot2)
library(ggh4x)
library(ggtext)

library(extrafont)

df = fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-10-11/yarn.csv')


for(i in colnames(df)) {
    
    if(is.character(df[[i]])) {
        
        df[[i]] = str_squish(df[[i]])
        
    }
    
}

df[which(df$yarn_weight_name == "No weight specified"), ]$yarn_weight_name = NA

df = df[which(!is.na(df$yarn_weight_name)), ]

df = df[order(yarn_weight_name), ]

df = df[which(df$grams < 6000), ]
df = df[which(df$yardage != 0), ]
df = df[which(df$grams != 0), ]

df = df[which(!(
    df$yarn_weight_name %in% c(
        "Aran / Worsted",
        "DK / Sport"
    )
)), ]


df$machine_washable_label = ifelse(
    df$machine_washable,
    "Yes", "No"
)

df[which(is.na(df$machine_washable_label)), ]$machine_washable_label = "Not Available"



df$group = paste0(
    "**", df$yarn_weight_name, "**", "<br>",
    "Knit guage: *", df$yarn_weight_knit_gauge, "*", "<br>",
    "Ply: *", df$yarn_weight_ply, "*", "<br>",
    "Wraps per inch: *", df$yarn_weight_wpi, "*"
)

df$group = str_replace_all(df$group, "NA", "not available")

x = unique(
    df[, c("yarn_weight_name", 
           "yarn_weight_ply", 
           "yarn_weight_knit_gauge", 
           "yarn_weight_wpi"), with = FALSE]
)


# gr = ggplot(data = df, aes(x = grams, y = yardage)) +
#     
#     geom_point(
#         aes(
#             color = rating_average,
#             shape = machine_washable_label
#         ), 
#         
#         alpha = .3, size = 1.5
#     ) +
#     
#     scale_color_gradientn(
#         colors = c("#E14D2A", "#CD104D", "#9C2C77"),
#         limits = c(1, 5),
#         breaks = c(1, 3, 4, 5),
#         trans = "exp",
#         
#         guide = guide_colorbar(
#             title.position = "top",
#             barwidth = 12,
#             barheight = 0.5
#         )
#     ) + 
#     
#     scale_shape_manual(
#         values = c(
#           "Yes" = 16,
#           "No"  = 17,
#           "Not Available" = 18
#         ),
#         
#         guide = guide_legend(
#             title.position = "top"
#         )
#     ) +
#     
#     scale_x_continuous(
#         trans = "log10", 
#         labels = scales::comma
#     ) +
#     
#     scale_y_continuous(
#         trans = "log10", 
#         labels = scales::comma
#     ) +
#     
#     facet_wrap2(
#         vars(group),
#         axes = "x",
#         nrow = 2
#     ) +
#     
#     # facet_grid(
#     #     rows = vars(yarn_weight_name),
#     #     cols = vars(yarn_weight_wpi)
#     # ) +
#     
#     coord_cartesian(expand = TRUE, clip = "off") +
#     
#     theme_minimal(base_family = "Century Gothic") +
#     
#     theme(
#         legend.position = "bottom",
#         
#         strip.text = element_markdown(hjust = 0),
#         
#         plot.title.position = "plot",
#         plot.caption.position = "plot",
#         
#         plot.title = element_text(size = 14, face = "bold"),
#         
#         plot.subtitle = element_markdown(
#             margin = margin(b = 10), size = 10,
#             color = "gray30"
#         ),
#         
#         plot.caption = element_markdown(margin = margin(t = 10)),
#         
#         panel.spacing = unit(1, "lines"),
#         
#         
#         # plot.background = element_rect(fill = "#f2f2f2", color = "#f2f2f2"),
#         plot.margin = margin(20, 20, 20, 20)
#     ) +
#     
#     labs(
#         color = "Rating Average",
#         shape = "Is Machine Washable?",
#         
#         title = "Ravelry Yarn",
#         subtitle = "textextextextextextextextext",
#         caption = paste0(
#             "Source: **ravelry.com** | ",
#             "Graphic: **Nikolaos Pechlivanis**",
#             ", **Styliani-Christina Fragkouli**"
#         )
#     )

gr = ggplot(data = df, aes(x = yardage, y = rating_average)) +
    
    geom_point(
        aes(
            color = grams,
            shape = machine_washable_label
        ), 
        
        alpha = .3, size = 1.5
    ) +
    
    scale_color_gradientn(
        colors = c("#FFF7E9", "#FF731D", "#1746A2"),
        limits = c(1, 2300),
        breaks = c(1, 10, 100, 1000),
        trans = "log10",

        guide = guide_colorbar(
            title.position = "top",
            barwidth = 12,
            barheight = 0.5
        )
    ) +
    
    scale_shape_manual(
        values = c(
            "Yes" = 16,
            "No"  = 17,
            "Not Available" = 18
        ),
        
        guide = guide_legend(
            title.position = "top"
        )
    ) +
    
    scale_x_continuous(
        trans = "log10", 
        labels = scales::comma,
        expand = c(0, 0)
    ) +
    
    scale_y_continuous(
        expand = c(0, 0),
        trans = "exp",
        breaks = c(1, 3, 4, 5)
    ) +
    
    facet_wrap2(
        vars(group),
        axes = "all",
        nrow = 2
    ) + 
    
    coord_cartesian(expand = TRUE, clip = "off") +
    
    theme_minimal(base_family = "Century Gothic") +
    
    theme(
        legend.position = "bottom",
        
        strip.text = element_markdown(hjust = 0),
        
        plot.title.position = "plot",
        plot.caption.position = "plot",
        
        plot.title = element_text(size = 14, face = "bold"),
        
        plot.subtitle = element_markdown(
            margin = margin(b = 10), size = 10,
            color = "gray30"
        ),
        
        plot.caption = element_markdown(margin = margin(t = 10)),
        
        panel.spacing = unit(1, "lines"),
        
        panel.grid.major = element_line(color = "gray90", size = .3),
        panel.grid.minor = element_line(color = "gray90", size = .3),
        
        axis.ticks = element_line(color = "gray90", size = .3),
        
        # panel.border = element_rect(fill = NA),
        
        axis.title.x = element_text(margin = margin(t = 5)),
        axis.title.y = element_text(margin = margin(r = 5)),
        
        
        
        plot.background = element_rect(fill = "#d9d9d9", color = "#d9d9d9"),
        plot.margin = margin(20, 20, 20, 20)
    ) +
    
    labs(
        color = "Unit weight in grams",
        shape = "Is Machine Washable?",
        
        x = "Yardage (length measured in yards)",
        y = "Rating Average",
        
        title = "Ravelry Yarn",
        subtitle = "The average rating out of 5 per Yarn weight name",
        caption = paste0(
            "Source: **ravelry.com** | ",
            "Graphic: **Nikolaos Pechlivanis**",
            ", **Styliani-Christina Fragkouli**"
        )
    )

ggsave(
    filename = "Rplot.pdf",
    plot = gr,
    width = 14, height = 10,
    device = cairo_pdf,
    units = "in"
)

ggsave(
    filename = "Rplot.png",
    plot = gr,
    width = 14, height = 10,
    units = "in"
)
