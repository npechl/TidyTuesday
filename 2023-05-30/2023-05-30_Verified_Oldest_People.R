
rm(list = ls())
gc()


library(data.table)
library(stringr)

library(ggplot2)
library(ggtext)

library(ggrepel)

library(extrafont)

df <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-05-30/centenarians.csv')

df = df[order(age)]

df$rank2 = 1:nrow(df)

df2 = df[which(still_alive == "alive")]
df  = df[which(still_alive != "alive")]

df2$label = paste0(
    df2$name, "\n",
    "birth date: ", df2$birth_date
)

my_font = "Raleway"


gr = ggplot(data = df, aes(x = gender, y = age)) +
    
    geom_point(
        shape = 21, color = "black", fill = "grey75",
        size = 2, stroke = .25
    ) +
    
    geom_text_repel(
        aes(label = name, size = age, color = still_alive), 
        max.overlaps = Inf, segment.size = .25, segment.linetype = "dotted",
        family = my_font
    ) +
    
    geom_label_repel(
        data = df2, aes(label = label, size = age, color = still_alive), 
        max.overlaps = Inf, segment.size = .25, segment.linetype = "dotted",
        box.padding = 1, label.size = NA, fill = alpha("white", alpha = .5),
        fontface = "bold", hjust = 0, family = my_font
    ) +
    
    scale_color_manual(
        values = c(
            "alive"    = "#e32636",
            "deceased" = "#8a2be2"
        )
    ) +
    
    scale_size_continuous(range = c(1, 4)) +
    
    scale_y_continuous(expand = c(0, 0), limits = c(110, 123),
                       breaks = c(112, 114, 116, 118, 120, 122)) +
    
    theme_minimal(base_family = my_font) +
    
    theme(
        legend.position = "bottom",
        legend.justification = "left",
        
        axis.line = element_line(linewidth = .3, arrow = arrow(type = "closed", length = unit(.075, "inches"))),
        axis.ticks = element_line(linewidth = .3),
        
        axis.text.x = element_text(face = "bold"),
        
        axis.title.x = element_blank(),
        axis.title.y = element_markdown(margin = margin(r = 10), hjust = 0, size = 10),
        
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(linetype = "dashed", linewidth = .3, color = "#f8c2be"),
        
        plot.title    = element_text(face = "bold", size = 32, family = my_font),
        plot.subtitle = element_markdown(margin = margin(b = 10), size = 11, color = "grey20", family = my_font),
        plot.caption  = element_markdown(margin = margin(t = 10), size = 6, family = my_font),
        
        plot.background = element_rect(fill = "#fcf4f8", color = NA),
        plot.margin = margin(20, 20, 20, 20)
    ) +
    
    guides(
        color = guide_legend(
            title = "Is the person still alive?",
            title.position = "top",
            title.theme = element_text(size = 10, family = my_font)
        ),
        
        size = guide_legend(
            title = "The person's age",
            title.position = "top",
            title.theme = element_text(size = 10, family = my_font)
        )
    ) +
    
    labs(
        y = paste0(
            "**The person's age**, either on the day of their death or ",
            "on the day when the dataset was extracted on 2023-05-25."
        ),
        
        title = 'Verified Oldest People',
        
        subtitle = paste0(
            "***These are lists of the 100 known verified oldest people sorted ",
            "in descending order by age in years and days.*** *The oldest person ",
            "ever whose age has been independently verified is Jeanne Calment ",
            "(1875–1997) of France,<br>who lived to the age of 122 years and 164 ",
            "days. The oldest verified man ever is Jiroemon Kimura (1897–2013) ",
            "of Japan, who lived to the age of 116 years and 54 days. The ",
            "oldest known living person is Maria Branyas of Spain,<br>aged 116 ",
            "years, 85 days. The oldest known living man is Juan Vicente Pérez ",
            "of Venezuela, aged 114 years, 1 day. The 100 oldest women have, on ",
            "average, lived several years longer than the 100 oldest men.*"
        ),
        
        caption = paste0(
            "Source: <b>Wikipedia List of the verified oldest people via frankiethull on GitHub</b> | ",
            "Graphic: <b>Nikolaos Pechlivanis</b>"
        )
    )




ggsave(
    plot = gr, filename = "Rplot.jpeg",
    width = 16, height = 12, units = "in", dpi = 600
)







# a = df2[c(1, 3, 4, 6, 7, 10, 12)]
# b = df2[c(2, 5, 8, 9, 11)]
# 
# ggplot(data = df, aes(x = rank, y = age)) +
#     
#     geom_point(aes(fill = gender), shape = 21, 
#                color = "black", size = 2, stroke = .25) +
#     
#     geom_richtext(
#        data = b,  aes(x = rank, y = age, label = label),
#        hjust = 0, position = position_nudge_repel(x = 1, y = 1),
#        label.size = NA, fill = alpha("white", alpha = .3)
#     ) +
#     
#     # geom_label_repel(
#     #     data = df2, aes(x = rank, y = age, label = label),
#     #     box.padding = .5,
#     #     label.size = NA, fill = alpha("white", alpha = .3),
#     #     max.overlaps = Inf, size = 2.5, hjust = 0, segment.size = .25
#     # ) +
#     
#     scale_x_continuous(expand = c(0, 0), limits = c(-3, 105),
#                        breaks = c(0, 100),
#                        labels = c("Oldest", "Youngest")) +
#     
#     scale_y_continuous(expand = c(0, 0), limits = c(110, 123),
#                        breaks = c(112, 114, 116, 118, 120, 122)) +
#     
#     theme_minimal() +
#     
#     theme(
#         legend.position = "bottom",
#         legend.title = element_blank(),
#         axis.line.x = element_line(
#             linewidth = .25, 
#             arrow = arrow(type = "closed", length = unit(.1, "inches"))
#         ),
#         
#         panel.grid.minor = element_blank(),
#         panel.grid.major.x = element_blank(),
#         panel.grid.major.y = element_line(linetype = "dashed"),
#         
#         axis.title.x = element_blank(),
#         
#         plot.margin = margin(20, 20, 20, 20)
#     )
#     
#     
# 
# 
# ggsave(
#     plot = gr, "Rplot.jpeg",
#     width = 10, height = 8, units = "in"
# )
# 
# library(gridtext)
# 
# 
# g = richtext_grob(text = df2$label)
# 
# library(ggforce)
# 
# ggplot(data = df, aes(x = age, y = place_of_death_or_residence)) +
#     
#     geom_point(aes(fill = gender), 
#                color = "black", 
#                shape = 21, size = 2, stroke = .25) +
#     
#     geom_label_repel(
#         data = df2, aes(x = age, y = place_of_death_or_residence, label = name),
#         box.padding = .75, max.overlaps = Inf, min.segment.length = 0,
#         label.size = NA, fill = alpha("white", alpha = .3),
#         size = 2.5, hjust = 0, segment.size = .25, fontface = "bold"
#     ) +
#     
#     scale_x_continuous(expand = c(0, 0), limits = c(110, 123), 
#                        breaks = c(112, 114, 116, 118, 120, 122)) +
#     
#     theme(
#         legend.title = element_blank(),
#         
#         axis.title.y = element_blank(),
#         plot.margin = margin(20, 20, 20, 20)
#     )














