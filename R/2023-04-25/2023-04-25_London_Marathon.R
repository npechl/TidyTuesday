


rm(list = ls())
gc()

library(data.table)
library(stringr)

library(ggplot2)
library(ggforce)
library(ggtext)

library(extrafont)

winners         <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-04-25/winners.csv')
london_marathon <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-04-25/london_marathon.csv')

df = merge(winners, london_marathon, by = "Year")


x0 = df[, by = .(Category, Nationality), .N]

# ggplot(data = london_marathon, aes(x = Year)) +
#     
#     geom_area(aes(y = Applicants), alpha = .5, color = "white", linewidth = .25) +
#     
#     geom_area(aes(y = Accepted), alpha = .5, color = "white", linewidth = .25) + 
#     
#     geom_area(aes(y = Starters), alpha = .5, color = "white", linewidth = .25) + 
#     
#     geom_area(aes(y = Finishers), alpha = .5, color = "white", linewidth = .25) +
#     
#     scale_y_continuous(expand = c(0, 0))
    

gr = ggplot(data = x0, aes(x = Nationality, y = Category)) +
    
    geom_point(aes(fill = Category, size = N),
               shape = 21, stroke = .25, color = "grey10",
               position = position_jitternormal(sd_y = 0, sd_x = 0)) +
    
    scale_size_continuous(
        range = c(2, 7),
        guide = guide_legend(
            title = "No. of winners",
            title.position = "top"
        )
    ) +
    
    scale_fill_manual(
        values = c("#16BAC5", "#5FBFF9", "#f9995f", "#f95f72"),
        guide = "none"
    ) +
    
    theme_minimal(base_family = "Zilla Slab") +
    
    theme(
        legend.position = "bottom",
        legend.justification = "left",
        
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(linetype = "dashed", color = "grey85"),
        
        axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
        axis.text.y = element_text(face = "bold"),
        axis.title = element_blank(),
        
        plot.title.position = "plot",
        plot.caption.position = "plot",
        
        plot.title    = element_text(face = "bold", size = 26),
        plot.subtitle = element_markdown(margin = margin(b = 20), size = 10, color = "grey20"),
        plot.caption  = element_markdown(margin = margin(t = 20), size = 6),
        
        plot.background = element_rect(fill = "#EFE9F4", color = NA),
        plot.margin = margin(20, 20, 20, 20)
    ) +
    
    labs(
        
        title = 'London Marathon',
        
        subtitle = paste0(
            "The graph summarizes the number of *London Marathon winners per country and category of race*"
        ),
        
        caption = paste0(
            "Source: <b>LondonMarathon R package</b> | ",
            "Graphic: <b>Nikolaos Pechlivanis</b>"
        )
    )


ggsave(
    plot = gr, filename = "Rplot.jpeg",
    width = 12, height = 6, units = "in"
)





