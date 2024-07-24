


rm(list = ls())
gc()


library(data.table)
library(stringr)

wcmatches <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-11-29/wcmatches.csv')
worldcups <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-11-29/worldcups.csv')

library(ggplot2)
library(ggtext)
library(ggforce)
library(ggtext)

library(extrafont)


worldcups$label = paste0(worldcups$year, " Host country: ", worldcups$host)
worldcups$desc  = paste0(
    "winner: ", worldcups$winner, "\n",
    "second: ", worldcups$second, "\n",
    "third: ", worldcups$third, "\n",
    "fourth: ", worldcups$fourth
)


# worldcups[which(host == "Japan, South Korea"), ]$host = "Japan"
# 
# worldcups = rbind(worldcups, worldcups[which(host == "Japan"), ])
# worldcups[nrow(worldcups), ]$host = "South Korea"


gr = ggplot(data = worldcups, aes(x = year, y = attendance)) +
    
    geom_line(color = "#1d0516", linewidth = 1) +
    
    geom_point() +
    
    geom_mark_circle(
        aes(label = label, description = desc),
        fill = NA,
        
        expand = unit(.3, "lines"),
        n = 1, 
        
        con.cap = 0,
        con.linetype = "dotted",
        con.colour = "#051d18",
        
        label.buffer = unit(1, "mm"),
        label.fontsize = 6,
        label.family = "Comfortaa",
        label.fill = alpha("white", alpha = .75)
    ) +
    
    scale_x_continuous(breaks = seq(1930, 2018, by = 11)) +
    
    scale_y_continuous(limits = c(0, 3600000), expand = c(0, 0),
                       labels = scales::comma_format(suffix = "M", scale = 1e-6),
                       breaks = c(1000000, 2000006, 3000000)) +
    
    coord_cartesian(expand = TRUE, clip = "off") +
    
    theme_minimal(base_family = "Comfortaa") +
    
    theme(
        
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        
        axis.line.x = element_line(),
        axis.ticks.x = element_line(),
        
        axis.title.x = element_blank(),
        
        plot.title.position = "plot",
        plot.caption.position = "plot",
        
        plot.title = element_text(family = "Zilla Slab", face = "bold", size = 24),
        plot.subtitle = element_text(family = "Zilla Slab", margin = margin(b = 10)),
        
        plot.caption = element_markdown(margin = margin(t = 10), size = 8),
        
        plot.margin = margin(20, 20, 20, 20)
    ) +
    
    labs(
        y        = "Attendance",
        
        title    = "World Cup",
        subtitle = "A summary of each World Cup held. ",
        caption = paste0(
            "Source: <b>Kaggle</b>",
            " | ",
            "Graphic: <b>Pechlivanis Nikolaos</b>"
            
        )
    )


ggsave(
    plot = gr, filename = "Rplot.pdf",
    width = 12, height = 10, units = "in",
    device = cairo_pdf
)

ggsave(
    plot = gr, filename = "Rplot.jpeg",
    width = 12, height = 10, units = "in"
)
