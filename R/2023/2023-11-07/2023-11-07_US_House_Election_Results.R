


rm(list = ls())
gc()

library(data.table)
library(stringr)

library(ggplot2)
library(ggtext)
library(ggsci)
library(geofacet)

library(extrafont)

house <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-11-07/house.csv')



house$office = NULL

house$level = ifelse(
    house$party %in% c("DEMOCRAT", "REPUBLICAN", "LIBERTARIAN", "CONSERVATIVE"),
    house$party,
    "OTHER"
)


df = house[which(!writein), by = .(year, state, level), .(
    candidatevotes = candidatevotes |> sum(),
    totalvotes     = totalvotes |> sum()
)]

df$candidatevotes_perc = df$candidatevotes / df$totalvotes

df = df[which(state != "DISTRICT OF COLUMBIA")]

df$state = df$state |> str_to_title()
df$level = df$level |> str_to_title()

my_font = "Jost"

gr = ggplot(data = df) +
    
    geom_line(aes(x = year, y = candidatevotes_perc, group = level, color = level)) +
    
    # geom_point(aes(x = year, y = candidatevotes_perc, group = level, color = level),
    #            shape = 21, stroke = .25, fill = "white") +
    
    scale_color_manual(
        values = c("#FF6F00FF", "#C71000FF", "#008EA0FF", "#8A4198FF", "#3F4041"),
        guide = guide_legend(
            # title = "Party", 
            # title.position = "top", 
            # title.theme = element_text(family = my_font, size = 10),
            label.theme = element_text(my_font, size = 8),
            override.aes = list(linewidth = 2)
        )
    ) +
    
    scale_y_continuous(labels = scales::percent) +
    
    facet_geo(vars(state)) +
    
    theme_minimal(base_family = my_font) +
    
    theme(
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.justification = "left",
        
        strip.text = element_text(face = "bold", hjust = 0),
        
        axis.text.x = element_text(angle = 45, hjust = 1, size = 6),
        axis.text.y = element_text(size = 8),
        
        axis.title.x = element_blank(),
        axis.title.y = element_text(margin = margin(r = 10)),
        
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(linewidth = .3, linetype = "dashed", color = "grey85"),
        
        panel.spacing = unit(1, "lines"),
        
        plot.title    = element_text(face = "bold", size = 36, margin = margin(b = 5), family = my_font),
        plot.subtitle = element_markdown(margin = margin(b = 25), size = 12, color = "grey20", family = my_font),
        plot.caption  = element_markdown(margin = margin(t = 25), size = 8, family = my_font),
        
        plot.title.position = "plot",
        plot.caption.position = "plot",
        
        plot.background = element_rect(fill = "#f5f5f5", color = NA),
        plot.margin = margin(20, 20, 20, 20)
    ) +
    
    labs(
        y = "Percentage of total number of votes cast for each election",
        
        title = 'US House Election Results',
        
        subtitle = paste0(
            "The breakdown of vote percentages for the ",
            "<b style='color:#FF6F00FF'>Conservative</b>, ",
            "<b style='color:#C71000FF'>Democrat</b>, ",
            "<b style='color:#008EA0FF'>Libertarian</b>, and ",
            "<b style='color:#3F4041'>Republic</b> parties from 1976 to 2022."
        ),
        
        caption = paste0(
            "Source: <b>MIT Election Data and Science Lab</b> | ",
            "Graphic: <b>Nikolaos Pechlivanis</b>"
        )
    )


ggsave(
    plot = gr, filename = "Rplot.jpeg",
    width = 14, height = 12, units = "in", dpi = 600
)

