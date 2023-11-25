


rm(list = ls())
gc()

library(data.table)
library(stringr)

library(ggplot2)
library(ggtext)

library(ggsci)

library(extrafont)



rladies_chapters <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-11-21/rladies_chapters.csv')

rladies_chapters$date2 = rladies_chapters$date |> 
    str_split("-") |>
    lapply(function(x) paste(x[1], x[2], "01", sep = "-")) |>
    unlist() |>
    as.Date()

df = rladies_chapters[, by = .(date2, location), .(
    N = .N,
    chapters = chapter |> unique() |> sort() |> paste(collapse = ", ")
)]

my_font = "Jost"

gr = ggplot(data = df) +
    
    geom_area(data = df[which(location == "inperson")], aes(x = date2, y = N, fill = location), alpha = .3) +
    geom_area(data = df[which(location == "online")], aes(x = date2, y = N, fill = location), alpha = .3) +
    
    geom_line(aes(x = date2, y = N, color = location, group = location)) +
    
    geom_point(aes(x = date2, y = N, color = location), size = .85) +
    
    scale_color_futurama(guide = "none") +
    scale_fill_futurama() +
    
    scale_y_continuous(expand = c(0, 0), limits = c(0, 120), 
                       breaks = c(20, 40, 60, 80, 100, 120)) +
    
    theme_minimal(base_family = my_font) +
    
    theme(
        legend.title = element_blank(),
        legend.position = "bottom",
        legend.justification = "left",
        
        axis.line.x = element_line(linewidth = .3),
        axis.ticks.x = element_line(linewidth = .3),
        
        panel.grid.minor = element_blank(),
        
        panel.grid.major.x = element_line(linewidth = .3, color = "grey85"),
        panel.grid.major.y = element_line(linewidth = .3, color = "grey85"),
        
        plot.title    = element_text(face = "bold", size = 36, margin = margin(b = 5), family = my_font),
        plot.subtitle = element_markdown(margin = margin(b = 25), size = 11, color = "grey20", family = my_font),
        plot.caption  = element_markdown(margin = margin(t = 25), size = 8, family = my_font),
        
        plot.title.position = "plot",
        plot.caption.position = "plot",
        
        plot.background = element_rect(fill = "#f5f5f5", color = NA),
        plot.margin = margin(20, 20, 20, 20)
    ) +
    
    labs(
        x = "Event date",
        y = "Number of R-Ladies Chapter Events",
        
        title = 'R-Ladies Chapter Events',
        
        subtitle = paste0(
            "*R-Ladies Global is an inspiring story of community, empowerment,",
            "and diversity in the field of data science with a simple ", 
            "mission:<br>to promote gender diversity in the R programming ",
            "community and provide a welcoming space for women and gender ",
            "minorities to learn, collaborate, and excel in data science.*"
        ),
        
        caption = paste0(
            "Source: <b>rladies meetup-archive</b> | ",
            "Graphic: <b>Nikolaos Pechlivanis</b>"
        )
    )

ggsave(
    plot = gr, filename = "Rplot.jpeg",
    width = 12, height = 10, units = "in", dpi = 600
)


