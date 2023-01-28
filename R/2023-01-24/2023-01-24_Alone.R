


rm(list = ls())
gc()


library(data.table)
library(stringr)

library(ggplot2)
library(ggrepel)

library(gggibbous)

library(ggsci)
library(ggh4x)

library(ggtext)

library(patchwork)
library(extrafont)

survivalists <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-01-24/survivalists.csv')
loadouts     <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-01-24/loadouts.csv')
episodes     <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-01-24/episodes.csv')
seasons      <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-01-24/seasons.csv')


my_font = "Comfortaa"
survivalists$season = paste0("Season ", survivalists$season)

survivalists[, by = .(season, result), gender_ratio := 1 / length(unique(gender))]
survivalists$gender_right = survivalists$gender == "Female"

gr1 = ggplot(data = survivalists, aes(x = result, y = days_lasted)) +
    
    geom_area(alpha = .3) +
    
    geom_line(color = "grey10") +
    
    geom_moon(
        aes(ratio = gender_ratio, right = gender_right, fill = gender),
        size = 3.5, stroke = .25
    ) +
    
    scale_y_continuous(expand = c(0, 0)) +
    
    scale_x_continuous(
        breaks = c(10, 9, 8, 7, 6, 5, 4, 3, 2, 1)
    ) +
    
    scale_fill_npg() +
    
    facet_wrap2(vars(season), scales = "free_x", axes = "all") +
    
    coord_cartesian(expand = TRUE, clip = "on") +
    
    theme_minimal(base_family = my_font) +
    
    theme(
        legend.position = "right",
        legend.title = element_blank(),
        
        panel.spacing = unit(1, "lines"),
        
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(linewidth = .3, linetype = "dashed", color = "grey"),
        
        axis.line.x = element_line(linewidth = .3),
        axis.ticks.x = element_line(linewidth = .3),
        
        axis.title.x = element_blank(),
        axis.title.y = element_text(hjust = 0, margin = margin(r = 0)),
        axis.text = element_text(size = 8),
        
        strip.text = element_text(face = "bold", margin = margin(b = 15))
    ) +
    
    labs(y = "Number of days\nlasted in the game")


index = match(loadouts$name, survivalists$name)

loadouts$result = survivalists[index, ]$result

loadouts = loadouts[, by = result, total := length(unique(name))]


x = loadouts[, by = .(result, total, item), .(
    N = length(unique(name))
)]

x$label = paste0(
    "*", x$N, "* (",
    round(100 * x$N / x$total, digits = 2), "%)"
)

x$Freq = x$N / x$total

x$result_label = ifelse(
    x$result == 1, "**1st** place",
    ifelse(
        x$result == 2, "**2nd** place",
        ifelse(
            x$result == 3, "**3rd** place",
            paste0("**", x$result, "th** place")
        )
    )
)

x = x[order(result, decreasing = TRUE), ]

x$result_label = factor(
    x$result_label,
    levels = unique(x$result_label)
)



gr2 = ggplot(data = x, aes(x = item, y = result_label)) +
    
    geom_point(size = 5, shape = 21, fill = "white", color = "grey") +
    
    geom_point(aes(size = Freq), color = "#00A087FF") +
    
    scale_size_continuous(
        range = c(2, 5),
        labels = scales::percent
    ) +
    
    theme_minimal(base_family = my_font) +
    
    theme(
        legend.position = "right",
        legend.title = element_text(size = 9),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
        axis.text.y = element_markdown(size = 9),
        
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(
            linewidth = .3, 
            color = "grey",
            linetype = "dashed"
        ),
        
        axis.title.x = element_blank(),
        axis.title.y = element_text(hjust = 0, margin = margin(r = 10))
    ) +
    
    labs(
        size = "Percentage of survivalists\nwith loadout item",
        y = "Place survivalist\nfinished in the game"
    )




multi = gr1 / gr2 + 
    
    plot_annotation(
        title = "Alone",
        
        subtitle = paste0(
            "This dataset contains data from the TV series Alone. The graph ",
            "summarises **the number of days that each survivalist<br>lasted in the game** ",
            "and **information on each survivalist’s loadout** (The rules ",
            "allow each survivalist to take 10 items with them).<br>The summary ",
            "was applied across the place that each survivalist finished in the season."
        ),
        
        caption = paste0(
            "Source: <b>{alone} data package</b> | ",
            "Graphic: <b>Nikolaos Pechlivanis</b>"
        )
    ) +
    
    plot_layout(heights = c(2, 1)) &
    
    theme(
        plot.background = element_rect(fill = "#fdfbfa", color = NA),
        
        plot.title.position = "plot",
        plot.caption.position = "plot",
        
        plot.title = element_text(face = "bold", size = 30, family = "Zilla Slab"),
        plot.subtitle = element_markdown(margin = margin(b = 20), size = 10, family = my_font),
        plot.caption = element_markdown(margin = margin(t = 20), size = 7, family = my_font),
        
        plot.margin = margin(10, 10, 10, 10)
    ) 



ggsave(
    plot = multi, filename = "Rplot.jpeg",
    width = 10, height = 12, units = "in"
)




