


rm(list = ls())
gc()

library(data.table)
library(stringr)

library(ggplot2)
library(ggtext)

library(ggrepel)

library(extrafont)

drwho_episodes  <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-11-28/drwho_episodes.csv')
drwho_directors <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-11-28/drwho_directors.csv')
drwho_writers   <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-11-28/drwho_writers.csv')

drwho_episodes = drwho_episodes[order(first_aired)]

drwho_episodes$season = paste0("Season: ", drwho_episodes$season_number)

drwho_episodes$season = ifelse(
    is.na(drwho_episodes$season_number), "Special",
    drwho_episodes$season
)

lvls = drwho_episodes$season |> unique()
lvls = lvls[which( lvls != "Special" )]

drwho_episodes$season = drwho_episodes$season |>
    factor(levels = c(lvls, "Special"))

drwho_episodes$episode_number = drwho_episodes$season |> rleid() |> rowid()

an = rbind(
    drwho_episodes[which.max(rating)],
    drwho_episodes[which.max(uk_viewers)]
)

an = an |>
    merge(drwho_directors, by = "story_number") |>
    merge(drwho_writers, by = "story_number")

an$label = paste0(
    an$episode_title, "\n",
    "Director: ", an$director, "\n",
    "Writer: ", an$writer
)

my_font = "Jost"

gr = ggplot(data = drwho_episodes) +
    
    geom_smooth(aes(x = first_aired, y = rating), alpha = .3, color = "grey75") +
    
    geom_point(aes(x = first_aired, y = rating, size = uk_viewers, fill = season),
               shape = 21, stroke = .25, color = "white") +
    
    geom_text_repel(
        data = an[1], 
        aes(
            x = first_aired, y = rating, 
            label = label
        ),
        hjust = 0, ylim = c(NA, 80), size = 3,
        family = my_font
    ) +
    
    geom_text_repel(
        data = an[2], 
        aes(
            x = first_aired, y = rating, 
            label = label
        ),
        hjust = 0, ylim = c(90, NA), xlim = c(as.Date("2010-01-01"), NA), 
        size = 3, family = my_font
    ) +
    
    
    scale_y_continuous(limits = c(60, 100), expand = c(0, 0)) +
    scale_x_date(expand = c(0.01, 0.01)) +
    
    scale_size_continuous(
        guide = guide_legend(
            override.aes = list(color = "grey10"),
            title = "No. of UK viewers\n(millions)",
        )
    ) +
    
    scale_fill_manual(
        values = c(
            "Season: 1"  = alpha("#008EA0", .75),
            "Season: 2"  = alpha("#8A4198", .75),
            "Season: 3"  = alpha("#008EA0", .75),
            "Season: 4"  = alpha("#8A4198", .75),
            "Season: 5"  = alpha("#008EA0", .75),
            "Season: 6"  = alpha("#8A4198", .75),
            "Season: 7"  = alpha("#008EA0", .75),
            "Season: 8"  = alpha("#8A4198", .75),
            "Season: 9"  = alpha("#008EA0", .75),
            "Season: 10" = alpha("#8A4198", .75),
            "Season: 11" = alpha("#008EA0", .75),
            "Season: 12" = alpha("#8A4198", .75),
            "Season: 13" = alpha("#008EA0", .75),
            "Special"    = alpha("#FF6F00", .75)
        ),
        
        guide = guide_legend(
            title = "Season",
            override.aes = list(size = 3, alpha = 1)
        )
    ) +
    
    coord_cartesian(expand = TRUE, clip = "off") +
    
    theme_minimal(base_family = my_font) +
    
    theme(
        panel.grid.major = element_line(linetype = "dashed", linewidth = .35, color = "grey85"),
        panel.grid.minor = element_line(linetype = "dashed", linewidth = .3, color = "grey85"),
        
        plot.title    = element_text(face = "bold", size = 36, margin = margin(b = 5), family = my_font),
        plot.subtitle = element_markdown(margin = margin(b = 25), size = 11, color = "grey20", family = my_font),
        plot.caption  = element_markdown(margin = margin(t = 25), size = 8, family = my_font),
        
        plot.title.position = "plot",
        plot.caption.position = "plot",
        
        plot.background = element_rect(fill = "#f5f5f5", color = NA),
        plot.margin = margin(20, 20, 20, 20)
    ) +
    
    labs(
        x = "Date the episode first aired in the U.K.",
        y = "Episode's rating",
        
        title = 'Doctor Who Episodes',
        
        subtitle = paste0(
            "Exploring the correlation between Doctor Who episode ratings ",
            "and UK viewership numbers.<br>Unveiling the peak in both ",
            "viewership and rating scores on the plot."
        ),
        
        caption = paste0(
            "Source: <b>{datardis} package</b> | ",
            "Graphic: <b>Nikolaos Pechlivanis</b>"
        )
    )


ggsave(
    plot = gr, filename = "Rplot.jpeg",
    width = 10, height = 8, units = "in", dpi = 600
)
