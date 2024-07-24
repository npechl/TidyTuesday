


rm(list = ls())
gc()

library(data.table)
library(stringr)

library(ggplot2)
library(ggtext)

library(ggh4x)

library(extrafont)
library(paletteer)


richmondway <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-09-26/richmondway.csv')


richmondway$Season = paste0("Season: ", richmondway$Season)

richmondway$Season_Episode = richmondway$Season_Episode |> 
    factor(levels = richmondway$Season_Episode)

richmondway$Season = richmondway$Season |>
    factor(
        levels = c("Season: 3", "Season: 2", "Season: 1")
    )


richmondway$group = character()

richmondway$group = ifelse(
    richmondway$Dating_flag == "Yes" & richmondway$Coaching_flag == "Yes",
    "Roy was dating Keeley and coaching the team",
    ifelse(
        richmondway$Dating_flag == "Yes",
        "Roy was dating Keeley",
        ifelse(
            richmondway$Coaching_flag == "Yes",
            "Roy was coaching the team",
            NA
        )
    )
)


richmondway$label = paste0(
    "F**k",
    
    "<sub style='font-family:Jost; font-size:8pt'>(", 
    richmondway$F_count_RK, ", ",
    richmondway$F_perc, "%",
    ")</sub>"
    

)

my_font = "Jost"


gr = ggplot(data = richmondway) +
    
    # geom_point(
    #     aes(x = Imdb_rating, y = Season_Episode), size = 2,
    #     shape = 21, stroke = .25, color = "white", fill = alpha("grey10", .5)
    # ) +
    
    geom_richtext(
        aes(
            x = Imdb_rating, y = Season_Episode, 
            size = F_count_RK, color = group, label = label
        ),
        
        hjust = 0.5,
        
        family = "Permanent Marker", label.size = NA, fill = NA
    ) +
    
    facet_grid2(
        rows = vars(Season),
        scales = "free_y",
        space = "free_y",
        
        axes = "all"
    ) +
    
    scale_size_continuous(
        range = c(2, 8),
        guide = "none" # guide_legend(nrow = 4, title.position = "top")
    ) +
    
    scale_color_manual(
        values = c("#C71000FF", "#2F509E", "#46732E"),
        breaks = c("Roy was dating Keeley", "Roy was coaching the team", "Roy was dating Keeley and coaching the team"),
        guide = guide_legend(title = "During the episode:", nrow = 3, title.position = "top", override.aes = list(size = 4)),
        na.value = "grey30"
    ) +
    
    scale_x_continuous(expand = c(0, 0), limits = c(6, 10)) +
    
    theme_minimal(base_family = my_font) +
    
    coord_cartesian(clip = "off", expand = TRUE) +
    
    theme(
        legend.position = "bottom",
        legend.justification = "left",
        
        panel.spacing = unit(2, "lines"),
        
        axis.title.y = element_text(hjust = 0),
        axis.title.x = element_text(hjust = 1),
        
        axis.ticks = element_line(linewidth = .3, color = "grey80"),
        
        panel.grid.major = element_line(linewidth = .3, color = "grey80", linetype = "dashed"),
        
        plot.title    = element_text(face = "bold", size = 36, margin = margin(b = 5), family = my_font),
        plot.subtitle = element_markdown(margin = margin(b = 20), size = 10, color = "grey20", family = my_font),
        plot.caption  = element_markdown(margin = margin(t = 20), size = 6, family = my_font),
        
        strip.text = element_text(face = "bold"),
        
        plot.title.position = "plot",
        plot.caption.position = "plot",
        
        plot.background = element_rect(fill = "#f5f5f5", color = NA),
        plot.margin = margin(20, 20, 20, 20)
    ) +
    
    labs(
        x = "IMDb rating",
        
        title = 'Roy Kent F**k count',
        
        subtitle = paste0(
            "The context of the ",
            "<span style='", paste0('font-family:"Permanent Marker"'), "'>",
            "F**k</span>", 
            
            "<sub>(number, percentage)</sub>",
            # "<sub>percentage</sub> ",
            " word used in the show for each episode by Roy Kent."
        ),
        
        caption = paste0(
            "Source: <b>richmondway R package</b> | ",
            "Graphic: <b>Nikolaos Pechlivanis</b>"
        )
    )


ggsave(
    plot = gr, filename = "Rplot.jpeg",
    width = 8, height = 12, units = "in", dpi = 600
)
