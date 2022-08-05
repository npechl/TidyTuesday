



rm(list = ls())
gc()



library(data.table)
library(stringr)

library(ggplot2)
library(ggalluvial)
library(ggrepel)
library(extrafont)

library(ggrepel)

library(ggsankey)

library(dplyr)

library(showtext)

library(ggtext)

font_add_google("Lora", "lora")
font_add_google("Quicksand", "Quicksand")
font_add_google("Zen Kaku Gothic Antique", "gothic")
showtext_auto()

frogs <- fread(
    'https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-08-02/frogs.csv'
)

# str(frogs)

frogs = frogs[order(Ordinal), ]

frogs$SurveyDate = as.Date(frogs$SurveyDate, format = "%m/%d/%Y")

frogs$Gender = ifelse(
    frogs$Female == 0, "Male", "Female"
)



frogs = frogs[, by = Subsite, total := .N]

frogs$Subsite2 = paste0(
    "<b>", frogs$Subsite, "</b> (", frogs$total, " frogs)"
)

dt2 = make_long(
    frogs, 
    HabType, 
    Water, 
    Type, 
    Structure, 
    Substrate,
    value = "Subsite2"
)





gr1 = ggplot(dt2, aes(x = x, 
               next_x = next_x, 
               node = node, 
               next_node = next_node)) +
    
    geom_sankey(flow.alpha = 0.5,
                node.fill = "#f5ccae",
                flow.fill = "#aeb4f5",
                
                node.color = "#aef5ef",
                flow.color = "#aef5ef",
                
                flow.size = 0.25,
                node.size = 0.25) +
    
    geom_sankey_text(aes(label = node), size = 3,
                     family = "Quicksand") +
    
    facet_wrap(vars(value), scales = "free", ncol = 3) +
    
    coord_cartesian(expand = TRUE, clip = "off") +
    
    theme_minimal(base_family = "lora") +
    
    theme(
        legend.position = "bottom",
        
        panel.grid = element_blank(),
        panel.spacing = unit(1, "lines"),
        
        plot.background = element_rect(
            fill = "#f5faf8", 
            color = "#f5faf8"
        ),
        
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        
        strip.text = element_markdown(
            # face = "bold",
            margin = margin(t = 10, b = 5)
        )
    )


dtg = frogs[, by = .(Subsite, Gender), .N]


dtg = dtg[, by = .(Subsite), total := sum(N)]


dtg$Freq = dtg$N / dtg$total

dtg$y = 1

library(gggibbous)

dtg$right = dtg$Gender == "Male"

dtg$label = paste0(
    dtg$N, " ", dtg$Gender
)

dtg$label = ifelse(
    dtg$right,
    paste0(
        "<span style = 'color:#e52636;'>",
        dtg$N, " ", dtg$Gender,
        "</span>"
    ),
    paste0(
        "<span style = 'color:#14aa9e;'>",
        dtg$N, " ", dtg$Gender,
        "</span>"
    )
)

gr2 = ggplot(data = dtg) +
    
    geom_moon(
        aes(
            x = Subsite, y = y, ratio = Freq, 
            right = right, fill = Gender
        )
    ) +
    
    geom_richtext(data = dtg[which(Gender == "Male"), ],
              aes(x = Subsite, y = y, label = label),
              hjust = 0,
              nudge_x = 0.15,
              # nudge_y = -0.025,
              size = 3,
              family = "Quicksand",
              fill = NA, label.color = NA) +
    
    geom_richtext(data = dtg[which(Gender == "Female"), ],
                    aes(x = Subsite, y = y, label = label),
              hjust = 1,
              nudge_x = -0.15,
              # nudge_y = 0.025,
              size = 3,
              family = "Quicksand",
              fill = NA, label.color = NA) +
    
    scale_fill_manual(values = c(
        "Male" = "#e52636",
        "Female" = "#14aa9e"
    )) +
    
    facet_wrap(vars(Subsite), scales = "free", nrow = 1) +
    
    coord_cartesian(expand = TRUE, clip = "off") +
    
    theme_minimal(base_family = "lora") +
    
    theme(
        legend.position = "none",
        legend.title = element_blank(),
        
        axis.title = element_blank(),
        axis.text = element_blank(),
        
        strip.text = element_text(face = "bold"),
        
        plot.background = element_rect(
            fill = "#f5faf8", 
            color = "#f5faf8"
        ),
        
        panel.grid = element_blank()
    )



library(patchwork)



multi = gr2 / gr1 +
    
    plot_layout(heights = c(1, 9)) +
    
    plot_annotation(
        title = paste0(
            'Oregon spotted frog (Rana pretiosa) telemetry and ',
            'habitat use at Crane Prairie Reservoir in Oregon, USA'
        ),
        
        subtitle = paste0(
            'Radio-telemetry has been used to study late-season movement and ',
            'habitat use by Oregon spotted frogs (Rana pretiosa) at Crane ',
            'Prairie Reservoir in Oregon.<br>This dataset includes individual ',
            'frog location data and habitat use during each tracking event ',
            'that occurred roughly weekly between September and late November ',
            'of 2018'
        ),
        
        caption = paste0(
            "<i>",
            "Source: <b>USGS</b> | ",
            "Graphic: <b>Nikolaos Pechlivanis</b>",
            "</i>"
        )
    ) &
    
    theme(
        plot.background = element_rect(
            fill = "#f5faf8", 
            color = "#f5faf8"
        ),
        
        plot.caption          = element_markdown(
            # family = "Quicksand",
            margin = margin(t = 10)
            # size = 11
        ),
        plot.caption.position = "plot",
        
        plot.title = element_markdown(
            margin = margin(t = 10, b = 5), 
            # size = 17,
            family = "lora",
            face = "bold"
        ),
        plot.title.position = "plot",
        
        plot.subtitle = element_markdown(
            margin = margin(b = 10), 
            # size = 14,
            color = "gray40", 
            family = "lora"
        ),
        
        plot.margin = margin(20, 20, 20, 20)
    )


ggsave(
    "Rplot.pdf",
    plot = multi,
    width = 14, height = 10,
    device = cairo_pdf,
    units = "in"
)

ggsave(
    "Rplot.png",
    plot = multi,
    width = 14, height = 10,
    units = "in"
)






