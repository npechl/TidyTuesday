


rm(list = ls())
gc()


library(data.table)
library(stringr)


library(ggplot2)
library(ggforce)
library(geofacet)
library(extrafont)
library(ggtext)

artists = fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-09-27/artists.csv')


x = artists[which(!is.na(artists$location_quotient)), ]
x = x[which(x$location_quotient != 0)]

x$location_quotient_log10 = log10(x$location_quotient)


# the state's labor force in an occupation is 
x$location_quotient_label = "equal to the occupation's national labor force share"
x$location_quotient_label = ifelse(
    x$location_quotient > 1,
    "greater than the occupation's national labor force share",
    "below the occupation's national labor force share"
)

gr = ggplot(data = x) +
    
    geom_vline(xintercept = 0, 
               linetype = "dotted",
               color = "gray50") +
    
    geom_point(
        aes(
            x = location_quotient_log10, y = type, 
            fill = location_quotient_label
        ),
        shape = 21, color = "#a65300", stroke = .1, size = 2,
        position = position_jitternormal(sd_y = .01, sd_x = 0),
    ) +
    
    scale_fill_manual(
       values = c(
           "greater than the occupation's national labor force share" = "#00a653",
           "below the occupation's national labor force share" = "#a60000"
       ) ,
       
       guide = guide_legend(
           title = "The state's labor force in an occupation is",
           title.position = "top",
           override.aes = list(size = 3)
       )
    ) +
    
    coord_cartesian(expand = TRUE, clip = "off") +
    
    theme_minimal(base_family = "Goudy Old Style") +
    
    facet_geo(~ state) +
    
    theme(
        legend.position = "bottom",
        
        axis.title.y = element_blank(),
        axis.title.x = element_markdown(),
        
        axis.text.y = element_text(face = "italic", size = 7),
        axis.text.x = element_text(size = 6),
        
        strip.text = element_text(face = "bold"),
        
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = "gray90", size = .35),
        
        plot.title = element_text(size = 20, face = "bold", family = "Goudy Old Style"),
        plot.subtitle = element_markdown(color = "gray30", margin = margin(b = 10)),
        plot.caption = element_markdown(),
        
        plot.title.position = "plot",
        plot.caption.position = "plot",
        
        plot.background = element_rect(fill = "#f2f2f2", color = "#f2f2f2"),
        plot.margin = margin(20, 20, 20, 20)
    ) +
    
    labs(
        x = "*log10*(LQ)",
        
        title = "Artists in the USA",
        subtitle = paste0(
            "**Location quotients** (**LQ**) measure an artist occupation's ",
            "concentration in the labor force, relative to the U.S. labor ",
            "force share. For example, an LQ of 1.2 indicates that the state's ",
            "labor<br>force in an occupation is 20 percent greater than the ",
            "occupation's national labor force share. An LQ of 0.8 indicates ",
            "that the state's labor force in an occupation is 20 percent below ",
            "the<br>occupation's national labor force share."
        ),
        
        caption = paste0(
            "Source: ***arts.gov*** | ",
            "Graphic: ***Nikolaos Pechlivanis***"
        )
    )


ggsave(
    filename = "Rplot.pdf",
    plot = gr,
    width = 14, height = 12,
    device = cairo_pdf,
    units = "in"
)

ggsave(
    filename = "Rplot.png",
    plot = gr,
    width = 14, height = 12,
    units = "in"
)








