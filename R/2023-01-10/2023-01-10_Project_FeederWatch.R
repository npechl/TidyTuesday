


rm(list = ls())
gc()


library(data.table)
library(stringr)

library(ggplot2)
library(ggrepel)
library(ggtext)
library(extrafont)

library(rnaturalearth)

feederwatch <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-01-10/PFW_2021_public.csv')
site_data   <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-01-10/PFW_count_site_data_public_2021.csv')

feederwatch <- feederwatch[which(subnational1_code != "XX-"), ]

feederwatch$date <- paste0(month.abb[feederwatch$Month], " ", feederwatch$Year)

feederwatch$date <- factor(
    feederwatch$date,
    levels = c(
        "Nov 2020", "Dec 2020", "Jan 2021", "Feb 2021", "Mar 2021", "Apr 2021"
    )
)

world = ne_countries(scale = "medium", returnclass = "sf")

df = feederwatch[, by = species_code, .(
    Nsub = .N,
    Nobs = max(how_many)
)]


df = df[which(Nobs == 1), ]

gr2 = ggplot() +
    
    geom_sf(data = world, fill = "#efdecd", color = "grey20", linewidth = .1) +
    
    # geom_pointdensity(
    #     data = feederwatch,
    #     aes(x = longitude, y = latitude),
    #     adjust = 1, size = .1
    # ) +
    
    geom_point(
        data = feederwatch[which(species_code %in% df$species_code), ],
        aes(x = longitude, y = latitude),
        shape = 21, stroke = .25, fill = "#a72a00", color = "#a70029", size = 2
    ) +
    
    geom_label_repel(
        data = feederwatch[which(species_code %in% df$species_code), ],
        aes(x = longitude, y = latitude, label = species_code),
        label.size = NA, max.overlaps = Inf, label.padding = .15,
        segment.size = .3, segment.linetype = "dotted",
        size = 2, fill = alpha("white", alpha = .5),
        fontface = "italic", family = "Zilla Slab"
    ) +
    
    facet_wrap(vars(date)) +
    
    scale_size_continuous(range = c(1, 5)) +
    # scale_color_gradient2(
    #     low = "#bc7c3c", mid = "#3cbcbc", high = "#bc3c7c",
    #     midpoint = 1000, # 3750,
    #     guide = guide_colorbar(
    #         title = "Density",
    #         title.theme = element_text(size = 10),
    #         title.position = "top",
    #         
    #         label.theme = element_text(size = 10),
    #         
    #         barwidth = unit(10, "lines"),
    #         barheight = unit(.5, "lines")
    #     )
    # ) +
    
    coord_sf(xlim = c(-160, -50), ylim = c(20, 70)) +
    
    theme_minimal(base_family = "Zilla Slab") +
    
    theme(
        
        legend.position = "bottom",
        
        axis.title = element_blank(),
        axis.text = element_blank(),
        
        panel.grid = element_blank(),
        
        panel.spacing = unit(2, "lines"),
        
        strip.text = element_text(face = "bold", margin = margin(b = 5), color = "white"),
        
        plot.background = element_rect(fill = "#007da7", color = NA),
        
        plot.title.position = "plot",
        plot.caption.position = "plot",
        
        plot.title = element_text(face = "bold", size = 32, color = "white"),
        plot.subtitle = element_markdown(margin = margin(b = 20), face = "bold", size = 10, color = "white"),
        plot.caption = element_markdown(margin = margin(t = 20), size = 8, color = "white"),
        
        plot.margin = margin(20, 20, 20, 20)
    ) +
    
    labs(
        title = 'Project FeederWatch',
        
        subtitle = paste0(
            "Bird species seen only at one time during observation period."
        ),
        
        caption = paste0(
            "Source: <b>Project FeederWatch</b> | ",
            "Graphic: <b>Nikolaos Pechlivanis</b>"
        )
    ) +
    
    guides(
        fill = guide_legend(nrow = 6)
    )


ggsave(
    plot = gr2, filename = "Rplot.jpeg",
    width = 16, height = 8, units = "in"
)




