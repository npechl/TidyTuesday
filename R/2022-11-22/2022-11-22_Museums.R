


rm(list = ls())
gc()


library(data.table)
library(stringr)

df <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-11-22/museums.csv')



year_opened = str_split(df$Year_opened, "\\:", simplify = TRUE)
index       = which(year_opened[, 1] == year_opened[, 2])
df          = df[index, ]


year_closed = str_split(df$Year_closed, "\\:", simplify = TRUE)
index       = which(year_closed[, 1] == year_closed[, 2])
df          = df[index, ]


df$Year_opened = str_split(df$Year_opened, "\\:", simplify = TRUE)[, 1]
df$Year_closed = str_split(df$Year_closed, "\\:", simplify = TRUE)[, 1]


library(ggplot2)
library(ggtext)
library(ggh4x)
library(maps)

library(extrafont)


UK <- map_data("world")
UK <- setDT(UK)
UK <- UK[which(region == "UK"), ]

df = df[which(df$Latitude <= max(UK$lat)), ]
df = df[which(Year_closed == "9999"), ]
df = df[which(Size != "unknown"), ]

df$Governance = str_split(df$Governance, "\\-", simplify = TRUE)[, 1]

df$Size_label = str_to_title(df$Size)

df$Year_opened = as.numeric(df$Year_opened)

df$year_label = ifelse(
    df$Year_opened >= 1621 & df$Year_opened < 1721, "1621 - 1720",
    ifelse(
        df$Year_opened >= 1721 & df$Year_opened < 1821, "1721 - 1820",
        ifelse(
            df$Year_opened >= 1821 & df$Year_opened < 1921, "1821 - 1920",
            "1921 - 2021"
        )
    )
)

df = df[which(Year_opened >= 1821), ]

df$year_label = ifelse(
    df$Year_opened >= 1821 & df$Year_opened < 1871, "1821 - 1870",
    ifelse(
        df$Year_opened >= 1871 & df$Year_opened < 1921, "1870 - 1920",
        ifelse(
            df$Year_opened >= 1921 & df$Year_opened < 1971, "1921 - 1970",
            "1971 - 2021"
        )
    )
)


gr = ggplot() +
    geom_polygon(
        data = UK, aes(x = long, y = lat, group = group), 
        fill = "grey50", color = "grey10", linewidth = .1
    ) +
    
    
    geom_point(
        data = df, aes(x = Longitude, y = Latitude, size = Size, fill = Area_Deprivation_index),
        inherit.aes = FALSE, stroke = .1,
        shape = 21, alpha = .75, color = "grey20"
    ) +
    
    facet_grid2(
        cols = vars(Governance),
        rows = vars(year_label),
        switch = "y"
    ) +
    
    # facet_wrap2(
    #     vars(Governance), nrow = 2
    # ) +
    
    scale_size_manual(
        values = c(
            "huge"   = 4,
            "large"  = 3,
            "medium" = 2,
            "small"  = 1
        )
    ) +
    
    scale_fill_gradient(
        high = "#ff045c",
        low = "#ffeff5"
    ) +

    theme_minimal(base_family = "Zilla Slab") +
    
    theme(
        strip.text = element_text(face = "bold"),
        panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        
        plot.title.position = "plot",
        plot.caption.position = "plot",
        
        plot.title = element_text(face = "bold", size = 18),
        plot.caption = element_markdown(margin = margin(t = 10)),
        plot.subtitle = element_markdown(margin = margin(b = 10), color = "grey10", size = 10),
        
        plot.background = element_rect(color = "#efe7e7", fill = "#efe7e7"),
        
        plot.margin = margin(15, 15, 15, 15)
    ) +
    
    
    coord_map() +
    
    labs(
        fill = "Area Deprevation\nIndex",
        
        title = 'Museums',
        
        subtitle = paste0(
            "The project’s research team has gathered, cleansed, and codified ",
            "data relating to over 4000<br>UK museums. <b>It covers the period ",
            "from 1821 to 2021, depicting the Governance of every museum.</b>"
        ),
        
        caption = paste0(
            "Source: <b>Mapping Museums project</b> | ",
            "Graphic: <b>Nikolaos Pechlivanis</b>"
        )
    ) +
    
    guides(
        size = guide_legend(
            override.aes = list(stroke = .5)
        ),
        
        fill = guide_colorbar(
            # title.theme = element_text(color = "white"), 
            # label.theme = element_text(color = "white"),
            
            barheight = unit(10, "line"), 
            barwidth = unit(.5, "line")

        )
    )



ggsave(
    plot = gr, "Rplot.pdf",
    width = 7, height = 10, units = "in", device = cairo_pdf
)

ggsave(
    plot = gr, "Rplot.png",
    width = 7, height = 10, units = "in"
)



