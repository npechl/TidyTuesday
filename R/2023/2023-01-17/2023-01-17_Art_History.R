


rm(list = ls())
gc()


library(data.table)
library(stringr)

library(ggplot2)
library(ggh4x)

library(ggtext)

df <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-01-17/artists.csv')

df = df[which(df$artist_gender != "N/A"), ]
df = df[which(df$artist_race != "N/A"), ]

library(showtext)

font_add_google("Merriweather", "Merriweather")

showtext_auto()
showtext_opts(dpi = 200)

my_font = "Merriweather"

gr = ggplot(data = df, aes(x = year, y = space_ratio_per_page_total)) +
    
    geom_point(
        aes(
            size = moma_count_to_year, 
            fill = artist_ethnicity, 
            color = artist_ethnicity
        ), alpha = .5, stroke = .1
    ) +
    
    scale_fill_manual(
        values = c(
            "Hispanic or Latino origin"     = "#ffeb07",
            "Not Hispanic or Latino origin" = "#00b8a9"
        )
    ) +
    
    scale_color_manual(
        values = c(
            "Hispanic or Latino origin"     = "#ffeb07",
            "Not Hispanic or Latino origin" = "#00b8a9"
        )
    ) +
    
    scale_size_continuous(range = c(2, 6), breaks = c(0, 20, 30, 40, 60)) +
    
    scale_y_continuous(breaks = c(1, 2, 3)) +
    
    facet_grid2(
        rows = vars(artist_race_nwi),
        cols = vars(artist_gender),
        
        axes = "all"
    ) +
    
    coord_cartesian(expand = TRUE, clip = "on") +
    
    theme_minimal(base_family = my_font) +
    
    theme(
        legend.position = "bottom",
        legend.justification = "left",
        
        panel.spacing = unit(2, "lines"),
        
        strip.text.x = element_text(face = "bold", margin = margin(b = 10), color = "white", size = 10),
        strip.text.y = element_text(face = "bold", margin = margin(l = 10), color = "white", size = 10),
        
        axis.line = element_line(linewidth = .3, color = "white"),
        axis.ticks = element_line(linewidth = .3, color = "white"),
        
        axis.title.x = element_blank(),
        axis.title.y = element_text(
            margin = margin(r = 10), 
            hjust = 0, 
            size = 10, 
            face = "italic", 
            color = "white"
        ),
        
        axis.text = element_text(size = 8, color = "white"),
        
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(
            linewidth = .3, linetype = "dashed", color = "grey25"
        ),
        
        plot.background = element_rect(fill = "#00171b", color = NA),
        
        plot.title.position = "plot",
        plot.caption.position = "plot",
        
        plot.title = element_text(face = "bold", size = 32, color = "white"),
        plot.subtitle = element_markdown(margin = margin(b = 20), face = "bold", size = 9, color = "white"),
        plot.caption = element_markdown(margin = margin(t = 20), size = 6, color = "white"),
        
        plot.margin = margin(20, 20, 20, 20)
    ) +
    
    guides(
        size = guide_legend(
            title.position = "top", 
            title.theme = element_text(size = 9, face = "italic", family = my_font, color = "white", margin = margin(b = 5)),
            label.theme = element_text(color = "white", family = my_font, size = 8),
            override.aes = list(shape = 1, color = "white", stroke = .5, alpha = 1)
        ),
        
        color = guide_legend(
            title.position = "top", 
            title.theme = element_text(size = 9, face = "italic", family = my_font, color = "white"),
            label.theme = element_text(color = "white", family = my_font, size = 8),
            override.aes = list(alpha = 1, size = 3), 
            ncol = 1
        ),
        
        fill = "none"
    ) +
    
    labs(
        y = paste0(
            "The area (cm) squared of both the text and the figure of a ",
            "particular artist\nin a given edition of Janson's History of Art ",
            "divided by the area squared\nof a single page of the respective edition."
        ),
        
        size = paste0(
            "The total count of exhibitions ever held by the Museum of ",
            "Modern Art (MoMA)\nof a particular artist at a given ",
            "year of publication"
        ),
        
        color = "Artist Ethnicity",
        
        title = 'Art History',
        
        subtitle = paste0(
            "The chart reveals underrepresentation of Female and Non-White ",
            "artists (<i>Black or African American, Asian,<br>Native Hawaiian or ",
            "Other Pacific Islander, American Indian or Alaska Native</i>) in ",
            "Janson’s History of Art<br>and Gardner’s Art Through the Ages."
        ),
        
        caption = paste0(
            "Source: <b>{arthistory} package</b> | ",
            "Graphic: <b>Nikolaos Pechlivanis</b>"
        )
    )


ggsave(
    plot = gr, filename = "Rplot.pdf", device = cairo_pdf,
    width = 12, height = 10, units = "in"
)

library(magick)

tmp = image_read_pdf("Rplot.pdf", density = 300)
image_write(tmp, "Rplot.jpeg")


