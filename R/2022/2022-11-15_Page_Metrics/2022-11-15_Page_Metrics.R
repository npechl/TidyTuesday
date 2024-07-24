


rm(list = ls())
gc()


library(data.table)
library(stringr)

image_alt      <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-11-15/image_alt.csv')
color_contrast <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-11-15/color_contrast.csv')

ally_scores    <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-11-15/ally_scores.csv')
bytes_total    <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-11-15/bytes_total.csv')
speed_index    <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-11-15/speed_index.csv')

library(ggplot2)
library(geomtextpath)
library(ggtext)
library(extrafont)

ally_scores$date = str_replace_all(ally_scores$date, "_", "-")
ally_scores$date = as.Date(ally_scores$date)

bytes_total$date = str_replace_all(bytes_total$date, "_", "-")
bytes_total$date = as.Date(bytes_total$date)

speed_index$date = str_replace_all(speed_index$date, "_", "-")
speed_index$date = as.Date(speed_index$date)


ally_scores$p10 = ally_scores$p10 / 100
ally_scores$p25 = ally_scores$p25 / 100
ally_scores$p50 = ally_scores$p50 / 100
ally_scores$p75 = ally_scores$p75 / 100
ally_scores$p90 = ally_scores$p90 / 100


gr_ally = ggplot() +
    
    geom_ribbon(
        data = ally_scores,
        aes(x = date, ymin = p10, ymax = p90),
        alpha = .1,
        fill = "#f37a00"
    ) +
    
    geom_ribbon(
        data = ally_scores,
        aes(x = date, ymin = p25, ymax = p75),
        alpha = .3,
        fill = "#f37a00"
    ) +
    
    geom_line(
        data = ally_scores,
        aes(x = date, y = p50),
        color = "#f37a00",
        linewidth = 1
    )  +
    
    scale_x_date(expand = c(0, 0)) +
    
    scale_y_continuous(expand = c(0, 0), limits = c(0, 1), 
                       labels = scales::percent) +
    
    theme_minimal(base_family = "Zilla Slab") +
    
    theme(
        axis.title.x = element_blank(),
        
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        
        panel.grid.major.y = element_line(linetype = "dashed"),
        panel.grid.minor.y = element_blank(),  # element_line(linetype = "dashed"),
        
        axis.line.x = element_line(color = "grey"),
        axis.ticks.x = element_line(color = "grey"),
        
        axis.title.y = element_text(color = "gray90"),
        axis.text = element_text(color = "gray90")
    ) +
    
    labs(y = "Ally Scores (percent)")


gr_bytes = ggplot() +
    
    geom_ribbon(
        data = bytes_total,
        aes(x = date, ymin = p10, ymax = p90),
        alpha = .1,
        fill = "#f37a00"
    ) +
    
    geom_ribbon(
        data = bytes_total,
        aes(x = date, ymin = p25, ymax = p75),
        alpha = .3,
        fill = "#f37a00"
    ) +
    
    geom_line(
        data = bytes_total,
        aes(x = date, y = p50),
        
        color = "#f37a00"
    )  +
    
    scale_x_date(expand = c(0, 0)) +
    
    scale_y_continuous(expand = c(0, 0), labels = function(x) format(x, big.mark = " ", decimal.mark = ",", scientific = FALSE)) +
    
    theme_minimal(base_family = "Zilla Slab") +
    
    theme(
        axis.title.x = element_blank(),
        
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        
        panel.grid.major.y = element_line(linetype = "dashed"),
        panel.grid.minor.y = element_blank(),  # element_line(linetype = "dashed"),
        
        axis.line.x = element_line(color = "grey"),
        axis.ticks.x = element_line(color = "grey"),
        
        axis.title.y = element_text(color = "gray90"),
        axis.text = element_text(color = "gray90")
    ) +
    
    labs(y = "Bytes Total")



gr_speed = ggplot() +
    
    geom_ribbon(
        data = speed_index,
        aes(x = date, ymin = p10, ymax = p90),
        alpha = .1,
        fill = "#f37a00"
    ) +
    
    geom_ribbon(
        data = speed_index,
        aes(x = date, ymin = p25, ymax = p75),
        alpha = .3,
        fill = "#f37a00"
    ) +
    
    geom_line(
        data = speed_index,
        aes(x = date, y = p50),
        
        color = "#f37a00"
    )  +
    
    scale_x_date(expand = c(0, 0)) +
    
    scale_y_continuous(expand = c(0, 0)) +
    
    theme_minimal(base_family = "Zilla Slab") +
    
    theme(
        axis.title.x = element_blank(),
        
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        
        panel.grid.major.y = element_line(linetype = "dashed"),
        panel.grid.minor.y = element_blank(),  # element_line(linetype = "dashed"),
        
        axis.line.x = element_line(color = "grey"),
        axis.ticks.x = element_line(color = "grey"),
        
        axis.title.y = element_text(color = "gray90"),
        axis.text = element_text(color = "gray90")
    ) +
    
    labs(y = "Speed Index")



# image alt -----------------------------------------------------

image_alt$date = str_replace_all(image_alt$date, "_", "-")
image_alt$date = as.Date(image_alt$date)

df = image_alt[which(client == "mobile"), ]

df$percent = df$percent / 100

gr_image = ggplot(data = df) +
    
    geom_area(
        aes(
            x = date, y = percent
        ),
        
        alpha = .3,
        fill = "#f37a00"
    ) +
    
    geom_line(
        aes(
            x = date, y = percent
        ),
        
        color = "#f37a00"
    ) +
    
    scale_x_date(expand = c(0, 0)) +
    
    scale_y_continuous(
        limits = c(0, .6), expand = c(0, 0), labels = scales::percent
    ) +
    
    theme_minimal(base_family = "Zilla Slab") +
    
    theme(
        legend.position = "none",
        
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        
        panel.grid.major.y = element_line(linetype = "dashed"),
        panel.grid.minor.y = element_blank(),  # element_line(linetype = "dashed"),
        
        axis.line.x = element_line(color = "grey"),
        axis.ticks.x = element_line(color = "grey"),
        
        axis.title.x = element_blank(),
        
        axis.title.y = element_text(color = "gray90"),
        axis.text = element_text(color = "gray90")
    ) +
    
    labs(
        y = "Image alt (percent)"
    )



# color contrast -----------------------------------------------------

color_contrast$date = str_replace_all(color_contrast$date, "_", "-")
color_contrast$date = as.Date(color_contrast$date)

df = color_contrast[which(client == "mobile"), ]

df$percent = df$percent / 100

gr_color = ggplot(data = df) +
    
    geom_area(
        aes(
            x = date, y = percent
        ),
        
        alpha = .3,
        fill = "#f37a00"
    ) +
    
    geom_line(
        aes(
            x = date, y = percent
        ),
        
        color = "#f37a00"
    ) +
    
    scale_x_date(expand = c(0, 0)) +
    
    scale_y_continuous(
        limits = c(0, .6), expand = c(0, 0), labels = scales::percent
    ) +
    
    theme_minimal(base_family = "Zilla Slab") +
    
    theme(
        legend.position = "none",
        
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        
        panel.grid.major.y = element_line(linetype = "dashed"),
        panel.grid.minor.y = element_blank(),  # element_line(linetype = "dashed"),
        
        axis.line.x = element_line(color = "grey"),
        axis.ticks.x = element_line(color = "grey"),
        
        axis.title.x = element_blank(),
        
        axis.title.y = element_text(color = "gray90"),
        axis.text = element_text(color = "gray90")
    ) +
    
    labs(
        y = "Color Contrast (percent)"
    )



library(patchwork)


gr = (gr_ally | gr_bytes | gr_speed) / (gr_image | gr_color) +
    
    plot_annotation(
        title = 'Page Metrics',
        subtitle = "An overview of <b>Web performance on mobile devices</b>.",
        caption = paste0(
            "Source: <b>httparchive.org</b> | ",
            "Graphic: <b>Nikolaos Pechlivanis</b>"
        )
    ) &
    
    theme(
        plot.title.position = "plot",
        plot.caption.position = "plot",
        
        plot.subtitle = element_markdown(
            margin = margin(b = 10),
            family = "Zilla Slab", color = "#f37a00"
        ),
        
        plot.title = element_text(
            face = "bold", size = 20, family = "Zilla Slab", color = "#f37a00"
        ),
        plot.caption = element_markdown(family = "Zilla Slab", color = "#f37a00"),
        
        plot.margin = margin(15, 15, 15, 15),
        
        plot.background = element_rect(fill = "#1b0e00", color = "#1b0e00")
    )



ggsave(
    plot = gr,
    filename = "Rplot.pdf",
    width = 14, height = 10,
    device = cairo_pdf,
    units = "in"
)

ggsave(
    plot = gr,
    filename = "Rplot.png",
    width = 14, height = 10,
    units = "in"
)




















