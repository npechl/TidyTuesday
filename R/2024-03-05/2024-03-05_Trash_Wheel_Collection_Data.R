


rm(list = ls())
gc()

# load libraries -----------------------------------

library(data.table)
library(stringr)

library(ggplot2)
library(ggtext)

library(colorspace)
library(extrafont)

# input data frame -----------------------------------

df <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-03-05/trashwheel.csv')


df$Date = paste0(df$Year, "/", df$Month) |> lubridate::ym()

my_font = "Jost"

library(ggforce)
library(ggh4x)
library(paletteer)

# plot 1 ------------------------------

gr1 = df[which(HomesPowered > 0)] |>
    ggplot(aes(Date, HomesPowered)) +
    
    # geom_mark_hull(aes(fill = Name, label = Name)) +
    geom_point(aes(fill = Name), shape = 21, size = 2, stroke = .15, color = "grey96") +
    
    geom_smooth(aes(group = Name, fill = Name, color = Name)) +
    
    scale_fill_manual(
        values = paletteer_d("ggsci::planetexpress_futurama") |> lighten(.25)
    ) +
    
    scale_color_manual(
        values = paletteer_d("ggsci::planetexpress_futurama") |> darken(.25)
    ) +
    
    facet_wrap2(vars(Name), nrow = 2, scales = "free_x", axes = "all") +
    
    theme_minimal(base_family = my_font) +
    
    theme(
        legend.position = "none",
        strip.text = element_text(face = "bold", margin = margin(b = 10)),
        
        axis.line = element_line(linewidth = .35, color = "grey"),
        axis.ticks = element_line(linewidth = .35, color = "grey"),
        
        axis.title.x = element_blank(),
        axis.title.y = element_text(margin = margin(r = 10)),
        
        panel.spacing = unit(1, "lines"),
        
        panel.grid = element_line(linewidth = .35),
        panel.grid.minor = element_line(linewidth = .25, linetype = "dashed"),
        
        plot.title    = element_text(margin = margin(b = 5), family = "Jost", face = "bold", size = 26, hjust = .5),
        plot.subtitle = element_markdown(margin = margin(b = 25), family = "Jost", size = 10, color = "grey25", hjust = .5),
        plot.caption  = element_markdown(margin = margin(t = 25), family = "Jost", size = 7, hjust = .5),
        
        plot.title.position = "plot",
        plot.caption.position = "plot",
        
        plot.background = element_rect(fill = "#f9fbfe", color = NA),
        plot.margin = margin(20, 20, 20, 20)
    ) +
    
    labs(
        y = "Number of homes powered",
        title = "Trash Wheel Collection Data",
        subtitle = "*Homes Powered - Each ton of trash equates to on average 500 kilowatts of<br>electricity. An average household will use 30 kilowatts per day.*",
        caption = paste0(
            "Source: <b>Mr. Trash Wheel Baltimore Healthy Harbor initiative</b> | ",
            "Graphic: <b>Nikos Pechlivanis</b>"
        )
    )

ggsave(
    plot = gr1, filename = "Rplot1.jpeg",
    width = 9, height = 9, units = "in", dpi = 600
)


# plot 2 ---------------------------------

library(ggnewscale)

df2 = df[, c("Name", "PlasticBottles", "Polystyrene", 
       "CigaretteButts", "GlassBottles", "PlasticBags", 
       "Wrappers", "SportsBalls"), with = FALSE] |>
    
    melt(id.vars = "Name", variable.factor = FALSE) 

# df2$Name = df2$Name |> str_wrap(12)

gr2 = df2 |>
    
    ggplot(aes(value, variable)) +
    
    geom_point(
        aes(fill = variable), color = "grey96", 
        shape = 21, size = 2, stroke = .15, 
        position = position_jitternormal(sd_x = 0, sd_y = .1)
    ) +
    
    scale_fill_manual(
        values = paletteer_d("ggsci::planetexpress_futurama") |> lighten(.15)
    ) +
    
    new_scale_fill() +
    
    geom_boxplot(aes(fill = variable), width = .15, outlier.shape = NA) +
    
    scale_fill_manual(
        values = paletteer_d("ggsci::planetexpress_futurama") |> darken(.15)
    ) +
    
    scale_x_continuous(
        transform = scales::pseudo_log_trans(base = 10),
        breaks = c(10, 100, 1000, 10000, 100000),
        labels = scales::trans_format("log10", format = scales::math_format())
    ) +

    facet_wrap2(vars(Name), nrow = 2, axes = "all") +
    
    theme_minimal(base_family = my_font) +
    
    theme(
        legend.position = "none",
        legend.title = element_blank(),
        
        strip.text = element_text(face = "bold"),
        
        axis.title.y = element_blank(),
        axis.title.x = element_text(margin = margin(t = 10)),
        
        panel.spacing = unit(1, "lines"),
        
        # axis.text.y = element_blank(),
        
        axis.line = element_line(linewidth = .35, color = "grey"),
        axis.ticks = element_line(linewidth = .35, color = "grey"),
        
        panel.grid = element_line(linewidth = .35),
        panel.grid.minor = element_line(linewidth = .25, linetype = "dashed"),
        
        plot.title    = element_text(margin = margin(b = 5), family = "Jost", face = "bold", size = 26, hjust = .5),
        plot.subtitle = element_markdown(margin = margin(b = 25), family = "Jost", size = 10, color = "grey25", hjust = .5),
        plot.caption  = element_markdown(margin = margin(t = 25), family = "Jost", size = 7, hjust = .5),
        
        plot.title.position = "plot",
        plot.caption.position = "plot",
        
        plot.background = element_rect(fill = "#f9fbfe", color = NA),
        plot.margin = margin(20, 20, 20, 20)
    ) +
    
    labs(
        x = "Number of items collected",
        title = "Trash Wheel Collection Data",
        subtitle = "*Number of items listed on a single conveyor paddle*",
        caption = paste0(
            "Source: <b>Mr. Trash Wheel Baltimore Healthy Harbor initiative</b> | ",
            "Graphic: <b>Nikos Pechlivanis</b>"
        )
    )


ggsave(
    plot = gr2, filename = "Rplot2.jpeg",
    width = 9, height = 9, units = "in", dpi = 600
)













