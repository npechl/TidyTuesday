


rm(list = ls())
gc()

# load libraries -----------------------------------

library(data.table)
library(stringr)

library(ggplot2)
library(ggtext)

library(ggrepel)
library(geomtextpath)
library(ggnewscale)

library(paletteer)
library(colorspace)
library(extrafont)

my_font = "Jost"

# input data frame ----------------------------------- 

federal_holidays <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-06-18/federal_holidays.csv')
proposed_federal_holidays <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-06-18/proposed_federal_holidays.csv')

federal_holidays$details =          NULL
proposed_federal_holidays$details = NULL


federal_holidays$label =          "Federal Holidays"
proposed_federal_holidays$label = "Proposed Federal Holidays"


df = rbind(federal_holidays, proposed_federal_holidays, use.names = TRUE, fill = TRUE)


rm(federal_holidays, proposed_federal_holidays)


df$month = df$date |> str_split_i("\\ ", 1) |> str_squish()
df$days  = df$date |> str_split_i("\\ ", 2) |> str_squish()


df$day_avg = df$days |> str_split("\\–") |> lapply(function(q) q |> as.numeric() |> mean()) |> unlist()

df2 = data.table(
    "Day" = seq_len(365),
    "Month" = c(
        rep("January", 31),
        rep("February", 28),
        rep("March", 31),
        rep("April", 30),
        rep("May", 31),
        rep("June", 30),
        rep("July", 31),
        rep("August", 31),
        rep("September", 30),
        rep("October", 31),
        rep("November", 30),
        rep("December", 31)
    )
)


df2$Month  = df2$Month |> factor(levels = df2$Month |> unique())

df3 = df2[, by = Month, head(.SD, 1)]


df$month = df$month |> factor(levels = levels(df2$Month))

df = df |> merge(df3, by.x = "month", by.y = "Month")

df$dayid = df$day_avg + df$Day - 1

df$label_wrap = df$official_name |> str_wrap(15)

gr = df2 |>
    ggplot(aes(Day, Month)) +
    geom_point(shape = 21, size = 1, stroke = .01, aes(fill = Month)) +
    geom_textpath(
        data = df3, aes(Day, Month, label = Month, color = Month),
        position = position_nudge(x = -1), size = 2,
        hjust = 0, vjust = .5, family = my_font, fontface = "bold",
        inherit.aes = FALSE
    ) +
    
    scale_fill_manual(values = paletteer_c("ggthemes::Sunset-Sunrise Diverging", 12), guide = "none") +
    scale_color_manual(values = paletteer_c("ggthemes::Sunset-Sunrise Diverging", 12), guide = "none") +
    
    new_scale_color() +
    
    geom_text_repel(
        data = df, aes(dayid, month, label = label_wrap, color = label),
        size = 2, box.padding = .5,  # direction = "x", 
        min.segment.length = 0, segment.size = .3, 
        family = my_font, fontface = "bold",
        bg.color = "white", bg.r = .05,
        max.overlaps = Inf
    ) +
    
    scale_color_manual(
        values = c(
            "Federal Holidays" = "grey20", # "#15a599", 
            "Proposed Federal Holidays" = "grey50" # "#a51521"
        ),
        
        guide = guide_legend(override.aes = list(size = 5))
    ) +

    scale_x_continuous(limits = c(0, 365), expand = c(0, 0)) +
    
    coord_radial(inner.radius = .25) +
    
    theme_minimal(base_family = my_font) +
    
    theme(
        legend.position = "bottom",
        legend.title = element_blank(),
        
        axis.title = element_blank(),
        axis.text = element_blank(),
        
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(linewidth = .45, linetype = "dashed", lineend = "round"),
        
        plot.title    = element_text(face = "bold", size = 26, margin = margin(b = 5), family = "Jost", hjust = .5),
        plot.subtitle = element_markdown(margin = margin(b = 15), size = 10, color = "grey25", family = "Jost", hjust = .5),
        plot.caption  = element_markdown(margin = margin(t = 15), size = 8, family = "Jost", hjust = .5),
        
        plot.background = element_rect(fill = "#f9fbfe", color = NA),
        plot.margin = margin(20, 20, 20, 20)
    ) +
    
    labs(
        title = "US Federal Holidays",
        subtitle = "(Proposed) Federal Holidays in the U.S. throughout the year.",
        
        caption = paste0(
            "Source: <b>Wikipedia: Federal holidays in the United States</b> | ",
            "Graphic: <b>Nikos Pechlivanis</b>"
        )
    )


ggsave(
    filename = "Rplot.png", plot = gr,
    width = 9, height = 9, units = "in", dpi = 600
)





