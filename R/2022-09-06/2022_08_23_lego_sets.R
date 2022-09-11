


rm(list = ls())
gc()

library(data.table)
library(stringr)

library(ggplot2)

colors         <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-09-06/colors.csv.gz')
inventory_sets <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-09-06/inventory_parts.csv.gz')
inventories    <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-09-06/inventories.csv.gz')
sets           <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-09-06/sets.csv.gz')
themes         <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-09-06/themes.csv.gz')






index = match(inventory_sets$color_id, colors$id)
inventory_sets$color_name = colors[index, ]$name
inventory_sets$color_rgb = colors[index, ]$rgb

index = match(inventory_sets$inventory_id, inventories$id)
inventory_sets$set_sum = inventories[index, ]$set_num


index = match(sets$theme_id, themes$id)
sets$theme_name = themes[index, ]$name


index = match(inventory_sets$set_sum, sets$set_num)

inventory_sets = cbind(
    inventory_sets,
    sets[index, 2:ncol(sets)]
)

st = sets[, by = theme_name, .N]
st = st[order(-N), ]

dt = inventory_sets[which(theme_name %in% st[1:10, ]$theme_name), ]
dt = dt[, -"img_url"]

my_colors = paste0("#", unique(dt$color_rgb))
names(my_colors) = unique(dt$color_rgb)

library(ggtext)
library(extrafont)

gr = ggplot(data = dt, aes(x = year, y = num_parts)) +
    
    geom_jitter(aes(color = color_rgb), 
                alpha = 0.3, size = 0.5, shape = 20) +
    
    scale_color_manual(values = my_colors) +
    scale_fill_manual(values = my_colors) +
    
    scale_x_continuous(breaks = scales::pretty_breaks()) +
    scale_y_continuous(labels = scales::comma_format()) +
    
    facet_wrap(vars(theme_name), scales = "free", ncol = 2) +
    
    theme_minimal(base_family = "Century Gothic") +
    
    theme(
        legend.position = "none",
        
        strip.text = element_text(face = "bold"),
        
        plot.title = element_text(face = "bold", color = "gray10", size = 16),
        plot.title.position = "plot",
        
        plot.subtitle = element_text(
            margin = margin(b = 10),
            size = 11,
            color = "gray40"
        ),
        
        plot.caption = element_markdown(),
        plot.caption.position = "plot",
        
        plot.background = element_rect(fill = "#faf5f5", color = "#faf5f5"),
        plot.margin = margin(20, 20, 20, 20)
    ) +
    
    labs(
        
        y = "Number of parts",
        x = "Year",
        
        title = "LEGO sets",
        subtitle = paste0(
            "Top 10 LEGO themes with their corresponding number of different parts and their related colors",
            " from 1965 to 2022."
        ),
        
        caption = paste0(
            "Source: **Rebrickable** | ",
            "Graphic: **Nikolaos Pechlivanis**"
        )
    )



ggsave(
    filename = "Rplot.pdf",
    plot = gr,
    width = 10, height = 10,
    units = "in"
)  


ggsave(
    filename = "Rplot.png",
    plot = gr,
    width = 10, height = 10,
    units = "in"
)    
