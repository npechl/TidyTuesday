


rm(list = ls())
gc()

# load libraries -----------------------------------

library(data.table)
library(stringr)

library(ggplot2)
library(ggtext)

library(paletteer)
library(colorspace)
library(extrafont)

my_font = "Jost"

# input data frame ----------------------------------- 

harvest_2020 <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-05-28/harvest_2020.csv')
harvest_2021 <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-05-28/harvest_2021.csv')
planting_2020 <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-05-28/planting_2020.csv')
planting_2021 <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-05-28/planting_2021.csv')
spending_2020 <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-05-28/spending_2020.csv')
spending_2021 <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-05-28/spending_2021.csv')

# clean data tables -------------------

setdiff(colnames(harvest_2020), colnames(harvest_2021))
setdiff(colnames(planting_2020), colnames(planting_2021))
setdiff(colnames(spending_2020), colnames(spending_2021))

spending_2020$eggplant_item_number = NULL

harvest = rbind(harvest_2020, harvest_2021)
planting = rbind(planting_2020, planting_2021)
spending = rbind(spending_2020, spending_2021)


rm(
    harvest_2020, harvest_2021,
    planting_2020, planting_2021, 
    spending_2020, spending_2021
)

# plotting -------------------

planting$year = planting$date |> str_split_i("-", 1)
harvest$year = harvest$date |> str_split_i("-", 1)

planting = planting[which(!is.na(year))]
harvest = harvest[which(!is.na(year))]

d1 = planting[, by = .(year, vegetable), .(N = number_seeds_planted |> sum())]
d2 = harvest[, by = .(year, vegetable), .(N = weight |> sum()) ]

d1$group = "Seeds planted"
d2$group = "Weight harvested (grams)"

d1$Nl = d1$N |> scales::comma()
d2$Nl = d2$N |> scales::comma()

d1$bl = paste0(
    d1$vegetable, 
    "<sub style='font-family:Comfortaa;color:#cc6666;font-size:8pt'>", 
    d1$Nl,
    "</sub>"
)

d2$bl = paste0(
    d2$vegetable, 
    "<sub style='font-family:Comfortaa;color:#cc6666;font-size:8pt'>", 
    d2$Nl,
    "</sub>"
)

library(ggwordcloud)



gr1 = d1 |> 
    ggplot() +
    
    geom_text_wordcloud_area(
        aes(label = bl, size = N),
        family = my_font, color = "#351212",
        use_richtext = TRUE, grid_size = 4
    ) +
    
    facet_grid(
        cols = vars(year), 
        rows = vars(group),
        switch = "y"
    ) +
    
    # scale_size_continuous(range = c(5, 30)) +
    scale_size_area(max_size = 35) +
    
    theme_minimal(base_family = my_font) +
    
    theme(
        strip.text = element_text(size = 11, face = "bold")
    )


gr2 = d2 |> 
    ggplot() +
    
    geom_text_wordcloud_area(
        aes(label = bl, size = N),
        family = my_font, color = "#351212", 
        use_richtext = TRUE, grid_size = 4
    ) +
    
    facet_grid(
        cols = vars(year), 
        rows = vars(group),
        switch = "y"
    ) +
    
    # scale_size_continuous(range = c(5, 30)) +
    scale_size_area(max_size = 35) +
    
    theme_minimal(base_family = my_font) +
    
    theme(
        strip.text.y = element_text(size = 11, face = "bold"),
        strip.text.x = element_blank()
    )

# patchwork ----------------

library(patchwork)

gr = (gr1 / gr2) + 
    plot_annotation(
        title = "Lisa's Vegetable Garden Data",
        
        subtitle = paste0(
            "Number of Vegetable Seeds Planted and Harvested Weight (in grams) from *Lisa's Garden*."
        ),
        
        caption = paste0(
            "Source: <b>{gardenR} package</b> | ",
            "Graphic: <b>Nikolaos Pechlivanis</b>"
        )
    ) & 
    theme(
        plot.title    = element_text(face = "bold", size = 26, margin = margin(b = 5), family = "Jost"),
        plot.subtitle = element_markdown(margin = margin(b = 25), size = 10, color = "grey25", family = "Jost"),
        plot.caption  = element_markdown(margin = margin(t = 25), size = 8, family = "Jost"),
        
        plot.title.position = "plot",
        plot.caption.position = "plot",
        
        plot.background = element_rect(fill = "#f9fbfe", color = NA),
        plot.margin = margin(20, 20, 20, 20)
    )


ggsave(
    plot = gr, filename = "Rplot.pdf", device = cairo_pdf,
    width = 10, height = 9, units = "in"
)



library(magick)

tmp = image_read_pdf("Rplot.pdf", density = 600)









