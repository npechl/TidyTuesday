


rm(list = ls())
gc()

# load libraries -----------------------------------

library(data.table)
library(stringr)

library(ggplot2)
library(ggtext)

library(ggnewscale)

library(paletteer)
library(colorspace)
library(extrafont)

my_font = "Jost"

# 1 ----------------------------------- 

olympics <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-08-06/olympics.csv')

# 2 ---------------------------------

m <- olympics[which(!is.na(medal))]

m <- m[, by = .(name, medal), .N]
m[, by = name, N2 := sum(N)]

m$medal = m$medal |> factor(levels = c("Gold", "Silver", "Bronze"))

m = m[order(-N2, medal, -N)]

# 3 ---------------------------------

index <- m$name |> unique()
index <- index[seq_len(12)]

m1 <- m |> subset(name %in% index)

m1$name <- m1$name |> 
    str_remove_all('Jenny|"\"') |>
    str_remove_all("Cumpelik|Diriy") |>
    str_remove_all("\\(|\\)|\\-") |>
    str_squish()

library(packcircles)

packing <- m1 |>
    split(by = "name") |>
    lapply(function(q) {
        
        data.table(
            "medal" = rep(q$medal, q$N),
            "area"  = 1
        )
        
    }) |>
    
    lapply(function(q) {
        
        o <- q$area |>
            circleProgressiveLayout(sizetype = 'area') |>
            circleLayoutVertices(npoints = 100) |>
            setDT()
        
        o <- cbind(o, q[o$id])
        
    }) |>
    
    rbindlist(idcol = "name")

# 5 ---------------------------------

my_col <- c("Gold" = "#d4af37", "Silver" = "#c0c0c0", "Bronze" = "#cd7f32")

m1 <- m1[, by = name, .(
    total = N2 |> unique(),
    strip = paste0(N, " ", medal) |> paste(collapse = ", ")
)]

packing <- packing |> merge(m1, by = "name")
packing <- packing[order(-total)]


packing$var_facet <- paste0(
    "**", packing$name, "**", 
    "<sup>", packing$total, "</sup>",
    "<br>", "<span style='font-size:8pt'>", packing$strip, "</span>"
)

packing$var_facet <- packing$var_facet |> factor(levels = packing$var_facet |> unique())

h <- packing[, by = .(var_facet, id, medal), .(
    x = (min(x) + max(x)) / 2,
    y = (min(y) + max(y)) / 2
)]

gr <- packing |>
    ggplot(aes(x, y, group = id)) + 
    
    
    geom_polygon(aes(fill = medal, color = medal), linewidth = .25) +
    scale_fill_manual(values = my_col |> lighten(.25), guide = "none") +
    scale_color_manual(values = my_col |> darken(.1), guide = "none") +
    
    new_scale_fill() +

    geom_point(
        data = h, aes(x, y, fill = medal), shape = 21,
        inherit.aes = FALSE, size = 5, color = "grey96", stroke = .5
    ) +

    scale_fill_manual(values = my_col |> darken(.25)) +
    
    # new_scale_fill() +
    # 
    # geom_point(
    #     data = h, aes(x, y, fill = medal), shape = 21,
    #     inherit.aes = FALSE, size = 3, color = "grey96", stroke = .25
    # ) +
    # 
    # scale_fill_manual(values = my_col |> darken(.25), guide = "none") +
    
    facet_wrap(vars(var_facet)) +
    
    coord_equal() +
    
    theme_void(base_family = my_font) + 
    theme(
        legend.position = "bottom",
        legend.title = element_blank(),
        
        strip.text = element_markdown(size = 11),
        
        plot.title    = element_text(face = "bold", size = 26, margin = margin(b = 5), family = "Jost", hjust = .5),
        plot.subtitle = element_markdown(margin = margin(b = 25), size = 10, color = "grey25", family = "Jost", hjust = .5),
        plot.caption  = element_markdown(margin = margin(t = 25), size = 8, family = "Jost", hjust = .5),
        
        plot.background = element_rect(fill = "#f9fbfe", color = NA),
        plot.margin = margin(20, 20, 20, 20)
    ) +
    
    labs(
        title = "Olympics athletes and medals",
        
        subtitle = paste0(
            "Top *#12* Athletes with the Most Olympic Medals in 120 Years of Olympic History."
        ),
        
        caption = paste0(
            "Source: <b>RGriffin Kaggle dataset: 120 years of Olympic history: athletes and results</b> | ",
            "Graphic: <b>Nikos Pechlivanis</b>"
        )
    )


ggsave(
    plot = gr, filename = "Rplot.png",
    width = 12, height = 10, units = "in", dpi = 600
)    








