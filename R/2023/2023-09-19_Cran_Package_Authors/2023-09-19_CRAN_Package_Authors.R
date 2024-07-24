


rm(list = ls())
gc()

library(data.table)
library(stringr)

library(ggplot2)
library(ggtext)

library(extrafont)
library(paletteer)

library(packcircles)


cran_20230905    <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-09-19/cran_20230905.csv')
package_authors  <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-09-19/package_authors.csv')
# cran_graph_nodes <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-09-19/cran_graph_nodes.csv')
# cran_graph_edges <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-09-19/cran_graph_edges.csv')


df = package_authors[, by = authorsR, .(Νo = Package |> unique() |> length())]
df$Perc = df$Νo / package_authors$Package |> unique() |> length()
df = df[order(-df$Νo)]


packing1 <- circleProgressiveLayout(df$Νo)
dat1     <- circleLayoutVertices(packing1) |> setDT()

dat1$author = df[dat1$id][["authorsR"]]
dat1$Perc   = df[dat1$id][["Perc"]]
dat1$N      = df$Νo[dat1$id]

library(venn)

dat2 = dat1[, by = .(id, author, Perc), .(
    x = ( min(x) + max(x) ) / 2,
    y = ( min(y) + max(y) ) / 2
)]

dat2 = dat2[order(-Perc)]

dat2$label = dat2$author |> str_replace_all("\\ ", "\n")

breaks = c(1, 50, 100, 150)
breaks_labels = paste0(
    breaks, " *<span style='font-size:6pt'>",
    round(100 * breaks / package_authors$Package |> unique() |> length(), digits = 3), "%</span>*"
)

my_font = "Jost"

gr = ggplot(data = dat1, aes(x, y)) + 
    
    geom_polygon(aes(group = id, fill = N), 
                 colour = "grey10", linewidth = .1) +
    
    geom_text(
        data = dat2[1:200], aes(x, y, label = label, size = Perc),
        hjust = .5, vjust = .5 , family = my_font
    ) +
    
    scale_fill_gradient2(
        low  = "#405db0",
        mid  = "#4095b0",
        high = "#b0405d",
        
        midpoint = 90,
        
        breaks = breaks,
        labels = breaks_labels,

        guide = guide_colorbar(
            title = "No. of R packages",
            title.position = "left",
            
            title.theme = element_markdown(angle = 90, hjust = 0, family = my_font),

            barheight = unit(18, "lines"),
            barwidth = unit(.35, "lines"),
            
            label.theme = element_markdown(hjust = 0, size = 10, family = my_font)
        )
    ) +
    
    scale_size_continuous(range = c(.5, 2), guide = "none") +
    
    coord_equal() +
    
    theme_void(base_family = my_font) +
    
    theme(
        legend.position = "right",
        # legend.justification = "right",
        
        plot.title    = element_text(face = "bold", size = 36, margin = margin(b = 5), family = my_font),
        plot.subtitle = element_markdown(margin = margin(b = 20), size = 10, color = "grey20", family = my_font),
        plot.caption  = element_markdown(margin = margin(t = 20), size = 6, family = my_font),
        
        plot.title.position = "plot",
        plot.caption.position = "plot",
        
        plot.background = element_rect(fill = "#f5f5f5", color = NA),
        plot.margin = margin(20, 20, 20, 20)
    ) +
    
    labs(
        title = 'CRAN Package Authors',
        
        subtitle = paste0(
            "**Number of R package contributions by Authors**: Within the expansive R package ",
            "ecosystem, Hadley Wickham stands out<br>as a prolific author with ",
            "162 packages to his name, showcasing his significant ",
            "contributions. However, this remarkable body of work<br>represents ",
            "**only 0.8% of the entire R universe**."
        ),
        
        caption = paste0(
            "Source: <b>CRAN collaboration graph</b>, a project by <b>David Schoch</b> | ",
            "Graphic: <b>Nikolaos Pechlivanis</b>"
        )
    )


ggsave(
    plot = gr, filename = "Rplot.jpeg",
    width = 10, height = 10, units = "in", dpi = 600
)

