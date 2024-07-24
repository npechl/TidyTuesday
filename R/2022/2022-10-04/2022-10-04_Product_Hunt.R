


rm(list = ls())
gc()


library(data.table)
library(stringr)


library(ggplot2)
library(ggtext)

library(extrafont)
library(ggrepel)

df = fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-10-04/product_hunt.csv')

df$category_tags = str_remove_all(df$category_tags, "\\[|\\]|\\'")
df$makers = str_remove_all(df$makers, "\\[|\\]|\\'")

df$upvotes = df$upvotes + 1

df$product_ranking = paste0("Rank *#", df$product_ranking, "*")

df[which(df$product_ranking == "Rank *#NA*"), ]$product_ranking = "No rank available"

df = df[which(df$product_ranking != "No rank available"), ]

top = df[order(product_ranking, -upvotes), ]

top = top[, by = product_ranking, head(.SD, 10)]

gr = ggplot(data = df, aes(x = release_date, y = upvotes)) +
    
    geom_point(
        size = 1.5, 
        alpha = 1, shape = 21, stroke = .3,
        fill = "#008e90", color = "#904a00"
    ) + 
    
    
    geom_smooth(span = .5, alpha = 0.75, color = "#900046") +
    
    geom_label_repel(
        data = top,
        aes(x = release_date, y = upvotes, label = name),
        max.overlaps = Inf,
        min.segment.length = 0,
        segment.linetype = "dotted",
        segment.size = 0.2,
        family = "Goudy Old Style",
        size = 2.3,
        label.size = NA
    ) +

    # geom_text_repel(
    #     data = top, 
    #     aes(x = release_date, y = upvotes, label = name),
    #     max.overlaps = Inf,
    #     min.segment.length = 0,
    #     segment.linetype = "dotted",
    #     segment.size = 0.2,
    #     family = "Goudy Old Style",
    #     size = 2.5
    # ) +
    
    scale_y_continuous(trans = "log10", expand = c(0, 0),
                       labels = scales::comma) +
    
    scale_x_datetime(expand = c(0, 0)) +
    
    facet_wrap(vars(product_ranking), nrow = 1) +
    
    coord_cartesian(expand = TRUE, clip = "off") +
    
    theme_minimal(base_family = "Goudy Old Style") +
    
    theme(
        legend.position = "bottom",
        
        strip.text = element_markdown(face = "bold"),
        
        axis.text.x = element_text(size = 8),
        
        plot.title.position = "plot",
        plot.caption.position = "plot",
        
        plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_markdown(margin = margin(b = 10), size = 10,
                                         color = "gray30"),
        plot.caption = element_markdown(margin = margin(t = 10)),
        
        
        panel.grid.major = element_line(color = "gray80", size = 0.25, linetype = "dashed"),
        panel.grid.minor = element_line(color = "gray80", size = 0.25, linetype = "dashed"),
        
        axis.ticks = element_line(color = "gray80", size = 0.25),
        
        panel.spacing = unit(1, "lines"),
        
        plot.background = element_rect(fill = "#f2f2f2", color = "#f2f2f2"),
        plot.margin = margin(20, 20, 20, 20)
    ) +
    
    labs(
        y = "Number Of Upvotes Received",
        x = "Release Date",
        
        title = "Product Hunt",
        subtitle = paste0(
            "Number of upvotes received per product ranking over the years. ",
            "Top *#10* products were labeled for each rank."
        ),
        
        caption = paste0(
            "Source: **components.one** | ",
            "Graphic: **Nikolaos Pechlivanis**"
        )
    )


ggsave(
    filename = "Rplot.pdf",
    plot = gr,
    width = 14, height = 7,
    device = cairo_pdf,
    units = "in"
)

x = image_read_pdf("Rplot.pdf", density = 500)
image_write(x, "Rplot.png")





