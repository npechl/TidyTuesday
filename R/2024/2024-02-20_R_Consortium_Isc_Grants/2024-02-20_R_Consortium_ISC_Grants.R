


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

df <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-02-20/isc_grants.csv')

df$summary = NULL
df$website = NULL

df$group = ifelse(df$group == 1, "Spring cycle", "Fall cycle")

df = df[which(funded != 0)]

df$funded_norm = 1000 * df$funded / max(df$funded)

library(packcircles)

df = df |> 
    split(by = c("year", "group")) |>
    lapply(function(q) {
        
        q <- q[order(-funded_norm)]
        
        packing <- circleProgressiveLayout(q$funded_norm, sizetype = 'area')
        
        dat.gg <- circleLayoutVertices(packing, npoints = 100)
        
        dat.gg <- cbind(dat.gg, q[dat.gg$id])
        
        dat.gg <- dat.gg |> setDT()
        
        return(dat.gg)
    }) |>
    rbindlist()

library(ggh4x)

df[, by = year, N := title |> unique() |> length()]
df[, by = .(year, group), N2 := title |> unique() |> length()]

df$year_strip = paste0(
    "**", df$year, "** *",  df$N, " grants*"
)

df$group_strip = paste0(
    df$group, " (", df$N2, ")"
)

df$group_strip = df$group_strip |> factor(levels = df$group_strip |> unique() |> sort(decreasing = TRUE))

df2 = df[, by = .(id, year, group, title, funded, proposed_by, funded_norm, N, N2, year_strip, group_strip), .(
    x = (min(x) + max(x)) / 2,
    y = (min(y) + max(y)) / 2
)]

df2 = df2[which(funded_norm >= 800)]

df2$title_label = df2$title |> str_wrap(width = 15)

library(ggrepel)
library(shadowtext)

my_font = "Jost"

gr = df |>
    ggplot() + 
    
    geom_polygon(aes(x, y, group = id, fill = funded), 
                 color = "grey20", linewidth = .15) +
    
    # geom_text_repel(
    #     data = df2, inherit.aes = FALSE,
    #     aes(x, y, label = title_label),
    #     size = 2, color = "white", family = my_font, fontface = "bold",
    #     bg.color = "grey20", min.segment.length = 0, 
    #     segment.size = .15, segment.color = "grey20"
    # ) +
    
    geom_shadowtext(
        data = df2, inherit.aes = FALSE,
        aes(x, y, label = title_label),
        size = 2, color = "white", family = my_font,
        bg.color = "grey20", fontface = "bold"
    ) +
    
    scale_x_continuous(limits = c(-45, 45), expand = c(0, 0)) +
    scale_y_continuous(limits = c(-45, 45), expand = c(0, 0)) +
    
    scale_fill_stepsn(
        colors = c('#00429d', '#73a2c6', '#f4777f', '#93003a'),
        labels = scales::dollar_format(scale = .001, suffix = "k"),
        guide = guide_colorsteps(
            title = "The dollar amount funded for the project",
            title.position = "top",
            title.theme = element_text(hjust = 0, size = 9, family = my_font),
            barheight = unit(.35, "lines"),
            barwidth = unit(12, "lines"),
            label.theme = element_text(face = "italic", family = my_font, size = 9)
        )
    ) +
    
    facet_nested_wrap(
        vars(year_strip, group_strip),
        ncol = 6, strip = strip_nested(bleed = FALSE),
        nest_line = element_line(inherit.blank = TRUE)
    ) +
    
    coord_equal() +
    
    theme_void(base_family = my_font) +
    
    theme(
        legend.position = "top",
        
        panel.grid.minor = element_line(linetype = "dashed", linewidth = .35, color = "grey90"),
        panel.grid.major = element_line(linetype = "dashed", linewidth = .35, color = "grey90"),

        panel.spacing.x = unit(1, "lines"),
        panel.spacing.y = unit(2, "lines"),

        ggh4x.facet.nestline = element_line(color = "grey10"),
        
        strip.text = element_markdown(margin = margin(b = 5, t = 5), hjust = .5),
        
        plot.title    = element_text(size = 26, family = my_font, face = "bold", hjust = .5),
        plot.subtitle = element_markdown(size = 9, family = my_font, margin = margin(b = 10), hjust = .5),
        plot.caption  = element_markdown(size = 6, family = my_font, margin = margin(t = 10), hjust = 0),

        plot.title.position = "plot",
        plot.caption.position = "plot",

        plot.background = element_rect(fill = "#fbfbf8", color = NA),
        plot.margin = margin(20, 20, 20, 20)
    ) +
    
    labs(
        title = "R Consortium ISC Grants",

        subtitle = paste0(
            "The total number of R Consortium ISC Grants awarded between 2016 and 2023.<br>",
            "Notably, the projects receiving the highest funding are the ",
            "*Ongoing infrastructural development for R on Windows and MacOS*,<br>",
            "as well as *Editorial assistance for the R Journal*. Each of these ",
            "initiatives has secured over $50,000 in funding."
        ),

        caption = paste0(
            "Source: <b>R Consortium ISC Grant Program</b> | ",
            "Graphic: <b>Nikos Pechlivanis</b>"
        )
    )




ggsave(plot = gr, filename = "Rplot.jpeg", width = 12, height = 10, units = "in", dpi = 600)








