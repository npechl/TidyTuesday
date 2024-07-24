


rm(list = ls())
gc()

library(data.table)
library(stringr)

library(ggplot2)
library(ggtext)
library(ggh4x)

library(geomtextpath)

library(extrafont)

demographics <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-09-05/demographics.csv')
wages        <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-09-05/wages.csv')
# states       <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-09-05/states.csv')


wages = wages[which(
    !(facet %in% c("all wage and salary workers", "private sector: all", "public sector: all"))
)]


df = list(
    "a" = wages[which(str_detect(wages$facet, "demographics"))],
    "b" = wages[which(str_detect(wages$facet, "public sector|private sector"))],
    "c" = wages[which(str_detect(wages$facet, "public sector|private sector|demographics", negate = TRUE))]
)

my_font = "Jost"

# a plot -----------------

df$a$facet = df$a$facet |>
    str_remove_all("demographics|\\:") |>
    str_squish() |>
    str_to_title()

df$a$facet = paste0("Demographics: **", df$a$facet, "**")

df$a$facet = df$a$facet |>
    factor(
        levels = c(
            "Demographics: **Less Than College**",
            "Demographics: **College Or More**",
            "Demographics: **Black Female**",
            "Demographics: **Black Male**",
            "Demographics: **Hispanic Female**",
            "Demographics: **Hispanic Male**",
            "Demographics: **White Female**",
            "Demographics: **White Male**",
            "Demographics: **Female**",
            "Demographics: **Male**"
            
        )
    )

t = df$a
t$group = t$facet
t$facet = NULL


gr1 = ggplot(data = df$a) +
    
    geom_line(
        data = t, aes(x = year, y = wage, group = group), 
        color = "grey75"
    ) +
    
    geom_line(
        aes(x = year, y = wage, group = facet), color = "red"
    ) +
    
    facet_wrap2(vars(facet), nrow = 2, axes = "all") +
    
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0), limits = c(0, 50)) +
    
    theme_minimal(base_family = my_font) +
    
    theme(
        axis.text = element_text(size = 8),
        axis.title.x = element_blank(),
        axis.title.y = element_text(hjust = 0),
        
        axis.line.x = element_line(linewidth = .3),
        axis.ticks.x = element_line(linewidth = .3),
        
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(linewidth = .3, linetype = "dashed", color = "grey80"),
        panel.spacing = unit(1, "lines"),
        
        strip.text = element_markdown(hjust = 0)
    ) + 
    
    labs(y = "Mean hourly earnings in nominal dollars")


# b plot -------------


df$b$type = df$b$facet |> 
    str_split("\\:") |>
    lapply(str_squish) |>
    lapply(function(x) x[1]) |>
    unlist() |>
    str_to_title()

df$b$facet = df$b$facet |>
    str_remove_all("private|public|sector|\\:") |>
    str_squish() |>
    str_to_title()

df$b$facet = paste0(df$b$type, ": **", df$b$facet, "**")

t = df$b
t$group = t$facet
t$facet = NULL

gr2 = ggplot(data = df$b) +
    
    geom_line(
        data = t, aes(x = year, y = wage, group = group), 
        color = "grey75"
    ) +
    
    geom_line(
        aes(x = year, y = wage, group = facet), color = "red"
    ) +
    
    facet_wrap2(vars(facet), nrow = 2, axes = "all") +
    
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0), limits = c(0, 50)) +
    
    theme_minimal(base_family = my_font) +
    
    theme(
        axis.text = element_text(size = 9),
        axis.title.x = element_blank(),
        axis.title.y = element_text(hjust = 0),
        
        axis.line.x = element_line(linewidth = .3),
        axis.ticks.x = element_line(linewidth = .3),
        
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(linewidth = .3, linetype = "dashed", color = "grey80"),
        panel.spacing = unit(1, "lines"),
        
        strip.text = element_markdown(hjust = 0)
    ) + 
    
    labs(y = "Mean hourly earnings in nominal dollars")

# c plot ----------------------

df$c$facet = df$c$facet |> str_to_title()

df$c$facet = paste0("**", df$c$facet, "**")

t = df$c
t$group = t$facet
t$facet = NULL

gr3 = ggplot(data = df$c) +
    
    geom_line(
        data = t, aes(x = year, y = wage, group = group), 
        color = "grey75"
    ) +
    
    geom_line(
        aes(x = year, y = wage, group = facet), color = "red"
    ) +
    
    facet_wrap2(vars(facet), nrow = 1, axes = "all") +
    
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0), limits = c(0, 50)) +
    
    theme_minimal(base_family = my_font) +
    
    theme(
        axis.text = element_text(size = 9),
        axis.title.x = element_blank(),
        axis.title.y = element_text(hjust = 0),
        
        axis.line.x = element_line(linewidth = .3),
        axis.ticks.x = element_line(linewidth = .3),
        
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(linewidth = .3, linetype = "dashed", color = "grey80"),
        panel.spacing = unit(1, "lines"),
        
        strip.text = element_markdown(hjust = 0)
    ) + 
    
    labs(y = "Mean hourly earnings in nominal dollars")


library(patchwork)


multi = gr1 / gr2 / gr3 +
    plot_layout(heights = c(2, 3, 1.5)) +
    plot_annotation(
        title = 'Union Membership in the United States',
        
        subtitle = paste0(
            "*Unionstats.com provides annual measures of union, nonunion, and ",
            "overall wages, beginning in 1973, compiled from the U.S. Current ",
            "Population Surveys.<br>Regression-based union wage gap estimates are ",
            "presented economy-wide, for demographic groups, and sectors ",
            "(private/public, industries). Union wage gaps<br>are higher in the ",
            "private than in the public sector, higher for men than women, ",
            "roughly similar for black and white men, and much higher for ",
            "Hispanic men than for Hispanic women.*"
        ),
        
        caption = paste0(
            "Source: <b>Union Membership, Coverage, and Earnings from the CPS</b> | ",
            "Graphic: <b>Nikolaos Pechlivanis</b>"
        )
    ) &
    
    theme(
        plot.title    = element_text(face = "bold", size = 40, family = my_font),
        plot.subtitle = element_markdown(margin = margin(b = 15), size = 12, color = "grey20", family = my_font),
        plot.caption  = element_markdown(margin = margin(t = 15), size = 10, family = my_font),
        
        plot.title.position = "plot",
        plot.caption.position = "plot",
        
        plot.background = element_rect(fill = "#f5f5f5", color = NA),
        plot.margin = margin(20, 20, 20, 20)
    )


ggsave(
    plot = multi, filename = "Rplot.jpeg",
    width = 14, height = 16, units = "in", dpi = 600
)








