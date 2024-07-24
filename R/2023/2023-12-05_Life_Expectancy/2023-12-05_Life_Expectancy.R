


rm(list = ls())
gc()

library(data.table)
library(stringr)

library(ggplot2)
library(ggtext)

library(ggh4x)

library(countrycode)
library(extrafont)

life_expectancy = fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-12-05/life_expectancy.csv')

continents = countrycode::codelist$continent |> unique() 
continents = continents[which( !is.na(continents) )] |> str_replace("Americas", "United States")


df = life_expectancy[which(Entity %in% c(
    continents, 
    "High-income countries",
    "Middle-income countries", 
    "Lower-middle-income countries",
    "Low-income countries",
    "World"
))]

df = df |> 
    dcast(Year ~ Entity, value.var = "LifeExpectancy") |>
    melt(
        id.vars = c("Year", "World"),
        variable.factor = FALSE, value.factor = FALSE,
        variable.name = "Entity", value.name = "LifeExpectancy"
    )

# df = df[which( !is.na(World) | !is.na(LifeExpectancy) )]



df$Entity = df$Entity |>
    factor(
        levels = c(
            "High-income countries",
            "Middle-income countries",
            "Lower-middle-income countries",
            "Low-income countries",
            "Africa",
            "Asia",
            "Europe",
            "Oceania",
            "United States"
        )
    )
# 
df2 = df
colnames(df2)[3] = "Entity2"
# 
# df3 = life_expectancy[which(Entity == "World" )]
# colnames(df3)[1] = "Entity2"

my_font = "Jost"

gr = ggplot(data = df) +
    
    geom_line(
        data = df2, aes(Year, LifeExpectancy, group = Entity2),
        color = "grey85", linewidth = .65
    ) +
    
    stat_difference(aes(x = Year, ymin = LifeExpectancy, ymax = World),
                    alpha = .3, show.legend = FALSE) +
    
    geom_line(
        aes(Year, LifeExpectancy, group = Entity, color = "Entity"),
        linewidth = .65
    ) +

    geom_line(
        aes(Year, World, group = 1, color = "World"),
        linewidth = .65
    ) +
    
    scale_color_manual(values = c(
        "World"  = "#0064ee", 
        "Entity" = "#ee1300"
    )) +
    
    scale_x_continuous(limits = c(1950, 2021), expand = c(0, 0), breaks = seq(1950, 2021, by = 20)) +
    
    facet_wrap2(vars(Entity), ncol = 3, axes = "all") +
    
    theme_minimal(base_family = my_font) +
    
    theme(
        legend.position = "bottom",
        legend.justification = "left",
        legend.title = element_blank(),
        
        strip.text = element_text(face = "bold", hjust = 0, size = 10),
        
        panel.spacing = unit(1.5, "lines"),
        panel.grid.major = element_line(linetype = "dashed", linewidth = .35, color = "grey85"),
        panel.grid.minor = element_line(linetype = "dashed", linewidth = .25, color = "grey85"),
        
        axis.ticks.y = element_line(linewidth = .35, color = "grey85"),

        axis.title.x = element_blank(),
        axis.title.y = element_text(hjust = 0, margin = margin(r = 10)),
        
        axis.text.x = element_text(size = 8),
        
        plot.title    = element_text(face = "bold", size = 36, margin = margin(b = 5), family = my_font),
        plot.subtitle = element_markdown(margin = margin(b = 25), size = 11, color = "grey20", family = my_font),
        plot.caption  = element_markdown(margin = margin(t = 25), size = 8, family = my_font),
        
        plot.title.position = "plot",
        plot.caption.position = "plot",
        
        plot.background = element_rect(fill = "#f5f5f5", color = NA),
        plot.margin = margin(20, 20, 20, 20)
    ) +
    
    labs(
        y = "Life Expectancy at Birth (Total)",
        
        title = 'Life Expectancy',
        
        subtitle = paste0(
            "Exploring lifespan disparities Across Continents:<br>*a comparative analysis of High-income, Middle-income, and Low-income countries*."
        ),
        
        caption = paste0(
            "Source: <b>Our World in Data Life Expectancy report</b> | ",
            "Graphic: <b>Nikolaos Pechlivanis</b>"
        )
    )


ggsave(
    plot = gr, filename = "Rplot.jpeg",
    width = 10, height = 12, units = "in", dpi = 600
)










