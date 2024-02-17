


rm(list = ls())
gc()

# load libraries -----------------------------------

library(data.table)
library(stringr)

library(ggplot2)
library(ggtext)

library(ggnewscale)

library(colorspace)
library(extrafont)

# input data frame -----------------------------------

historical_spending <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-02-13/historical_spending.csv')
gifts_age <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-02-13/gifts_age.csv')
gifts_gender <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-02-13/gifts_gender.csv')

gifts_age$SpendingCelebrating = NULL
gifts_gender$SpendingCelebrating = NULL

df = gifts_age |> melt(id.vars = "Age", variable.factor = FALSE, value.factor = FALSE)
df2 = gifts_gender |> melt(id.vars = "Gender", variable.factor = FALSE, value.factor = FALSE)

df$label = paste0(df$value)

# library(shadowtext)

base_color = "#6e2037" |> lighten(.75)
grid_color = "#AEB6E5"
# perc_color = "#b4276d"


gr = df |>
    ggplot(aes(variable, Age)) +
    
    geom_point(size = 23, shape = 21, stroke = .3, fill = base_color, color = grid_color) +

    geom_point(aes(size = value, fill = value), shape = 21, stroke = .3, color = grid_color) +

    scale_size_continuous(range = c(1, 23), limits = c(0, 100)) +
    scale_fill_gradient(low = "#9336ba", high = "#6e2037", limits = c(8, 70)) +

    new_scale("size") +
    
    geom_text(aes(label = label, size = value), color = "#f9fbfe", family = "Anton") +
    
    scale_size_continuous(range = c(3, 7)) +
    
    theme_minimal(base_family = "Jost") +
    
    theme(
        legend.position = "none",
        
        axis.text = element_text(face = "bold"),
        
        axis.title.x = element_text(margin = margin(t = 10)),
        axis.title.y = element_text(margin = margin(r = 10)),
        
        panel.grid = element_line(linewidth = .3, linetype = "dotted", color = grid_color),
        plot.title    = element_text(size = 26, margin = margin(b = 5), family = "Jost", face = "bold"),
        plot.subtitle = element_markdown(size = 10, margin = margin(b = 25), family = "Jost"),
        plot.caption  = element_markdown(size = 6,  margin = margin(t = 25), family = "Jost"),
        
        plot.title.position = "plot",
        plot.caption.position = "plot",
        
        plot.background = element_rect(fill = "#f9fbfe", color = NA),
        plot.margin = margin(20, 20, 20, 20)
    ) +
    
    coord_equal() +
    
    labs(
        x = "Type of gift",
        title = "Valentine's Day Consumer Data",
        
        subtitle = paste0(
            "Average Percentage of Spending on Various Types of Gifts by Age Group for Valentine's Day"
        ),
        
        caption = paste0(
            "Source: <b>Valentine's Day survey data</b> | ",
            "Graphic: <b>Nikos Pechlivanis</b>"
        )
    )
    
    
ggsave(
    plot = gr, filename = "Rplot.jpeg",
    width = 10, height = 10, units = "in", dpi = 600
)
    

# scale_size_continuous(range = c(1, 10))

# ggplot(gifts_age, aes(Candy, Flowers)) +
#     geom_delaunay_tile(aes(fill = Age, group = Age), colour = 'black')
# 
# ggplot() +
#     geom_col(data = df, aes(value, variable, fill = Age), position = "dodge") +
#     new_scale_fill() +
#     geom_col(data = df2, aes(-value, variable, fill = Gender), position = "dodge")




