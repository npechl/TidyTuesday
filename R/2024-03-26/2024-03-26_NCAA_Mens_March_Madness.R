


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

d1 <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-03-26/team-results.csv')
# d2 <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-03-26/public-picks.csv')


d1 |> head()


p1 = d1[, c("TEAM", "R64", "R32", "S16", "E8", "F4", "F2"), with = FALSE] |>
    melt(id.vars = "TEAM", variable.factor = FALSE)

t = p1[, by = TEAM, .(value = value |> sum())]
t = t[order(value)]



p1$TEAM = p1$TEAM |> factor(levels = t$TEAM)
p1$variable = p1$variable |> 
    str_replace_all("R64", "Round of 64") |>
    str_replace_all("R32", "Round of 32") |>
    str_replace_all("S16", "Sweet 16") |>
    str_replace_all("E8", "Elite 8") |>
    str_replace_all("F4", "Final 4") |>
    str_replace_all("F2", "Finals") |>
    factor(levels = c("Finals", "Final 4", "Elite 8", "Sweet 16", "Round of 32", "Round of 64"))


gr = p1 |>
    ggplot(aes(value, TEAM)) +
    
    geom_col(aes(fill = variable), color = "grey96", linewidth = .1) +
    
    annotate("richtext", x = 50, y = 162, label = "NCAA Men's<br>March Madness", family = "Bebas Neue", fontface = "bold", size = 20, hjust = 1, color = "grey96", fill = NA, label.color = NA) +
    annotate("richtext", x = 50, y = 150, label = "Source: <b>Nishaan Amin's Kaggle dataset</b><br>Graphic: <b>Nikos Pechlivanis</b>", family = my_font, size = 4, hjust = 1, color = "grey96", fill = NA, label.color = NA) +
    
    scale_fill_manual(
        values = paletteer_dynamic("cartography::harmo.pal", 6, -1), 
        guide = guide_legend(
            title = "Amount of times the team made it to:",
            title.position = "left",
            title.theme = element_text(hjust = 0, color = "grey96", angle = 90, face = "bold", family = my_font),
            label.theme = element_text(color = "grey96", face = "bold", family = my_font),
            ncol = 1,
            
            barheight = unit(3, "line"),
            barwidth = unit(.5, "lines")
        )
    ) +
    
    scale_x_continuous(expand = c(0, 0), limits = c(0, 55), position = "top", sec.axis = dup_axis()) +
    
    theme_minimal(base_family = my_font) +
    theme(
        legend.position = c(.85, .85),
        # legend.justification = "center",
        legend.background = element_rect(fill = "#265763" |> alpha(.5), color = NA),
        legend.title.position = "top",
        
        panel.grid.major.x = element_line(linewidth = .35, color = "grey96"),
        panel.grid.major.y = element_line(linewidth = .25, color = "grey96", linetype = "dotted"),
        panel.grid.minor = element_line(linewidth = .25, color = "grey96", linetype = "dotted"),
        
        axis.ticks = element_line(linewidth = .35, color = "grey96"),
        
        axis.title.x.top = element_text(color = "grey96", face = "bold", margin = margin(b = 10)),
        axis.title.x.bottom = element_text(color = "grey96", face = "bold", margin = margin(t = 10)),
        axis.title.y = element_text(color = "grey96", face = "bold", margin = margin(r = 10)),
        
        axis.text = element_text(color = "grey96"),
        
        # plot.title    = element_text(margin = margin(b = 5), family = "Raleway", ),
        # plot.caption  = element_markdown(margin = margin(t = 10), family = "Jost", size = 10, hjust = .5, color = "grey96"),
        
        plot.background = element_rect(fill = "#265763", color = NA),
        plot.margin = margin(20, 20, 20, 20)
    ) +
    
    labs(
        x = "The total amount of tournament games the team has played", 
        y = "Division I college basketball team name"
        
        # caption = paste0(
        #     "Source: <b>Nishaan Amin's Kaggle dataset</b>",
        #     " | ",
        #     "Graphic: <b>Nikos Pechlivanis</b>"
        # )
    )



ggsave(
    plot = gr, filename = "Rplot.pdf", device = cairo_pdf,
    width = 8, height = 24, units = "in" # dpi = 600
)

library(magick)


tmp = image_read_pdf("Rplot.pdf", density = 600)
image_write(tmp, "Rplot.png")

# p1 = d2[, c("TEAM", "R64", "R32", "S16", "E8", "F4", "FINALS"), with = FALSE] |>
#     melt(id.vars = "TEAM", variable.factor = FALSE)
# 
# p1$value = p1$value |> str_remove_all("%") |> as.numeric() / 100
# 
# t = p1[, by = TEAM, .(value = value |> sum())]
# t = t[order(value)]
# 
# p1$TEAM = p1$TEAM |> factor(levels = t$TEAM) |>
#     fct_rev()
# 
# p1$variable = p1$variable |> 
#     factor(levels = c("FINALS", "F4", "E8", "S16", "R32", "R64")) |>
#     fct_rev()
# 
# p1 |>
#     
#     ggplot(aes(TEAM, value)) +
#     
#     geom_col(aes(fill = variable), position = "dodge") +
#     
#     scale_y_continuous(
#         labels = scales::percent, 
#         expand = c(0, 0),
#         limits = c(0, 1)
#     ) +
#     
#     scale_fill_manual(
#         values = c('#ffffe0', '#ffcab9', '#fd9291', '#e75d6f', '#c52a52', '#93003a')
#     ) +
#     
#     theme_minimal() +
#     
#     theme(
#         plot.margin = margin(20, 20, 20, 20)
#     )
# 
# 
# 
# p1 |>
#     ggplot(aes(TEAM, value)) +
#     
#     geom_area(aes(group = variable, fill = variable, color = variable), alpha = .5) 

