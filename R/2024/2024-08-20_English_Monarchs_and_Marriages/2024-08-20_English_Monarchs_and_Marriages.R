


rm(list = ls())
gc()

# load libraries -----------------------------------

library(data.table)
library(stringr)

library(ggplot2)
library(ggtext)

library(ggforce)
library(shadowtext)

library(paletteer)
library(colorspace)
library(extrafont)

my_font = "Jost"

# 1 ----------------------------------- 

df <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-08-20/english_monarchs_marriages_df.csv')

# 2 ----------------------------

df$king_age    = df$king_age |> as.numeric()
df$consort_age = df$consort_age |> as.numeric()

df$year_of_marriage = df$year_of_marriage |> as.numeric()

df = df[order(year_of_marriage)]

df = df[which(!is.na(king_age) & !is.na(consort_age))]

df$year_of_marriage = df$year_of_marriage |> as.character() |> str_replace_na("NA")

df$index = df |> nrow() |> seq_len() |> paste0("i")
df$index = df$index |> factor(levels = df$index)

df$ann = ifelse(df$king_age >= df$consort_age, "king", "consort")

# 3 ----------------------------

# 4 --------------------------------

gr = df |> 
    ggplot(aes(x = index)) +
    
    geom_hline(yintercept = mean(df$king_age), linetype = "dotted", lineend = "round", color = "#a1000d") +
    geom_hline(yintercept = mean(df$consort_age), linetype = "dotted", lineend = "round", color = "#00a194") +
    
    geom_segment(aes(y = king_age, yend = consort_age, xend = index, color = ann), linewidth = .75) +
    
    geom_point(aes(y = king_age, fill = "Ruler"), color = "grey96", shape = 21, size = 4, stroke = .35) +
    geom_point(aes(y = consort_age, fill = "Consort"), color = "grey96", shape = 21, size = 4, stroke = .35) +
    
    geom_shadowtext(
        aes(y = -9, label = year_of_marriage), angle = 90,
        hjust = 1, vjust = .5, size = 4, color = "grey75", family = "Anton",
        bg.color = "white", bg.r = .05
    ) +
    
    geom_mark_circle(
        aes(
            y = king_age, filter = king_age == 60,
            label = king_name, 
            description = paste0(king_age, " years old")
        ),
        
        label.fill = alpha("grey96", .75),
        label.family = my_font,
        label.fontface = c("bold", "italic"),
        
        expand = unit(3, "mm")
    ) +
    
    geom_mark_circle(
        aes(
            y = consort_age, filter = consort_age == 7,
            label = consort_name, 
            description = paste0(consort_age, " years old")
        ),
        
        label.fill = alpha("grey96", .75),
        label.family = my_font,
        label.fontface = c("bold", "italic"),
        
        expand = unit(3, "mm")
    ) +

    scale_color_manual(
        values = c(
            "consort" = "#00a194" |> lighten(.15), 
            "king" = "#a1000d" |> lighten(.15)
        ),
        guide = "none"
    ) +
    
    scale_fill_manual(
        values = c(
            "Ruler" = "#a1000d" |> darken(.15),
            "Consort" = "#00a194" |> darken(.15)
        )
    ) +
    
    scale_y_continuous(limits = c(-10, 63)) +
    
    theme_minimal(base_family = my_font) +
    
    theme(
        legend.position = "top",
        legend.justification = "center",
        legend.title = element_blank(),
        
        axis.text.x = element_blank(),
        
        axis.title.x = element_text(margin = margin(t = 10)),
        axis.title.y = element_text(margin = margin(r = 10)),
        
        panel.grid = element_line(linetype = "dashed", lineend = "round"),
        
        plot.title    = element_text(face = "bold", size = 26, margin = margin(b = 5), family = "Jost", hjust = .5),
        plot.subtitle = element_markdown(margin = margin(b = 15), size = 10, color = "grey25", family = "Jost", hjust = .5),
        plot.caption  = element_markdown(margin = margin(t = 20), size = 8, family = "Jost", hjust = .5),
        
        plot.background = element_rect(fill = "#f9fbfe", color = NA),
        plot.margin = margin(20, 20, 20, 20)
    ) +
    
    labs(
        x = "Historical Year of Marriage",
        y = "Age of Ruler or Consort",
        
        title = "English Monarchs and Marriages",
        
        subtitle = paste0(
            "A timeline of the ages and marriages of various <b style='color:#a1000d'>kings</b>, ",
            "<b style='color:#a1000d'>queens</b>, and their <b style='color:#00a194'>consorts</b>, ",
            "spanning from 858 to the present day."
        ),
        
        caption = paste0(
            "Source: <b>Ian Visits</b> by <b>f. hull</b> | ",
            "Graphic: <b>Nikos Pechlivanis</b>"
        )
    )

ggsave(
    plot = gr, filename = "Rplot.png", dpi = 600,
    width = 12, height = 10, units = "in"
)








