


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

# 1 ----------------------------------- 

a1 <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-09-24/country_results_df.csv')
a2 <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-09-24/individual_results_df.csv')
a3 <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-09-24/timeline_df.csv')

# 2 ---------------------------

library(ggstream)

b3 <- a3[, c("year", "country", "city", "male_contestant", "female_contestant"), with = FALSE] |>
    melt(id.vars = c("year", "country", "city"), variable.factor = FALSE, value.factor = FALSE)

gr1 = b3 |>
    ggplot(aes(year, value, fill = variable)) +
    
    geom_stream(geom = "polygon", color = "grey96") +
    
    annotate("richtext", x = 2019.5, y = 275, label = "**Female**<br>contestants", family = my_font, fill = NA, label.size = NA, size = 4, color = "#CD534C" |> darken(.9)) +
    annotate("richtext", x = 2015, y = 0, label = "**Male**<br>contestants", family = my_font, fill = NA, label.size = NA, size = 5, color = "#003C67" |> darken(.9)) +
    
    scale_fill_manual(values = c("male_contestant" = "#003C67" |> lighten(.35), "female_contestant" = "#CD534C" |> lighten(.35))) +
    scale_y_continuous(breaks = c(-300, -200, -100, 0, 100, 200, 300), 
                       labels = c("+300", "+200", "+100", "0", "+100", "+200", "+300")) +
    
    scale_x_continuous(breaks = seq(1960, 2024, by = 10), expand = c(0, 0)) +
    
    coord_cartesian(clip = "off") +
    
    theme_minimal(base_family = my_font) +
    theme(
        legend.position = "none",
        legend.title = element_blank(),
        axis.line = element_line(),
        axis.ticks = element_line(),
        
        axis.title.x = element_text(margin = margin(t = 10)),
        axis.title.y = element_markdown(margin = margin(r = 10)),
        
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(linetype = "dashed", lineend = "round", color = "grey85")
    ) +
    
    labs(y = "**Number of Participating Contestants**", x = "Year of IMO")


# 3 ----------------------------------------

b1 = a1[which(!(is.na(team_size_male) & is.na(team_size_female)))]

b1$team_size_female = ifelse(
    !is.na(b1$team_size_male), 
    b1$team_size_all - b1$team_size_male,
    b1$team_size_female
)

b1$cluster = ifelse(b1$team_size_female > 0, "Female contestant(s) in the team", "Only Male contestant(s) teams")

b1 = b1[, by = .(year, cluster), .(
    "Gold medals" = awards_gold |> mean(na.rm = TRUE),
    "Silver medals" = awards_silver |> mean(na.rm = TRUE),
    "Bronze medals" = awards_bronze |> mean(na.rm = TRUE),
    "Honorable mentions" = awards_honorable_mentions |> mean(na.rm = TRUE)
    # "p1" = p1 |> mean(na.rm = TRUE), 
    # "p2" = p2 |> mean(na.rm = TRUE), 
    # "p3" = p3 |> mean(na.rm = TRUE), 
    # "p4" = p4 |> mean(na.rm = TRUE), 
    # "p5" = p5 |> mean(na.rm = TRUE), 
    # "p6" = p6 |> mean(na.rm = TRUE), 
    # "p7" = p7 |> mean(na.rm = TRUE)
)] |> melt(id.vars = c("year", "cluster"), variable.factor = FALSE, value.factor = FALSE)

b1$variable = b1$variable |> factor(levels = c("Gold medals", "Silver medals", "Bronze medals", "Honorable mentions"))

library(ggdist)

gr2 = b1 |>
    ggplot(aes(year, value)) +
    
    # geom_line(aes(group = variable, color = variable), linewidth = .75, lineend = "round") +
    
    # stat_lineribbon(aes(group = variable, color = variable, fill = variable), linewidth = .75, lineend = "round") +
    
    geom_smooth(aes(group = variable, color = variable, fill = variable), linewidth = .75, lineend = "round") +
    
    geom_point(aes(color = variable, fill = variable), shape = 21, size = 1, stroke = .5) +
    
    scale_y_continuous(expand = c(0, 0), limits = c(0, 3)) +
    scale_x_continuous(expand = c(0, 0), breaks = seq(1960, 2024, by = 10)) +
    
    scale_color_manual(values = c('#FF6F00', '#FF95A8', '#008EA0', '#8A4198'), guide = guide_legend(nrow = 2)) +
    scale_fill_manual(values = c('#FF6F00', '#FF95A8', '#008EA0', '#8A4198') |> lighten(.75), guide = guide_legend(nrow = 2)) +
    
    facet_wrap(vars(cluster), ncol = 1, axes = "all") +
    
    theme_minimal(base_family = my_font) +
    
    theme(
        legend.position = c(.7, .37),
        legend.direction = "horizontal",
        legend.title = element_blank(),
        
        axis.line = element_line(),
        axis.ticks = element_line(),
        
        axis.title.x = element_text(margin = margin(t = 10)),
        axis.title.y = element_markdown(margin = margin(r = 10)),
        
        strip.text = element_text(face = "bold", margin = margin(b = 10)),
        
        panel.spacing = unit(1, "lines"),
        
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(linetype = "dashed", lineend = "round", color = "grey85")
    ) +
    
    labs(y = "**Average number of medals/awards** won", x = "Year of IMO")

# patchwork ----------------------------

library(patchwork)


gr_multi <- (gr1 | gr2) +
    plot_annotation(
        title = "International Mathematical Olympiad (IMO) Data",
        
        subtitle = paste0(
            "**Gender distribution of participation in the International ", 
            "Mathematical Olympiad** (IMO),<br>the world championship mathematics ",
            "competition for high school students"
        ),
        
        caption = paste0(
            "Source: <b>International Mathematical Olympiad (IMO)</b> | ",
            "Graphic: <b>Nikos Pechlivanis</b>"
        )
    ) &
    theme(
        plot.title    = element_text(face = "bold", size = 20, margin = margin(b = 5), family = "Jost", hjust = .5),
        plot.subtitle = element_markdown(margin = margin(b = 15), size = 10, color = "grey25", family = "Jost", hjust = .5),
        plot.caption  = element_markdown(margin = margin(t = 20), size = 8, family = "Jost", hjust = .5),
        
        plot.background = element_rect(fill = "#f8f2f9", color = NA),
        plot.margin = margin(20, 20, 20, 20)
    )


ggsave(
    plot = gr_multi, filename = "Rplot.png",
    width = 12, height = 9, units = "in", dpi = 600
)


