


rm(list = ls())
gc()

# load libraries -----------------------------------

library(data.table)
library(stringr)

library(ggplot2)
library(ggtext)

library(ggridges)
library(ggdist)

library(paletteer)
library(colorspace)
library(extrafont)

# input data frame -----------------------------------

english_education <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-01-23/english_education.csv')

english_education$size_flag <- english_education$size_flag |>
    factor(levels = c("City", "Large Towns", "Medium Towns", "Small Towns"))

english_education = english_education[which(!is.na(size_flag))]

my_font = "Jost"

# plot 1 --------------------------------

library(shadowtext)

gr1 = english_education |>
    ggplot(aes(x = education_score, y = size_flag, fill = university_flag)) +
    
    geom_density_ridges(color = "grey96", alpha = .75, linewidth = .15) +
    geom_vline(xintercept = 0, linewidth = .35, linetype = "dotdash", color = lighten("#F5B355", .25)) +
    
    geom_shadowtext(
        aes(x = -13, y = size_flag, label = size_flag),
        position = position_nudge(x = .1, y = .2), size = 5,
        hjust = 0, family = "Bebas Neue", fontface = "bold", color = "grey50",
        bg.color = "white", bg.r = .02
    ) +
    
    scale_y_discrete(expand = c(0, 0)) +
    
    scale_fill_manual(
        values = c(
            "No university" = "#A71B4B",
            "University"    = "#584B9F"
        )
    ) +
    
    theme_minimal(base_family = my_font) +
    
    theme(
        # legend.title = element_blank(),
        # legend.position = c(.15, .9),
        
        legend.position = "none",
        
        axis.line.x = element_line(linewidth = .3),
        axis.ticks.x = element_line(linewidth = .3),
        
        # axis.title = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_text(margin = margin(t = 10), face = "bold", size = 8, hjust = 0),
        
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 6),
        
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        
        plot.margin = margin(20, 20, 20, 20)
    ) +
    
    labs(x = "Education score")


# plot 2 -----------------------------------------

library(ggforce)

gr2 = english_education |>
    
    ggplot(aes(y = highest_level_qualification_achieved_b_age_22_average_score, x = size_flag)) +
    
    stat_slab(aes(fill = university_flag), side = "left", alpha = .5, expand = TRUE, trim = FALSE) +
    
    stat_interval(
        aes(color_ramp = after_stat(level)), 
        position = position_nudge(x = .13),
        linewidth = 2, color = "#F5B355"
    ) +
    
    geom_point(
        aes(fill = university_flag), 
        position = position_jitternormal(sd_x = .025),
        shape = 21, stroke = .1, size = 2.5, color = "white"
    ) +
    
    scale_fill_manual(
        values = c(
            "No university" = "#A71B4B",
            "University"    = "#584B9F"
        )
    ) +
    
    theme_minimal(base_family = my_font) +
    
    theme(
        legend.position = "bottom",
        legend.justification = "left",
        legend.margin = margin(t = 10),
        
        axis.title.x = element_blank(),
        axis.title.y = element_markdown(margin = margin(r = 10), hjust = 0),
        
        axis.text.x = element_text(face = "bold"),
        
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(linetype = "dashed"),
        
        plot.title    = element_text(face = "bold", size = 26, margin = margin(b = 5), family = "Jost"),
        plot.subtitle = element_markdown(margin = margin(b = 25), size = 10, color = "grey25", family = "Jost"),
        plot.caption  = element_markdown(margin = margin(t = 25), size = 7, family = "Jost"),
        
        plot.title.position = "plot",
        plot.caption.position = "plot",
        
        plot.background = element_rect(fill = "#f9fbfe", color = NA),
        plot.margin = margin(20, 20, 20, 20)
    ) +
    
    guides(
        color_ramp = guide_legend(
            title = "% of observations",
            title.position = "top"
            # title.theme = element_text(angle = 90, hjust = 1)
        ),
        
        fill = guide_legend(
            title = "University flag",
            title.position = "top"
        )
    ) +
    
    labs(
        y = paste0(
            "**Town/city highest qualification average score** based on<br>highest ",
            "levels of qualifications achieved of the 2012/13 KS4 cohort."
        ),
        
        title = 'Educational attainment of young people in English towns',
        
        subtitle = paste0(
            "Elevated town/city education score and higher average ",
            "qualification attainment among young residents in ", 
            "English small towns hosting a university"
        ),
        
        caption = paste0(
            "Source: <b>The UK Office for National Statistics</b> | ",
            "Graphic: <b>Nikolaos Pechlivanis</b>"
        )
    )


# patchwork -------------------

library(patchwork)

multi = gr2 + inset_element(gr1, .05, .65, .45, 1, align_to = "plot", clip = FALSE)

    
ggsave(
    plot = multi, filename = "Rplot.pdf", device = cairo_pdf,
    width = 11, height = 11, units = "in", 
)


library(magick)

tmp = image_read_pdf("Rplot.pdf", density = 600)
image_write(tmp, "Rplot.jpeg")





