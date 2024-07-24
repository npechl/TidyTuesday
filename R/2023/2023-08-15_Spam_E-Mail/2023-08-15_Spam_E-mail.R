


rm(list = ls())
gc()


library(data.table)
library(stringr)

library(ggplot2)
library(ggtext)

library(paletteer)
library(extrafont)

spam <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-08-15/spam.csv')

df = spam

df$crl.tot = NULL

df = df |>
    melt(id.vars = "yesno", variable.factor = FALSE, value.factor = FALSE)

df$value = df$value / 100

df$yesno = ifelse(df$yesno == "n", "No", "Yes")

df$variable = df$variable |>
    str_replace_all("dollar", "Occurrences of the <b>dollar sign</b>,<br>as percent of total number of characters") |>
    str_replace_all("bang", "Occurrences of <b>‘!’</b>,<br>as percent of total number of characters") |>
    str_replace_all("money", "Occurrences of <b>‘money’</b>,<br>as percent of total number of characters") |>
    str_replace_all("n000", "Occurrences of the string <b>‘000’</b>,<br>as percent of total number of words") |>
    str_replace_all("make", "Occurrences of <b>‘make’</b>,<br>as a percent of total number of words")

library(ggforce)
library(ggdist)

my_font = "Jost"


gr = ggplot(data = df, aes(y = variable, x = value)) +
    
    geom_point(aes(fill = yesno), color = "white",
               shape = 21, size = 2, stroke = .1,
               position = position_jitterdodge(jitter.width = .25, dodge.width = .75)) +
    
    stat_pointinterval(aes(color = yesno), 
                       position = position_dodge(width = .75)) +
    
    scale_fill_manual(
        values = c(
            "Yes" = alpha("#0099cc", alpha = .3),
            "No"  = alpha("#cc0033", alpha = .3)
        ),
        
        guide = "none"
    ) +
    
    scale_color_manual(
        values = c(
            "Yes" = "#005e7e",
            "No"  = "#7e001f"
        ),
        
        guide = guide_legend(
            title = "Does this email qualify as spam?",
            title.position = "top",
            override.aes = list(size = 3)
        )
    ) +
    
    scale_x_continuous(
        trans = scales::pseudo_log_trans(base = 10, sigma = .001),
        expand = c(0, 0), labels = scales::percent, limits = c(0, .35),
        breaks = c(0, .001, .01, .1, .35)
    ) +
    
    theme_minimal(base_family = my_font) +
    
    theme(
        legend.position = "bottom",
        legend.justification = "left",
        
        axis.title = element_blank(),
        
        axis.text.y = element_markdown(),
        
        axis.line.x = element_line(linewidth = .3),
        axis.ticks.x = element_line(linewidth = .3),
        
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(linewidth = .3, linetype = "dashed", color = "grey85"),
        
        plot.title    = element_text(face = "bold", size = 32, family = my_font),
        plot.subtitle = element_markdown(margin = margin(b = 15), size = 10, color = "grey20", family = my_font),
        plot.caption  = element_markdown(margin = margin(t = 15), size = 6, family = my_font),
        
        plot.title.position = "plot",
        plot.caption.position = "plot",
        
        plot.background = element_rect(fill = "#f5f5f5", color = NA),
        plot.margin = margin(20, 20, 20, 20)
    ) +
    
    labs(
        title = 'Spam E-mail',
        
        subtitle = paste0(
            "<i>This is a dataset collected at Hewlett-Packard Labs by Mark ",
            "Hopkins, Erik Reeber, George Forman, and Jaap Suermondt and ",
            "shared with the UCI Machine Learning Repository.<br>The dataset ",
            "classifies 4601 e-mails as spam or non-spam, with additional ",
            "variables indicating the frequency of certain words and ",
            "characters in the e-mail.</i>"
        ),
        
        caption = paste0(
            "Source: <b>Vincent Arel-Bundock's Rdatasets package</b> | ",
            "Graphic: <b>Nikolaos Pechlivanis</b>"
        )
    )


ggsave(
    plot = gr, filename = "Rplot.jpeg",
    width = 12, height = 8, units = "in", dpi = 600
)



# ggplot(data = df) +
#     
#     geom_density(aes(x = value, fill = yesno), position = "stack", # adjust = 10, 
#                  alpha = .3, linewidth = .25, color = "white") +
#     
#     scale_x_continuous(
#         trans = scales::pseudo_log_trans(base = exp(100)),
#         expand = c(0, 0), labels = scales::percent
#     ) +
#     
#     scale_y_continuous(expand = c(0, 0)) +
#     
#     facet_wrap(vars(variable), ncol = 1, scales = "free") +
#     
#     theme_minimal() +
#     
#     theme(
#         legend.position = "bottom",
#         strip.text = element_text(hjust = 0)
#     )












