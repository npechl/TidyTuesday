


rm(list = ls())
gc()


library(data.table)
library(stringr)



tuesdata <- tidytuesdayR::tt_load('2022-06-28')



paygap = tuesdata$paygap

rm(tuesdata)


paygap = setDT(paygap)
paygap = paygap[order(employer_name, date_submitted), ]

length(unique(paygap$employer_name))



library(ggplot2)
# library(ggdist)
library(ggsci)
library(ggtext)


paygap = paygap[which(paygap$date_submitted >= "2018-01-01"), ]

paygap$year_submitted = str_split(paygap$date_submitted, "\\-", simplify = TRUE)[,1]



x = paygap[, c(
    # "employer_name",
    # "employer_id",
    "diff_mean_hourly_percent",
    # "diff_median_hourly_percent",
    "diff_mean_bonus_percent",
    # "diff_median_bonus_percent",
    "year_submitted"
), with = FALSE]

x = melt(x, id.vars = "year_submitted", variable.factor = FALSE)
x = x[which(!is.na(x$value)), ]
x$level = ifelse(x$value > 0, "higher men's pay", "higher women's pay")
x$value = ifelse(x$value > 0, x$value, -x$value)

x$pay = ifelse(
    str_detect(x$variable, "bonus"),
    "bonus pay", "hourly pay"
)

# x$variable = str_replace_all(x$variable, "_", " ")
# x$variable = str_replace_all(x$variable, "mean", "<b>mean</b>")
# x$variable = str_replace_all(x$variable, "median", "<b>median</b>")

x$variable = factor(
    x$variable, 
    levels = unique(x$variable)[4:1]
)

y = x[, by = .(year_submitted, variable, level, pay), .N]
z = dcast(y, year_submitted+variable ~ level, value.var = "N")

gr1 = ggplot() +
    
    geom_segment(data = z,
                 aes(x = `higher women's pay`, xend = `higher men's pay`, 
                     y = variable, yend = variable),
                 linetype = "dashed", alpha = 0.75, color = "grey") +
    
    geom_point(data = y, 
               aes(
                   y = variable, x = N, 
                   shape = pay, fill = level, color = level
               ),
               size = 5, stroke = 1) +
    
    scale_x_continuous(
        expand = c(0.05, 0.05),
        labels = scales::comma
    ) +
    
    # scale_color_jama() +
    # scale_fill_jama() +
    
    scale_color_manual(
        values = c(
            "higher men's pay"   = "#171717FF",
            "higher women's pay" = "#D33C32FF"
        )
    ) + 
    scale_fill_manual(
        values = c(
            "higher men's pay"   = "#171717FF",
            "higher women's pay" = "#D33C32FF"
        )
    ) +
    
    
    scale_shape_manual(
        values = c(
            "hourly pay" = 21,
            "bonus pay"  = 23
        )
    ) +
    
    facet_wrap(vars(year_submitted), ncol = 1) +
    
    coord_cartesian(expand = TRUE, clip = "off") +
    
    theme_minimal() + 
    
    theme(
        legend.position = "bottom",
        legend.title = element_blank(),
        
        plot.title = element_markdown(margin = margin(b = 10)),
        plot.title.position = "plot",
        
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        
        plot.background = element_rect(fill = "#fafaf5", color = "#fafaf5"),
        
        axis.title.x = element_text(margin = margin(t = 10)),

        strip.text = element_text(face = "bold"),
        
        axis.line.x  = element_line(color = "grey"),
        axis.ticks.x = element_line(color = "grey"),
        
        panel.grid = element_blank()
    ) +
    
    labs(
        x = "Number of employers",
        title = paste0(
            "Difference between the number of employers with higher ",
            "<b style='color:#D33C32FF'>women</b>'s mean pay or higher ",
            "<b style='color:#171717FF'>men</b>'s mean pay"
        )
    )


library(ggdist)

x = paygap[, c(
    "male_lower_quartile",
    "female_lower_quartile",
    "male_lower_middle_quartile",
    "female_lower_middle_quartile",
    "male_upper_middle_quartile",
    "female_upper_middle_quartile",
    "male_top_quartile",
    "female_top_quartile",
    "year_submitted"
), with = FALSE]


x = melt(x, id.vars = "year_submitted", variable.factor = FALSE)

x$level = str_replace_all(x$variable, "male|female|_", " ")
x$level = str_squish(x$level)
x$level = str_to_title(x$level)

x$level = factor(
    x$level,
    levels = unique(x$level)
)

x$variable = str_split(x$variable, "_", simplify = TRUE)[, 1]
x$variable = str_replace_all(x$variable, "female", "women")
x$variable = str_replace_all(x$variable, "male", "men")


x$value = x$value / 100

gr2 = ggplot(data = x, aes(x = year_submitted, y = value)) +
    
    stat_interval( aes(color = variable, color_ramp = stat(level)), 
                   position = position_dodge(width=.5)) +
    
    facet_wrap(vars(level), nrow = 1) +
    
    scale_color_manual(values = c(
        "men"   = "#171717FF",
        "women" = "#D33C32FF"
    )) +
    
    scale_y_continuous(labels = scales::percent) +
    
    theme_minimal() +
    
    theme(
        legend.position = "bottom",
        legend.title = element_blank(),
        
        plot.title = element_markdown(margin = margin(b = 10)),
        plot.title.position = "plot",
        
        strip.text = element_text(face = "bold"),
        
        panel.grid = element_blank(),
        
        plot.background = element_rect(fill = "#fafaf5", color = "#fafaf5"),
        
        axis.line = element_line(color = "grey"),
        axis.ticks = element_line(color = "grey"),
        
        axis.title = element_blank()
    ) +
    
    labs(
        title = paste0(
            "Percentage of <b style='color:#171717FF'>men</b> and ",
            "<b style='color:#D33C32FF'>women</b> across hourly pay quarters"
        )
    )


library(patchwork)

gr1 / gr2 + 
    plot_layout(heights = c(2, 1)) +
    
    theme(
        plot.caption = element_markdown(margin = margin(t = 20)),
        plot.caption.position = "plot",
        
        plot.margin = margin(15, 15, 15, 15)
    ) +
    
    labs(
        caption = paste0(
            "Source: <b>gender-pay-gap.service.gov.uk</b>",
            " | ",
            "Graphic: <b>Nikolaos Pechlivanis</b>"
        )
    )


ggsave(
    "GenderGap.pdf", 
    width = 12, height = 10, units = "in"
)


ggsave(
    "GenderGap.png", 
    width = 12, height = 10, units = "in"
)





