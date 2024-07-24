


rm(list = ls())
gc()

library(data.table)
library(stringr)

library(ggplot2)
library(ggsci)
library(ggtext)
library(ggh4x)

library(extrafont)


df <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-10-24/patient_risk_profiles.csv')

df$personId |> duplicated() |> any()

# part 1 ---------------------------------------

x1 = df[, df |> colnames() |> str_which("age\\ group|personId"), with = FALSE] |> 
    melt(
        id.vars = "personId", variable.name = "Age group",
        variable.factor = FALSE, value.factor = FALSE
    )

x1 = x1[which(value != 0)]

x1$`Age group` = x1$`Age group` |> 
    str_remove_all("age|group|\\:") |> 
    str_squish()

x1 = x1[order(personId)]
x1$value = NULL


# part 2 ---------------------------------------

x2 = df[, df |> colnames() |> str_which("Sex|personId"), with = FALSE] |> 
    melt(
        id.vars = "personId", variable.name = "sex",
        variable.factor = FALSE, value.factor = FALSE
    )

x2 = x2[which(value != 0)]

x2$sex = x2$sex |> 
    str_remove_all("Sex|=") |> 
    str_squish() |>
    str_to_lower()

x2 = x2[order(personId)]
x2$value = NULL


# part 3 ---------------------------------------

x3 = df[, df |> colnames() |> str_which("Antibiotics|personId"), with = FALSE] |> 
    melt(
        id.vars = "personId", variable.name = "Antibiotics",
        variable.factor = FALSE, value.factor = FALSE
    )

x3 = x3[which(value != 0)]

x3$Antibiotics = x3$Antibiotics |> 
    str_remove_all("Antibiotics|in|prior|year") |> 
    str_squish()

x3 = x3[order(personId)]
x3$value = NULL

# part 4 ---------------------------------------

x4 = df[, df |> colnames() |> str_which("predicted\\ risk|personId"), with = FALSE] |> 
    melt(
        id.vars = "personId", variable.name = "predicted_risk",
        variable.factor = FALSE, value.factor = FALSE
    )

x4$predicted_risk = x4$predicted_risk |> 
    str_remove_all("predicted|risk|of") |> 
    str_squish()

x4 = x4[order(personId, -value)]

# part 5 ---------------------------------------

x5 = df[, df |> colnames() |> str_which("predicted\\ risk|Antibiotics|Sex|age\\ group", negate = TRUE), with = FALSE] |> 
    melt(
        id.vars = "personId",
        variable.factor = FALSE, value.factor = FALSE
    )

x5 = x5[which(value != 0)]

x5$variable = x5$variable |> 
    str_remove_all("in|prior|year") |> 
    str_squish() |>
    str_to_lower()

x5 = x5[order(personId)]
x5$value = NULL








# merge ----------------

df = x1 |>
    merge(x2, all = TRUE, by = "personId") |>
    # merge(x3, all = TRUE, by = "personId") |>
    # merge(x5, all = TRUE, by = "personId", allow.cartesian = TRUE) |>
    merge(x4, all = TRUE, by = "personId", allow.cartesian = TRUE)

df = df[str_order(`Age group`, numeric = TRUE)]

df$`Age group` = df$`Age group` |> factor(levels = df$`Age group` |> unique())
    

df$predicted_risk = str_split(df$predicted_risk, "\\,", simplify = TRUE)[, 1]

my_font = "Jost"




library(ggdist)
library(distributional)

gr = ggplot(data = df, aes(y = `Age group`, x = value)) +
    
    stat_interval(aes(color = sex, color_ramp = after_stat(level)),
                  position = position_dodge(width = .85), linewidth = 1.5) +

    # stat_pointinterval(aes(color = sex),
    #                    position = position_dodge(width = .75), linewidth = 1, size = .5) +
    
    scale_x_continuous(trans = "log10", labels = scales::scientific) +
    
    scale_color_manual(
        values = c(
            "female" = "#CC0C00",
            "male"   = "#5C88DA"
        ),
        
        guide = guide_legend(
            title.position = "top", 
            title.theme = element_text(size = 12, hjust = 1, family = my_font),
            label.theme = element_text(size = 11, family = my_font)
        )
    ) +
    
    scale_color_ramp_discrete(
        guide = guide_legend(
            title.position = "top", 
            title.theme = element_text(size = 12, hjust = 1, family = my_font),
            label.theme = element_text(size = 11, family = my_font)
        )
    ) +
    
    facet_wrap2(vars(predicted_risk), axes = "all") +
    
    theme_minimal(base_family = my_font) +
    
    theme(
        legend.position = "bottom",
        legend.justification = "right",
        
        strip.text = element_text(face = "bold", size = 12),
        
        axis.title.x = element_text(margin = margin(t = 15), size = 12, hjust = 0),
        axis.title.y = element_text(margin = margin(r = 10), size = 12),
        
        axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8),
        
        panel.spacing = unit(1, "lines"),
        
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_line(linewidth = .3, linetype = "dashed", color = "grey85"),
        panel.grid.major.x = element_line(linewidth = .3, linetype = "dashed", color = "grey75"),
        
        plot.title    = element_text(face = "bold", size = 36, margin = margin(b = 5), family = my_font),
        plot.subtitle = element_markdown(margin = margin(b = 20), size = 12, color = "grey20", family = my_font),
        plot.caption  = element_markdown(margin = margin(t = 20), size = 10, family = my_font),
        
        plot.title.position = "plot",
        plot.caption.position = "plot",
        
        plot.background = element_rect(fill = "#f5f5f5", color = NA),
        plot.margin = margin(20, 20, 20, 20)
    ) +
    
    labs(
        title = 'Patient Risk Profiles',
        
        subtitle = paste0(
            "*This dataset contains 100 simulated patient's medical history ",
            "features and the predicted 1-year risk of 14 outcomes based on ",
            "each patient's medical history features.<br>The predictions used ",
            "real logistic regression models developed on a large real ",
            "world healthcare dataset.*"
        ),
        
        x = "Probability of the predicted 1-year risk of the corresponing profile",
        
        
        
        caption = paste0(
            "Source: <b>R/Pharma Conference</b> | ",
            "Graphic: <b>Nikolaos Pechlivanis</b>"
        )
    )



ggsave(
    plot = gr, filename = "Rplot.jpeg",
    width = 14, height = 14, units = "in", dpi = 600
)




