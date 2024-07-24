


rm(list = ls())
gc()


library(data.table)
library(stringr)

library(ggplot2)

library(ggh4x)
library(ggsci)
library(ggridges)
library(ggtext)

library(showtext)

cats_uk           <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-01-31/cats_uk.csv')
cats_uk_reference <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-01-31/cats_uk_reference.csv')


df = merge(cats_uk, cats_uk_reference, by = "tag_id", all.x = TRUE)

rm(cats_uk, cats_uk_reference)
gc()

df$event_id     = NULL
df$study_name   = NULL
df$animal_taxon = NULL
df$study_site = NULL


df = df[which(!is.na(df$food_dry))]
df = df[which(!is.na(df$food_wet))]
df = df[which(!is.na(df$food_other))]

df$food_dry   = ifelse(df$food_dry, "dry", "")
df$food_wet   = ifelse(df$food_wet, "wet", "")
df$food_other = ifelse(df$food_other, "other", "")

df$food = paste0(df$food_dry, " ", df$food_wet, " ", df$food_other)
df$food = str_squish(df$food)

df$animal_sex = ifelse(
    df$animal_sex == "f", "Female", "Male"
)

df2 = df[, by = .(animal_id), .(
    mean_ground_speed = mean(ground_speed)
)]

df2 = df2[order(df2$mean_ground_speed), ]

df$animal_id = factor(
    df$animal_id,
    levels = df2$animal_id
)

font_add_google("Mulish", "Mulish")

showtext_auto()

my_font = "Mulish"

gr = ggplot(data = df[which(ground_speed != 0),], 
       aes(y = animal_id, x = ground_speed, fill = food)) +
    
    geom_density_ridges(
        size = .15, color = "white"
    ) +
    
    scale_x_continuous(
        trans = "log10", expand = c(0, 0),
        labels = scales::comma_format(suffix = "k", scale = 1e-3)
    ) +
    
    scale_y_discrete(expand = c(0, 0)) +

    scale_fill_futurama(alpha = .95) +
    
    facet_wrap(vars(animal_sex), scales = "free_y") +
    
    coord_cartesian(clip = "off") +
    
    theme_minimal(base_family = my_font) +
    
    theme(
        legend.position = "bottom",
        legend.justification = "left",
        
        axis.title.x = element_text(margin = margin(t = 10), color = "white"),
        axis.title.y = element_blank(),
        
        axis.text.x = element_text(size = 8, color = "white"),
        axis.text.y = element_text(face = "bold", color = "white"),
        
        panel.grid.minor = element_blank(),
        
        panel.grid.major.x = element_line(linewidth = .3, linetype = "dashed", color = "grey75"),
        panel.grid.major.y = element_blank(),
        
        axis.ticks.x = element_line(linewidth = .3, color = "white"),
        
        panel.spacing = unit(1, "lines"),
        
        strip.text = element_text(face = "bold", margin = margin(b = 10), 
                                  color = "white", size = 12),
        
        plot.title.position = "plot",
        plot.caption.position = "plot",
        
        plot.title    = element_text(face = "bold", size = 32, color = "white"),
        plot.subtitle = element_markdown(margin = margin(b = 20), face = "bold", size = 10, color = "white"),
        plot.caption  = element_markdown(margin = margin(t = 20), size = 6, color = "white"),
        
        plot.background = element_rect(fill = "#3F4041FF", color = NA),
        
        plot.margin = margin(20, 20, 20, 20)
    ) +
    
    labs(
        x = "Ground Speed",
        
        title = 'Pet Cats UK',
        
        subtitle = paste0(
            "Ground Speed distributions for every animal provided. ",
            "The chart is divided into male and<br>female cats with the color ",
            "indicating the type of food that every cat is fed"
        ),
        
        caption = paste0(
            "Source: <b>Movebank for Animal Tracking Data</b> | ",
            "Graphic: <b>Nikolaos Pechlivanis</b>"
        )
    ) +
    
    guides(
        fill = guide_legend(
            title = "What kind of food was the cat fed?",
            title.position = "top",
            
            override.aes = list(color = NA),
            
            title.theme = element_text(color = "white", family = my_font, size = 10),
            label.theme = element_text(color = "white", family = my_font, size = 8)
        )
    )


ggsave(
    plot = gr, filename = "Rplot.pdf", device = cairo_pdf,
    width = 10, height = 12, units = "in"
)




library(magick)


tmp = image_read_pdf("Rplot.pdf", density = 300)
image_write(tmp, "Rplot.jpeg")




