


rm(list = ls())
gc()


library(data.table)
library(stringr)

library(ggplot2)
library(ggforce)
library(ggtext)
library(ggsci)

library(extrafont)

afrisenti          <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-02-28/afrisenti.csv')
languages          <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-02-28/languages.csv')
language_scripts   <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-02-28/language_scripts.csv')
language_countries <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-02-28/language_countries.csv')
country_regions    <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-02-28/country_regions.csv')

language_countries = language_countries[-20]

afrisenti$tweet = NULL

country_regions = country_regions[-c(8, 10)]



df = merge(afrisenti, languages, by = "language_iso_code", all.x = TRUE)

language_countries = merge(language_countries, country_regions, by = "country", all.x = TRUE)


language_scripts = language_scripts[, by = language_iso_code, .(scripts = paste(sort(script), collapse = ", "))]

df = merge(df, language_scripts, by = "language_iso_code", all.x = TRUE)

language_countries = language_countries[, by = language_iso_code, .(
    country = paste(sort(unique(country)), collapse = ", "),
    region  = paste(sort(unique(region)), collapse = ", ")
)]

language_countries$region = str_replace_all(language_countries$region, "Northern Africa", "North Africa")
language_countries[c(4, 9)]$region = "Southeastern Africa"

df = merge(df, language_countries, by = "language_iso_code", all.x = TRUE)

rm(language_scripts, languages, country_regions, afrisenti, language_countries)



df = df[, by = .(language_iso_code, language, scripts, country, region, label), .(N = .N)]
df = df[, by = .(language_iso_code, language, scripts, country, region), Freq := N / sum(N)]


df$name = paste0(
    "<span style='font-size:11pt; color:grey20'>**", df$language, "**</span><br>",
    "<span style='color:grey30'>", df$country, "</span>"
)


positives = df[which(label == "positive")]

positives = positives[order(Freq), ]

df$name = factor(
    df$name, levels = unique(positives$name)
)

df$region = factor(
    df$region, levels = c("North Africa", "West Africa", "East Africa", "Southeastern Africa")
)


gr = ggplot(data = df, aes(x = N, y = name, fill = label)) +
    
    geom_col(position = "fill") +
    
    scale_x_continuous(expand = c(0, 0), labels = scales::percent) +
    
    scale_fill_npg() +
    
    facet_col(
        vars(region),
        scales = "free",
        space = "free"
    ) +
    
    theme_minimal(base_family = "Sitka Text") +
    
    theme(
        legend.title = element_blank(),
        
        axis.title = element_blank(),
        
        axis.text.y = element_markdown(),
        axis.text.x = element_text(size = 8),
        
        panel.spacing = unit(1, "lines"),
        
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        
        panel.grid.major.x = element_line(linewidth = .3, linetype = "dashed", color = "grey50"),
        
        strip.text = element_text(face = "bold", hjust = 0, color = "grey10", size = 12),
        
        plot.title.position = "plot",
        plot.caption.position = "plot",
        
        plot.title    = element_text(face = "bold", size = 26),
        plot.subtitle = element_markdown(margin = margin(b = 20), size = 10),
        plot.caption  = element_markdown(margin = margin(t = 20), size = 6),
        
        plot.background = element_rect(fill = "grey98", color = NA),
        
        plot.margin = margin(20, 20, 20, 20)
    ) +
    
    labs(
        
        title = 'African Language Sentiment',
        
        subtitle = paste0(
            "Sentiment Analysis for 14 African Languages"
        ),
        
        caption = paste0(
            "Source: <b>AfriSenti: Sentiment Analysis dataset for 14 African languages</b> | ",
            "Graphic: <b>Nikolaos Pechlivanis</b>"
        )
    )


ggsave(
    plot = gr, filename = "Rplot.pdf", device = cairo_pdf,
    width = 9, height = 11, units = "in"
)


library(magick)

tmp = image_read_pdf("Rplot.pdf", density = 500)
image_write(tmp, "Rplot.jpeg")

