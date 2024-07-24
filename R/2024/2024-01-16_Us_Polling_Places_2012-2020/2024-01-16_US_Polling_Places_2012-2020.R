


rm(list = ls())
gc()

library(data.table)
library(stringr)

library(ggplot2)
library(ggtext)

library(ggforce)
library(ggdist)
library(ggh4x)
library(paletteer)
library(colorspace)

library(extrafont)

# library(sf)

polling_places <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-01-16/polling_places.csv')


df = polling_places[, by = .(election_date, state, county_name, jurisdiction_type), .N]

df$jurisdiction_type = df$jurisdiction_type |>
    str_replace_all("_", " ") |>
    str_to_title()

df$jurisdiction_type = ifelse(is.na(df$jurisdiction_type), "Not Available", df$jurisdiction_type) |>
    factor(levels = c("County", "County Municipality", "Municipality", "Parish", "City", "Borough", "Town", "Not Available"))

df$my_date = df$election_date |> as.character()

df = df[which(my_date %in% c("2012-11-06", "2014-11-04", "2016-11-08", "2018-11-06", "2020-11-03"))]

my_font = "Jost"

gr = ggplot(data = df, aes(y = my_date, x = N)) +
    
    geom_point(aes(fill = jurisdiction_type),
               position = position_jitternormal(sd_y = .05, sd_x = 0),
               shape = 21, color = "grey96", stroke = .15, size = 2) +
    
    stat_pointinterval(
        data = df[which(jurisdiction_type %in% c("County", "County Municipality", "Municipality", "Parish"))],
        aes(y = my_date, x = N, color = jurisdiction_type)
    ) +
    
    facet_wrap2(vars(jurisdiction_type), nrow = 2, axes = "x") +
    
    scale_fill_manual(values = paletteer_d("ggsci::hallmarks_light_cosmic") |> lighten(.35)) +
    scale_color_manual(values = paletteer_d("ggsci::hallmarks_light_cosmic") |> darken(.25)) +
    
    scale_x_continuous(trans = "log10", labels = scales::comma) +
    scale_y_discrete(
        breaks = c("2012-11-06", "2014-11-04", "2016-11-08", "2018-11-06", "2020-11-03"),
        labels = c("2012 Nov. 6", "2014 Nov. 4", "2016 Nov. 8", "2018 Nov. 6", "2020 Nov. 3")
    ) +
    
    theme_minimal(base_family = my_font) +
    
    theme(
        legend.position = "none",
        panel.spacing = unit(2, "lines"),
        
        axis.title.x = element_text(margin = margin(t = 10), hjust = 0),
        axis.title.y = element_text(margin = margin(r = 10), hjust = .5),
        
        axis.text.x = element_text(size = 8),
        axis.text.y = element_text(face = "bold"),
        
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(linetype = "dashed", linewidth = .35, color = "grey85"),
        
        strip.text = element_text(face = "bold"),
        
        plot.title    = element_text(face = "bold", size = 26, margin = margin(b = 5), family = "Jost"),
        plot.subtitle = element_markdown(margin = margin(b = 25), size = 10, color = "grey25", family = "Jost"),
        plot.caption  = element_markdown(margin = margin(t = 25), size = 8, family = "Jost"),
        
        plot.title.position = "plot",
        plot.caption.position = "plot",
        
        plot.background = element_rect(fill = "#f9fbfe", color = NA),
        plot.margin = margin(20, 20, 20, 20)
    ) +
    
    labs(
        y = "Election date",
        x = "Number of polling places",
        
        title = 'US Polling Places 2012-2020',
        
        subtitle = paste0(
            "The count of polling locations designated for counties in the United States ",
            "categorized by jurisdiction types,<br>including ",
            "<b style='color:#2E2A2BFF'>County</b>, ",
            "<b style='color:#CF4E9CFF'>County Municipality</b>, ",
            "<b style='color:#8C57A2FF'>Municipality</b>, ",
            "<b style='color:#358DB9FF'>Parish</b>, ",
            "<b style='color:#82581FFF'>City</b>, ",
            "<b style='color:#2F509EFF'>Borough</b>, ",
            "<b style='color:#E5614CFF'>Town</b>",
            "<br>and those labeled as ",
            "<b style='color:#97A1A7FF'>Not Available</b>."
        ),
        
        caption = paste0(
            "Source: <b>The Center for Public Integrity</b> | ",
            "Graphic: <b>Nikolaos Pechlivanis</b>"
        )
    )

ggsave(
    plot = gr, filename = "Rplot.jpeg",
    width = 10, height = 11, units = "in", dpi = 600
)

# devtools::install_github("UrbanInstitute/urbnmapr")
# counties_sf <- urbnmapr::get_urbn_map("counties", sf = TRUE) |> st_crs()
# 
# ggplot() +
#     
#     geom_sf(data = counties_sf) 
