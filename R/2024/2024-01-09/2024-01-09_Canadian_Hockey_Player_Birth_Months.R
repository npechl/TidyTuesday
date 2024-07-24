


rm(list = ls())
gc()

library(data.table)
library(stringr)

library(ggplot2)
library(ggtext)

library(extrafont)

canada_births_1991_2022 <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-01-09/canada_births_1991_2022.csv')
nhl_player_births       <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-01-09/nhl_player_births.csv')
nhl_rosters             <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-01-09/nhl_rosters.csv')
nhl_teams               <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-01-09/nhl_teams.csv')


# plot_A ----------------------------------

df = nhl_rosters[, by = birth_country, .N]
df = df[order(N)]

country_codes =  countrycode::codelist

index = match(df$birth_country, country_codes$iso3c)

df$birth_country_name = country_codes$country.name.en[index] |> str_to_upper()

df$birth_country_name = df$birth_country_name |> factor(levels = df$birth_country_name)

gr1 <- ggplot(data = df, aes(N, birth_country_name)) +
    
    geom_col(aes(fill = N)) +
    
    geom_text(
        aes(label = scales::comma(N)),
        hjust = 1, vjust = .5, position = position_nudge(x = -.05),
        color = "white", size = 3, fontface = "bold", family = "Bebas Neue"
    ) +
    
    scale_fill_gradient(high = "#B81840", low = "#33608C") +
    scale_x_continuous(trans = "log10", expand = c(0, 0)) +
    
    coord_cartesian(clip = "off") +
    
    theme_minimal(base_family = "Bebas Neue") +
    
    theme(
        legend.position = "none",
        
        axis.title = element_blank(),
        
        axis.text.x = element_blank(),
        axis.text.y = element_text(face = "bold", size = 8),
        
        panel.grid = element_blank(),
        
        plot.title = element_text(face = "bold", size = 9, margin = margin(b = 5), family = "Bebas Neue"),
        
        # plot.title.position = "plot",
        
        plot.margin = margin(20, 20, 20, 20)
    ) +
    
    labs(title = "Where are Canada's hockey players born?")


# plot_B ----------------------------------

canada_players = nhl_rosters[which(birth_country == "CAN")]
df = canada_players[, by = .(birth_state_province), .N]

df$birth_state_province = df$birth_state_province |> 
    str_remove_all("Territory") |> 
    str_squish()
    

library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(rnaturalearthhires)

library(paletteer)
library(ggrepel)

world <- ne_countries(scale = "large", returnclass = "sf") |>
    st_transform(crs = "+proj=adams_ws1")

canada <- ne_states(country = "Canada", returnclass = "sf") |>
    st_transform(crs = "+proj=adams_ws1")

lakes <- ne_download(scale = "large", type = 'lakes', category = 'physical', returnclass = "sf") |>
    st_transform(crs = "+proj=adams_ws1")


index = match(canada$name_en, df$birth_state_province)

canada$N = df[index]$N

canada_centroids = canada |> 
    st_centroid()

canada_centroids = canada_centroids[which(!is.na(canada_centroids$N)), ]

gr2 = ggplot() +

    geom_sf(
        data = world, fill = "grey96",
        linewidth = .1, color = "grey50"
    ) +

    geom_sf(
        data = lakes, fill = "#eafdff",
        linewidth = .1, color = "grey50"
    ) +

    geom_sf(
        data = canada, aes(fill = N),
        linewidth = .1, color = "grey50"
    ) +
    
    geom_text_repel(
        data = canada_centroids, 
        aes(label = scales::comma(N), geometry = geometry),
        stat = "sf_coordinates", min.segment.length = 0, segment.size = .25,
        size = 4, fontface = "bold", family = "Bebas Neue", box.padding = .5
    ) +

    coord_sf(
        xlim = c(-9000000, -2000000),
        ylim = c(2000000, 9000000)
    ) +
    
    scale_fill_gradient(low = "#e7dcf3", high = "#e52b50") +

    theme_minimal() +

    theme(
        legend.position = "none",
        
        axis.title = element_blank(),
        
        plot.title    = element_text(face = "bold", size = 26, margin = margin(b = 5), family = "Jost"),
        plot.subtitle = element_markdown(margin = margin(b = 25), size = 10, color = "grey25", family = "Jost"),
        plot.caption  = element_markdown(margin = margin(t = 25), size = 8, family = "Jost"),
        
        plot.title.position = "plot",
        plot.caption.position = "plot",
        
        
        plot.background = element_rect(fill = "#f9fbfe", color = NA),
        plot.margin = margin(20, 20, 20, 20)
    ) +
    
    labs(
        title = 'Canadian Hockey Player Birth Months',
        
        subtitle = paste0(
            "**National Distribution of Canada's Hockey Players**: Canada Takes the Lead ",
            "with 36,405 Players (16,378 from Ontario),<br>followed by the United ",
            "States (8,906) and Sweden (2,423)."
        ),
        
        caption = paste0(
            "Source: <b>Statistics Canada, NHL team list endpoint & NHL API</b> | ",
            "Graphic: <b>Nikolaos Pechlivanis</b>"
        )
    )



# patchwork -------------------

library(patchwork)

multi = gr2 + inset_element(gr1, 0, 0.2, 0.4, 1, align_to = "plot", clip = FALSE)


ggsave(
    plot = multi, filename = "Rplot.pdf", device = cairo_pdf,
    width = 9, height = 11, units = "in"
)

library(magick)


tmp = image_read_pdf("Rplot.pdf", density = 600)
image_write(tmp, "Rplot.jpeg")

# ggsave(
#     plot = multi, filename = "Rplot.jpeg", # device = cairo_pdf,
#     width = 10, height = 10, units = "in", dpi = 600
# )


# test <- function(q, deposit = 100) {
# 
#     annual_deposit = deposit * 12
# 
#     if(q == 1) {
# 
#         return(annual_deposit + annual_deposit * 1e-04)
# 
#     } else {
# 
#         new_balance <- test(q - 1, deposit = deposit) + annual_deposit
# 
#         return(new_balance + new_balance * 1e-04)
# 
#     }
# 
# }





