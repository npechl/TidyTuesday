


rm(list = ls())
gc()


library(data.table)
library(stringr)

library(ggplot2)
library(ggtext)
library(maps)

library(extrafont)

tuesdata <- tidytuesdayR::tt_load('2022-11-08')

state_stations = tuesdata$state_stations
station_info   = tuesdata$station_info


rm(tuesdata)

state_stations = setDT(state_stations)
station_info = setDT(station_info)




state_stations = state_stations[, c("freq", "V1", "V2") := tstrsplit(frequency, " ", fixed = TRUE)][]

index = match(state_stations$call_sign, station_info$call_sign)

state_stations$service = station_info[index, ]$service

state_stations[which(is.na(state_stations$V1)), ]$V1 = state_stations[which(is.na(state_stations$V1)), ]$service

state_stations = state_stations[which(!is.na(state_stations$V1))]

state_stations[which(!is.na(state_stations$V2))]$V1 = state_stations[which(!is.na(state_stations$V2))]$V2
state_stations$V2 = NULL


state_stations$freq  = as.numeric(state_stations$freq)
state_stations$state = str_to_lower(state_stations$state)
state_stations$state = str_replace_all(state_stations$state, "_", " ")

state.fips$polyname = str_split(state.fips$polyname, "\\:", simplify = TRUE)[, 1]
state.fips = unique(state.fips[, c("abb", "polyname")])


index = match(state_stations$state, state.fips$polyname)
state_stations$abb = state.fips[index, ]$abb

state_stations = state_stations[which(!is.na(abb))]

state_stations$name = paste(str_to_lower(state_stations$city), str_to_lower(state_stations$abb))


df = fread("uscities.csv")

df$name = paste(str_to_lower(df$city), str_to_lower(df$state_id))

df = merge(state_stations, df, by.x = "name", by.y = "name", all.x = TRUE)

df = df[which(!is.na(df$lat)), ]

usa.county <- map_data("county")
usa.state <- map_data("state")

df$format = str_to_title(df$format)

df$modulation = ifelse(
    df$V1 == "AM", 
    "Amplitude Modulation (*AM*)",
    "Frequency Modulation (*FM*)"
)

df$modulation = factor(
    df$modulation,
    levels = c("Frequency Modulation (*FM*)", "Amplitude Modulation (*AM*)")
)

gr = ggplot() +
    
    # geom_polygon(
    #     data = usa.county, 
    #     aes(x = long, y = lat, group = group), 
    #     color = "gray75", linewidth = .01,
    #     fill = "grey", alpha = 0.3
    # ) +
    
    geom_polygon(
        data = usa.state,
        aes(x = long, y = lat, group = group),
        color = "gray95", linewidth = .1,
        fill = "#2c656b", alpha = 0.3
    ) +
    
    geom_point(data = df, 
               aes(x = lng, y = lat),
               alpha = .3, shape = 21,
               color = "#990000", fill = "#990000") +
    
    facet_wrap(vars(modulation), ncol = 1) +
    
    theme_void(base_family = "Comfortaa") +
    
    coord_map() +
    
    theme(
        
        strip.text = element_markdown(face = "bold", color = "black", family = "Comfortaa SemiBold"),
        
        plot.title.position = "plot",
        plot.caption.position = "plot",
        
        plot.title = element_text(family = "Zilla Slab", size = 20, face = "bold"),
        plot.subtitle = element_text(margin = margin(b = 15, t = 5), size = 9),
        plot.caption = element_markdown(margin = margin(t = 10), size = 7),
        
        plot.background = element_rect(fill = "#f5f5fa", color = "#f5f5fa"),
        plot.margin = margin(20, 20, 20, 20)
    ) +
    
    labs(
        title = 'Radio Stations',
        subtitle = 'Geography of public radio stations in the United States',
        caption = paste0(
            "Source: **Wikipedia** | ",
            "Graphic: **Nikolaos Pechlivanis**"
        )
    )


ggsave(
    plot = gr,
    filename = "Rplot.pdf",
    width = 8, height = 10,
    device = cairo_pdf,
    units = "in"
)

library(magick)

tmp = image_read_pdf("Rplot.pdf", density = 500)
image_write(tmp, "Rplot.png")







