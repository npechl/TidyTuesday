
rm(list = ls())
gc()


library(data.table)
library(stringr)


us_place_names   <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-06-27/us_place_names.csv')
us_place_history <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-06-27/us_place_history.csv')


us_place_names |> head()
us_place_history |> head()

us_place_names = us_place_names[which( !is.na(prim_long_dec) & !is.na(prim_lat_dec) )]

df2 = us_place_names[, c("prim_long_dec", "prim_lat_dec"), with = FALSE]

# us_place_names$`No. of neighbors` = mapply(function(x, y) {
#     
#     neighbors = sqrt(
#         (x - df2$prim_long_dec) ^2 + (y - df2$prim_lat_dec)^2
#     )
#     
#     neighbors = neighbors[which(neighbors <= summary(neighbors)[2])] |> 
#         length() - 1
#     
#     return(neighbors)
#     
# }, df2$prim_long_dec, df2$prim_lat_dec)


de = MASS::kde2d(df2$prim_long_dec, df2$prim_lat_dec, n = 25)


library(ggplot2)

library(sf)


usa   <- maps::map("world", fill=TRUE, plot =FALSE) |> st_as_sf()
lakes <- maps::map("lakes", fill=TRUE, plot =FALSE) |> st_as_sf()

df = us_place_names[which( !is.na(prim_long_dec) & !is.na(prim_lat_dec) )] |>
    st_as_sf(coords = c("prim_long_dec", "prim_lat_dec"), crs=4326) |>
    st_transform(crs = st_crs("+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs")) |>
    st_coordinates() |>
    as.data.frame() |>
    setDT()

library(ggtext)
library(extrafont)

my_font = "Raleway"

gr = ggplot() +
    
    geom_sf(data = usa, fill = "grey95", color = "grey20", linewidth = .25) +
    geom_sf(data = lakes, fill = "#85b6ce", alpha = .75, color = "grey20", linewidth = .25) +
    geom_sf(data = usa, fill = NA, color = "grey20", linewidth = .25) +
    
    geom_point(
        data = df, aes(X, Y), 
        fill = alpha("#873260", alpha = .5), 
        color = alpha("#873260"),
        size = .5, stroke = 0, shape = 21
    ) +
    
    coord_sf(
        crs = st_crs("+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs"),
        xlim = c(-2500000, 2500000), ylim = c(-2300000,  730000)
    ) +
    
    theme_minimal(base_family = my_font) +
    
    theme(
        axis.title.x = element_blank(),
        axis.title.y = element_markdown(margin = margin(r = 15), size = 16, hjust = 0),
        axis.text = element_blank(),
        
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = "grey75", linewidth = .25),
        
        # plot.title    = element_text(face = "bold", size = 18, family = my_font, margin = margin(b = 20)),
        plot.caption  = element_markdown(margin = margin(t = 15), size = 4, family = my_font),
        
        plot.background = element_rect(fill = "#def3f6", color = NA)
    ) +
    
    labs(
        y = '*Populated Areas in the United States*',
        
        caption = paste0(
            "Source: <b>National Map Staged Products Directory from the US Board of Geographic Names</b> | ",
            "Graphic: <b>Nikolaos Pechlivanis</b>"
        )
    )

ggsave(
    plot = gr, filename = "Rplot.jpeg",
    width = 10, height = 6, units = "in", dpi = 600
)






