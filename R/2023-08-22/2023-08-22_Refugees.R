


rm(list = ls())
gc()

library(data.table)
library(stringr)

library(ggplot2)
library(ggtext)

library(extrafont)

population <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-08-22/population.csv')

# population <- population[which(!(coo_iso %in% c("UNK", "GLP", "GUF", "MTQ", "TIB", "XXA")) & coa_iso != "UNK")]
population <- population[which(coa_iso != coo_iso)]

df = population[which(coo_iso != coa_iso & refugees != 0), by = .(year, coa_iso), .(
    "refugees"          = sum(refugees),
    "asylum_seekers"    = sum(asylum_seekers),
    "returned_refugees" = sum(returned_refugees)
)] |> setDF()

# library(giscoR)


# world = gisco_get_countries(resolution = "10")
# 
# world$highlight = ifelse(
#     world$ISO3_CODE %in% df$coa_iso, "yes", "no"
# )

# coo_countries = gisco_get_countries(country = population$coo_iso |> unique()) |> st_centroid() |> st_transform(crs = "+proj=adams_ws1")
# coa_countries = gisco_get_countries(country = population$coa_iso |> unique()) |> st_centroid() |> st_transform(crs = "+proj=adams_ws1")


# index = match(population$coo_iso, coo_countries$ISO3_CODE)
# population$coo_X = st_coordinates(coo_countries)[index, "X"]
# population$coo_Y = st_coordinates(coo_countries)[index, "Y"]
# 
# index = match(population$coa_iso, coa_countries$ISO3_CODE)
# population$coa_X = st_coordinates(coa_countries)[index, "X"]
# population$coa_Y = st_coordinates(coa_countries)[index, "Y"]
# 
# index = match(df$coa_iso, coa_countries$ISO3_CODE)
# df$coa_X = st_coordinates(coa_countries)[index, "X"]
# df$coa_Y = st_coordinates(coa_countries)[index, "Y"]

library(sf)
library(rnaturalearth)
library(rnaturalearthdata)

world <- ne_countries(scale = "large", returnclass = "sf")

df_world <- merge(world, df, by.x = "iso_a3", by.y = "coa_iso")

my_font = "Jost"



produce_graph <- function(x) {
    
    out = ggplot() +
        
        geom_sf(
            data = world, fill = "white", 
            linewidth = .1, color = "grey10"
        ) +
        
        geom_sf(
            data = df_world[which(df_world$year == x), ], 
            aes(fill = refugees), 
            linewidth = .1, color = "grey10"
        ) +
        
        # scale_fill_gradient2(
        #     low =  "grey95",
        #     mid = "#bf00f6",
        #     high = "#cf002e",
        #     midpoint = 2000000,
        #     na.value = "white",
        #     breaks = c(
        #         50000,
        #         500000,
        #         1000000,
        #         2000000, 
        #         3000000
        #     ),
        #     limits = c(5, 3758216),
        #     
        #     labels = scales::comma_format(suffix = "M", scale = 1e-6),
        #     guide = guide_colorbar(
        #         title = paste0("**Year ", x, "** - number of refugees"),
        #         title.position = "top",
        #         barwidth = unit(16, "lines"),
        #         barheight = unit(.35, "lines"),
        #         
        #         title.theme = element_markdown(size = 8, family = my_font),
        #         label.theme = element_text(size = 6, family = my_font)
        #     )
        # ) +
        
        scale_fill_gradientn(
            colors = c("grey96", "#007dfb", "#fbfb00", "#fb0000"), 
            na.value = "white",
            breaks = c(
                50000,
                500000,
                1000000,
                2000000, 
                3000000
            ),
            limits = c(5, 3758216),
            
            labels = scales::comma_format(suffix = "M", scale = 1e-6),
            guide = guide_colorbar(
                title = paste0("**Year ", x, "** - number of refugees"),
                title.position = "top",
                barwidth = unit(16, "lines"),
                barheight = unit(.35, "lines"),
                
                title.theme = element_markdown(size = 8, family = my_font),
                label.theme = element_text(size = 6, family = my_font)
            )
        ) +
        
        coord_sf(crs = "+proj=adams_ws1", expand = FALSE, 
                 ylim = c(-8000000, 10000000)) +
        
        theme_minimal(base_family = my_font) +
        
        theme(
            legend.position = "bottom",
            legend.justification = "left",
            
            axis.title = element_blank(),
            
            panel.spacing = unit(1, "lines"),
            panel.grid.minor = element_blank(),
            panel.grid.major = element_line(linewidth = .05, linetype = "dashed", color = "grey50"),
            panel.background = element_rect(fill = "#f6fcff", color = NA),
            
            plot.margin = margin(20, 20, 20, 20)
        ) 
    
    
    return(out)
    
}


gr = list()


for(i in df_world$year |> unique()) {
    
    gr[[ as.character(i) ]] = produce_graph(i)
    
    ggsave(
        plot = gr[[ as.character(i) ]], 
        filename = paste0("Rplot_Year", i, ".jpeg"),
        width = 12, height = 10, units = "in", dpi = 600
    )
    
}


library(patchwork)

my_font = "Jost"

multi = wrap_plots(
    gr$`2010`, gr$`2011`, gr$`2012`, gr$`2013`, 
    gr$`2014`, gr$`2015`, gr$`2016`, gr$`2017`, 
    gr$`2018`, gr$`2019`, gr$`2020`, gr$`2021`,
    gr$`2022`,
    
    ncol = 4
) + 
    
    plot_annotation(
        title = 'Refugees',
        
        subtitle = paste0(
            "Refugee intake from asylum-granting nations, spanning 2010 to 2022"
        ),
        
        caption = paste0(
            "Source: <b>PopulationStatistics {refugees} R package</b> | ",
            "Graphic: <b>Nikolaos Pechlivanis</b>"
        )
    ) &
    
    theme(
        plot.title    = element_text(face = "bold", size = 40, family = my_font),
        plot.subtitle = element_markdown(margin = margin(b = 15), size = 16, color = "grey20", family = my_font),
        plot.caption  = element_markdown(margin = margin(t = 15), size = 10, family = my_font),
        
        plot.title.position = "plot",
        plot.caption.position = "plot",
        
        plot.background = element_rect(fill = "#f5f5f5", color = NA),
        plot.margin = margin(20, 20, 20, 20)
    )


ggsave(
    plot = multi, filename = "Rplot.jpeg",
    width = 16, height = 16, units = "in"
)


# gr = ggplot(airquality, aes(Day, Temp)) +
#     geom_point(aes(colour = Month)) +
#     labs(title = 'Day: {frame_time}') +
#     transition_time(Day)





# df = population[which(coo_iso != coa_iso), by = .(year, coa_iso), .(
#     "refugees"          = sum(refugees),
#     "asylum_seekers"    = sum(asylum_seekers),
#     "returned_refugees" = sum(returned_refugees)
# )]
# 
# ggplot(data = df, aes(x = year, y = refugees)) +
#     
#     geom_line(aes(group = coa_iso, color = coa_iso)) +
#     
#     guides(
#         color = guide_legend(title.position = "top", ncol = 12)
#     ) +
#     
#     theme(
#         legend.position = "bottom"
#     )
