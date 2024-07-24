



rm(list = ls())
gc()



library(data.table)
library(stringr)
library(ggplot2)
library(maps)
library(mapdata)
library(sf)
library(od)

library(ggforce)
library(extrafont)

pell      = fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-08-30/pell.csv')
state_abb = fread("state_abb.csv")


pell$NAME = str_remove_all(pell$NAME, '"\"')
pell$NAME = str_remove_all(pell$NAME, "\\(|the|\\)")
pell$NAME = str_replace_all(pell$NAME, "-", " - ")
pell$NAME = str_squish(pell$NAME)




index      = match(pell$STATE, state_abb$abb)
pell$STATE = state_abb[index, ]$State
pell$STATE = str_to_title(pell$STATE)
pell$STATE = str_squish(pell$STATE)


states            = st_as_sf(map('state', plot = FALSE, fill = TRUE))
row.names(states) = states$ID

nc_point = st_geometry(states)
nc_point = st_point_on_surface(nc_point)

states_cendroids = as.data.frame(sfc_point_to_matrix(nc_point))
row.names(states_cendroids) = row.names(states)

states_cendroids = setDT(states_cendroids, keep.rownames = "State")

colnames(states_cendroids) = c("State", "lon", "lat")

states_cendroids$State = str_to_title(states_cendroids$State)

index = match(pell$STATE, states_cendroids$State)

pell = cbind(pell, states_cendroids[index, 2:3])

pell = pell[which(!is.na(pell$lon)), ]



states = map_data("state")

states_all = list()

for(i in unique(pell$YEAR)) {
    
    states_all[[i]] = states
    
}

states_all = rbindlist(states_all, idcol = "YEAR")

states_all = states_all[which(YEAR >= 2000), ]
pell = pell[which(YEAR >= 2000), ]

pell$ratio = pell$AWARD / pell$RECIPIENT

pell = pell[order(YEAR, -ratio), ]

sub = pell[, by = YEAR, head(.SD, 10)]

gr1 = ggplot() + 
    
    geom_polygon(data = states, 
                 aes(x = long, y = lat, group = group),
                 color = "gray70", fill = "gray90",
                 size = 0.1) + 
    
    geom_point(data = sub,
               aes(x = lon, y = lat, size = ratio),
               position = position_jitternormal(sd_x = 0.25, sd_y = 0.25),
               shape = 21, stroke = 0.25,
               color = "#0b5ea5", fill = "#a50b5e",
               alpha = 0.75) +
    
    
    scale_size_binned(
        range = c(1, 4),
        
        labels = scales::dollar_format(prefix = "$",
                                       suffix = "k", 
                                       scale = 1e-3),
    ) +
    
    facet_wrap(vars(YEAR), ncol = 6) +
    
    theme_void() + 
    
    theme(
        legend.position = "bottom",
        legend.title = element_blank(),
        strip.text = element_text(face = "bold", family = "Malgun Gothic",
                                  margin = margin(b = 5)),
        
        plot.title.position = "plot",
        
        plot.title = element_text(family = "Malgun Gothic", face = "bold", color = "gray10", size = 12),
        
        plot.subtitle = element_text(
            margin = margin(b = 10, t = 5),
            size = 10,
            family = "Malgun Gothic",
            color = "gray40"
        )
    ) +
    
    guides(
        size = guide_legend(title.position = "top")
    ) +
    
    labs(
        title = "Top 10 Pell awards from 2000 to 2017 across the United States",
        subtitle = "The plot depicts a ratio of award amount divided by the total number of recipients for every state per year."
    )
    
    coord_map()


# ggsave(
#     "test.pdf",
#     width = 12, height = 6,
#     device = cairo_pdf,
#     units = "in"
# )



# sub2 = pell[, by = .(YEAR, STATE), .(
#     AWARD = sum(AWARD),
#     RECIPIENT = sum(RECIPIENT)
# )]
# 
# sub2$ratio = sub2$AWARD / sub2$RECIPIENT
# 
# sub2 = sub2[which(!is.na(sub2$ratio))]
# 
# library(geomtextpath)
# library(ggsci)
# library(pals)
# 
# my_colors = coolwarm(n = length(unique(sub2$STATE)))
# 
# 
# sub2 = sub2[order(YEAR, ratio), ]
# 
# names(my_colors) = sub2[which(YEAR == 2017), ]$STATE
# 
# sub2$top = ifelse(
#     sub2$STATE %in% c("Mississippi", "New York"),
#     "sign", "notsign" 
# )
# 
# 
# gr2 = ggplot(data = sub2, aes(x = YEAR, y = ratio, group = STATE)) +
#     
#     geom_textline(aes(label = STATE, color = top), 
#                   hjust = "auto", size = 3, linewidth = 0.5,
#                   text_smoothing = 50) +
#     
#     scale_x_continuous(expand = c(0, 0)) +
#     
#     scale_y_continuous(
#         expand = c(0, 0),
#         labels = scales::dollar_format(
#             prefix = "$ ",
#             suffix = "k", 
#             scale = 1e-3
#         )
#     ) +
#     
#     scale_color_manual(
#         values = c(
#             "sign" = "#32127a",
#             "notsign" = "gray80"
#         )
#     ) +
#     
#     coord_cartesian(expand = TRUE, clip = "off") +
#     
#     theme_minimal() +
#     
#     theme(
#         legend.position = "none",
#         
#         panel.grid.major.x = element_blank(),
#         panel.grid.minor.x = element_blank(),
#         panel.grid.minor.y = element_blank(),
#         
#         panel.grid.major.y = element_line(
#             color = "gray90", size = 0.25, linetype = "dashed"
#         ),
#         
#         axis.line.x = element_line(color = "grey"),
#         axis.ticks.x = element_line(color = "grey"),
#         axis.ticks.y = element_line(color = "gray90", size = 0.25, linetype = "dashed"),
#         
#         axis.title = element_blank()
#     )



sub3 = pell[, by = YEAR, head(.SD, 1)]

sub3$NAME = str_remove_all(sub3$NAME, "-|Foundation")
sub3$NAME = str_squish(sub3$NAME)

sub3[which(sub3$NAME == "California University of Management and"), ]$NAME = "California University of Management"

library(ggrepel)
library(ggtext)

index = str_split(sub3$NAME, " ")

index = lapply(index, function(x) {
    
    if(length(x) < 3) {
        
        return(paste(x, collapse = " "))
        
    }
    
    f = floor(length(x) / 3)
    c = ceiling(length(x) / 3)
    
    paste0(
        paste(x[1:c], collapse = " "), "<br>",
        paste(x[(c + 1):(c + f)], collapse = " "), "<br>",
        paste(x[(c + f + 1):length(x)], collapse = " ")
    )
    
})

sub3$NAME2 = unlist(index)


sub3$label = paste0(
    "**", sub3$STATE, "**<br>",
    sub3$NAME2
)

gr3 = ggplot(data = sub3, aes(x = YEAR, y = ratio)) +
    
    # geom_area(fill = "gray95") +
    
    geom_line(color = "#0b5ea5") +
    
    geom_point(color = "#0b5ea5") +
    
    geom_richtext(aes(label = label), 
                  family = "Malgun Gothic",
                  size = 2.5, fill = NA, label.color = NA) +
    
    scale_x_continuous(expand = c(0, 0)) +
    
    scale_y_continuous(
        # expand = c(0, 0),
        labels = scales::dollar_format(
            prefix = "$ ",
            suffix = "k", 
            scale = 1e-3
        )
    ) +
    
    coord_cartesian(expand = TRUE, clip = "off") +
    
    theme_minimal(base_family = "Malgun Gothic") +
    
    theme(
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        
        panel.grid.major.y = element_line(
            color = "gray90", size = 0.5, linetype = "dashed"
        ),
        
        axis.line.x = element_line(color = "grey"),
        axis.ticks.x = element_line(color = "grey"),
        axis.ticks.y = element_line(color = "gray80", size = 0.25, linetype = "dashed"),
        
        axis.title = element_blank(),
        
        plot.title = element_text(face = "bold", color = "gray10", size = 12),
        plot.title.position = "plot",
        
        plot.subtitle = element_text(
            margin = margin(b = 10),
            size = 10,
            color = "gray40"
        ),
        
        plot.margin = margin(l = 20, b = 20, r = 20, t = 10)
    ) +
    
    labs(
        title = "Highest Pell award from 2000 to 2017",
        subtitle = "The plot depicts the highest ratio of award amount divided by the total number of recipients for every state per year."
    )



library(patchwork)


multi = gr1 / gr3 +
    
    plot_annotation(
        caption = paste0(
            "Source: **U.S. Department of Education** | ",
            "Graphic: **Nikolaos Pechlivanis**"
        )
    ) & 
    theme(
        plot.caption = element_markdown(size = 8, family = "Malgun Gothic"),
        plot.caption.position = "plot",
        
        plot.background = element_rect(fill = "#faf5f5", color = "#faf5f5"),
        
        plot.margin = margin(10, 10, 10, 10)
    )


ggsave(
    "Rplot.pdf",
    plot = multi,
    width = 14, height = 10,
    device = cairo_pdf,
    units = "in"
)


ggsave(
    "Rplot.png",
    plot = multi,
    width = 14, height = 10,
    units = "in"
)
