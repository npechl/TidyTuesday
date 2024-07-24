


rm(list = ls())
gc()


library(data.table)
library(stringr)

library(ggplot2)
library(ggtext)

library(paletteer)

library(extrafont)

global_temps <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-07-11/global_temps.csv')
nh_temps     <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-07-11/nh_temps.csv')
sh_temps     <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-07-11/sh_temps.csv')
# zonann_temps <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-07-11/zonann_temps.csv')

my_font = "Jost"

# plot a ------------------------------

df = list()

df[["global_temps"]] = global_temps |>
    melt(
        id.vars = "Year", 
        variable.name = "Month", value.name = "value",
        variable.factor = FALSE, value.factor = FALSE
    )

df[["nh_temps"]] = nh_temps |>
    melt(
        id.vars = "Year", 
        variable.name = "Month", value.name = "value",
        variable.factor = FALSE, value.factor = FALSE
    )

df[["sh_temps"]] = sh_temps |>
    melt(
        id.vars = "Year", 
        variable.name = "Month", value.name = "value",
        variable.factor = FALSE, value.factor = FALSE
    )

# df[["zonann_temps"]] = zonann_temps |>
#     melt(
#         id.vars = "Year", 
#         variable.name = "Month", value.name = "value",
#         variable.factor = FALSE, value.factor = FALSE
#     )


df = df |> rbindlist(idcol = "type")


df = df[which(Month %in% month.abb)]

df$Month = df$Month |> str_to_lower()

df = df[which(!is.na(df$value))]

df = df[order(Year, Month)]

df$date = paste0(df$Year, "-", df$Month, "-", "1") |> 
    as.Date(format = "%Y-%b-%d")

gr = ggplot(data = df, aes(x = date, y = value)) +
    
    geom_point(
        aes(fill = type),
        shape = 21, size = 1.2, stroke = .15,
        color = "grey96"
    ) +
    
    geom_smooth(aes(color = type, fill = type)) +
    
    scale_color_manual(values = paletteer_d("ggsci::planetexpress_futurama")) +
    scale_fill_manual(values = paletteer_d("ggsci::planetexpress_futurama")) +
    
    scale_x_date(expand = c(0, 0), date_breaks = "10 years", date_labels = "%Y", minor_breaks = NULL) +
    scale_y_continuous(labels = scales::unit_format(suffix = " °C")) +
    
    theme_minimal(base_family = my_font) +
    
    theme(
        legend.position = "none",
        legend.title = element_blank(),
        legend.justification = "left",
        
        panel.grid.major = element_line(linewidth = .3, linetype = "dashed", color = "grey75"),
        panel.grid.minor = element_blank(),
        
        axis.line.x = element_line(linewidth = .3),
        axis.ticks.x = element_line(linewidth = .3),
        
        axis.ticks.y = element_line(linewidth = .3, color = "grey75"),
        
        axis.title = element_blank(),
        
        axis.text.y = element_text(face = "bold", margin = margin(r = 5)),
        
        plot.title    = element_text(face = "bold", size = 32, family = my_font),
        plot.subtitle = element_markdown(margin = margin(b = 15), size = 11, color = "grey20", family = my_font),
        plot.caption  = element_markdown(margin = margin(t = 15), size = 6, family = my_font),
        
        plot.background = element_rect(fill = "#f5f5f5", color = NA),
        plot.margin = margin(20, 20, 20, 20)
    ) +
    
    labs(
        title = 'Global surface temperatures',
        
        subtitle = paste0(
            "Monthly average surface temperatures at a <b style='color:#FF6F00FF'>global</b> scale,",
            "as well as in the <b style='color:#C71000FF'>northern</b> and <b style='color:#008EA0FF'>southern</b> hemispheres."
        ),
        
        caption = paste0(
            "Source: <b>NASA GISS Surface Temperature Analysis (GISTEMP v4)</b> | ",
            "Graphic: <b>Nikolaos Pechlivanis</b>"
        )
    )


ggsave(
    plot = gr, filename = "Rplot.jpeg",
    width = 12, height = 10, units = "in", dpi = 600
)






