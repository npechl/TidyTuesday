


rm(list = ls())
gc()

# load libraries -----------------------------------

library(data.table)
library(stringr)

library(ggplot2)
library(ggtext)

library(paletteer)
library(colorspace)
library(extrafont)

my_font = "Jost"

# 1 ----------------------------------- 

a1 <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-09-17/hamlet.csv')
a2 <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-09-17/macbeth.csv')
a3 <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-09-17/romeo_juliet.csv')

# 2 ----------------------------------------

df <- list("hamlet" = a1, "macbeth" = a2, "romeo_juliet" = a3) |> rbindlist(idcol = "play")

rm(a1, a2, a3)

# 3 --------------------

df <- df[which(!is.na(line_number)), by = .(play, act, scene, character), .N]
df <- df[order(play, act, scene, -N)]

df[, by = play, Freq := N / max(N)]

# 4 --------------------------

library(packcircles)

df <- df |> 
    split(by = c("play", "act", "scene")) |>
    lapply(function(x) {
        
        packing <- x$Freq |>
            circleProgressiveLayout(sizetype = 'area') |>
            circleLayoutVertices(npoints = 100) |>
            setDT()
        
        packing <- cbind(packing, x[packing$id])
        
    }) |>
    rbindlist()

# 5 --------------------------

h <- df[, by = .(play, act, scene, character, N, Freq), .(
    x = ( min(x) + max(x) ) / 2,
    y = ( min(y) + max(y) ) / 2
)]

h$character <- h$character |> str_wrap(width = 8)
h$strip <- ifelse(h$Freq >= .1, h$character, "")

df <- df |> split(by = "play")
h <- h |> split(by = "play")


# 6 -------------------------

library(shadowtext)
library(ggh4x)

plot_circles <- function(main, sec, play, play_col, palettee = "ggthemes::Blue Light", fill_breaks = c(25, 50, 75, 100)) {
    
    ggplot() + 
        
        geom_polygon(
            data = main, aes(x, y, group = id, fill = N, color = N),
            linewidth = .25
        ) +
        
        geom_shadowtext(
            data = sec, aes(x, y, label = strip, size = Freq, color = N), 
            inherit.aes = FALSE, family = "Anton",
            color = "grey1", bg.color = "grey96", bg.r = .05
        ) +
        
        scale_size_continuous(range = c(1, 3), guide = "none") +
        
        scale_color_stepsn(
            colors =  paletteer_c(palettee, 5, -1) |> as.character() |> darken(.15),
            breaks = fill_breaks,
            guide = "none"
        ) +
        
        scale_fill_stepsn(
            colors =  paletteer_c(palettee, 5, -1),
            breaks = fill_breaks,
            guide = guide_colorsteps(
                barwidth = unit(12, "lines"),
                barheight = unit(.35, "lines")
            )
        ) +
        
        facet_grid2(vars(act), vars(scene), render_empty = FALSE, switch = "y") +
        
        theme_minimal(base_family = my_font) + 
        
        theme(
            legend.position = "bottom",
            legend.title.position = "top",
            
            axis.text = element_blank(),
            axis.title = element_blank(),
            
            panel.spacing = unit(1, "lines"),
            panel.grid.major = element_line(linetype = "dotted", lineend = "round", color = "grey85", linewidth = .25),
            panel.grid.minor = element_blank(),
            
            
            strip.text.x = element_text(face = "bold", margin = margin(b = 10)),
            strip.text.y = element_text(face = "bold", margin = margin(r = 10)),
            
            plot.title    = element_text(face = "bold", size = 26, margin = margin(b = 5), family = "Jost", hjust = .5),
            plot.subtitle = element_markdown(margin = margin(b = 15), size = 10, color = "grey25", family = "Jost", hjust = .5),
            plot.caption  = element_markdown(margin = margin(t = 20), size = 8, family = "Jost", hjust = .5),
            
            plot.background = element_rect(fill = "#f8f2f9", color = NA),
            plot.margin = margin(20, 20, 20, 20)
        ) +
        
        coord_equal() +
        
        labs(
            fill = "No. of lines",
            
            title = "Shakespeare Dialogue",
            
            subtitle = paste0(
                "Number of lines each character has spoken in ",
                "<b style='color:", play_col, "'>", play, "</b>"
            ),
            
            caption = paste0(
                "Source: <b>shakespeare.mit.edu</b> (via <b>github.com/nrennie/shakespeare)</b> | ",
                "Graphic: <b>Nikos Pechlivanis</b>"
            )
        )
    
}

# 7 ------------------------------

p_1 <- plot_circles(df$hamlet,       h$hamlet,       play = "Hamlet",           play_col = "#C6C6C6" |> darken(.75), palettee = "ggthemes::Classic Red-White-Black Light", fill_breaks = c(50, 100, 150, 200)) 
p_2 <- plot_circles(df$macbeth,      h$macbeth,      play = "Macbeth",          play_col = "#FFCC9E" |> darken(.75), palettee = "ggthemes::Classic Orange-White-Blue Light")
p_3 <- plot_circles(df$romeo_juliet, h$romeo_juliet, play = "Romeo and Juliet", play_col = "#FFB2B6" |> darken(.75), palettee = "ggthemes::Classic Red-Green Light")

# 8 -------------------------

ggsave(
    plot = p_1, filename = "Rplot-1.png", dpi = 600,
    width = 18, height = 10, units = "in"
)

ggsave(
    plot = p_2, filename = "Rplot-2.png", dpi = 600,
    width = 18, height = 10, units = "in"
)


ggsave(
    plot = p_3, filename = "Rplot-3.png", dpi = 600,
    width = 18, height = 8, units = "in"
)


