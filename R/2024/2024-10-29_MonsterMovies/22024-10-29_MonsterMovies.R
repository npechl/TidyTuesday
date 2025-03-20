


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

# load data ----------------------------------- 

d1 <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-10-29/monster_movies.csv')

loadfonts(device = "win")

# split genres -----------------

t <- d1$genres |> 
    str_split("\\,") |> 
    lapply(str_squish) |> 
    lapply(function(x) data.table("genre" = x)) |> 
    rbindlist(idcol = "id")

d1 <- cbind(d1[t$id, -c("genres")], t[, -1])

# drop na ------------------

d1 <- d1[which(!is.na(genre))]

# plot 1 ---------------------------


library(ggridges)
library(shadowtext)

d1[, by = genre, N := .N]

d2 <- d1[which(N >= 3)]

gr1 <- d2[which(year >= 1980)] |>
    ggplot(aes(average_rating, genre)) +
    geom_density_ridges(linewidth = .15, color = "white" |> darken(.25), fill = "#c1405a" |> lighten(.2))  +
    
    geom_vline(xintercept = c(2.5, 7.5), color = "#61146f", linewidth = .15, linetype = "dashed", lineend = "round") +
    geom_vline(xintercept = 5, color = "#61146f", linewidth = .5, linetype = "dashed", lineend = "round") +
    
    scale_x_continuous(limits = c(0, 10), breaks = seq(2.5, 7.5, by = 2.5)) +
    
    theme_minimal(base_family = my_font) +
    
    theme(
        axis.title.x = element_markdown(margin = margin(t = 10), size = 5),
        axis.title.y = element_blank(), # element_markdown(margin = margin(r = 10), size = 5),
        
        axis.text.y = element_shadowtext(family = "Creepster", size = 5, bg.color = "grey96", bg.r = .05),
        axis.text.x = element_text(size = 5),
        
        panel.grid = element_blank(),
    ) +
    
    labs(x = "**Weighted Average on IMDb**", y = "**Genres** associated with the movies")

# plot 2 ------------------

d3 <- d1[, -c("genre", "N")] |> unique()


gr2 <- d3[which(year >= 1980)] |>
    ggplot(aes(year, average_rating)) +
    
    geom_density_2d_filled(color = "grey50", linewidth = .25) +
    
    geom_hline(yintercept = c(2, 4, 6, 8), linewidth = .25, linetype = "dashed", lineend = "round", color = "grey") +
    geom_hline(yintercept = c(1, 3, 5, 7, 9), linewidth = .1, linetype = "dashed", lineend = "round", color = "grey") +
    
    geom_vline(xintercept = seq(1985, 2024, by = 5), linewidth = .25, linetype = "dashed", lineend = "round", color = "grey") +
    
    geom_point(shape = 21, color = "#ab1d4f" |> darken(.15), fill = "#c1405a" |> lighten(.35), stroke = .25, size = 1.5) +
    
    scale_x_continuous(expand = c(0, 0), breaks = seq(1985, 2024, by = 5)) +
    scale_y_continuous(expand = c(0, 0), breaks = c(2, 4, 6, 8), limits = c(0, 10)) +
    
    scale_fill_manual(values = paletteer_c("ggthemes::Orange Light", 13, direction = 1)) +
    
    theme_minimal(base_family = my_font) +
    
    theme(
        legend.position = "none",
        
        axis.ticks = element_line(linewidth = .25, lineend = "round", color = "grey"),
        
        axis.title.x = element_markdown(margin = margin(t = 10)),
        axis.title.y = element_markdown(margin = margin(r = 10)),
        
        panel.grid = element_blank(),
        
        plot.title    = element_text(face = "bold", size = 28, margin = margin(b = 10), family = "Creepster", hjust = .5),
        plot.subtitle = element_markdown(margin = margin(b = 10), size = 8, color = "grey25", family = "Jost", hjust = .5),
        plot.caption  = element_markdown(margin = margin(t = 10), size = 6, family = "Jost", hjust = .5),
        
        plot.background = element_rect(fill = "#fbf2f4", color = "#fbf2f4"),
        plot.margin = margin(20, 20, 20, 20)
    ) +
    
    labs(
        x = "**Release Year** of a movie", 
        y = "**Weighted Average** of all the individual user ratings **on IMDb**",
        
        title = "Monster Movies",
        
        subtitle = paste0(
            "**Weighted average of IMDb user ratings** for Monster Movies from 1980 to 2024 (categorized by genre)."
        ),
        
        caption = paste0(
            "Source: <b>Internet Movie Database</b> | ",
            "Graphic: <b>Nikos Pechlivanis</b>"
        )
    )



# patchwork -----------------

library(patchwork)


multi <- gr2 + inset_element(gr1, .075, .005, .325, .55)


ggsave(
    plot = multi, filename = "Rplot.pdf", device = cairo_pdf,
    width = 8, height = 9, units = "in",
)



# magick --------

library(magick)

tgr <- image_read_pdf("Rplot.pdf", density = 600)

image_write(tgr, "Rplot.png")









