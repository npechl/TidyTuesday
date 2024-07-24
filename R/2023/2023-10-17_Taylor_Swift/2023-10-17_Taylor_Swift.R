


rm(list = ls())
gc()

library(data.table)
library(stringr)

library(ggplot2)
library(ggtext)

library(extrafont)
library(paletteer)

taylor_album_songs <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-10-17/taylor_album_songs.csv')
taylor_all_songs   <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-10-17/taylor_all_songs.csv')
taylor_albums      <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-10-17/taylor_albums.csv')



taylor_album_songs |> head()
taylor_all_songs |> head()
taylor_albums |> head()


taylor_albums$metacritic_score = taylor_albums$metacritic_score / 100
taylor_albums$user_score = taylor_albums$user_score / 10

# taylor_albums = taylor_albums |> 
#     melt(
#         id.vars = c("album_name", "ep", "album_release"),
#         variable.factor = FALSE, value.factor = FALSE,
#         variable.name = "score_type", value.name = "Score"
#     )

# taylor_albums$score_type = taylor_albums$score_type |>
#     str_replace("_", " ") |>
#     str_to_title()

# df = taylor_albums[, by = .(album_name, album_release), .(y = mean(Score))]

taylor_albums$ypos = (taylor_albums$metacritic_score + taylor_albums$user_score) / 2

library(geomtextpath)
library(ggrepel)

library(ggh4x)

library(colorspace)

taylor_albums$ribbfill = ifelse(
    taylor_albums$metacritic_score <= taylor_albums$user_score,
    "user", "metacritic"
)


gr = ggplot(data = taylor_albums[which(!is.na(ypos))]) +
    
    # geom_ribbon(aes(x = album_release, ymin = metacritic_score, ymax = user_score, fill = ribbfill)) +
    
    stat_difference(aes(x = album_release, ymin = metacritic_score, ymax = user_score), alpha = .5) +
    
    geom_richtext(
        aes(x = album_release, y = .56, label = album_name), 
        size = 8, label.size = NA, fill = "#f5f5f5", color = alpha("grey10", alpha = .3),
        angle = 90, hjust = 0, family = "Bebas Neue", fontface = "bold"
    ) +
    
    # geom_point(aes(x = album_release, y = .655), alpha = .25) +
    
    geom_textline(aes(x = album_release, y = metacritic_score), family = "Jost",
                  hjust = "auto",  vjust = 1.5, label = "Metacritic Score",
                  color = "#C71000", linewidth = .75) +
    
    geom_point(aes(x = album_release, y = metacritic_score),
               shape = 21, stroke = .75, fill = "#f5f5f5", color = "#C71000") +
    
    geom_textline(aes(x = album_release, y = user_score), family = "Jost",
                  hjust = "auto",  vjust = -.5, label = "User Score",
                  color = "#FF6F00", linewidth = .75) +
    
    geom_point(aes(x = album_release, y = user_score),
               shape = 21, stroke = .75, fill = "#f5f5f5", color = "#FF6F00") +
    
    scale_y_continuous(limits = c(.55, .95), expand = c(0, 0), labels = scales::percent) +
    
    scale_x_date(breaks = taylor_albums[which(!is.na(ypos))]$album_release,
                 labels = scales::date_format(format = "%Y-%m")) +
    
    scale_fill_manual(values = c(lighten("#1A5354"), "#8A4198")) +
     
    theme_minimal(base_family = "Jost") +
    
    theme(
        legend.position = "none",
        
        panel.grid.major.y = element_line(linewidth = .3, linetype = "dashed"),
        panel.grid.minor.y = element_line(linewidth = .3, linetype = "dashed"),
        panel.grid.major.x = element_line(linewidth = .3, linetype = "dashed", color = "grey85"),
        panel.grid.minor.x = element_blank(),
        
        axis.text.x = element_text(angle = 45, hjust = 1),
        
        axis.line.x = element_line(linewidth = .3),
        axis.ticks.x = element_line(linewidth = .3),
        
        plot.title    = element_text(face = "bold", size = 36, margin = margin(b = 5), family = "Jost"),
        plot.subtitle = element_markdown(margin = margin(b = 20), size = 11, color = "grey20", family = "Jost"),
        plot.caption  = element_markdown(margin = margin(t = 20), size = 8, family = "Jost"),
        
        plot.title.position = "plot",
        plot.caption.position = "plot",
        
        plot.background = element_rect(fill = "#f5f5f5", color = NA),
        plot.margin = margin(20, 20, 20, 20)
    ) +
    
    labs(
        x = "Album release date",
        y = "Score",
        
        title = 'Taylor Swift',
        subtitle = paste0(
            "Taylor Swift's album history, including ", 
            "<b style='color:#FF6F00'>user</b> and <b style='color:#C71000'>metacritic</b> ratings."
        ),
        
        caption = paste0(
            "Source: <b>taylor R package</b> | ",
            "Graphic: <b>Nikolaos Pechlivanis</b>"
        )
    )


ggsave(
    plot = gr, filename = "Rplot.pdf", device = cairo_pdf,
    width = 16, height = 8, units = "in", dpi = 600
)


library(magick)


tmp = image_read_pdf("Rplot.pdf", density = 600)
image_write(tmp, "Rplot.jpeg")