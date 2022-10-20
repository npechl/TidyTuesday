


rm(list = ls())
gc()


library(data.table)
library(stringr)

library(ggplot2)
library(ggtext)

library(ggh4x)

tuesdata <- tidytuesdayR::tt_load('2022-10-18')


episodes = setDT(tuesdata$episodes)
dialogues = setDT(tuesdata$stranger_things_all_dialogue)


rm(tuesdata)


dialogues$dialogue = str_remove_all(dialogues$dialogue, "\\♪")
dialogues$dialogue = str_squish(dialogues$dialogue)


# library(ggwordcloud)
# 
# df = dialogues[, by = dialogue, .N]
# df = df[which(!is.na(df$dialogue) & df$dialogue != ""), ]
# df = df[order(-N), ]
#
# sample(1:nrow(df), 200)
#
# ggplot(df[sample(1:nrow(df), 200), ], 
#        aes(label = dialogue, size = N)) +
#     
#     geom_text_wordcloud_area(
#         rm_outside = TRUE, 
#         shape = "square", color = "#1E193C", 
#         alpha = .5
#     ) +
#     # scale_size_area(max_size = 20) +
#     theme_minimal() +
#     
#     theme(
#         plot.background = element_rect(fill = "#bfe2e2", color = "#bfe2e2")
#     )


episodes$chapter = str_split(episodes$title, "\\:", simplify = TRUE)[, 1]
episodes$title = str_split(episodes$title, "\\:", simplify = TRUE)[, 2]

episodes$title = str_squish(episodes$title)

episodes$chapter = factor(
    episodes$chapter,
    levels = unique(episodes$chapter)
)

episodes$season = paste0("Season: ", episodes$season)
episodes$title_length = str_length(episodes$title)
# episodes$title = paste0(episodes$chapter, ": ", episodes$title)

titles = str_split(episodes$title, "\\ ")
titles = lapply(titles, function(x) {
    
    len = str_length(x)
    total = 0
    out = ""
    
    for(i in seq_len(length.out = length(len))) {
        
        if(total + len[i] > 10) {
            
            out = paste0(out, " ", x[i], "+")
            total = 0
            
        } else {
            
            out = paste0(out, x[i], " ")
            total = total + len[i]
            
        }
        
    }
    
    return(out)
    
})

titles = unlist(titles)
titles = str_squish(titles)

titles = ifelse(
    str_sub(titles, -1, -1) == "+",
    str_sub(titles, 1, -2),
    titles
)

titles = str_replace_all(titles, "\\+", "<br>")

# episodes$richtitle = paste0(
#     "<b>", episodes$chapter, "</b>: ",
#     titles
# )



episodes$richtitle = titles

# library(ggstream)

episodes$written_by = str_replace_all(episodes$written_by, "\\&", "\\&<br>")

episodes$annotation = paste0(
    "directed by: <i>", episodes$directed_by, "</i><br>",
    "written by: <i>", episodes$written_by, "</i>"
)

# episodes$episode = episodes$episode * 3
# 
# gr = ggplot(data = episodes, aes(x = episode, y = episode)) +
#     
#     # geom_stream(aes(fill = season), true_range = "none") +
#     
#     # geom_segment(aes(x = episode, y = episode - 0.5, 
#     #                  xend = episode, yend = episode)) +
#     # 
#     # geom_point(aes(x = episode, y = episode - 0.5)) +
#     
#     geom_richtext(aes(label = richtitle), vjust = 0) +
#     
#     geom_richtext(
#         aes(
#             x = episode, y = episode - .5, label = annotation
#         ),
#         
#         fill = NA, label.color = NA, size = 3, vjust = 1
#     ) +
#     
#     facet_grid(
#         cols = vars(season),
#         space = "free_x"
#     ) +
#     
#     scale_x_continuous(expand = c(0, 0)) +
#     scale_y_continuous(expand = c(0, 0), limits = c(0, 55)) +
#     
#     coord_cartesian(expand = TRUE, clip = "off") +
#     
#     theme_void() +
#     
#     theme(
#         axis.title = element_blank(),
#         
#         axis.text = element_blank(),
#         
#         axis.ticks = element_blank(),
#         
#         plot.margin = margin(t = 20, b = 20, l = 80, r = 80)
#     )
# 
# 
# ggsave(
#     plot = gr,
#     filename = "Rplot.pdf",
#     width = 16, height = 12,
#     device = cairo_pdf,
#     units = "in"
# )
# 
# episodes$episode = episodes$episode / 3

episodes$episode = paste0("Εp. ", episodes$episode)

episodes$chapter = factor(
    episodes$chapter,
    levels = unique(episodes$chapter)
)

library(extrafont)




gr = ggplot(data = episodes, aes(x = 1, y = 5)) +
    
    geom_segment(aes(x = 1, xend = 9,
                     y = 5, yend = 5),
                 color = "#FF1515") +
    
    geom_segment(aes(x = 1, xend = 9,
                     y = 4.8, yend = 4.8),
                 color = "#FF1515", linetype = "dotted") +
    
    geom_richtext(aes(label = richtitle), vjust = 0,
                  fill = NA, colour = "white", label.colour = NA,
                  fontface = "bold", hjust = 0, 
                  label.padding = unit(c(.5, .5, .5, .5), "lines"),
                  family = "Benguiat") +
    
    geom_richtext(
        aes(x = 1, y = 4.5, label = annotation),
        fill = NA, label.color = NA, size = 3,
        vjust = 1, colour = "white", hjust = 0,
        family = "Benguiat"
    ) +
    
    facet_grid2(
        cols = vars(chapter),
        rows = vars(season),
        
        switch = "y"
    ) +
    
    scale_x_continuous(expand = c(0, 0), limits = c(0, 10)) +
    scale_y_continuous(expand = c(0, 0), limits = c(0, 10)) +
    
    coord_cartesian(expand = TRUE, clip = "off") +
    
    theme_void() + 
    
    theme(
        strip.text.x = element_text(face = "bold", colour = "white", family = "Benguiat"),
        strip.text.y = element_text(face = "bold", colour = "white", family = "Benguiat"),
        
        plot.margin = margin(t = 20, b = 20, l = 20, r = 20),
        
        plot.title.position = "plot",
        plot.caption.position = "plot",
        
        plot.title = element_markdown(color = "white", margin = margin(b = 5), face = "bold", family = "Benguiat", size = 20),
        plot.subtitle = element_markdown(color = "white", margin = margin(b = 20), family = "Benguiat"),
        plot.caption = element_markdown(color = "white", family = "Benguiat"),
        
        plot.background = element_rect(fill = "#1E0707", color = "#1E0707")
    ) +
    
    
    labs(
        title = "Stranger Things Episodes",
        subtitle = paste0(
            "Stranger Things is an American science fiction fantasy horror ",
            "drama television series created by the Duffer Brothers that is ",
            "streaming on Netflix.<br>The brothers serve as showrunners and are ",
            "executive producers along with Shawn Levy and Dan Cohen. The ",
            "first season of the series was released<br>on Netflix on July 15, ",
            "2016, with the second, third, and fourth seasons following in ",
            "October 2017, July 2019, and May and July 2022, respectively.<br>",
            "<b>The graph summarizes all Stranger Things episodes that have ",
            "been streamed during the four seasons.</b>"
        ),
        caption = paste0(
            "Source: **8flix.com** | ",
            "Graphic: **Nikolaos Pechlivanis**"
        )
    )

ggsave(
    plot = gr,
    filename = "Rplot.pdf",
    width = 18, height = 9,
    device = cairo_pdf,
    units = "in"
)

library(magick)

x = image_read_pdf("Rplot.pdf", density = 500)
image_write(x, "Rplot.png")








