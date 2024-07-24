


rm(list = ls())
gc()

library(data.table)
library(stringr)

library(ggplot2)
library(ggh4x)
library(ggtext)

library(patchwork)

library(extrafont)

holiday_episodes       <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-12-19/holiday_episodes.csv')
holiday_episode_genres <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-12-19/holiday_episode_genres.csv')


holiday_episodes$tconst |> unique() |> length()
holiday_episodes$parent_tconst |> unique() |> length()

holiday_episodes$tconst |> duplicated() |> any()

index = match(holiday_episode_genres$tconst, holiday_episodes$tconst)

holiday_episode_genres$year           = holiday_episodes[index]$year
holiday_episode_genres$season_number  = holiday_episodes[index]$season_number
holiday_episode_genres$episode_number = holiday_episodes[index]$episode_number

holiday_episode_genres = holiday_episode_genres[which(!is.na(genres) & !is.na(year))]

df = holiday_episode_genres[, by = .(genres, year), .N]

df$label = ifelse(
    df$genres %in% c("Adventure", "Animation", "Comedy", "Drama", "Family", "Game-Show", "Reality-TV", "Romance"),
    df$genres, "Other"
)


df$label = df$label |>
    factor(levels = c("Adventure", "Animation", "Comedy", "Drama", "Family", "Game-Show", "Reality-TV", "Romance", "Other"))


plot_barplot <- function(x) {
    
    gr = ggplot(x) +
        
        geom_col(
            aes(year, y = N, fill = label),
            color = "grey50", linewidth = .05
        ) + 
        
        scale_fill_viridis_d(
            option = "plasma", 
            guide = guide_legend(
                nrow = 1,
                keywidth = .5,
                keyheight = .5,
                label.theme = element_text(family = "Jost", size = 6)
            )
        ) +
        
        theme_minimal(base_family = "Jost")
    
    
    return(gr)
}

gr1 = plot_barplot(df) + 
    scale_y_continuous(expand = c(0, 0), limits = c(0, 255)) +
    theme(
        legend.position = "bottom",
    
        axis.title.x = element_blank(),
        axis.title.y = element_text(hjust = 0),
        axis.text = element_text(size = 6),
        axis.ticks.x = element_line(linetype = "dashed", linewidth = .25, color = "grey85"),
        
        panel.grid.major = element_line(linetype = "dashed", linewidth = .25, color = "grey85"),
        panel.grid.minor = element_line(linetype = "dashed", linewidth = .25, color = "grey85"),
    ) +
    
    labs(y = "No. of episodes",)

gr2 = plot_barplot(df) + 
    facet_wrap2(vars(label), axes = "all") + 
    scale_y_continuous(expand = c(0, 0), limits = c(0, 80)) +
    theme(
        legend.position = "bottom",
        axis.title = element_blank(), 
        axis.text = element_text(size = 5),
        axis.ticks.x = element_line(linetype = "dashed", linewidth = .25, color = "grey85"),
        
        strip.text = element_text(face = "bold"),
        
        panel.spacing = unit(1, "line"),
        panel.grid.major = element_line(linetype = "dashed", linewidth = .25, color = "grey85"),
        panel.grid.minor = element_line(linetype = "dashed", linewidth = .25, color = "grey85"),
    )


mgr = gr1 + gr2 + 
    plot_layout(guides = 'collect') + 
    plot_annotation(
        title = 'Holiday Episodes',
        
        subtitle = paste0(
            "The count of TV episodes labeled as 'Holiday,' featuring terms such as 'Christmas,' 'Hanukkah,' or 'Kwanzaa' in their titles, categorized by the respective genres of each episode."
        ),
        
        caption = paste0(
            "Source: <b>Internet Movie Database</b> | ",
            "Graphic: <b>Nikolaos Pechlivanis</b>"
        )
    ) &
    theme(
        legend.position = "bottom",
        legend.justification = "left",
        legend.title = element_blank(),
        
        plot.title    = element_text(face = "bold", size = 26, margin = margin(b = 5), family = "Jost"),
        plot.subtitle = element_markdown(margin = margin(b = 25), size = 8, color = "grey25", family = "Jost"),
        plot.caption  = element_markdown(margin = margin(t = 25), size = 6, family = "Jost"),
        
        plot.title.position = "plot",
        plot.caption.position = "plot",
        
        plot.background = element_rect(fill = "#f5f5f5", color = NA),
        plot.margin = margin(20, 20, 20, 20)
    )


ggsave(
    plot = mgr, filename = "Rplot.jpeg",
    width = 12, height = 8, units = "in", dpi = 600
)


# ggplot(df, aes(x = year, y = N, fill = genres)) +
#     
#     geom_stream(color = "white", linewidth = .05, type = "ridge")






