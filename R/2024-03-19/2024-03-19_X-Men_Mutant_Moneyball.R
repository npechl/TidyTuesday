


rm(list = ls())
gc()

# load libraries -----------------------------------

library(data.table)
library(stringr)

library(ggplot2)
library(ggtext)

library(colorspace)
library(extrafont)

# input data frame -----------------------------------

df <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-03-19/mutant_moneyball.csv')

my_font = "Jost"

df$totalIssueCheck = NULL


df2 = df[, c(
    "Member", 
    "TotalIssues60s", "TotalIssues70s", "TotalIssues80s", "TotalIssues90s",
    "60s_Appearance_Percent", "70s_Appearance_Percent", "80s_Appearance_Percent","90s_Appearance_Percent"
), with = FALSE]


df3 = df |>
    melt(
        id.vars = "Member", 
        measure.vars = list(
            TotalIssues = c("TotalIssues60s", "TotalIssues70s", "TotalIssues80s", "TotalIssues90s"), 
            Appearance = c("60s_Appearance_Percent", "70s_Appearance_Percent", "80s_Appearance_Percent","90s_Appearance_Percent")
        ),
        
        variable.factor = FALSE
    )


df3$variable = c("60s", "70s", "80s", "90s")[as.numeric(df3$variable)]

df3$Appearance = df3$Appearance |> str_remove_all("%") |> as.numeric()
df3$Appearance = df3$Appearance / 100

gr = df3 |> 
    ggplot(aes(x = Appearance, Member)) +
    
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey20", linewidth = .25) +
    
    geom_segment(aes(y = Member, yend = Member, x = 0, xend = Appearance), color = "grey20", linewidth = .25) +
    
    geom_point(aes(fill = TotalIssues), shape = 21, color = "grey20", size = 3, stroke = .25) +
    
    facet_wrap(vars(variable), nrow = 1) +
    
    scale_fill_stepsn(
        colors = c('#00429d', '#73a2c6', '#f4777f', '#93003a'),
        guide = guide_colorsteps(
            title.position = "top",
            title.theme = element_text(hjust = .5),
            barheight = unit(.5, "lines"),
            barwidth = unit(16, "lines")
        )
    ) +
    scale_x_continuous(labels = scales::percent) +
    
    theme_minimal(base_family = my_font) +
    
    theme(
        legend.position = "top",
        # legend.justification = "left",
        
        panel.spacing = unit(1, "lines"),
        
        panel.grid.major = element_line(linewidth = .35),
        panel.grid.minor = element_line(linewidth = .25),
        
        axis.title.y = element_blank(),
        axis.title.x = element_text(margin = margin(t = 10)),
        
        axis.text.y = element_text(face = "bold"),
        
        strip.text = element_text(face = "bold"),
        
        plot.title    = element_text(margin = margin(b = 5), family = "Jost", face = "bold", size = 26, hjust = .5),
        plot.subtitle = element_markdown(margin = margin(b = 10), family = "Jost", size = 10, color = "grey25", hjust = .5),
        plot.caption  = element_markdown(margin = margin(t = 10), family = "Jost", size = 7, hjust = .5),
        
        plot.background = element_rect(fill = "#f9fbfe", color = NA),
        plot.margin = margin(20, 20, 20, 20)
    ) +
    
    labs(
        fill = "Total number of issues each X-Men\nmember appeared in between 1963 and 1992",
        x = "The percentage each X-Men member appeared in an issue published between 1963 and 1992",
        title = "X-Men Mutant Moneyball",
        
        subtitle = paste0(
            "*It’s no accident that the name of this piece is Mutant Moneyball, ",
            "as the overall point is an exercise in valuing each individual character", 
            "<br>",
            "on the X-Men team in a variety of data driven ways. But buying, reading, and ",
            "loving comic books should be purely for fun and the raw expression of devotion toward ",
            "<br>",
            "characters, art, and story. True: It’s a lesson we fans have failed to learn in the past. ",
            "The trend of buying comics based on value speculation crippled the entire comic book ",
            "<br>",
            "industry for over a decade in the late 1990’s and early 2000’s. ",
            "And the point of this isn’t to repeat those mistakes, it’s to gamify ",
            # "<br>",
            "open and available financial data,<br>giving us special insight, once unattainable, ",
            "regarding our magnificent mutants. Like mature adults.", 
            "<br><br>",
            "Why are some characters sought after more than others, what stories did some mighty mutants convey that made ",
            "them sought after, while others that eek out<br>a continued existence are unable to resonate ",
            # "<br>",
            "enough with a fan base to garner the same kind of love and appreciation in the secondary ",
            "market?<br>Are there answers that a close reading of value data can offer?*"
        ),
        
        caption = paste0(
            "Source: <b>Mutant moneyball: a data driven ultimate X-men</b> by <b>Anderson Evans</b>",
            " | ",
            "Graphic: <b>Nikos Pechlivanis</b>"
        )
    )


ggsave(
    plot = gr, filename = "Rplot.jpeg",
    width = 12, height = 8, units = "in", dpi = 600
)












