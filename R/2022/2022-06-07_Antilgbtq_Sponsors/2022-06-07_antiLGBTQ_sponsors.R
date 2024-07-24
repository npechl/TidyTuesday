# Get the Data

# Read in with tidytuesdayR package 
# Install from CRAN via: install.packages("tidytuesdayR")
# This loads the readme and all the datasets for the week of interest

# Either ISO-8601 date or year/week works!


rm(list = ls())
gc()


library(data.table)
library(stringr)

library(ggplot2)
library(ggtext)

tuesdata <- tidytuesdayR::tt_load('2022-06-07')

pride_aggregates             = tuesdata$pride_aggregates
fortune_aggregates           = tuesdata$fortune_aggregates
static_list                  = tuesdata$static_list
pride_sponsors               = tuesdata$pride_sponsors
corp_by_politician           = tuesdata$corp_by_politician
donors                       = tuesdata$donors
contribution_data_all_states = tuesdata$contribution_data_all_states

rm(tuesdata)

pride_aggregates             = setDT(pride_aggregates)
fortune_aggregates           = setDT(fortune_aggregates)
static_list                  = setDT(static_list)
pride_sponsors               = setDT(pride_sponsors)
corp_by_politician           = setDT(corp_by_politician)
donors                       = setDT(donors)
contribution_data_all_states = setDT(contribution_data_all_states)

static_list = static_list[order(static_list$`Amount Contributed Across States`), ]

static_list$Company = factor(
    static_list$Company,
    levels = static_list$Company
)

static_list_sub = static_list[which(static_list$`Pride?`), ]

gr = ggplot(data = static_list_sub) +
    
    geom_segment( aes(y = Company, yend = Company, 
                      x = 0, xend = `Amount Contributed Across States`),
                  size = 0.3) +
    
    
    
    geom_point(aes(y = Company, x = `Amount Contributed Across States`, 
                   size = `# of States Where Contributions Made`,
                   fill = `# of Politicians Contributed to`),
               # color = "red", # fill = "orange", 
               shape = 21, stroke = 0.5) + 
    
    # geom_col(aes(x = Company, y = `Total Contributed`)) +
    
    scale_size_binned(
        breaks = c(6, 5, 4, 3, 2, 1),
        guide = guide_legend(ncol = 1, title.position = "top")
    ) +
    
    scale_fill_gradient(
        
        low = "gray100",
        high = "red4",

        guide = guide_colorbar(
            title.position = "top",
            barwidth = 0.5,
            barheight = 12
        )
    ) +
    
    scale_x_continuous(
        expand = c(0, 0),
        labels = scales::dollar_format(suffix = "k", scale = 1e-3),
        breaks = seq(100000, 600000, by = 100000)
    ) +

    coord_cartesian(expand = TRUE, clip = "off") + 
    
    labs(
        x        = "Amount Contributed Across States",
        size     = "# of States Where\nContributions Made",
        fill     = "# of Politicians\nContributed to",
        title    = "Pride sponsors who have donated to Anti-LGBTQ campaigns",
        subtitle = "amount contributed across states to anti-LBGTQ politicians",
        caption = paste0(
            "Source: <b>Data for Progress</b>",
            " <b>•</b> ",
            "visualization: <b>Pechlivanis Nikolaos</b>"
            
        )
    ) +

    theme_minimal() +

    theme(

        plot.subtitle = element_text(color = "gray30",
                                     margin = margin(b = 15)), 
        plot.caption = element_textbox(color = "gray30"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),

        panel.grid.major.x = element_line(color = "gray85"),

        axis.title.x = element_text(margin = margin(t = 10, b = 15)),
        axis.title.y = element_blank(),
        plot.title.position = "plot",
        plot.caption.position = "plot",
        plot.margin = margin(15, 15, 10, 15)
    )
    


ggsave(
    filename = "Rplot.pdf",
    plot = gr,
    width = 10,
    height = 8,
    units = "in"
)

ggsave(
    filename = "Rplot.jpeg",
    plot = gr,
    width = 8,
    height = 8,
    units = "in"
)
