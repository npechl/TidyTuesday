


rm(list = ls())
gc()


library(data.table)
library(stringr)

library(ggplot2)
library(ggtext)
library(ggh4x)
library(extrafont)



big_tech_stock_prices <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-02-07/big_tech_stock_prices.csv')
big_tech_companies    <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-02-07/big_tech_companies.csv')


df = merge(
    big_tech_stock_prices, big_tech_companies, 
    by = "stock_symbol", all.x = TRUE
)

rm(big_tech_companies, big_tech_stock_prices)

df$company = str_split(df$company, "\\,", simplify = TRUE)[,1]
df$company = str_remove_all(df$company, "\\Inc.|Corporation|Platforms")
df$company = str_squish(df$company)

df$stock_symbol = NULL
df$high         = NULL
df$low          = NULL
df$volume       = NULL

high = df
high$company2 = high$company
high$company = NULL


gr = ggplot(data = df, aes(x = date, y = adj_close)) +
    
    geom_line(
        aes(group = company), 
        linewidth = .25, color = "grey"
    ) +
    
    geom_line(
        data = high, 
        aes(x = date, y = adj_close, group = company2), 
        linewidth = .3, color = "#e70606"
    ) +
    
    facet_wrap2(vars(company2), axes = "all") +
    
    coord_cartesian(expand = TRUE, clip = "off") +
    
    theme_minimal(base_family = "PT Serif") +
    
    theme(
        strip.text = element_text(face = "bold", hjust = 0),
        
        axis.text = element_text(size = 7, color = "grey20"),
        
        panel.grid.major = element_line(linewidth = .2, linetype = "dashed", color = "#af86ff"),
        panel.grid.minor = element_line(linewidth = .2, linetype = "dashed", color = "#af86ff"),
        
        axis.title.x = element_blank(),
        axis.title.y = element_markdown(hjust = 0, margin = margin(r = 15)),
        
        panel.spacing = unit(1, "lines"),
        
        plot.title.position   = "plot",
        plot.caption.position = "plot",
        
        plot.title    = element_text(size = 32, color = "grey10", face = "bold"),
        plot.subtitle = element_markdown(margin = margin(b = 20), size = 10, color = "grey20", face = "bold"),
        plot.caption  = element_markdown(margin = margin(t = 20), size = 5, color = "grey20"),
        
        plot.margin = margin(20, 20, 20, 20),
        plot.background = element_rect(fill = "#fcfdff", color = NA)
    ) +
    
    labs(
        y = paste0(
            "The closing price after adjustments for all applicable splits ",
            "and dividend distributions<br>",
            "<span style = 'font-size:8pt'>Data is adjusted using appropriate ",
            "split and dividend multipliers, adhering to Center for Research ",
            "in Security Prices (CRSP) standards</span>"
        ),
        
        title = 'Big Tech Stock Prices',
        
        subtitle = paste0(
            "The graph summarizes daily stock prices for 14 different ",
            "tech companies"
        ),
        
        caption = paste0(
            "Source: <b>Yahoo Finance via Kaggle</b> | ",
            "Graphic: <b>Nikolaos Pechlivanis</b>"
        )
    )


ggsave(
    plot = gr, filename = "Rplot.jpeg",
    width = 11, height = 10, units = "in"
)







