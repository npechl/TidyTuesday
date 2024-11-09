


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

d0 <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-11-05/democracy_data.csv')

d0 <- d0[which(!is.na(is_democracy))]

# plot 1 ------------------------

d0$value <- ifelse(d0$is_democracy, 1, 0)

ht <- d0 |> 
    dcast(country_name ~ year, value.var = "value", fill = 0) |>
    as.matrix(rownames = 1) |>
    dist(method = "euclidean") |>
    hclust(method = "ward.D2")

d0$country_fct <- d0$country_name |> factor(levels = ht$labels[ht$order])


d0$continent <- countrycode::countrycode(
    sourcevar = d0$country_name, 
    origin = "country.name",  
    destination = "continent"
)


gr1 <- d0 |>
    ggplot(aes(year, country_fct)) +
    
    geom_tile(aes(fill = is_democracy), color = "grey85", linewidth = .1) +
    
    scale_fill_manual(values = c('#93003a', '#00429d') |> lighten(.5)) +
    
    scale_x_continuous(breaks = seq(1955, 2015, by = 10), expand = c(0, 0)) + 
    scale_y_discrete(position = "right", expand = c(0, 0)) +
    
    facet_grid(rows = vars(continent), switch = "y", scales = "free_y", space = "free_y") +
    
    theme_minimal(base_family = my_font) +
    
    theme(
        legend.position = "none",
        strip.text = element_text(face = "bold"),
        
        axis.text.y = element_text(size = 3),
        axis.title = element_blank(),
        
        panel.grid = element_blank()
    )

# plot 2 ----------------------

d0$year_cluster <- ifelse(
    d0$year <= 1960, "1950 - 1960",
    ifelse(
        d0$year <= 1970, "1961 - 1970",
        ifelse(
            d0$year <= 1980, "1971 - 1980",
            ifelse(
                d0$year <= 1990, "1981 - 1990",
                ifelse(
                    d0$year <= 2000, "1991 - 2000",
                    ifelse(
                        d0$year <= 2010, "2001 - 2010",
                        ifelse(
                            d0$year <= 2020, "2011 - 2020"
                        )
                    )
                )
            )
        )
    )
)


d1 <- d0[, by = .(year_cluster, is_democracy), .N]


library(packcircles)

d2 <- d1 |>
    split(by = "year_cluster") |>
    lapply(function(x) {
        
        packing <- x$N |>
            circleProgressiveLayout(sizetype = 'area') |> 
            circleLayoutVertices(npoints = 100) |>
            setDT()
        
        packing <- cbind(x[packing$id], packing)
        
        return(packing)
        
    }) |>
    rbindlist()



library(shadowtext)

ht <- d2[, by = .(year_cluster, is_democracy, N), .(x = mean(x), y = mean(y))]

gr2 <- d2 |> 
    ggplot(aes(x, y, group = id)) + 
    
    geom_vline(xintercept = 0, color = "grey", linetype = "dashed", lineend = "round", linewidth = .25) +
    
    geom_polygon(aes(fill = is_democracy, color = is_democracy), linewidth = .25) +
    
    geom_shadowtext(
        data = ht, inherit.aes = FALSE,
        aes(x, y, label = scales::comma(N), size = N, color = is_democracy), 
        family = "Anton", # color = "grey5",
        bg.r = .075, bg.color = "grey96"
    ) +
    
    scale_fill_manual(values = c('#93003a', '#00429d') |> lighten(.85)) +
    scale_color_manual(values = c('#93003a', '#00429d'), guide = "none") +
    
    scale_size_continuous(range = c(3, 5), guide = "none") +
    
    facet_wrap(vars(year_cluster), ncol = 1) +
    
    coord_fixed() +
    
    theme_minimal(base_family = my_font) +
    
    theme(
        legend.position = "none",
        
        panel.grid = element_blank(),
        panel.spacing = unit(.75, "lines"),
        
        axis.text = element_blank(),
        axis.title = element_blank(),
        
        strip.text = element_text(face = "bold")
    )


# patchwork ------------------------

library(patchwork)

mgr <- (gr1 | gr2) +
    plot_layout(widths = c(2, 1)) +
    plot_annotation(
        title = "Democracy and Dictatorship",
        
        subtitle = paste0(
            "**Number of <span style='color:#00429d'>democratic</span> and ",
            "<span style='color:#93003a'>non-democratic</span> countries from 1950 to 2020.**<br><br>",
            
            "*A country is defined as democratic, if elections were conducted, ",
            "these were free and fair,<br>and if there was a peaceful turnover of ",
            "legislative and executive offices following those elections.*"
        ),
        
        caption = paste0(
            "Bjørnskov, C., Rode, M. Regime types and regime change: A new dataset on democracy, ",
            "coups,<br>and political institutions. *Rev Int Organ* **15**, 531–551 (2020).",
            "*10.1007/s11558-019-09345-1*<br><br>",
            "Graphic: **Nikos Pechlivanis**"
        )
    ) &
    theme(
        plot.title    = element_text(face = "bold", size = 20, margin = margin(b = 5), family = "Jost", hjust = .5),
        plot.subtitle = element_markdown(margin = margin(b = 5), size = 10, color = "grey25", family = "Jost", hjust = .5),
        plot.caption  = element_markdown(margin = margin(t = 10), size = 8, family = "Jost", hjust = .5),
        
        plot.background = element_rect(fill = "#f8f2f9", color = NA),
        plot.margin = margin(10, 10, 10, 10)
    )


ggsave(
    plot = mgr, filename = "Rplot.png", dpi = 600,
    width = 6, height = 11, units = "in"
)















