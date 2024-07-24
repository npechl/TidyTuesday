


rm(list = ls())
gc()

library(data.table)
library(stringr)

childcare_costs <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-05-09/childcare_costs.csv')
counties        <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-05-09/counties.csv')

df = merge(childcare_costs, counties, by = "county_fips_code")

df = df[order(study_year, county_fips_code)]

rm(childcare_costs, counties)


library(ggplot2)

library(maps)
library(mapdata)

library(ggtext)
library(extrafont)

us <- map_data('county')
us = setDT(us)


fips = county.fips
fips = fips |> setDT()

fips$polyname = str_split(fips$polyname, "\\:", simplify = TRUE)[,1]
fips = unique(fips)

df = merge(df, fips, by.x = "county_fips_code", by.y = "fips")

us$polyname = paste0(us$region, ",", us$subregion)

all(df$polyname %in% us$polyname)

df = merge(df, us, by = "polyname", all = TRUE, allow.cartesian = TRUE)

rm(fips, us)
gc()

df = df[, c(
    "polyname", 
    "county_fips_code", 
    "study_year", 
    "county_name",
    "state_name",
    "state_abbreviation",
    "long",
    "lat",
    "group",
    "order",
    "region",
    "subregion",
    "emp_m",
    "emp_service",
    "emp_sales",
    "emp_n",
    "emp_p"
), with = FALSE]

df = melt(
    df, 
    id.vars = c(
        "polyname", 
        "county_fips_code", 
        "study_year", 
        "county_name",
        "state_name",
        "state_abbreviation",
        "long",
        "lat",
        "group",
        "order",
        "region",
        "subregion"
    ),
    
    variable.factor = FALSE, value.factor = FALSE
)

df = df[which(!(is.na(study_year)))]

df$variable = df$variable |>
    
    str_replace_all("emp_m", "% of civilians employed in<br>**management**, **business**, <br>**science**, and **arts occupations**") |>
    str_replace_all("emp_service", "% of civilians employed in<br>**service occupations**") |>
    str_replace_all("emp_sales", "% of civilians employed in<br>**sales and office occupations**") |>
    str_replace_all("emp_n", "% of civilians employed in<br>**natural resources**,<br>**construction**, and<br>**maintenance occupations**") |>
    str_replace_all("emp_p", "% of civilians employed in<br>**production**, **transportation**,<br>and **material moving<br>occupations**")
    
df$value = df$value / 100

gr = ggplot(data = df, 
       aes(x = long, y = lat, fill = value, group = group)) + 
    
    geom_polygon(color = "grey75", linewidth = .01) + 
    
    scale_fill_steps2(
        labels = scales::percent_format(),
        n.breaks = 6,
        limits = c(0, 1),
        # colors = c("#006666", "#33FFFF", "#CCFFFF", "#FF6E00", "#662700"),
        low = "#006666", mid = "#b3b300", high = "#660000", midpoint = .5,
        guide = guide_colorsteps(title.position = "top")
    ) +
    
    facet_grid(
        rows = vars(variable),
        cols = vars(study_year),
        switch = "y",
    ) +
    
    coord_map(clip = "off") +
    
    theme_void(base_family = "Zilla Slab") +
    
    theme(
        legend.position = "bottom",
        # legend.direction = "left",
        
        strip.clip = "off",
        panel.spacing = unit(1, "lines"),
        
        strip.text.y.left = element_markdown(hjust = 1),
        strip.text.x.top = element_text(face = "bold"),
        
        legend.key.width = unit(2.5, "lines"),
        legend.key.height = unit(0.6, "lines"),
        
        plot.title    = element_text(face = "bold", size = 26, family = "Zilla Slab"),
        plot.subtitle = element_markdown(margin = margin(b = 10), size = 10, color = "grey20", family = "Zilla Slab"),
        plot.caption  = element_markdown(margin = margin(t = 10), size = 6, family = "Zilla Slab"),
        
        plot.background = element_rect(color = NA, fill = "#ffeff7"),
        plot.margin = margin(20, 20, 20, 20)
    ) +
    
    labs(
        fill = "Percentage of civilians",
        title = 'Childcare Costs',
        subtitle = paste0(
            "The percentage of civilians working in various occupations changed between 2008 and 2018"
        ),
        caption = paste0(
            "Source: <b>National Database of Childcare Prices</b> | ",
            "Graphic: <b>Nikolaos Pechlivanis</b>"
        )
    )



ggsave(
    plot = gr, filename = "Rplot.jpeg",
    width = 16, height = 6, units = "in", dpi = 1200
)
