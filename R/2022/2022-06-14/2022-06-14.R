# Get the Data

# Read in with tidytuesdayR package 
# Install from CRAN via: install.packages("tidytuesdayR")
# This loads the readme and all the datasets for the week of interest

# Either ISO-8601 date or year/week works!


rm(list = ls())
gc()

tuesdata = tidytuesdayR::tt_load('2022-06-14')

# Or read in the data manually

drought      = tuesdata$drought
drought_fips = tuesdata$`drought-fips`

rm(tuesdata)

library(data.table)
library(stringr)

library(ggplot2)
library(ggHoriPlot) 

library(tidyverse)

drought      = setDT(drought)
drought_fips = setDT(drought_fips)


drought$DATE = str_remove(drought$DATE, "d_")
drought$DATE = lubridate::ymd(drought$DATE)

drought$state = str_to_title(
    str_replace_all(
        drought$state, "-", " "
    )
)

# df1 = drought %>%
#     mutate(DATE=str_remove(DATE,"d_"),
#            date=lubridate::ymd(DATE),
#            year=lubridate::year(date)) %>%
#     # filter(year>=2010) %>%
#     mutate(state=str_to_title(gsub("-", " ", state)),
#            st = state.abb[match(state,state.name)])
# 
# 
# df1a = df1 %>% select(state, st, date, 3:7) %>%
#     pivot_longer(!1:3)
# 
# df1b = df1 %>% select(state, st, date, 9:13) %>%
#     pivot_longer(!1:3) %>%
#     mutate(value=value*-1) 



# tmp = drought[which(drought$state == "Alabama"), ]

tmp = drought[which(drought$DATE > "2010-01-01"), ]

ggplot(tmp) + 
    
    geom_area(aes(x = DATE, y = D0), fill = "red4", alpha = 0.1) +
    geom_area(aes(x = DATE, y = D1), fill = "red4", alpha = 0.2) +
    geom_area(aes(x = DATE, y = D2), fill = "red4", alpha = 0.3) +
    geom_area(aes(x = DATE, y = D3), fill = "red4", alpha = 0.4) +
    geom_area(aes(x = DATE, y = D4), fill = "red4", alpha = 0.5) +
    
    
    geom_area(aes(x = DATE, y = W0), fill = "blue4", alpha = 0.1) +
    geom_area(aes(x = DATE, y = W1), fill = "blue4", alpha = 0.2) +
    geom_area(aes(x = DATE, y = W2), fill = "blue4", alpha = 0.3) +
    geom_area(aes(x = DATE, y = W3), fill = "blue4", alpha = 0.4) +
    geom_area(aes(x = DATE, y = W4), fill = "blue4", alpha = 0.5) +
    
    scale_y_continuous(expand = c(0, 0 )) +
    scale_x_date(expand = c(0, 0)) +
    
    facet_wrap(vars(state), ncol = 1, strip.position = "left") +
    
    theme_minimal() +
    
    theme(
        panel.grid = element_blank(),
        
        axis.title = element_blank(),
        axis.text.y = element_blank()
    )













ggplot(data = drought_fips) +
    
    geom_horizon(aes(x = date, y = DSCI), origin = "min", horizonscale = 5) +
    
    facet_wrap(vars(State), ncol = 1, strip.position = "left")


tmp = drought_fips[which(drought_fips$FIPS == "02013"),] 

ggplot(data = tmp) + 
    
    geom_line(aes(x = date, y = DSCI))



utils::data(sports_time)

sports_time %>% ggplot() +
    geom_horizon(aes(time/60, p), origin = 'min', horizonscale = 4) +
    facet_wrap(~activity, ncol = 1, strip.position = 'right') +
    scale_fill_hcl(palette = 'Peach', reverse = T) +
    theme(
        panel.spacing.y=unit(0, "lines"),
        strip.text.y = element_text(angle = 0),
        legend.position = 'none',
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank()
    ) +
    scale_x_continuous(
        name = 'Time',
        breaks=seq(from = 3, to = 27, by = 3),
        labels = function(x) {sprintf("%02d:00", as.integer(x %% 24))}) +
    ggtitle('Peak time of day for sports and leisure')






