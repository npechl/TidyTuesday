


rm(list = ls())
gc()

library(data.table)
library(stringr)

library(ggplot2)
library(ggtext)

library(extrafont)
library(paletteer)

grant_opportunity_details <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-10-03/grant_opportunity_details.csv')
grants                    <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-10-03/grants.csv')


# grant_opportunity_details |> head()
# 
# grants$opportunity_id |> duplicated() |> any()

df = grants[which(estimated_funding != 0)]

df$estimated_funding = df$estimated_funding |> as.numeric()
df$expected_number_of_awards = df$expected_number_of_awards |> as.numeric()


df2 = grant_opportunity_details[, c(
    "opportunity_id",
    "category_agriculture",                                               
    "category_arts",                             
    "category_business",                                                  
    "category_community_development",                                     
    "category_consumer_protection",                                       
    "category_disaster",                                                  
    "category_education",                                                 
    "category_employment",                                               
    "category_energy",                                                    
    "category_environment",                                               
    "category_food",                                                      
    "category_health",                                                    
    "category_housing",                                                   
    "category_humanities",                                                
    "category_iija",                                                      
    "category_income_security",                                           
    "category_info",                                                      
    "category_law",                                                       
    "category_natural_resources",                                         
    "category_opportunity_zone",                                          
    "category_regional_development",                                      
    "category_science",                                                   
    "category_transportation",                                            
    "category_other"
), with = FALSE] |>
    
    melt(id.vars = "opportunity_id", variable.name = "category", variable.factor = FALSE)

df2$category = df2$category |> 
    str_remove_all("category_") |>
    
    str_replace_all("business", "Business and Commerce") |>
    str_replace_all("disaster", "Disaster Prevention and Relief") |>
    str_replace_all("employment", "Employment, Labor and Training") |>
    str_replace_all("food", "Food and Nutrition") |>
    str_replace_all("iija", "Infrastructure Investment and Jobs Act") |>
    str_replace_all("income_security", "Income Security and Social Services") |>
    str_replace_all("info", "Information and Statistics") |>
    str_replace_all("law", "Law, Justice and Legal Services") |>
    str_replace_all("opportunity_zone", "Opportunity Zone Benefits") |>
    str_replace_all("science", "Science and Technology\nResearch and Development") |>
    str_replace_all("info", "Information and Statistics") |>
    
    str_replace_all("_", "\\ ") |>
    str_to_title() |>
    str_replace_all("And", "and")
    

df2 = df2[which(value)]

index = match(df2$opportunity_id, df$opportunity_id)

df2$estimated_funding = df[index, ]$estimated_funding





df2 = df2[, by = category, .(
    N = opportunity_id |> unique() |> length(),
    funding = estimated_funding |> mean(na.rm = TRUE)
)]

    
df2 = df2[order(N)]

df2$category = df2$category |> 
    str_replace_all("and", "&")



library(ggrepel)

my_font = "Jost"

gr = ggplot(data = df2) +
    
    geom_point(
        aes(x = funding, y = N), 
        shape = 21, size = 3, stroke = .25, 
        color = "white", fill = alpha("#c62857", alpha = .95)
    ) +
    
    geom_text_repel(
        aes(x = funding, y = N, label = category), size = 3,
        family = my_font, segment.size = .25, box.padding = .35
    ) +
    
    scale_x_continuous(trans = "log10", labels = scales::dollar_format(suffix = " M", scale = 1e-6)) +
    scale_y_continuous(trans = "log10") +
    
    theme_minimal(base_family = my_font) +
    
    theme(
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = "grey85", linetype = "dashed"),
        
        axis.title.x = element_text(margin = margin(t = 5)),
        axis.title.y = element_text(margin = margin(r = 5)),
        
        plot.title    = element_text(face = "bold", size = 36, margin = margin(b = 5), family = my_font),
        plot.subtitle = element_markdown(margin = margin(b = 20), size = 11, color = "grey20", family = my_font),
        plot.caption  = element_markdown(margin = margin(t = 20), size = 8, family = my_font),
        
        plot.title.position = "plot",
        plot.caption.position = "plot",
        
        plot.background = element_rect(fill = "#f5f5f5", color = NA),
        plot.margin = margin(20, 20, 20, 20)
    ) +
    
    labs(
        title = 'US Government Grant Opportunities',
        
        x = "Average funding amount in dollars",
        y = "Number of US grant opportunities",
        
        subtitle = paste0(
            "Total **number of grant opportunities** in the United States and **average funding per category**"
        ),
        
        caption = paste0(
            "Source: <b>Grants.gov</b> | ",
            "Graphic: <b>Nikolaos Pechlivanis</b>"
        )
    )




ggsave(
    plot = gr, filename = "Rplot.jpeg",
    width = 10, height = 10, units = "in", dpi = 600
)



