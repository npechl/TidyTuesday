


rm(list = ls())
gc()


library(data.table)
library(stringr)

library(tidycensus)

state_retail   <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-12-13/state_retail.csv')
coverage_codes <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-12-13/coverage_codes.csv')

coverage_codes$coverage = str_remove_all(coverage_codes$coverage, "\\.")


state_retail$month.chr = month.abb[state_retail$month]


state_retail$month = as.character(state_retail$month)
state_retail$month = ifelse(
    str_length(state_retail$month) == 1,
    paste0("0", state_retail$month),
    state_retail$month
)

state_retail$date = paste0(state_retail$year, "-", state_retail$month, "-01")

state_retail$date = as.Date(state_retail$date)

library(ggplot2)
library(ggtext)
library(ggh4x)
library(ggsci)

library(showtext)

font_add_google("Lora")

showtext_auto()

df = state_retail # [which(subsector == "total"), ]

df$month.chr = factor(df$month.chr, levels = month.abb)

df$change_yoy = as.numeric(df$change_yoy)

df = df[which(!is.na(change_yoy)), ]

df = merge(df, coverage_codes, by = "coverage_code")
df$coverage_id = paste0(
    df$coverage_code, ": ",
    df$coverage
)

summary(df$change_yoy)

df = df[which(change_yoy >= -100 & change_yoy <= 100), ]

df$change_yoy = df$change_yoy / 100

gr = ggplot(data = df, aes(x = date, y = change_yoy)) +
    
    geom_hline(
        yintercept = 0, linewidth = .3, linetype = "dashed", color = "gray20"
    ) +
    
    geom_point(
        aes(color = coverage_id), alpha = .5, size = 1
        # position = position_jitternormal(sd_y = 0, sd_x = 0.05)
    ) +
    
    facet_wrap2(vars(subsector), axes = "x") +
    
    scale_y_continuous(
        expand = c(0, 0), 
        labels = scales::percent,
        limits = c(-1, 1),
        breaks = c(-1, -.75, -.5, -.25, 0, .25, .5, .75, 1)
    ) +
    
    scale_color_npg(
        guide = guide_legend(
            ncol = 1,
            override.aes = list(size = 2, alpha = 1)
        )
    ) +
    
    coord_cartesian(expand = TRUE, clip = "off") +
    
    theme_minimal(base_family = "Lora") +
    
    theme(
        legend.position = "bottom",
        legend.justification = "left",
        legend.title = element_blank(),
        
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(linewidth = .3, linetype = "dashed", color = "#c6c6d2"),
        
        panel.spacing = unit(1, "lines"),
        
        # axis.ticks = element_line(linewidth = .3),
        
        axis.text = element_text(size = 8),
        
        axis.title.x = element_blank(),
        
        strip.text = element_text(face = "bold", margin = margin(b = 10)),
        
        plot.title.position = "plot",
        plot.caption.position = "plot",
        
        plot.title = element_text(face = "bold", size = 16),
        plot.subtitle = element_markdown(margin = margin(b = 15), size = 10),
        plot.caption = element_markdown(margin = margin(t = 10), size = 8),
        
        plot.background = element_rect(fill = "#f1f1f4", color = NA),
        
        plot.margin = margin(20, 20, 20, 20)
    ) +
    
    labs(
        y = "Percent Change in Retail Sales",
        
        title = 'Monthly State Retail Sales',
        
        subtitle = paste0(
            "The Monthly State Retail Sales (MSRS) is the Census Bureau's new ",
            "experimental data product featuring modeled state-level retail ",
            "sales.<br><b>The graph summarizes the monthly percent change in retail ",
            "sales across every state in America.</b>"
        ),
        
        caption = paste0(
            "Source: <b>United States Census Bureau's Monthly State Retail Sales</b> | ",
            "Graphic: <b>Nikolaos Pechlivanis</b>"
        )
    )


ggsave(
    plot = gr, filename = "Rplot.pdf",
    width = 14, height = 10, units = "in",
    device = cairo_pdf
)

# ggsave(
#     plot = gr, filename = "Rplot.jpeg",
#     width = 14, height = 10, units = "in"
# )

library(magick)

tmp = image_read_pdf("Rplot.pdf", density = 500)
image_write(tmp, "Rplot.jpeg")











