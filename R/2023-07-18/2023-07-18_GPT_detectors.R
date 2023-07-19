


rm(list = ls())
gc()


library(data.table)
library(stringr)

library(ggplot2)
library(ggrepel)
library(ggforce)
library(ggtext)

library(ggnewscale)

library(paletteer)
library(extrafont)

library(packcircles)


detectors <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-07-18/detectors.csv')

detectors$native = ifelse(detectors$native == "Yes", TRUE, FALSE)

# detectors$label = ifelse(detectors$kind == detectors$.pred_class, "yes", "no")


df = detectors[, by = .(detector, kind, `.pred_class`, native), .N]
df = df[, by = .(detector, kind, `.pred_class`), c("Freq", "N2") := list(N / sum(N), sum(N))]

df$kind = df$kind |> factor(levels = c("Human", "AI"))
df$.pred_class = df$.pred_class |> factor(levels = c("AI", "Human"))

# df$N2 = df$N / max(df$N)

max_value = max(df$N2)


df$class = paste0(df$kind, " - ", df$.pred_class)

df = df |> split(df$class)

packing = lapply(df, function(x) {
    
    x = x[order(detector)]
    
    radius = x[, c("detector", "N2"), with = FALSE] |> unique()
    
    out = circleProgressiveLayout( (radius$N2 / max_value) / 20 )
    out = setDT(out)
    
    index = match(x$detector, radius$detector)
    
    x$x0 = out[index]$x + x$kind |> as.numeric()
    x$y0 = out[index]$y + x$`.pred_class` |> as.numeric()
    x$r  = out[index]$radius
    
    return(x)
})

packing <- rbindlist(packing)


lbl_data = packing[, c("detector", "kind", ".pred_class", "N2", "x0", "y0", "r"), with = FALSE] |>
    unique()

lbl_data$label = paste0(
    "**", lbl_data$detector, "**<br>",
    lbl_data$N2, " pred."
)

my_font = "Jost"


library(gggibbous)

gr = ggplot() +
    
    geom_point(
        data = packing[which(is.na(native))],
        aes(x = x0, y = y0, size = r), fill = "#00A087",
        color = "white", shape = 21, stroke = .25
    ) +
    
    geom_moon(
        data = packing[which(!is.na(native))],
        aes(x0, y0, ratio = Freq, right = native, fill = native, size = r),
        color = "white", stroke = .25
    ) +
    
    scale_size_continuous(range = c(10, 25)) +
    
    new_scale("size") +

    # geom_circle(aes(x0 = x0, y0 = y0, r = r, fill = detector),
    #             color = "white", linewidth = .25) +
    # 
    geom_richtext(
        data = lbl_data,
        aes(x = x0, y = y0, label = label, size = r),
        label.size = NA, fill = alpha("white", alpha = .65),
        label.padding = unit(c(.15, .15, .15, .15), "lines"),
        family = my_font
        # max.overlaps = Inf,
        # min.segment.length = 0,
        # segment.linetype = "dotted",
        # segment.size = .5

    ) +

    scale_size_continuous(range = c(.75, 3)) +
    
    # coord_equal(xlim = c(0.5, 2.5), ylim = c(0.5, 2.5)) +
    
    geom_vline(xintercept = 1.5, linetype = "dashed", linewidth = .3) +
    geom_hline(yintercept = 1.5, linetype = "dashed", linewidth = .3) +
    
    scale_x_continuous(breaks = c(1, 2), labels = c("Human", "AI"), position = "top") +
    scale_y_continuous(breaks = c(1, 2), labels = c("AI", "Human")) +
    
    scale_fill_manual(
        values = paletteer_d("ggsci::nrc_npg"),
        guide = guide_legend(
            # title = "Detector used to generate the predictions",
            title.position = "top",
            title.theme = element_text(face = "bold", family = my_font, size = 10),
            nrow = 1
        )
    ) +
    
    theme_minimal(base_family = my_font) +
    
    theme(
        legend.position = "none",
        # legend.margin = margin(t = 10),
        
        panel.grid = element_blank(),
        
        axis.title.y = element_text(face = "bold", margin = margin(r = 10)),
        axis.title.x.top = element_markdown(face = "bold", margin = margin(b = 10)),
        
        axis.text.x.top = element_text(size = 10, margin = margin(b = 5)),
        axis.text.y = element_text(size = 10, margin = margin(r = 5)),
        
        plot.title    = element_text(face = "bold", size = 32, family = my_font),
        plot.subtitle = element_markdown(margin = margin(b = 15), size = 10, color = "grey20", family = my_font),
        plot.caption  = element_markdown(margin = margin(t = 15), size = 6, family = my_font),
        
        plot.title.position = "plot",
        plot.caption.position = "plot",
        
        plot.background = element_rect(fill = "#f5f5f5", color = NA),
        plot.margin = margin(20, 20, 20, 20)
    ) +
    
    labs(
        x = 'Whether the essay was written by a *Human* or *AI*',
        y = "The GPT detector's class prediction",
        
        title = 'GPT detectors',
        
        subtitle = paste0(
            "Comparing detector predictions between papers authored by ",
            "<b style='color:#4DBBD5FF'>native</b> English writers and those ",
            "by <b style='color:#E64B35FF'>non-native</b> English writers.",
            "<br>GPT detectors exhibit a significant bias, misclassifying genuine ",
            "writing from non-native English writers as ",
            "<b style='color:#00A087'>AI-generated</b>."
        ),
        
        caption = paste0(
            "Source: <b>detectors R package</b> | ",
            "Graphic: <b>Nikolaos Pechlivanis</b>"
        )
    )


ggsave(
    plot = gr, filename = "Rplot.jpeg", dpi = 600,
    width = 10, height = 10, units = "in"
)





