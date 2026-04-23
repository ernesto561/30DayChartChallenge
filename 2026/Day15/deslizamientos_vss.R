library(tidyverse)
library(ggrepel)
library(ggtext)
library(ragg)

Sys.setenv(TZ='America/El_Salvador')
data_total2 <- read.csv("deslizamientos_vss.csv")

lbl_spa <- c("deslizamiento", "no deslizamiento")
col <- c("landslide" = "red", "no landslide" = "blue")
siz <- c("landslide" = 5, "no landslide" = 3)

id_plot <- function(data, title, subtitle, lbl, name) { 
  ggplot(data %>% arrange(tipo, desc(event)), 
         aes(duration, E, color=event, fill=event, size=event, label=date2)) +
    geom_point() +
    geom_text_repel(box.padding = 2, max.overlaps = Inf, 
                    min.segment.length = 0, size=5, color="red") +
    scale_fill_manual(values = col, labels = lbl, name = name,
                      guide = guide_legend(title.position = "top")) +
    scale_color_manual(values = col, labels = lbl, name = name) +
    scale_size_manual(values = siz, labels = lbl, name = name) +
    scale_x_log10(limits = c(1,200)) +
    scale_y_log10(limits = c(1,1000)) +
    labs(x = "Duración (h)", y = "Lluvia acumulada (mm)", 
         title = title, subtitle = subtitle,
         caption = "\n#30DayChartChallenge, Day 15: Correlation\nMario Reyes\nFuente: MARN") +
    theme_bw(base_family = "Roboto", base_size = 22) +
    theme(plot.title = element_markdown(face = "bold"),
          panel.grid.major.x = element_line(color = "darkgray", linewidth = 0.5),
          panel.grid.major.y = element_line(color = "darkgray", linewidth = 0.5),
          axis.text.x = element_text(vjust = -2),
          axis.title.x = element_text(vjust = -2), 
          plot.title.position = 'plot',
          legend.margin = margin(0, 0, 0, 0),
          legend.position = 'top',
          legend.location = 'plot', 
          legend.justification.top = "left",
          plot.margin = margin(t=20, b=20, l=20, r=20)) +
    coord_cartesian(clip = "off")
}

p3 <- id_plot(data_total2 %>% filter(I_type=="mean"), 
              "Los <span style='color:red'>deslizamientos</span> en el Volcán de San Salvador ocurren<br>con lluvias extremas", 
              "Todos los deslizamientos registrados ocurrieron con lluvias acumuladas\nsuperiores a 100 mm", 
              lbl_spa, "Evento de lluvia")

agg_png("deslizamientos_vss.png", width=10.5, height=12, units="in", res=300)
print(p3)
dev.off()