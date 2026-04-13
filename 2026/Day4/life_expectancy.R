library(dplyr)
library(tidyr)
library(ggplot2)
library(janitor)
library(RColorBrewer)
library(ggrepel)
library(stringr)
library(ggtext)

life_exp <- read.csv("_GM-Life Expectancy- Dataset - v14 - data-for-countries-etc-by-year.csv") |> 
  clean_names() |> 
  dplyr::filter(life_expectancy != 0 & !is.na(life_expectancy)) 

life_exp_1925_2025 <- life_exp |> 
  dplyr::filter(time == 1925 | time == 2025) 

cambios <- life_exp_1925_2025 |>
  tidyr::pivot_wider(
    names_from = time,
    values_from = life_expectancy,
    names_prefix = "le_"
  ) |>
  tidyr::drop_na(le_1925, le_2025) |>
  dplyr::filter(le_1925 != 0, le_2025 != 0) |>
  dplyr::mutate(cambio_le = le_2025 - le_1925) |>
  arrange(cambio_le)

life_exp_plot_1925_2025_top5 <- cambios |> 
  slice_max(le_2025, n = 1)

life_exp_plot_1925_2025_low5 <- cambios |> 
  slice_min(le_2025, n = 1)

life_exp_plot_1925_2025_els <- cambios |> 
  dplyr::filter(name == "El Salvador")

#life_exp_plot_1925_2025_ca <- cambios |> 
  #dplyr::filter(name %in% c("Honduras", "Guatemala", "Costa Rica", "Nicaragua" ))

#life_exp_plot <- bind_rows(life_exp_plot_1925_2025_top5, life_exp_plot_1925_2025_low5, life_exp_plot_1925_2025_els, life_exp_plot_1925_2025_ca)
life_exp_plot <- bind_rows(life_exp_plot_1925_2025_top5, life_exp_plot_1925_2025_low5, life_exp_plot_1925_2025_els)


df_long <- life_exp_plot |>
  pivot_longer(
    cols = c(le_1925, le_2025),
    names_to = "año",
    values_to = "le"
  ) |>
  mutate(
    año = recode(año, le_1925 = "1925", le_2025 = "2025"),
    año = factor(año, levels = c("1925", "2025"))
  )

df_long_w <- df_long |> 
  dplyr::filter(name != "El Salvador")

df_long_els <- df_long |> 
  dplyr::filter(name == "El Salvador")

df_izq <- filter(df_long, año == "1925")
df_der <- filter(df_long, año == "2025")

orden <- unique(df_long$name)

# Identificar países dinámicos
pais_top <- df_der %>% filter(le == max(le)) %>% pull(name)
pais_bot <- df_der %>% filter(le == min(le)) %>% pull(name)

# Crear paleta dinámica
pal <- c("steelblue", "#1B9E77", "#D95F02")
names(pal) <- c("El Salvador", pais_top, pais_bot)


titulo <- "La esperanza de vida en <span style='color:steelblue;'>**El Salvador<br>aumentó 47 años**</span> en el último siglo"
subtitulo <- "Comparativa con los extremos globales en 2025. En paréntesis: incremento total desde 1925."


p <- ggplot(df_long_w, aes(x = factor(año), y = le, group = name, color = name)) +
  # Fondo (Gris - otros países)
  geom_line(data = life_exp_1925_2025,
            aes(x = factor(time), y = life_expectancy, group = name),
            color = "gray80", linewidth = 0.3, inherit.aes = FALSE) +
  geom_point(data = life_exp_1925_2025,
             aes(x = factor(time), y = life_expectancy),
             color = "gray80", size = 1, inherit.aes = FALSE) +
  # Mayor y menor esperanza de vida en 2025
  geom_line(linewidth = 1.0) + 
  geom_point(size = 2) +
  scale_color_manual(values = pal) +
  # El Salvador
  geom_line(data = df_long_els,
            aes(x = factor(año), y = le, group = 1), 
            color = "steelblue", linewidth = 2, inherit.aes = FALSE) +
  geom_point(data = df_long_els,
             aes(x = factor(año), y = le),
             color = "steelblue", size = 4, inherit.aes = FALSE) +
  scale_x_discrete(expand = expansion(add = c(1.4, 1.0)), position = "top") +
  # Etiquetas de la IZQUIERDA
  geom_text_repel(
    data = df_izq,
    aes(
      x = año, y = le,
      label = paste0(name, " (", le, ")"),
      color = name,
      fontface = ifelse(name == "El Salvador", "bold", "plain"),
      size = ifelse(name == "El Salvador", 7, 6)
    ),
    hjust = 1, nudge_x = -0.2, direction = "y",
    inherit.aes = FALSE
  ) +
  # Etiquetas de la DERECHA
  geom_text_repel(
    data = df_der,
    aes(
      x = año, y = le,
      label = paste0(le, " (", round(cambio_le, 1), ")"),
      color = name,
      fontface = ifelse(name == "El Salvador", "bold", "plain"),
      size = ifelse(name == "El Salvador", 7, 6)
    ),
    hjust = 0, nudge_x = 0.2, direction = "y",
    inherit.aes = FALSE
  ) +
  scale_color_manual(values = pal) +
  scale_size_identity() +
  scale_color_manual(values = pal) +
  labs(
    title    = str_wrap(titulo, width = 45),
    subtitle = str_wrap(subtitulo, width = 45),
    y        = "esperanza de vida (años)",
    x        = NULL,
    caption  = "\n#30DayChartChallenge, Day 4: Slope\nMario Reyes\nFuente: Gapminder"
  ) +
  theme_minimal(22) +
  theme(
    plot.title = element_markdown(
      face = "bold",
      lineheight = 1.1,
      hjust = 0, 
      margin     = margin(t = 10, r = 30, b = 10, l = 0)),
    plot.subtitle = element_text(
      hjust = 0, 
      margin = margin(b = 30)
    ),
    axis.title.y = element_text(
      margin = margin(r = 10)
    ),
    plot.margin = margin(b= 20, r = 20, l = 20),
    plot.caption       = element_text(hjust = 1),
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank(),
    legend.position    = "none",
    axis.text.x        = element_text(face = "bold",
                                      size = 20),  
    axis.ticks.x       = element_blank(),
    axis.text.x.top    = element_text(face = "bold"),  
    axis.ticks.length  = unit(0, "pt"),
    plot.title.position = "plot"
  ) +
  theme(legend.position = "none") +
  coord_cartesian(clip = "off")

ggsave("slope_chart.png", p, width = 7, height = 10, dpi = 300)
