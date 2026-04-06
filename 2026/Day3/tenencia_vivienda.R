library(tidyverse)
library(marimekko)
library(showtext)
library(ggtext)

font_add_google("Roboto", "roboto")
showtext_auto()

showtext_opts(dpi = 300) 

theme_marimekko_mod <- function(base_size = 22, base_family = "roboto") {
  list(
    theme_marimekko(base_size = base_size, base_family = base_family),
    theme(
      plot.title.position = "plot",
      plot.title = element_markdown(
        face = "bold",
        size = rel(1.2),
        hjust = 0,
        margin = margin(b = 10)
      ),
      plot.subtitle = element_text(
        size = rel(0.9),
        hjust = 0,
        margin = margin(b = 20)
      ),
      legend.position = "top",
      legend.location = "plot",
      legend.justification = "left",
      legend.title = element_text(size = rel(0.8)),
      legend.text = element_text(size = rel(0.8)),
      legend.margin = margin(l = 0, b = 15),
      axis.title.x.top = element_text(
        size = rel(1),
        margin = margin(b = 10)
      ),
      plot.caption = element_text(
        size = rel(0.7),
        hjust = 1,
        margin = margin(t = 15)
      ),
      panel.grid = element_blank()
    )
  )
}

vivienda <- read.csv("../../../censo_2024/Bases-Finales-CPV2024SV-CSV/Base de Datos de Viviendas - CPV 2024 SV.csv")

recode_tenencia <- c(
  "1" = "Propia (pagada)",
  "2" = "Propia (pagándose)",
  "3" = "Alquilada",
  "4" = "Prestada",
  "5" = "Prestada por trabajo",
  "6" = "Otros",
  "9" = "NS/NR"
)

vivienda_mp <- vivienda %>%
  mutate(
    tenencia = factor(V07_VIV_TENENCIA),
    area = factor(AREA)
  ) |> 
  mutate(
    tenencia = recode(tenencia, !!!recode_tenencia),
    area = recode(area,
                          `1` = "Urbana",
                          `2` = "Rural")
    ) |>
  drop_na(tenencia)


pesos_area <- vivienda_mp |>
  count(area) |>
  mutate(pct = n / sum(n),
         label = paste0(area, "\n(", round(pct * 100, 1), "%)"))

# Gráfico base
p <- ggplot(vivienda_mp) +
  geom_marimekko(aes(fill = tenencia),
                 formula = ~ area | tenencia,
                 gap = 0.02,
                 show.legend = FALSE)

p +
  geom_marimekko_text(
    aes(
      label = after_stat(
        ifelse(.proportion > 0.05,
               paste0(round(.proportion * 100, 1), "%"),
               "")
      )
    ),
    colour   = "white",
    fontface = "bold",
    size     = 5,
    na.rm    = TRUE
  ) +
  scale_x_continuous(
    breaks = layer_data(p, 1) |>
      distinct(area, x) |>
      pull(x),
    labels = pesos_area$label
  ) +
  labs(
    title    = "El porcentaje de vivienda propia en El Salvador es mayor en el área rural",
    subtitle = "El 68% de viviendas rurales son propias (pagadas o en proceso) vs. 64.5% en zonas urbanas.\nAdemás, 7 de cada 10 viviendas del país se encuentran en la zona urbana.\nFuente: Censo de Población y Vivienda 2024, Banco Central de Reserva.\nNo se muestran porcentajes menores a 5%.",
    caption  = "\n#30DayChartChallenge, Day 3\nMosaic\nMario Reyes",
    x        = "Área",
    y        = "Tenencia"
  ) +
  theme_marimekko_mod(base_size = 21)
  
ggsave("tenencia_vivienda.png", width = 14, height = 10, dpi = 300)
