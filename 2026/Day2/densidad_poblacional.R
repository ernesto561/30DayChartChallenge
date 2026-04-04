library(tidyverse)
library(janitor)
library(showtext)
library(sf)
library(ggtext)

font_add(family = "fa-solid", regular = "fa-solid-900.woff2")
font_add_google("Roboto", "roboto")
showtext_auto()

showtext_opts(dpi = 300) 


theme_day2 <- function(base_size = 26, base_family = "roboto") {
  theme_minimal(base_size = base_size, base_family = base_family) %+replace% 
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
        size = rel(0.8), 
        margin = margin(b = 10)
      ),
      axis.title.y = element_text(
        size = rel(0.8), 
        angle = 90, 
        margin = margin(r = 10)),
      plot.caption = element_text(
        size = rel(0.7), 
        hjust = 1, 
        margin = margin(t = 15)
      ),
      panel.grid = element_blank(),
      axis.line.x = element_line(color = "grey80"),
      axis.line.y = element_line(color = "grey80"),
      axis.ticks.x.top = element_line(color = "grey80"),
      plot.margin = margin(t = 20, r = 40, b = 20, l = 20)
    )
}

poblacion <- read.csv("../../../censo_2024/Bases-Finales-CPV2024SV-CSV/Base de Datos de Población - CPV 2024 SV.csv")
distritos <- read_sf("LÍMITES_MUNICIPALES.geojson") |> 
  mutate(area_km2 = AREA * 0.000001)

codigos_distritos <- read.csv("catalogo-de-municipios-y-distritos.csv", 
                              fileEncoding = "latin1",
                              sep = ";") |> 
  clean_names()


dens_dist <- poblacion |> 
  group_by(DISTODESC) |> 
  summarise(n = n(), .groups = "drop") |> 
  drop_na() |> 
  mutate(
    DISTODESC = recode_values(DISTODESC, 
                              from = codigos_distritos$codigo_distritos, 
                              to = codigos_distritos$distritos),
    DISTODESC = recode(DISTODESC, 
                       "Santa Tecla antes: Nueva San Salvador" = "Santa Tecla")
  ) |> 
  left_join(
    distritos |> distinct(NAM, .keep_all = TRUE), 
    by = join_by(DISTODESC == NAM)
  ) |> 
  mutate(dens = n / area_km2) |> 
  slice_max(order_by = dens, n = 10) |> 
  arrange(desc(dens))


ggplot(df_icons, aes(x = x_pos, y = reorder(DISTODESC, dens))) +
  scale_x_continuous(
    position = "top",
    limits = function(x) c(0, ceiling(max(x) / 2) * 2),
    breaks = function(x) seq(0, ceiling(max(x) / 2) * 2, by = 2),
    expand = expansion(mult = c(0, 0.08)) 
  ) +
  geom_text(label = "\uf183", # Código de 'person-standing'
            family = "fa-solid", 
            size = 10, 
            color = "steelblue") +
  geom_text(aes(color = "= 1,000 hab/km²"), 
            label = "\uf183", 
            family = "fa-solid", 
            size = 10) +
  
  # Definimos el color manualmente para que coincida con tu diseño
  scale_color_manual(name = NULL, values = c("= 1,000 hab/km²" = "steelblue")) +
  
  # IMPORTANTE: Configurar la guía para que muestre el ícono en la leyenda
  guides(
    color = guide_legend(override.aes = list(label = "\uf183", family = "fa-solid", size = 8))
  ) +
  labs(
    title = "Cuscatancingo tiene la mayor densidad poblacional de El Salvador",
    subtitle = "El distrito de Cuscatancingo tiene 10,501 hab/km². Datos Censo 2024, Banco Central de Reserva.\nSe muestran los 10 distritos con mayor densidad poblacional.",
    x = "Densidad poblacional (miles hab/km²)",
    y = "Distrito", 
    caption = "\n#30DayChartChallenge, Day 2\nPictogram\nMario Reyes"
  ) +
  theme_day2()

ggsave("dens_pop_dist.png", width = 15, height = 10, dpi = 300)