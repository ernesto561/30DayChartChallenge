library(tidyverse)


hogares <- read.csv("../../../censo_2024/Bases-Finales-CPV2024SV-CSV/Base de Datos de Hogares - CPV 2024 SV.csv")

recode_deptos <- c(
  "1" = "Ahuachapán",
  "2" = "Santa Ana",
  "3" = "Sonsonate",
  "4" = "Chalatenango",
  "5" = "La Libertad",
  "6" = "San Salvador",
  "7" = "Cuscatlán",
  "8" = "La Paz",
  "9" = "Cabañas",
  "10" = "San Vicente",
  "11" = "Usulután",
  "12" = "San Miguel",
  "13" = "Morazán",
  "14" = "La Unión"
)

ac_dep <- hogares |> 
  group_by(DEPTO, H02_5_HOG_AC) |> 
  summarise(n = n(), .groups = "drop") |> 
  drop_na() |> 
  mutate(
    DEPTO = recode(DEPTO, !!!recode_deptos),
    H02_5_HOG_AC = recode(H02_5_HOG_AC,
                          `1` = "Sí",
                          `2` = "No",
                          `9` = "NS/NR"
    ),
    H02_5_HOG_AC = factor(H02_5_HOG_AC,
                          levels = c("Sí", "No", "NS/NR")
    )
  )



orden_deptos <- ac_dep |> 
  group_by(DEPTO) |> 
  mutate(prop = n / sum(n)) |> 
  filter(H02_5_HOG_AC == "Sí") |> 
  arrange(desc(prop)) |> 
  pull(DEPTO)


ggplot(ac_dep, aes(
  fill = H02_5_HOG_AC,
  y = n,
  x = factor(DEPTO, levels = rev(orden_deptos))
)) +
  geom_bar(position = position_fill(reverse = TRUE), stat = "identity") +
  coord_flip() + 
  scale_y_continuous(
    expand = c(0,0), 
    breaks = c(0, 0.25, 0.50, 0.75, 1), 
    labels = c("0", "25%", "50%", "75%", "100%")) +
  theme_bw(16) +
  labs(x = "Departamento", y = "Porcentaje de hogares", 
       fill = "¿Posee aire acondicionado?", 
       title = "San Miguel tiene el mayor porcentaje de hogares con aire acondicionado") +
  theme(plot.title.position = 'plot')


ggsave("aire_acondicionado_censo_2024.png", width = 10, height = 8)