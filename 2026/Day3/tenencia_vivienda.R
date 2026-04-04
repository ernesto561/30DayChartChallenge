library(tidyverse)
library(ggmosaic)

vivienda <- read.csv("../../../censo_2024/Bases-Finales-CPV2024SV-CSV/Base de Datos de Viviendas - CPV 2024 SV.csv")

vivienda_mp <- vivienda %>%
  mutate(
    tenencia = factor(V07_VIV_TENENCIA),
    area = factor(AREA)
  ) |> 
  drop_na(tenencia)

ggplot(vivienda_mp) +
  geom_mosaic(aes(
    x = product(tenencia, area),
    fill = tenencia
  )) +
  geom_mosaic_text(aes(x = product(tenencia, area), 
                       label = after_stat(paste0(round(.wt/sum(.wt)*100, 1), "%")))) +
  labs(title = "Tenencia de vivienda por área")



