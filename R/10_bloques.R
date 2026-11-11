# =============================================================================
# 10_bloques.R — validación de la clasificación en bloques ideológicos
# Mediana de los puntos ideales de cada partido en la Cámara (2018-2026) y
# asignación de los partidos sin familia clara al bloque de mediana más cercana.
# =============================================================================
source("R/00_funciones.R")
AMBIGUOS <- c("PDG", "FRVS", "PRO")
d <- readRDS(file.path(dir_proc, "base_camara.rds")) |> distinct(diputado_id, t, partido, x) |>
  filter(!is.na(x), !is.na(partido), partido != "IND") |> mutate(partido = str_remove(partido, "^IND-"), bloque = bloque_de(partido))
part <- d |> group_by(partido, bloque) |> summarise(mediana = median(x), diputados = n_distinct(diputado_id), .groups = "drop")
blq <- d |> filter(!partido %in% AMBIGUOS, bloque != "Sin partido") |> group_by(bloque) |> summarise(mediana_bloque = median(x), .groups = "drop")
asig <- part |> filter(partido %in% AMBIGUOS) |> select(partido, mediana) |> cross_join(blq) |>
  mutate(distancia = abs(mediana - mediana_bloque)) |> group_by(partido) |> slice_min(distancia, n = 1) |> ungroup()
write_csv(part |> arrange(mediana), file.path(dir_out, "tablas", "bloques_medianas_partidos.csv"))
write_csv(blq, file.path(dir_out, "tablas", "bloques_medianas.csv"))
write_csv(asig, file.path(dir_out, "tablas", "bloques_asignacion_ambiguos.csv"))
print(blq); print(asig); print(part |> arrange(mediana), n = 40)
