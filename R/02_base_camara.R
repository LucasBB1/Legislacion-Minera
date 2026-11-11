# =============================================================================
# 02_base_camara.R — base diputado-votación de la Cámara, 2018-2026
# -----------------------------------------------------------------------------
# Entradas: votaciones de Sala y votos individuales (base del Documento de
# trabajo N.° 5), posiciones y militancia por trimestre, distrito de cada
# diputado y ocupados en minería por distrito (Censo 2024).
# Salida: data/processed/base_camara.rds
# =============================================================================
source("R/00_funciones.R")

vot <- read_csv(file.path(dir_proc, "votaciones.csv"), show_col_types = FALSE) |>
  mutate(fecha = as.Date(substr(fecha, 1, 10))) |>
  left_join(read_csv(file.path(dir_proc, "votaciones_temas.csv"), show_col_types = FALSE) |> select(votacion_id, boletin), by = "votacion_id") |>
  filter(fecha >= as.Date("2018-03-11"), tipo == "Proyecto de Ley") |>
  mutate(t = trimestre(fecha), periodo = case_when(fecha < as.Date("2022-03-11") ~ "2018-2022",
                                                   fecha < as.Date("2026-03-11") ~ "2022-2026", TRUE ~ "2026-2030"),
         minera = boletin %in% PROYECTOS$boletin) |>
  left_join(PROYECTOS |> rename(tipo_proy = tipo), by = "boletin")

votos <- read_csv(file.path(dir_proc, "votos.csv.gz"), show_col_types = FALSE) |>
  filter(votacion_id %in% vot$votacion_id, opcion_valor %in% c(0, 1)) |>
  transmute(votacion_id, diputado_id, si = as.integer(opcion_valor == 1))

panel <- read_csv(file.path(dir_proc, "panel_trimestral.csv"), show_col_types = FALSE) |> select(diputado_id, t, x, partido)
dist <- read_csv(file.path(dir_proc, "variables_distrito.csv"), show_col_types = FALSE) |> distinct(periodo, diputado_id = id, distrito)
mineria <- read_csv(file.path(dir_fuentes, "mineria_distrito_censo2024.csv"), show_col_types = FALSE)

d <- votos |>
  inner_join(vot |> select(votacion_id, fecha, t, periodo, boletin, minera, corto, tipo_proy, agenda), by = "votacion_id") |>
  left_join(panel, by = c("diputado_id", "t")) |>
  left_join(dist, by = c("periodo", "diputado_id")) |>
  left_join(mineria, by = "distrito") |>
  mutate(bloque = bloque_de(partido), derecha = bloque %in% c("Derecha", "Derecha radical"),
         distrito_minero = pct_ocupados_mineria >= 5)

# Votaciones disputadas: la minoría reúne al menos 10% de los votos SI y NO
disp <- d |> group_by(votacion_id) |> summarise(n = n(), p_si = mean(si), .groups = "drop") |>
  filter(n >= 60, pmin(p_si, 1 - p_si) >= 0.10)
d <- d |> mutate(disputada = votacion_id %in% disp$votacion_id)

# Disidencia: votar distinto de la mayoría del propio partido (partidos con al
# menos tres diputados votando; se excluyen independientes)
d <- d |> group_by(votacion_id, partido) |>
  mutate(n_part = n(), may_part = as.integer(mean(si) > 0.5), empate = mean(si) == 0.5) |> ungroup() |>
  mutate(disidente = if_else(!is.na(partido) & partido != "IND" & n_part >= 3 & !empate, as.integer(si != may_part), NA_integer_))
# Lado de la derecha: el de la mayoría de los diputados de derecha
d <- d |> group_by(votacion_id) |> mutate(lado_der = as.integer(mean(si[derecha], na.rm = TRUE) > 0.5)) |> ungroup() |>
  mutate(con_derecha = as.integer(si == lado_der))

saveRDS(d, file.path(dir_proc, "base_camara.rds"))
message("Votos: ", nrow(d), " | votaciones: ", n_distinct(d$votacion_id), " | disputadas: ", nrow(disp),
        " | mineras disputadas: ", n_distinct(d$votacion_id[d$minera & d$disputada]),
        " | sin distrito: ", sum(is.na(d$distrito)), " | sin posición: ", sum(is.na(d$x)))
