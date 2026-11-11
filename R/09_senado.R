# =============================================================================
# 09_senado.R — modelo de disidencia en el Senado, 2018-2026
# Votos de Sala de senadores en votaciones disputadas (minoría de al menos 10%,
# 20 votos o más), militancia trimestral de la base del Documento de trabajo
# N.° 5 y empleo minero de la circunscripción (Censo 2024). Cubre los períodos
# 2018-2022, 2022-2026 y 2026-2030 hasta la fecha de corte. Circunscripción
# minera: al menos 5% del empleo en minería.
# =============================================================================
source("R/00_funciones.R")
map <- senado_lista() |> distinct(parlid, circunscripcion) |> group_by(parlid) |> filter(n() == 1) |> ungroup()
circ <- read_csv(file.path(dir_fuentes, "mineria_circunscripcion_censo2024.csv"), show_col_types = FALSE)
vs <- read_csv(file.path(dir_proc, "votaciones_senado.csv"), show_col_types = FALSE) |>
  mutate(fecha = as.Date(substr(fecha, 1, 10), "%d-%m-%Y"), boletin = str_remove_all(boletin, "\\.")) |>
  filter(fecha >= as.Date("2018-03-11"), !is.na(boletin))
ps <- senado_partidos()
s <- read_csv(file.path(dir_proc, "votos_senado.csv.gz"), show_col_types = FALSE) |> filter(id_votacion %in% vs$id_votacion, opcion %in% c("SI", "NO")) |>
  left_join(vs |> select(id_votacion, fecha, boletin), by = "id_votacion") |>
  mutate(t = trimestre(fecha), si = as.integer(opcion == "SI"), periodo = case_when(fecha < as.Date("2022-03-11") ~ "2018-2022", fecha < as.Date("2026-03-11") ~ "2022-2026", TRUE ~ "2026-2030")) |>
  left_join(ps, by = c("parlid" = "senador_id", "t")) |> mutate(partido = str_remove(partido, "^IND-")) |> left_join(map, by = "parlid") |> left_join(circ, by = "circunscripcion") |>
  mutate(minera = boletin %in% PROYECTOS$boletin, circ_minera = pct_ocupados_mineria >= 5)
disp_s <- s |> group_by(id_votacion) |> summarise(n = n(), m = pmin(mean(si), 1 - mean(si)), .groups = "drop") |> filter(n >= 20, m >= 0.10)
s <- s |> filter(id_votacion %in% disp_s$id_votacion, !is.na(circunscripcion)) |>
  group_by(id_votacion, partido) |> mutate(n_part = n(), may = as.integer(mean(si) > 0.5), emp = mean(si) == 0.5) |> ungroup() |>
  filter(!is.na(partido), partido != "IND", n_part >= 3, !emp) |> mutate(disidente = as.integer(si != may))
rs <- mco_ef(s$disidente, cbind(x = s$minera * s$circ_minera), s$id_votacion, paste(s$parlid, s$periodo), s$parlid)
tasas_s <- s |> group_by(minera, circ_minera) |> summarise(disidencia = mean(disidente), votos = n(), votaciones = n_distinct(id_votacion), .groups = "drop")
rob_s <- tibble(prueba = "Senado, circunscripción minera desde 5% del empleo", coef = rs$coef, ee = rs$ee, p = rs$p,
                             votaciones_mineras = n_distinct(s$id_votacion[s$minera]), n = rs$n)
cat("Senadores con circunscripción asignada:", n_distinct(s$parlid), "\n")

write_csv(rob_s, file.path(dir_out, "tablas", "robustez_senado.csv"))
write_csv(tasas_s, file.path(dir_out, "tablas", "disidencia_tasas_senado.csv"))
print(rob_s); print(tasas_s)
cat("Votaciones disputadas:", n_distinct(s$id_votacion), " mineras:", n_distinct(s$id_votacion[s$minera]), "\n")
print(s |> filter(minera) |> count(boletin))
