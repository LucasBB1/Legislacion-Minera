# =============================================================================
# 04_coaliciones.R — apoyo por bloque en las votaciones clave de cada proyecto
# Cámara: base_camara.rds. Senado: votaciones y votos de Sala con la militancia
# trimestral de cada senador (base del Documento de trabajo N.° 5).
# =============================================================================
source("R/00_funciones.R")
d <- readRDS(file.path(dir_proc, "base_camara.rds")) |> filter(minera)
clave_cam <- d |> group_by(boletin, corto, votacion_id, fecha) |> summarise(no = sum(1 - si), si = sum(si), .groups = "drop") |>
  group_by(boletin) |> arrange(fecha, votacion_id) |>
  mutate(hito = case_when(row_number() == 1 ~ "Primera votación", row_number() == n() ~ "Última votación", TRUE ~ NA_character_)) |>
  filter(!is.na(hito)) |> ungroup()
cam <- d |> semi_join(clave_cam, by = "votacion_id") |> left_join(clave_cam |> select(votacion_id, hito), by = "votacion_id") |>
  group_by(camara = "Cámara", boletin, corto, hito, fecha, bloque) |> summarise(apoyo = mean(si), n = n(), .groups = "drop")

vs <- read_csv(file.path(dir_proc, "votaciones_senado.csv"), show_col_types = FALSE) |>
  mutate(fecha = as.Date(substr(fecha, 1, 10), "%d-%m-%Y"), boletin = str_remove_all(boletin, "\\.")) |>
  filter(boletin %in% PROYECTOS$boletin, fecha >= as.Date("2018-03-11"))
ps <- senado_partidos()
sv <- read_csv(file.path(dir_proc, "votos_senado.csv.gz"), show_col_types = FALSE) |> filter(id_votacion %in% vs$id_votacion, opcion %in% c("SI", "NO")) |>
  left_join(vs |> select(id_votacion, fecha, boletin, si_tot = si, no_tot = no), by = "id_votacion") |>
  mutate(t = trimestre(fecha)) |> left_join(ps, by = c("parlid" = "senador_id", "t")) |>
  mutate(bloque = bloque_de(partido), si = as.integer(opcion == "SI"))
clave_sen <- vs |> filter(si + no >= 20) |> group_by(boletin) |> arrange(fecha, id_votacion) |>
  mutate(hito = case_when(row_number() == 1 ~ "Primera votación", row_number() == n() ~ "Última votación", TRUE ~ NA_character_)) |>
  filter(!is.na(hito)) |> ungroup() |> select(id_votacion, hito)
sen <- sv |> inner_join(clave_sen, by = "id_votacion") |> left_join(PROYECTOS |> select(boletin, corto), by = "boletin") |>
  group_by(camara = "Senado", boletin, corto, hito, fecha, bloque) |> summarise(apoyo = mean(si), n = n(), .groups = "drop")

out <- bind_rows(cam, sen) |> mutate(bloque = factor(bloque, ORDEN_BLOQUES)) |> arrange(boletin, camara, fecha, bloque)
write_csv(out, file.path(dir_out, "tablas", "apoyo_bloques.csv"))
tot <- bind_rows(clave_cam |> transmute(camara = "Cámara", boletin, hito, fecha, si, no),
                 vs |> inner_join(clave_sen, by = "id_votacion") |> transmute(camara = "Senado", boletin, hito, fecha, si, no))
write_csv(tot, file.path(dir_out, "tablas", "votaciones_clave.csv"))
print(out |> filter(boletin %in% c("18216-05", "18259-08", "11608-09", "12093-08", "16566-03"), hito == "Última votación") |>
        mutate(apoyo = round(100 * apoyo)) |> select(camara, corto, bloque, apoyo, n), n = 100)
print(tot, n = 50)
