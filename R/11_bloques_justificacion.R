# =============================================================================
# 11_bloques_justificacion.R — evidencia para la clasificación en bloques
# -----------------------------------------------------------------------------
# 1. Saltos entre medianas consecutivas de los partidos en la escala ideológica.
# 2. Coincidencia de la mayoría de cada par de partidos en las votaciones
#    disputadas, por gobierno (Piñera, Boric, Kast).
# 3. Robustez del modelo B a clasificaciones alternativas.
# =============================================================================
source("R/00_funciones.R")
d <- readRDS(file.path(dir_proc, "base_camara.rds")) |> filter(disputada)

# --- 1. Saltos entre medianas de partidos ------------------------------------
part <- read_csv(file.path(dir_out, "tablas", "bloques_medianas_partidos.csv"), show_col_types = FALSE) |>
  filter(diputados >= 3) |> arrange(mediana) |>
  mutate(siguiente = lead(partido), salto = lead(mediana) - mediana)
write_csv(part, file.path(dir_out, "tablas", "bloques_saltos.csv"))

# --- 2. Coincidencia entre mayorías de partidos ------------------------------
gob <- function(f) case_when(f < as.Date("2022-03-11") ~ "Piñera", f < as.Date("2026-03-11") ~ "Boric", TRUE ~ "Kast")
may <- d |> filter(!is.na(partido), partido != "IND") |> mutate(partido = str_remove(partido, "^IND-")) |>
  group_by(votacion_id, fecha, partido) |> summarise(n = n(), p = mean(si), .groups = "drop") |>
  filter(n >= 3, p != 0.5) |> mutate(m = as.integer(p > 0.5), gobierno = gob(as.Date(fecha)))
PARES <- list(c("PC", "FA"), c("PC", "PS"), c("FA", "PS"), c("PS", "PPD"),
              c("UDI", "RN"), c("PREP", "UDI"), c("PREP", "RN"), c("PNL", "PREP"), c("PNL", "UDI"))
coinc <- map_dfr(PARES, function(pp) {
  a <- may |> filter(partido == pp[1]); b <- may |> filter(partido == pp[2])
  inner_join(a, b, by = c("votacion_id", "gobierno")) |>
    bind_rows(inner_join(a, b, by = c("votacion_id", "gobierno")) |> mutate(gobierno = "Total")) |>
    group_by(gobierno) |> summarise(par = paste(pp, collapse = "-"), coincidencia = mean(m.x == m.y), votaciones = n(), .groups = "drop")
})
write_csv(coinc, file.path(dir_out, "tablas", "bloques_coincidencia.csv"))

# --- 3. Robustez del modelo B ------------------------------------------------
b <- d |> filter(!is.na(x), !is.na(distrito)) |> mutate(p = str_remove(coalesce(partido, ""), "^IND-"))
modelo_b <- function(z, bl) {
  X <- cbind(posicion = z$x, mineria = z$pct_ocupados_mineria / 10, model.matrix(~ factor(bl), z)[, -1, drop = FALSE])
  mco_ef(z$con_derecha, X, z$votacion_id, NULL, z$diputado_id) |> filter(termino == "posicion")
}
ALT <- list(
  original = function(z) z$bloque,
  pc_aparte = function(z) if_else(z$p == "PC", "Izquierda radical (PC)", z$bloque),
  izq_radical_amplia = function(z) if_else(z$p %in% c("PC", "PH", "PAH", "IGUAL"), "Izquierda radical (amplia)", z$bloque),
  der_radical_posicional = function(z) if_else(z$p %in% c("PSC", "PCC", "PCCH"), "Derecha", z$bloque))
rob <- imap_dfr(ALT, function(f, nm) bind_rows(
  modelo_b(b |> filter(minera), f(b |> filter(minera))) |> mutate(muestra = "mineras"),
  modelo_b(b |> filter(!minera), f(b |> filter(!minera))) |> mutate(muestra = "resto")) |> mutate(clasificacion = nm))
write_csv(rob, file.path(dir_out, "tablas", "bloques_robustez_modeloB.csv"))
print(part, n = 40); print(coinc |> mutate(coincidencia = round(coincidencia, 3)), n = 50)
print(rob |> mutate(across(where(is.numeric), ~ signif(.x, 4))))
