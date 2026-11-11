# =============================================================================
# 03_modelos_camara.R — qué explica el voto minero en la Cámara, 2018-2026
# -----------------------------------------------------------------------------
# H1 (ideología): el voto en proyectos mineros sigue la posición general de
#     cada diputado, como en el resto de la agenda.
# H2 (partido): los diputados votan con su partido en minería igual que en
#     otros temas.
# H3 (distrito): los diputados de distritos con más empleo minero se apartan
#     de su partido y de su posición ideológica en los proyectos mineros.
# =============================================================================
source("R/00_funciones.R")
d <- readRDS(file.path(dir_proc, "base_camara.rds")) |> filter(disputada, !is.na(distrito))
d$pm10 <- d$pct_ocupados_mineria / 10      # 10 puntos de ocupados en minería

# --- Modelo A: disidencia respecto del propio partido -------------------------
a <- d |> filter(!is.na(disidente))
ma <- list(
  continuo = mco_ef(a$disidente, cbind(minera_x_mineria = a$minera * a$pm10), a$votacion_id, paste(a$diputado_id, a$periodo), a$diputado_id),
  binario  = mco_ef(a$disidente, cbind(minera_x_distrito_minero = a$minera * a$distrito_minero), a$votacion_id, paste(a$diputado_id, a$periodo), a$diputado_id),
  por_tipo = mco_ef(a$disidente, sapply(c("carga", "regulacion", "estado", "general"), function(k) (a$tipo_proy %in% k) * a$pm10) |>
                      `colnames<-`(paste0("tipo_", c("carga", "regulacion", "estado", "general"), "_x_mineria")),
                    a$votacion_id, paste(a$diputado_id, a$periodo), a$diputado_id))
tasas <- a |> group_by(minera, distrito_minero) |> summarise(disidencia = mean(disidente), n = n(), .groups = "drop")

# --- Modelo B: votar con la mayoría de la derecha -------------------------------
# Dentro de cada votación (efecto fijo), según posición ideológica, bloque y
# empleo minero del distrito. Se estima por separado en votaciones mineras y en
# el resto (placebo).
b <- d |> filter(!is.na(x), bloque != "Sin partido" | TRUE)
X_b <- function(z) cbind(posicion = z$x, mineria = z$pm10, model.matrix(~ factor(bloque, levels = ORDEN_BLOQUES), z)[, -1, drop = FALSE])
mb <- list(
  mineras = with(b |> filter(minera), mco_ef(con_derecha, X_b(b |> filter(minera)), votacion_id, NULL, diputado_id)),
  resto   = with(b |> filter(!minera), mco_ef(con_derecha, X_b(b |> filter(!minera)), votacion_id, NULL, diputado_id)),
  carga   = with(b |> filter(tipo_proy %in% "carga"), mco_ef(con_derecha, X_b(b |> filter(tipo_proy %in% "carga")), votacion_id, NULL, diputado_id)),
  regulacion = with(b |> filter(tipo_proy %in% "regulacion"), mco_ef(con_derecha, X_b(b |> filter(tipo_proy %in% "regulacion")), votacion_id, NULL, diputado_id)),
  general = with(b |> filter(tipo_proy %in% "general"), mco_ef(con_derecha, X_b(b |> filter(tipo_proy %in% "general")), votacion_id, NULL, diputado_id)))

res <- bind_rows(c(imap(ma, ~ mutate(.x, modelo = paste0("A_", .y))), imap(mb, ~ mutate(.x, modelo = paste0("B_", .y)))))
write_csv(res, file.path(dir_out, "tablas", "modelos_camara.csv"))
write_csv(tasas, file.path(dir_out, "tablas", "disidencia_tasas.csv"))
print(tasas); print(res |> filter(!str_detect(termino, "factor")) |> mutate(across(c(coef, ee, t, p), ~ round(.x, 4))), n = 50)
