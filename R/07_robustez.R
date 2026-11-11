# =============================================================================
# 07_robustez.R — pruebas de robustez del modelo de disidencia (H2)
# -----------------------------------------------------------------------------
# 1. Composición de la muestra: sin la Ley de Reconstrucción Nacional, solo esa
#    ley, sin las dos leyes con más votaciones, los 13 proyectos identificados por los títulos, solo
#    regímenes habilitantes y por tipo de proyecto.
# 2. Definiciones alternativas: umbral de distrito minero (7% y 10%) y de
#    votación disputada (minoría de al menos 5% y 20%).
# 3. Exclusión de cada distrito minero.
# 4. Estimación proyecto por proyecto y promedio con igual peso por proyecto.
# 5. Inferencia por permutación: se reasigna al azar la condición de distrito
#    minero entre los 28 distritos (500 sorteos de siete distritos).
# El modelo equivalente para el Senado está en 09_senado.R.
# =============================================================================
source("R/00_funciones.R")
set.seed(20261110)
V1 <- c("12324-08", "10638-08", "11876-12", "12093-08", "15265-08", "15511-08", "15510-08", "16566-03",
        "16605-08", "11608-09", "18216-05", "18259-08", "18194-08")
HABILITANTES <- c("7543-12", "9404-12", "16552-12", "16553-12")  # agua, áreas protegidas y fiscalización ambiental
full <- readRDS(file.path(dir_proc, "base_camara.rds")) |> filter(!is.na(distrito), !is.na(disidente))
min_disp <- full |> group_by(votacion_id) |> summarise(n = n(), m = pmin(mean(si), 1 - mean(si)), .groups = "drop")
d <- full |> filter(disputada)

modelo <- function(a, var = "distrito_minero", lab) {
  r <- mco_ef(a$disidente, cbind(x = a$minera * a[[var]]), a$votacion_id, paste(a$diputado_id, a$periodo), a$diputado_id)
  tibble(prueba = lab, coef = r$coef, ee = r$ee, p = r$p, votaciones_mineras = n_distinct(a$votacion_id[a$minera]), n = r$n)
}

R <- list(modelo(d, lab = "Especificación principal"))
# 1. Composición
R <- c(R, list(
  modelo(d |> filter(!(minera & boletin %in% "18216-05")), lab = "Sin la Ley de Reconstrucción Nacional"),
  modelo(d |> filter(!minera | boletin %in% "18216-05"), lab = "Solo la Ley de Reconstrucción Nacional"),
  modelo(d |> filter(!(minera & boletin %in% c("18216-05", "16566-03"))), lab = "Sin Reconstrucción ni Autorizaciones Sectoriales"),
  modelo(d |> filter(!minera | boletin %in% V1), lab = "Solo los 13 proyectos identificados por los títulos"),
  modelo(d |> filter(!minera | boletin %in% HABILITANTES), lab = "Solo Código de Aguas, SBAP y SMA"),
  modelo(d |> filter(!minera | tipo_proy %in% "carga"), lab = "Solo proyectos de carga"),
  modelo(d |> filter(!minera | tipo_proy %in% "regulacion"), lab = "Solo proyectos de regulación")))
# 2. Definiciones alternativas
for (u in c(7, 10)) R <- c(R, list(modelo(d |> mutate(dm = pct_ocupados_mineria >= u), "dm", paste0("Distrito minero desde ", u, "% del empleo"))))
for (u in c(0.05, 0.20)) {
  ids <- min_disp$votacion_id[min_disp$n >= 60 & min_disp$m >= u]
  R <- c(R, list(modelo(full |> filter(votacion_id %in% ids), lab = paste0("Votación disputada con minoría desde ", 100 * u, "%"))))
}
# 3. Exclusión de cada distrito minero (se excluyen sus diputados)
for (k in c(1, 2, 3, 4, 5, 6, 15)) R <- c(R, list(modelo(d |> filter(distrito != k), lab = paste0("Sin el distrito ", k))))
rob <- bind_rows(R)

# 4. Proyecto por proyecto (con al menos 5 votaciones disputadas) y promedio con igual peso
pp <- d |> filter(minera) |> distinct(boletin, votacion_id) |> count(boletin) |> filter(n >= 5)
por_proy <- map_dfr(pp$boletin, function(b) modelo(d |> filter(!minera | boletin %in% b), lab = b)) |>
  left_join(PROYECTOS |> select(prueba = boletin, corto), by = "prueba")
write_csv(por_proy, file.path(dir_out, "tablas", "robustez_por_proyecto.csv"))
k <- nrow(por_proy)
rob <- bind_rows(rob, tibble(prueba = paste0("Promedio con igual peso por proyecto (", k, " proyectos)"), coef = mean(por_proy$coef),
                             ee = sd(por_proy$coef) / sqrt(k), p = 2 * pt(-abs(mean(por_proy$coef) / (sd(por_proy$coef) / sqrt(k))), df = k - 1),
                             votaciones_mineras = sum(por_proy$votaciones_mineras), n = NA))

# 5. Permutación de la condición de distrito minero
f1 <- as.integer(factor(d$votacion_id)); f2 <- as.integer(factor(paste(d$diputado_id, d$periodo)))
n1 <- tabulate(f1); n2 <- tabulate(f2)
dm <- function(v, f, n) v - (rowsum(v, f, reorder = TRUE)[, 1] / n)[f]
demean2 <- function(v) { for (k in 1:200) { v0 <- v; v <- dm(dm(v, f1, n1), f2, n2); if (max(abs(v - v0)) < 1e-9) break }; v }
yt <- demean2(as.numeric(d$disidente))
coef_de <- function(dist_min) { xt <- demean2(as.numeric(d$minera & d$distrito %in% dist_min)); sum(xt * yt) / sum(xt^2) }
obs <- coef_de(c(1, 2, 3, 4, 5, 6, 15))
perm <- replicate(500, coef_de(sample(1:28, 7)))
p_perm <- mean(abs(perm) >= abs(obs))
rob <- bind_rows(rob, tibble(prueba = "Permutación de distritos (500 sorteos)", coef = obs, ee = NA, p = p_perm,
                             votaciones_mineras = n_distinct(d$votacion_id[d$minera]), n = nrow(d)))
write_csv(tibble(coef = perm), file.path(dir_out, "tablas", "permutacion_distritos.csv"))

write_csv(rob, file.path(dir_out, "tablas", "robustez.csv"))
print(rob |> mutate(across(c(coef, ee, p), ~ round(.x, 4))), n = 40); print(por_proy)
