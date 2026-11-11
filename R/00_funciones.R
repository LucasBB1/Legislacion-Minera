# =============================================================================
# Legislación minera en el Congreso (Documento de trabajo N.° 6)
# 00_funciones.R — paquetes, rutas, universo de proyectos y funciones comunes
# =============================================================================
suppressPackageStartupMessages({ library(dplyr); library(tidyr); library(readr); library(stringr); library(purrr) })
dir_proc <- file.path("data", "processed"); dir_fuentes <- file.path("data", "fuentes"); dir_out <- "output"
dir.create(dir_out, showWarnings = FALSE); dir.create(file.path(dir_out, "tablas"), showWarnings = FALSE)

trimestre <- function(fecha) (as.integer(format(fecha, "%Y")) - 2014) * 4 + (as.integer(format(fecha, "%m")) - 1) %/% 3 + 1

# Universo. tipo: carga (tributos y patentes), regulacion (permisos, cierre,
# agua, glaciares), estado (empresas públicas y litio), general (reformas
# tributarias o de inversión que afectan a la minería sin ser mineras).
PROYECTOS <- tribble(
  ~boletin,   ~corto,                          ~tipo,        ~agenda,
  "12324-08", "Cierre de faenas",              "regulacion", "antecedente",
  "10638-08", "Litio de interés nacional",     "estado",     "antecedente",
  "11876-12", "Protección de glaciares",       "regulacion", "antecedente",
  "12093-08", "Royalty minero",                "carga",      "antecedente",
  "15265-08", "ENAMI y pequeña minería",       "estado",     "antecedente",
  "15511-08", "Posterga Ley 21.420 en minería","carga",      "antecedente",
  "15510-08", "Patentes mineras 2023",         "carga",      "antecedente",
  "16566-03", "Ley Marco de Autorizaciones Sectoriales", "regulacion", "antecedente",
  "16605-08", "Capacidad de fundición de ENAMI","estado",    "antecedente",
  "11608-09", "Desalinización",                "regulacion", "2026",
  "18216-05", "Reconstrucción Nacional",       "general",    "2026",
  "18259-08", "Patentes y amparo minero",      "carga",      "2026",
  "18194-08", "Minerales críticos",            "regulacion", "2026",
  # Proyectos que cumplen el criterio pero cuyo título no menciona la minería, y proyectos de 2026
  "7543-12",  "Reforma al Código de Aguas",    "regulacion", "antecedente",
  "9404-12",  "Servicio de Biodiversidad y Áreas Protegidas", "regulacion", "antecedente",
  "16552-12", "Reforma al SEIA",               "regulacion", "2026",
  "16553-12", "Reforma a la SMA",              "regulacion", "2026",
  "14763-05", "Reduce exenciones tributarias (Ley 21.420)", "general", "antecedente",
  "7678-02",  "Deroga la Ley Reservada del Cobre", "estado",  "antecedente",
  "10629-12", "Transporte de minerales", "regulacion", "antecedente",
  "18653-08", "Directorio de Codelco",         "estado",     "2026")

# Bloques ideológicos (2018-2026). Cinco bloques ordenados de izquierda a
# derecha, sin nombres de partidos. Cada partido se asigna según su familia en la
# literatura y su coalición electoral; la derecha radical sigue a Mudde (2007) y
# a Díaz et al. (2023). Los partidos sin familia clara (PDG, FRVS y Partido
# Progresista) se asignan al bloque cuya mediana de puntos ideales en la Sala
# está más cerca de la suya (ver 10_bloques.R); el resultado para el PDG coincide
# con Mascareño et al. (2022), que lo ubican entre la DC y RN.
bloque_de <- function(p) {
  p <- toupper(str_remove(coalesce(p, ""), "^IND-"))
  case_when(p %in% c("PREP", "REP", "PNL", "PSC", "PCC", "PCCH") ~ "Derecha radical",
            p %in% c("UDI", "RN", "EVOP", "PRI") ~ "Derecha",
            p %in% c("DC", "PDC", "DEM", "AMA", "AMAR", "CU", "PDG") ~ "Centro",
            p %in% c("PS", "PPD", "PR", "PRSD", "PL", "LIBERAL", "CIU", "FRVS", "PRO") ~ "Centroizquierda",
            p %in% c("PC", "FA", "RD", "CS", "PCS", "COMUNES", "CONVERGENCIA", "PEV", "PH", "IGUAL", "PAH") ~ "Izquierda",
            TRUE ~ "Sin partido")
}
ORDEN_BLOQUES <- c("Izquierda", "Centroizquierda", "Centro", "Derecha", "Derecha radical", "Sin partido")

# MCO con efectos fijos en dos dimensiones (proyecciones alternadas) y
# errores estándar agrupados por legislador.
mco_ef <- function(y, X, f1, f2 = NULL, cl, iter = 200, tol = 1e-9) {
  X <- as.matrix(X); Z <- cbind(y, X)
  dm <- function(M, f) M - rowsum(M, f)[as.character(f), , drop = FALSE] / as.vector(table(f)[as.character(f)])
  for (k in seq_len(iter)) { Z0 <- Z; Z <- dm(Z, f1); if (!is.null(f2)) Z <- dm(Z, f2); if (max(abs(Z - Z0)) < tol) break }
  yt <- Z[, 1]; Xt <- Z[, -1, drop = FALSE]
  keep <- colSums(abs(Xt)) > 1e-8; Xt <- Xt[, keep, drop = FALSE]; X <- X[, keep, drop = FALSE]
  XtX <- crossprod(Xt); b <- solve(XtX, crossprod(Xt, yt)); e <- yt - Xt %*% b
  S <- rowsum(Xt * as.vector(e), cl); G <- nrow(S); n <- length(y); k <- ncol(Xt)
  V <- solve(XtX) %*% crossprod(S) %*% solve(XtX) * G / (G - 1) * (n - 1) / (n - k)
  tibble(termino = colnames(X), coef = as.vector(b), ee = sqrt(diag(V)), t = coef / ee, p = 2 * pnorm(-abs(t)), n = n, grupos = G)
}

# Senado: correspondencia entre senadores (parlid) y la lista de senadores por
# período y circunscripción, por nombre completo o, si no coincide, por apellidos.
senado_lista <- function() {
  norm <- function(x) iconv(tolower(x), "UTF-8", "ASCII//TRANSLIT") |> str_replace_all("[^a-z ]", "") |> str_squish()
  sp <- read_delim(file.path(dir_fuentes, "senadores_periodos.csv"), delim = ";", show_col_types = FALSE) |>
    filter(periodo %in% c("2018-2022", "2022-2026", "2026-2030")) |> mutate(clave = norm(senador), ap = word(clave, -2, -1))
  sen <- read_csv(file.path(dir_proc, "senadores_votaciones.csv"), show_col_types = FALSE) |>
    mutate(clave = norm(paste(nombre, ap_paterno, ap_materno)), ap = norm(paste(ap_paterno, ap_materno)))
  m1 <- sen |> inner_join(sp |> select(-ap), by = "clave")
  sp_u <- sp |> group_by(ap) |> filter(n_distinct(clave) == 1) |> ungroup()
  m2 <- sen |> filter(!parlid %in% m1$parlid) |> group_by(ap) |> filter(n() == 1) |> ungroup() |> select(-clave) |>
    inner_join(sp_u |> select(-clave), by = "ap")
  manual <- tibble(parlid = c(1066, 1328), circunscripcion = c(8, 7))   # García Huidobro y Rojo Edwards
  bind_rows(m1, m2) |> select(parlid, periodo, circunscripcion, partido_inicio) |>
    left_join(manual |> rename(circ_m = circunscripcion), by = "parlid") |>
    mutate(circunscripcion = coalesce(circ_m, circunscripcion)) |> select(-circ_m) |> distinct()
}
# Partido de cada senador en cada trimestre: militancia de la base del DT5 y,
# cuando falta, el partido con que inició el período.
senado_partidos <- function() {
  ps <- read_csv(file.path(dir_proc, "posiciones_senado.csv"), show_col_types = FALSE) |> filter(especificacion == "w01") |>
    distinct(senador_id, t, partido)
  ini <- c("2018-2022" = trimestre(as.Date("2018-03-11")), "2022-2026" = trimestre(as.Date("2022-03-11")), "2026-2030" = trimestre(as.Date("2026-03-11")))
  resp <- senado_lista() |> filter(!is.na(partido_inicio)) |> mutate(t0 = ini[periodo]) |>
    reframe(t = t0:(t0 + 15), partido_r = partido_inicio, .by = c(parlid, periodo, t0, partido_inicio)) |> distinct(parlid, t, partido_r)
  resp |> full_join(ps |> rename(parlid = senador_id), by = c("parlid", "t")) |>
    mutate(partido = coalesce(partido, partido_r)) |> distinct(senador_id = parlid, t, partido) |>
    group_by(senador_id, t) |> slice(1) |> ungroup()
}

# Carpeta donde quedaron los documentos de un proyecto (script 01, script 01b o
# una copia combinada en data/raw/docs/documentos).
DIRS_DOC <- file.path("data", "raw", c("documentos", "documentos_ampliacion", file.path("docs", "documentos")))
dir_documentos <- function(boletin) {
  d <- file.path(DIRS_DOC, boletin); d <- d[dir.exists(d)]
  if (length(d)) d[1] else NA_character_
}
