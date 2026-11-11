# =============================================================================
# 01d_cambio_texto.R — cuánto cambia el texto de cada proyecto
# -----------------------------------------------------------------------------
# Compara el articulado de ingreso, el del oficio a la cámara revisora (fin del
# primer trámite) y el del oficio al Ejecutivo (despacho). Cada texto se divide
# en secuencias de n palabras consecutivas (n = 5 en el documento; 3 y 8 en la
# robustez). Retención es la proporción de secuencias del texto anterior que
# reaparecen en el posterior; novedad, la proporción de secuencias del texto
# posterior que no estaban en el anterior.
# Uso: Rscript R/01d_cambio_texto.R [n]
# =============================================================================
source("R/00_funciones.R")
args <- commandArgs(trailingOnly = TRUE)
NG <- if (length(args)) as.integer(args[1]) else 5L

leer <- function(b, arch) paste(readLines(file.path(dir_documentos(b), sub("\\.[^.]+$", ".txt", arch)), warn = FALSE, encoding = "UTF-8"), collapse = "\n")

# Articulado: desde el primer "Artículo 1", "primero" o "único" que sigue a un
# encabezado PROYECTO DE LEY en mayúsculas; si no lo hay, desde el primer
# artículo numerado con signo ordinal o guion (no "artículo 19 N°24"). Termina
# en la fórmula de cierre del oficio.
ART <- regex("Art[íi]culo\\s+(1\\s*[°º.\\-]|1\\s*\\.-|primero\\b|[úu]nico\\b)", ignore_case = TRUE)
articulado <- function(t) {
  s <- NULL
  for (p in str_locate_all(t, fixed("PROYECTO DE LEY"))[[1]][, "end"]) {
    ventana <- substr(t, p + 1, p + 400); m <- str_locate(ventana, ART)
    if (!is.na(m[1])) { s <- substr(t, p + m[1], nchar(t)); break }
  }
  if (is.null(s)) { m <- str_locate(t, ART); s <- if (!is.na(m[1])) substr(t, m[1], nchar(t)) else t }
  m3 <- str_locate(s, "\n\\s*(Dios guarde|Saluda atentamente|Lo que tengo a honra)")
  if (!is.na(m3[1])) substr(s, 1, m3[1] - 1) else s
}
tokens <- function(t) str_extract_all(stringi::stri_trans_nfkd(tolower(t)) |> str_remove_all("\\p{Mn}"), "[a-z0-9]+")[[1]]
ngr <- function(w) if (length(w) >= NG) unique(vapply(seq_len(length(w) - NG + 1), function(k) paste(w[k:(k + NG - 1)], collapse = " "), "")) else character(0)

etapa <- function(fila) {
  f <- tolower(fila)
  case_when(str_detect(f, fixed("ingreso de proyecto")) ~ "ingreso",
            str_detect(f, fixed("texto del mensaje")) ~ "ingreso_texto",
            str_detect(f, fixed("oficio de ley a cámara revisora")) ~ "fin_primer",
            str_detect(f, fixed("oficio de ley al ejecutivo")) | str_detect(f, fixed("oficio al ejecutivo para promulgación")) |
              str_detect(f, fixed("comunica a s.e. el presidente de la república que el congreso")) ~ "despacho",
            TRUE ~ NA_character_)
}

f_ind <- if (file.exists(file.path("data", "indice_docs.csv"))) file.path("data", "indice_docs.csv") else file.path(dir_fuentes, "indice_documentos.csv")
i <- read_csv(f_ind, col_types = cols(.default = "c")) |>
  mutate(palabras = as.integer(palabras), etapa = etapa(fila), fecha = as.Date(substr(fila, 1, 10), "%d/%m/%Y"))
filas <- list()
for (b in sort(unique(i$boletin[!is.na(i$etapa) & i$palabras > 50]))) {
  g <- i |> filter(boletin == b, !is.na(etapa), palabras > 50)
  ini <- if (any(g$etapa == "ingreso_texto")) g |> filter(etapa == "ingreso_texto") else g |> filter(etapa == "ingreso")
  fin1 <- g |> filter(etapa == "fin_primer"); desp <- g |> filter(etapa == "despacho") |> arrange(fecha)
  textos <- list()
  if (nrow(ini)) textos$ingreso <- articulado(leer(b, ini$archivo[1]))
  if (nrow(fin1)) textos$fin_primer <- articulado(leer(b, fin1$archivo[nrow(fin1)]))
  if (nrow(desp)) textos$despacho <- articulado(leer(b, desp$archivo[nrow(desp)]))
  W <- lapply(textos, tokens); G <- lapply(W, ngr)
  ret <- function(a, z) if (!is.null(G[[a]]) && !is.null(G[[z]]) && length(G[[a]])) length(intersect(G[[a]], G[[z]])) / length(G[[a]]) else NA
  nuevo <- function(a, z) if (!is.null(G[[a]]) && !is.null(G[[z]]) && length(G[[z]])) 1 - length(intersect(G[[a]], G[[z]])) / length(G[[z]]) else NA
  n_pal <- function(k) if (is.null(W[[k]])) 0L else length(W[[k]])
  filas[[b]] <- tibble(boletin = b, palabras_ingreso = n_pal("ingreso"), palabras_fin_primer = n_pal("fin_primer"), palabras_despacho = n_pal("despacho"),
                       retencion_primer = ret("ingreso", "fin_primer"), retencion_final = ret("ingreso", "despacho"),
                       nuevo_final = nuevo("ingreso", "despacho"), retencion_segundo = ret("fin_primer", "despacho"),
                       nuevo_segundo = nuevo("fin_primer", "despacho"))
}
out <- bind_rows(filas) |> mutate(across(starts_with(c("retencion", "nuevo")), ~ round(.x, 3)))
write_csv(out, if (NG == 5) file.path(dir_out, "cambio_texto.csv") else file.path(dir_out, "tablas", paste0("cambio_texto_n", NG, ".csv")), na = "")
print(out, n = 30)
