# =============================================================================
# 01e_tramitacion.R — duración, urgencias e hitos de la tramitación
# Lee la ficha del proyecto, los trámites y las urgencias descargados del Senado
# y el XML de la ley publicada desde LeyChile.
# =============================================================================
source("R/00_funciones.R")
# Decodifica las entidades HTML numéricas y las nombradas más comunes en español
ENT <- c(amp = "&", lt = "<", gt = ">", quot = "\"", apos = "'", nbsp = " ", aacute = "á", eacute = "é", iacute = "í", oacute = "ó",
         uacute = "ú", Aacute = "Á", Eacute = "É", Iacute = "Í", Oacute = "Ó", Uacute = "Ú", ntilde = "ñ", Ntilde = "Ñ", uuml = "ü",
         Uuml = "Ü", ordm = "º", ordf = "ª", deg = "°", laquo = "«", raquo = "»", iexcl = "¡", iquest = "¿", middot = "·")
desescapar <- function(s) {
  s <- str_replace_all(s, "&#[xX]([0-9A-Fa-f]+);", function(m) intToUtf8(strtoi(sub("&#[xX]([0-9A-Fa-f]+);", "\\1", m), 16L), multiple = FALSE))
  s <- str_replace_all(s, "&#(\\d+);", function(m) vapply(as.integer(sub("&#(\\d+);", "\\1", m)), intToUtf8, ""))
  str_replace_all(s, "&([A-Za-z]+);", function(m) { k <- sub("&([A-Za-z]+);", "\\1", m); v <- ENT[k]; ifelse(is.na(v), m, v) })
}
texto_html <- function(f) {
  s <- paste(readLines(f, warn = FALSE, encoding = "UTF-8"), collapse = " ")
  s <- gsub("<[^>]+>", " ", s)
  str_squish(desescapar(s))
}
MES <- c(enero = 1, febrero = 2, marzo = 3, abril = 4, mayo = 5, junio = 6, julio = 7, agosto = 8, septiembre = 9, octubre = 10, noviembre = 11, diciembre = 12)
bols <- sort(unique(unlist(lapply(DIRS_DOC, function(d) if (dir.exists(d)) list.dirs(d, full.names = FALSE, recursive = FALSE)))))
bols <- bols[nzchar(bols)]
filas <- lapply(bols, function(b) {
  D <- dir_documentos(b)
  dp <- texto_html(file.path(D, "datos_proy.html"))
  ing <- str_match(dp, "Fecha de Ingreso:\\s*\\w+\\s+(\\d+)\\s+de\\s+(\\w+),\\s*(\\d{4})")
  f_ing <- if (!is.na(ing[1])) as.Date(sprintf("%s-%02d-%02d", ing[4], MES[tolower(ing[3])], as.integer(ing[2]))) else as.Date(NA)
  origen <- str_match(dp, "Cámara de Origen:\\s*(\\S+)")[2]; inic <- str_match(dp, "Iniciativa:\\s*(\\S+)")[2]
  et <- str_match(dp, "Etapa:\\s*(.+?)\\s+(Ley N|Link|Subetapa|$)")[2]
  xml <- list.files(D, "^ley_.*\\.xml$", full.names = TRUE); ley <- NA_character_; pub <- as.Date(NA)
  if (length(xml)) {
    t <- paste(readLines(xml[1], warn = FALSE, encoding = "UTF-8"), collapse = " ")
    ley <- str_match(t, "<Numero>(\\d+)</Numero>")[2]; pub <- as.Date(str_match(t, "fechaPublicacion=\"([\\d-]+)\"")[2])
  }
  tr <- tolower(texto_html(file.path(D, "tramites.html"))); urg <- tolower(texto_html(file.path(D, "urgencias.html")))
  n_urg <- str_count(urg, "hace presente la urgencia|hace presente urgencia")
  if (n_urg == 0) n_urg <- str_count(urg, "\\b(suma|simple|discusión inmediata)\\b")
  tibble(boletin = b, ingreso = f_ing, origen = origen, iniciativa = inic, etapa = et, ley = ley, publicacion = pub,
         dias = as.numeric(pub - f_ing), urgencias = n_urg, discusion_inmediata = str_count(urg, "discusión inmediata"),
         comision_mixta = str_detect(tr, fixed("comisión mixta")), tribunal_constitucional = str_detect(tr, fixed("tribunal constitucional")),
         veto = str_detect(tr, fixed("observaciones de s.e.")) | str_detect(tr, fixed("discusión veto")))
})
out <- bind_rows(filas)
write_csv(out, file.path(dir_out, "tramitacion.csv"), na = "")
print(out, n = 30)
