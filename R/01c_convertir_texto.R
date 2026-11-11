# =============================================================================
# 01c_convertir_texto.R — convierte a texto los documentos descargados
# doc y docx con LibreOffice, pdf con pdftotext (poppler). Agrega al índice el
# número de palabras de cada conversión y guarda data/indice_docs.csv.
# Requiere LibreOffice (soffice) y pdftotext en el PATH.
# =============================================================================
source("R/00_funciones.R")
convertir <- function(dir_doc) {
  i <- read_csv(file.path(dir_doc, "indice_documentos.csv"), col_types = cols(.default = "c"))
  i$txt <- FALSE; i$palabras <- 0L
  for (k in seq_len(nrow(i))) {
    f <- file.path(dir_doc, i$boletin[k], i$archivo[k]); t <- sub("\\.[^.]+$", ".txt", f)
    if (file.exists(f) && !file.exists(t)) {
      if (grepl("\\.pdf$", f, ignore.case = TRUE)) system2("pdftotext", c("-layout", shQuote(f), shQuote(t)), stdout = FALSE, stderr = FALSE)
      else system2("soffice", c("--headless", "--convert-to", shQuote("txt:Text (encoded):UTF8"), "--outdir", shQuote(dirname(f)), shQuote(f)), stdout = FALSE, stderr = FALSE)
    }
    if (file.exists(t)) {
      i$txt[k] <- TRUE
      # Separadores equivalentes a los espacios en blanco de Unicode (la marca de orden de bytes no cuenta como palabra)
      w <- str_split(paste(readLines(t, warn = FALSE, encoding = "UTF-8"), collapse = " "), "[\\s\\x{1C}-\\x{1F}\\x{85}\\p{Zs}\\x{2028}\\x{2029}]+")[[1]]
      i$palabras[k] <- sum(nzchar(w))
    }
  }
  i
}
# Documentos de los 13 proyectos identificados por los títulos (01) y de los 8 agregados (01b)
indices <- lapply(c(file.path("data", "raw", "documentos"), file.path("data", "raw", "documentos_ampliacion")),
                  function(d) if (file.exists(file.path(d, "indice_documentos.csv"))) convertir(d))
write_csv(bind_rows(indices), file.path("data", "indice_docs.csv"))
