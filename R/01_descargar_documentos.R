# =============================================================================
# Legislación minera en el Congreso (Documento de trabajo N.° 6)
# 01_descargar_documentos.R  (versión 2)
# -----------------------------------------------------------------------------
# Descarga, para cada proyecto, los documentos que publica el Senado en su
# sistema de tramitación: mensaje o moción, informes de comisión, oficios,
# indicaciones, comparados y la lista de trámites y urgencias. Agrega el texto
# final de cada ley publicada desde LeyChile (BCN). Deja todo en
# data/raw/documentos y lo comprime en documentos_mineria.zip.
#
# La versión 1 leía la ficha del proyecto, pero el Senado la arma con
# JavaScript y el servidor devolvía la portada. Esta versión llama directamente
# a los mismos servicios que usa la página.
#
# Uso: abrir R o RStudio y ejecutar el script completo. Tarda varios minutos.
# =============================================================================

paquetes <- c("httr", "xml2", "stringr", "zip")
faltan <- paquetes[!paquetes %in% rownames(installed.packages())]
if (length(faltan) > 0) install.packages(faltan)
library(httr); library(xml2); library(stringr)

# Agenda 2026: patentes y amparo, Reconstrucción Nacional, desalinización,
# minerales críticos. Antecedentes 2018-2025: el resto.
boletines <- c("18259-08", "18216-05", "11608-09", "18194-08",
               "10638-08", "12093-08", "12324-08", "15265-08", "15510-08",
               "15511-08", "16566-03", "16605-08", "11876-12")

base <- "https://tramitacion.senado.cl/appsenado/index.php?mo=tramitacion"
dir_doc <- file.path("data", "raw", "documentos")
unlink(dir_doc, recursive = TRUE)
dir.create(dir_doc, recursive = TRUE, showWarnings = FALSE)

traer <- function(url, destino = NULL, intentos = 4) {
  for (i in seq_len(intentos)) {
    r <- tryCatch(if (is.null(destino)) GET(url, user_agent("R (investigacion academica)"), timeout(120)) else
                    GET(url, user_agent("R (investigacion academica)"), timeout(180), write_disk(destino, overwrite = TRUE)),
                  error = function(e) NULL)
    if (!is.null(r) && status_code(r) == 200) return(r)
    Sys.sleep(3 * i)
  }
  NULL
}
texto <- function(r) content(r, "text", encoding = "UTF-8")

indice <- list()
for (b in boletines) {
  message("Proyecto ", b)
  carpeta <- file.path(dir_doc, b); dir.create(carpeta, showWarnings = FALSE)
  num <- sub("-.*", "", b)
  r <- traer(paste0(base, "&ac=datos_proy&nboletin=", num))
  if (is.null(r)) { message("  no se pudo leer la ficha"); next }
  ficha <- texto(r)
  writeLines(ficha, file.path(carpeta, "datos_proy.html"), useBytes = TRUE)
  proyid <- str_match(ficha, "proyid[^0-9]{1,6}(\\d+)")[, 2]
  if (is.na(proyid)) { message("  sin proyid"); next }
  links <- NULL
  for (ac in c("tramites", "informes", "oficios", "indicaciones", "comparados", "urgencias")) {
    rr <- traer(paste0(base, "&ac=", ac, "&proyid=", proyid))
    if (is.null(rr)) next
    html <- texto(rr)
    writeLines(html, file.path(carpeta, paste0(ac, ".html")), useBytes = TRUE)
    if (!nzchar(html)) next
    pag <- read_html(html)
    a <- xml_find_all(pag, "//a[contains(@href, 'getDocto')]")
    if (length(a) == 0) next
    fila <- xml_find_first(a, "./ancestor::tr[1]")
    links <- rbind(links, data.frame(seccion = ac, texto = str_squish(xml_text(a)),
                                     fila = str_squish(xml_text(fila)), href = xml_attr(a, "href")))
  }
  if (is.null(links)) next
  links$iddocto <- str_match(links$href, "iddocto=(\\d+)")[, 2]
  links$tipodoc <- str_match(links$href, "tipodoc=([a-z_]+)")[, 2]
  links <- links[!is.na(links$iddocto) & !duplicated(links[c("iddocto", "tipodoc")]), ]
  message("  ", nrow(links), " documentos")
  links$archivo <- NA_character_
  for (k in seq_len(nrow(links))) {
    url <- paste0(base, "&ac=getDocto&iddocto=", links$iddocto[k], "&tipodoc=", links$tipodoc[k])
    tmp <- file.path(carpeta, paste0(links$tipodoc[k], "_", links$iddocto[k], ".tmp"))
    rr <- traer(url, tmp)
    if (is.null(rr)) next
    tipo <- headers(rr)[["content-type"]]
    ext <- if (grepl("pdf", tipo)) ".pdf" else if (grepl("msword", tipo)) ".doc" else
      if (grepl("officedocument", tipo)) ".docx" else if (grepl("html", tipo)) ".html" else ".bin"
    final <- sub("\\.tmp$", ext, tmp); file.rename(tmp, final)
    links$archivo[k] <- basename(final)
    Sys.sleep(0.3)
  }
  links$boletin <- b; links$proyid <- proyid
  indice[[b]] <- links
  idn <- str_match(ficha, "idNorma=(\\d+)")[, 2]
  if (!is.na(idn)) traer(paste0("https://www.leychile.cl/Consulta/obtxml?opt=7&idNorma=", idn),
                         file.path(carpeta, paste0("ley_idNorma_", idn, ".xml")))
}
write.csv(do.call(rbind, indice), file.path(dir_doc, "indice_documentos.csv"), row.names = FALSE)
if (file.exists("documentos_mineria.zip")) file.remove("documentos_mineria.zip")
zip::zipr("documentos_mineria.zip", dir_doc)
message("Listo. Sube documentos_mineria.zip (", round(file.size("documentos_mineria.zip") / 1e6, 1), " MB).")
