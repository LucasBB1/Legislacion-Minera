# La agenda minera en el Congreso chileno: votos, partidos, territorio y cambios en el texto, 2018–2026

Código en R y datos del **Documento de trabajo N.° 6** de Lucas Bottinelli Baltra. El estudio mide cuánto cambia el texto de los proyectos de ley que afectan a la minería durante su tramitación, cuánto tardan y qué explica el voto de los diputados, con el bloque, la posición ideológica y el empleo minero del distrito como explicaciones rivales. Analiza 21 proyectos seleccionados con un criterio explícito, con pruebas de robustez, un modelo para el Senado y el caso de la Estrategia Nacional del Litio.

**Documento:** [https://doi.org/10.5281/zenodo.24149934](https://doi.org/10.5281/zenodo.24149934) · **Código y datos:** [https://doi.org/10.5281/zenodo.24150095](https://doi.org/10.5281/zenodo.24150095)

**Autor:** Lucas Bottinelli Baltra · [lucasbb1.github.io](https://lucasbb1.github.io) · ORCID [0009-0000-4865-7261](https://orcid.org/0009-0000-4865-7261)

## Scripts

| Script | Qué hace |
|---|---|
| `R/00_funciones.R` | Paquetes, universo de proyectos, bloques y estimador MCO con dos efectos fijos y errores agrupados |
| `R/01_descargar_documentos.R` | Descarga del Senado los documentos de tramitación de los 13 proyectos identificados por los títulos de las votaciones y el texto de las leyes desde LeyChile |
| `R/01b_descargar_ampliacion.R` | Descarga los documentos de los 8 proyectos cuyo título de votación no menciona la minería y de la agenda de 2026 |
| `R/01c_convertir_texto.R` | Convierte los documentos a texto (LibreOffice y pdftotext) y agrega el número de palabras al índice |
| `R/01d_cambio_texto.R` | Retención y novedad del texto entre el ingreso, el primer trámite y el despacho. Con el argumento 3 u 8 repite el cálculo con secuencias de otra longitud |
| `R/01e_tramitacion.R` | Duración, urgencias, comisión mixta, Tribunal Constitucional y veto |
| `R/02_base_camara.R` | Base diputado-votación de la Cámara 2018-2026 con partido, posición, distrito y empleo minero |
| `R/03_modelos_camara.R` | Modelos de disidencia partidaria y de voto con la derecha |
| `R/04_coaliciones.R` | Apoyo por bloque en las votaciones clave de Cámara y Senado |
| `R/05_figuras.R` | Figuras del documento |
| `R/06_seguimiento.R` | Apoyo por bloque en los proyectos en curso de 2026 (SEIA, SMA y mercado de capitales) |
| `R/07_robustez.R` | Pruebas de robustez del modelo de disidencia, con exclusión de proyectos y distritos, umbrales alternativos y permutación |
| `R/08_figura_robustez.R` | Figura 4 y Figura B1 |
| `R/09_senado.R` | Modelo de disidencia en el Senado con el empleo minero de cada circunscripción |
| `R/10_bloques.R` | Mediana de los puntos ideales por partido y bloque, y asignación de los partidos sin familia clara |
| `R/11_bloques_justificacion.R` | Evidencia para la clasificación en bloques: saltos entre medianas de partidos, coincidencia de las mayorías partidarias por gobierno y robustez del modelo B a clasificaciones alternativas (Tablas C3 a C5) |
| `documento/build6.py` | Arma el documento Word desde `documento/manuscrito.md` |

Orden de ejecución: los scripts de `R/` en orden numérico (01, 01b, 01c, 01d, 01e y 02 a 11). La conversión a texto requiere LibreOffice (`soffice`) y `pdftotext` (poppler). En Linux, ejecutar los scripts con `LC_ALL=C.UTF-8`. El documento Word se arma con `documento/build6.py` (Python y python-docx), igual que en los documentos de trabajo anteriores.

## Datos

- `data/processed/`: votaciones y votos de Sala de la Cámara y del Senado, militancia y posición por trimestre y distrito de cada diputado, tomados de la base del Documento de trabajo N.° 5 (https://doi.org/10.5281/zenodo.24026991).
- `data/fuentes/`: empleo minero por comuna, distrito y circunscripción (INE, Censo 2024), correspondencia entre comunas y distritos (SERVEL), circunscripción de cada senador 2018–2030 e índice de los 427 documentos de tramitación.
- `data/raw/`: no se versiona; se genera con el script 01.

## Licencia

Código bajo licencia MIT (`LICENSE`). Datos, tablas, figuras y texto bajo CC BY 4.0 (`LICENSE-DATOS.md`). Para citar, ver `CITATION.cff`.

## Cómo citar

Bottinelli Baltra, L. (2026). *La agenda minera en el Congreso chileno: votos, partidos, territorio y cambios en el texto, 2018–2026* (Documento de trabajo N.° 6). https://doi.org/10.5281/zenodo.24149934

Código y datos: Bottinelli Baltra, L. (2026). *Legislacion-Minera* (v1.1.0) [Software]. Zenodo. https://doi.org/10.5281/zenodo.24150095
