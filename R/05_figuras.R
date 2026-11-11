# =============================================================================
# 05_figuras.R — figuras del documento
# =============================================================================
source("R/00_funciones.R"); library(ggplot2)
tinta <- "#2b2b29"; tinta2 <- "#6b6a63"
tema <- theme_minimal(base_size = 10, base_family = "sans") +
  theme(panel.grid.minor = element_blank(), panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(color = "#ecebe6", linewidth = 0.3),
        axis.text = element_text(color = tinta2), axis.title = element_text(color = tinta2),
        strip.text = element_text(color = tinta, face = "bold", hjust = 0),
        plot.background = element_rect(fill = "white", color = NA),
        legend.position = "top", legend.justification = "left", legend.title = element_blank(), legend.text = element_text(color = tinta))
guardar <- function(p, nombre, ancho = 16, alto = 9) ggsave(file.path(dir_out, "figuras", nombre), p, width = ancho, height = alto, units = "cm", dpi = 300, bg = "white")
dir.create(file.path(dir_out, "figuras"), showWarnings = FALSE)

# Figura 1. Cuánto del texto de ingreso sobrevive y cuánto del texto final es nuevo
ct <- read_csv(file.path(dir_out, "cambio_texto.csv"), show_col_types = FALSE) |> filter(!is.na(retencion_final)) |>
  left_join(PROYECTOS, by = "boletin") |>
  mutate(etq = paste0(corto, if_else(agenda == "2026", " (2026)", "")), etq = reorder(etq, -retencion_final))
f1 <- ct |> select(etq, `Texto de ingreso que sobrevive en el texto despachado` = retencion_final,
                   `Texto despachado que no estaba en el ingreso` = nuevo_final) |>
  pivot_longer(-etq) |>
  ggplot(aes(value, etq, fill = name)) + geom_col(position = position_dodge(width = 0.75), width = 0.7) +
  geom_text(aes(label = scales::percent(value, accuracy = 1)), position = position_dodge(width = 0.75), hjust = -0.15, size = 2.8, color = tinta) +
  scale_x_continuous(labels = scales::percent, limits = c(0, 1.12), breaks = seq(0, 1, 0.25)) +
  scale_fill_manual(values = c("#2a78d6", "#eb6834")) + labs(x = NULL, y = NULL) + tema + guides(fill = guide_legend(ncol = 1))
guardar(f1, "fig1_cambio_texto.png", alto = 10)

# Figura 2. Disidencia respecto del propio partido
ta <- read_csv(file.path(dir_out, "tablas", "disidencia_tasas.csv"), show_col_types = FALSE) |>
  mutate(votacion = if_else(minera, "Proyectos mineros", "Resto de los proyectos de ley"),
         distrito = if_else(distrito_minero, "Distritos mineros (5% o más de ocupados en minería)", "Resto de los distritos"),
         ee = sqrt(disidencia * (1 - disidencia) / n))
f2 <- ggplot(ta, aes(disidencia, votacion, fill = distrito)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.65) +
  geom_text(aes(label = scales::percent(disidencia, accuracy = 0.1)), position = position_dodge(width = 0.7), hjust = -0.2, size = 3, color = tinta) +
  scale_x_continuous(labels = scales::percent, limits = c(0, 0.09)) + scale_fill_manual(values = c("#eb6834", "#b9b8b0")) +
  labs(x = "Votos contra la mayoría del propio partido", y = NULL) + tema + guides(fill = guide_legend(ncol = 1))
guardar(f2, "fig2_disidencia.png", alto = 7.5)

# Figura 3. Apoyo por bloque en la Cámara: primera y última votación de Sala
ap <- read_csv(file.path(dir_out, "tablas", "apoyo_bloques.csv"), show_col_types = FALSE) |>
  filter(camara == "Cámara", boletin %in% c("18216-05", "18259-08", "11608-09", "16566-03", "12093-08"), bloque != "Sin partido") |>
  mutate(bloque = factor(bloque, rev(ORDEN_BLOQUES)), hito = factor(hito, c("Primera votación", "Última votación")),
         corto = factor(corto, c("Royalty minero", "Ley Marco de Autorizaciones Sectoriales", "Desalinización", "Reconstrucción Nacional", "Patentes y amparo minero"),
                        c("Royalty minero (2021-2023)", "Autorizaciones sectoriales (2024-2025)", "Desalinización (2026)", "Reconstrucción Nacional (2026)", "Patentes y amparo (2026)")))
f3 <- ggplot(ap, aes(apoyo, bloque)) + geom_segment(aes(x = 0, xend = apoyo, yend = bloque), color = "#d9d8d2", linewidth = 1.2) +
  geom_point(aes(size = n), color = "#2a78d6") + facet_grid(hito ~ corto, labeller = labeller(corto = label_wrap_gen(16))) +
  scale_x_continuous(labels = scales::percent, breaks = c(0, 0.5, 1), limits = c(0, 1.05)) + scale_size_area(max_size = 3, guide = "none") +
  labs(x = "Votos a favor dentro del bloque", y = NULL) + tema + theme(panel.spacing.x = unit(0.8, "lines"), strip.text = element_text(size = 8))
guardar(f3, "fig3_apoyo_bloques.png", ancho = 17, alto = 10)
message("Figuras listas")
