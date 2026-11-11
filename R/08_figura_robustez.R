# =============================================================================
# 08_figura_robustez.R — Figura 4 (coeficientes de las pruebas de robustez) y
# Figura B1 (distribución de los coeficientes permutados)
# =============================================================================
source("R/00_funciones.R"); library(ggplot2)
tinta <- "#2b2b29"; tinta2 <- "#6b6a63"
tema <- theme_minimal(base_size = 10, base_family = "sans") +
  theme(panel.grid.minor = element_blank(), panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(color = "#ecebe6", linewidth = 0.3),
        axis.text = element_text(color = tinta2), axis.title = element_text(color = tinta2),
        plot.background = element_rect(fill = "white", color = NA), legend.position = "none")
guardar <- function(p, nombre, ancho = 16, alto = 9) ggsave(file.path(dir_out, "figuras", nombre), p, width = ancho, height = alto, units = "cm", dpi = 300, bg = "white")

rob <- bind_rows(read_csv(file.path(dir_out, "tablas", "robustez.csv"), show_col_types = FALSE),
                 read_csv(file.path(dir_out, "tablas", "robustez_senado.csv"), show_col_types = FALSE)) |>
  filter(!is.na(ee), !str_detect(prueba, "desde 3%")) |>
  mutate(grupo = case_when(prueba == "Especificación principal" ~ "Principal",
                           str_detect(prueba, "^Senado") ~ "Senado", TRUE ~ "Cámara"),
         prueba = factor(prueba, levels = rev(prueba)))
f4 <- ggplot(rob, aes(coef, prueba, color = grupo)) +
  geom_vline(xintercept = 0, color = "#b9b8b0", linewidth = 0.4) +
  geom_vline(xintercept = rob$coef[rob$grupo == "Principal"], color = "#eb6834", linewidth = 0.3, linetype = "dashed") +
  geom_errorbarh(aes(xmin = coef - 1.96 * ee, xmax = coef + 1.96 * ee), height = 0, linewidth = 0.6) +
  geom_point(size = 1.8) +
  scale_color_manual(values = c("Principal" = "#eb6834", "Cámara" = "#2a78d6", "Senado" = "#6b6a63")) +
  scale_x_continuous(labels = function(x) paste0(round(100 * x), " pp")) +
  labs(x = "Efecto en puntos porcentuales", y = NULL) +
  tema + theme(axis.title.x = element_text(size = 8))
guardar(f4, "fig4_robustez.png", alto = 11)

perm <- read_csv(file.path(dir_out, "tablas", "permutacion_distritos.csv"), show_col_types = FALSE)
obs <- read_csv(file.path(dir_out, "tablas", "robustez.csv"), show_col_types = FALSE) |> filter(str_detect(prueba, "^Permutación")) |> pull(coef)
fb1 <- ggplot(perm, aes(coef)) + geom_histogram(bins = 40, fill = "#b9b8b0", color = "white", linewidth = 0.2) +
  geom_vline(xintercept = obs, color = "#eb6834", linewidth = 0.8) +
  annotate("text", x = obs, y = Inf, label = "Distritos mineros observados", hjust = 1.05, vjust = 1.5, size = 3, color = "#eb6834") +
  scale_x_continuous(labels = function(x) paste0(round(100 * x, 1), " pp")) +
  labs(x = "Coeficiente con siete distritos sorteados al azar", y = "Sorteos") + tema +
  theme(panel.grid.major.y = element_line(color = "#ecebe6", linewidth = 0.3))
guardar(fb1, "figB1_permutacion.png", alto = 8)
