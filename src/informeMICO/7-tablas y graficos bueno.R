library(readr)
library(dplyr)
library(ggplot2)
library(flextable)
library(officer)
library(data.table)
library(scales)

rm(list = ls())

# Crear carpeta
dir.create("out/informeMICO/tablas_y_graficos", showWarnings = FALSE)

# ==============================================================================
# BLOQUE 1 - Régimen de tenencia
# ==============================================================================

df <- read_csv("out/informeMICO/1-inequality-tenancy_multi.csv")
df2 <- read_csv("out/informeMICO/1-inequality-tenancy_multi2.csv")

df2_casero_multi <- df2 %>%
  filter(regten == "Casero_Multi")

df <- bind_rows(df, df2_casero_multi) %>%
  mutate(regten = factor(regten, levels = c(
    "Alquiler", "Propiedad",
    "Casero1", "Casero_Multi", "Casero2", "Casero3", "Casero4+"
  ))) %>%
  arrange(year, regten)

df_last_year <- df %>%
  filter(year == max(year), regten != "Cesion") %>%
  select(
    year, regten, median_renthog, median_riquezanet,
    ratio_median_income_alq, ratio_median_wealth_alq
  ) %>%
  mutate(
    year = as.integer(year),
    median_renthog = floor(median_renthog),
    median_riquezanet = floor(median_riquezanet),
    across(c(ratio_median_income_alq, ratio_median_wealth_alq), ~ round(., 2))
  )

# Función para formatear tabla con jerarquía
format_tabla <- function(data, col_valor, col_ratio) {
  data %>%
    rename(valor = all_of(col_valor), ratio = all_of(col_ratio)) %>%
    mutate(
      categoria = case_when(
        regten == "Alquiler" ~ "Alquiler",
        regten == "Propiedad" ~ "Propiedad",
        regten == "Casero1" ~ "Casero (1 vivienda en alquiler)",
        regten == "Casero_Multi" ~ "Multiarrendadores",
        regten == "Casero2" ~ "Dos viviendas en alquiler",
        regten == "Casero3" ~ "Tres viviendas en alquiler",
        regten == "Casero4+" ~ "Cuatro o más viviendas en alquiler"
      ),
      es_subcategoria = regten %in% c("Casero2", "Casero3", "Casero4+"),
      valor = paste0(format(valor, big.mark = ".", decimal.mark = ",", scientific = FALSE), " €"),
      ratio = format(ratio, decimal.mark = ",")
    ) %>%
    select(categoria, es_subcategoria, valor, ratio)
}

# Tabla renta
df_renta_fmt <- format_tabla(df_last_year, "median_renthog", "ratio_median_income_alq")

ft_renta <- flextable(df_renta_fmt %>% select(categoria, valor, ratio)) %>%
  set_header_labels(categoria = "", valor = "Renta mediana", ratio = "Ratio de desigualdad renta") %>%
  padding(i = which(df_renta_fmt$es_subcategoria), j = 1, padding.left = 30) %>%
  theme_booktabs() %>%
  autofit()

doc <- read_docx() %>% body_add_flextable(ft_renta)
print(doc, target = "out/informeMICO/tablas_y_graficos/tabla_last_year_renta.docx")



# Gráfico renta - grupos principales
g_tabla_renta_main <- ggplot(
  df_last_year %>% filter(regten %in% c("Alquiler", "Propiedad", "Casero1", "Casero_Multi")),
  aes(
    x = factor(regten, levels = c("Casero_Multi", "Casero1", "Propiedad", "Alquiler")),
    y = median_renthog
  )
) +
  geom_col(fill = "#1a5276") +
  geom_text(aes(label = paste0(format(median_renthog, big.mark = ".", decimal.mark = ","), " €")),
    hjust = -0.1, size = 3.2
  ) +
  coord_flip() +
  scale_x_discrete(labels = c(
    "Alquiler" = "Inquilinos",
    "Propiedad" = "Propietarios",
    "Casero1" = "Arrendadores\n(una vivienda en alquiler)",
    "Casero_Multi" = "Multiarrendadores"
  )) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.25)),
    labels = function(x) paste0(format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE), " €")
  ) +
  labs(
    title = "El mercado del alquiler como mecanismo de transferencia de rentas",
    subtitle = "Renta mediana anual según la relación de los hogares con la vivienda (2022)",
    x = NULL, y = NULL
  ) +
  theme_minimal() +
  theme(
    panel.grid.major.y = element_blank(),
    legend.position = "none",
    plot.title = element_text(face = "bold", size = 12),
    plot.subtitle = element_text(color = "grey50", size = 9)
  )

print(g_tabla_renta_main)
ggsave("out/informeMICO/tablas_y_graficos/grafico_tabla_renta_main.png", g_tabla_renta_main, width = 9, height = 5, dpi = 300)

# Gráfico renta - arrendadores y multiarrendadores por número de viviendas
g_tabla_renta_multi <- ggplot(
  df_last_year %>% filter(regten %in% c("Casero1", "Casero2", "Casero3", "Casero4+")),
  aes(
    x = factor(regten, levels = c("Casero4+", "Casero3", "Casero2", "Casero1")),
    y = median_renthog
  )
) +
  geom_col(fill = "#7fbfff") +
  geom_text(aes(label = paste0(format(median_renthog, big.mark = ".", decimal.mark = ","), " €")),
    hjust = -0.1, size = 3.2
  ) +
  coord_flip() +
  scale_x_discrete(labels = c(
    "Casero1"  = "Arrendadores\n(una vivienda en alquiler)",
    "Casero2"  = "Multiarrendadores\n(dos viviendas en alquiler)",
    "Casero3"  = "Multiarrendadores\n(tres viviendas en alquiler)",
    "Casero4+" = "Multiarrendadores\n(cuatro o más viviendas en alquiler)"
  )) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.25)),
    labels = function(x) paste0(format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE), " €")
  ) +
  labs(
    title = "La renta crece con el número de viviendas en alquiler",
    subtitle = "Renta mediana anual según número de viviendas en alquiler (2022)",
    x = NULL, y = NULL
  ) +
  theme_minimal() +
  theme(
    panel.grid.major.y = element_blank(),
    legend.position = "none",
    plot.title = element_text(face = "bold", size = 12),
    plot.subtitle = element_text(color = "grey50", size = 9)
  )

print(g_tabla_renta_multi)
ggsave("out/informeMICO/tablas_y_graficos/grafico_tabla_renta_multi.png", g_tabla_renta_multi, width = 9, height = 5, dpi = 300)




# Tabla riqueza
df_riqueza_fmt <- format_tabla(df_last_year, "median_riquezanet", "ratio_median_wealth_alq")

ft_riqueza <- flextable(df_riqueza_fmt %>% select(categoria, valor, ratio)) %>%
  set_header_labels(categoria = "", valor = "Riqueza neta mediana", ratio = "Ratio de desigualdad riqueza") %>%
  padding(i = which(df_riqueza_fmt$es_subcategoria), j = 1, padding.left = 30) %>%
  theme_booktabs() %>%
  autofit()

doc <- read_docx() %>% body_add_flextable(ft_riqueza)
print(doc, target = "out/informeMICO/tablas_y_graficos/tabla_last_year_riqueza.docx")

# Gráfico tabla riqueza

# Gráfico riqueza - grupos principales
g_tabla_riqueza_main <- ggplot(
  df_last_year %>% filter(regten %in% c("Alquiler", "Propiedad", "Casero1", "Casero_Multi")),
  aes(
    x = factor(regten, levels = c("Casero_Multi", "Casero1", "Propiedad", "Alquiler")),
    y = median_riquezanet
  )
) +
  geom_col(fill = "#1a5276") +
  geom_text(aes(label = paste0(format(median_riquezanet, big.mark = ".", decimal.mark = ","), " €")),
    hjust = -0.1, size = 3.2
  ) +
  coord_flip() +
  scale_x_discrete(labels = c(
    "Alquiler" = "Inquilinos",
    "Propiedad" = "Propietarios",
    "Casero1" = "Arrendadores\n(una vivienda en alquiler)",
    "Casero_Multi" = "Multiarrendadores"
  )) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.25)),
    labels = function(x) paste0(format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE), " €")
  ) +
  labs(
    title = "Los multiarrendadores acumulan 450 veces más riqueza que los inquilinos",
    subtitle = "Renta mediana anual según la relación de los hogares con la vivienda (2022)",
    x = NULL, y = NULL
  ) +
  theme_minimal() +
  theme(
    panel.grid.major.y = element_blank(),
    legend.position = "none",
    plot.title = element_text(face = "bold", size = 12),
    plot.subtitle = element_text(color = "grey50", size = 9)
  )

print(g_tabla_riqueza_main)
ggsave("out/informeMICO/tablas_y_graficos/grafico_tabla_riqueza_main.png", g_tabla_riqueza_main, width = 9, height = 5, dpi = 300)

# Gráfico riqueza - arrendadores y multiarrendadores por número de viviendas
g_tabla_riqueza_multi <- ggplot(
  df_last_year %>% filter(regten %in% c("Casero1", "Casero2", "Casero3", "Casero4+")),
  aes(
    x = factor(regten, levels = c("Casero4+", "Casero3", "Casero2", "Casero1")),
    y = median_riquezanet
  )
) +
  geom_col(fill = "#7fbfff") +
  geom_text(aes(label = paste0(format(median_riquezanet, big.mark = ".", decimal.mark = ","), " €")),
    hjust = -0.1, size = 3.2
  ) +
  coord_flip() +
  scale_x_discrete(labels = c(
    "Casero1"  = "Arrendadores\n(una vivienda en alquiler)",
    "Casero2"  = "Multiarrendadores\n(dos viviendas en alquiler)",
    "Casero3"  = "Multiarrendadores\n(tres viviendas en alquiler)",
    "Casero4+" = "Multiarrendadores\n(cuatro o más viviendas en alquiler)"
  )) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.25)),
    labels = function(x) paste0(format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE), " €")
  ) +
  labs(
    title = "La riqueza crece con el número de viviendas en alquiler",
    subtitle = "Riqueza neta mediana según número de viviendas en alquiler (2022)",
    x = NULL, y = NULL
  ) +
  theme_minimal() +
  theme(
    panel.grid.major.y = element_blank(),
    legend.position = "none",
    plot.title = element_text(face = "bold", size = 12),
    plot.subtitle = element_text(color = "grey50", size = 9)
  )

print(g_tabla_riqueza_multi)
ggsave("out/informeMICO/tablas_y_graficos/grafico_tabla_riqueza_multi.png", g_tabla_riqueza_multi, width = 9, height = 5, dpi = 300)



# ==============================================================================
# BLOQUE 2 - Edad
# ==============================================================================

df_age <- read_csv("out/informeMICO/2-inequality-age.csv", locale = locale(encoding = "latin1"))

df_age_last_year <- df_age %>%
  filter(year == max(year)) %>%
  mutate(
    year = as.integer(year),
    median_renthog = floor(median_renthog),
    median_riquezanet = floor(median_riquezanet),
    ratio_median_renthog = round(ratio_median_renthog, 2),
    ratio_median_wealth = round(ratio_median_wealth, 2)
  )

niveles_edad <- c(
  "Mayores de 74 anos", "Entre 65 y 74 anos", "Entre 55 y 64 anos",
  "Entre 45 y 54 anos", "Entre 35 y 44 anos", "Menor de 35 anos"
)

labels_edad <- c(
  "Menor de 35 anos"    = "Menor de 35 años",
  "Entre 35 y 44 anos"  = "Entre 35 y 44 años",
  "Entre 45 y 54 anos"  = "Entre 45 y 54 años",
  "Entre 55 y 64 anos"  = "Entre 55 y 64 años",
  "Entre 65 y 74 anos"  = "Entre 65 y 74 años",
  "Mayores de 74 anos"  = "Mayores de 74 años"
)

# Tabla renta edad
ft_age_renta <- df_age_last_year %>%
  mutate(
    valor = paste0(format(median_renthog, big.mark = ".", decimal.mark = ",", scientific = FALSE), " €"),
    ratio = format(ratio_median_renthog, decimal.mark = ",")
  ) %>%
  select(bage, valor, ratio) %>%
  flextable() %>%
  set_header_labels(bage = "", valor = "Renta mediana", ratio = "Ratio de desigualdad renta") %>%
  theme_booktabs() %>%
  autofit()

doc <- read_docx() %>% body_add_flextable(ft_age_renta)
print(doc, target = "out/informeMICO/tablas_y_graficos/tabla_age_renta.docx")

# Gráfico tabla renta edad
g_tabla_age_renta <- ggplot(
  df_age_last_year,
  aes(x = factor(bage, levels = niveles_edad), y = median_renthog)
) +
  geom_col(fill = "#2980b9") +
  geom_text(aes(label = paste0(format(median_renthog, big.mark = ".", decimal.mark = ","), " €")),
    hjust = -0.1, size = 3.2
  ) +
  coord_flip() +
  scale_x_discrete(labels = labels_edad) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.25)),
    breaks = seq(0, 40000, by = 5000),
    labels = function(x) paste0(format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE), " €")
  ) +
  labs(
    title = "No existe brecha generacional en los ingresos",
    subtitle = "Renta mediana anual por tramos de edad (2022)",
    x = NULL, y = NULL
  ) +
  theme_minimal() +
  theme(
    panel.grid.major.y = element_blank(),
    plot.title = element_text(face = "bold", size = 12),
    plot.subtitle = element_text(color = "grey50", size = 9)
  )

print(g_tabla_age_renta)
ggsave("out/informeMICO/tablas_y_graficos/grafico_tabla_renta_edad.png", g_tabla_age_renta, width = 9, height = 5, dpi = 300)

# Tabla riqueza edad
ft_age_riqueza <- df_age_last_year %>%
  mutate(
    valor = paste0(format(median_riquezanet, big.mark = ".", decimal.mark = ",", scientific = FALSE), " €"),
    ratio = format(ratio_median_wealth, decimal.mark = ",")
  ) %>%
  select(bage, valor, ratio) %>%
  flextable() %>%
  set_header_labels(bage = "", valor = "Riqueza neta mediana", ratio = "Ratio de desigualdad riqueza") %>%
  theme_booktabs() %>%
  autofit()

doc <- read_docx() %>% body_add_flextable(ft_age_riqueza)
print(doc, target = "out/informeMICO/tablas_y_graficos/tabla_age_riqueza.docx")

# Gráfico tabla riqueza edad
g_tabla_age_riqueza <- ggplot(
  df_age_last_year,
  aes(x = factor(bage, levels = niveles_edad), y = median_riquezanet)
) +
  geom_col(fill = "#2980b9") +
  geom_text(aes(label = paste0(format(median_riquezanet, big.mark = ".", decimal.mark = ","), " €")),
    hjust = -0.1, size = 3.2
  ) +
  coord_flip() +
  scale_x_discrete(labels = labels_edad) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.25)),
    breaks = seq(0, 250000, by = 25000),
    labels = function(x) paste0(format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE), " €")
  ) +
  labs(
    title = "La desigualdad generacional es de patrimonio",
    subtitle = "Riqueza neta mediana según grupo de edad (2022)",
    x = NULL, y = NULL
  ) +
  theme_minimal() +
  theme(
    panel.grid.major.y = element_blank(),
    plot.title = element_text(face = "bold", size = 12),
    plot.subtitle = element_text(color = "grey50", size = 9)
  )

print(g_tabla_age_riqueza)
ggsave("out/informeMICO/tablas_y_graficos/grafico_tabla_riqueza_edad.png", g_tabla_age_riqueza, width = 9, height = 5, dpi = 300)


# ==============================================================================
# BLOQUE 3 - Desigualdad generacional vs. posición en el mercado
# ==============================================================================

tenencia <- data.frame(
  grupo = c("Inquilinos", "Propietarios", "Arrendadores\n(una vivienda en alquiler)", "Multiarrendadores"),
  renta = c(
    df_last_year %>% filter(regten == "Alquiler") %>% pull(median_renthog),
    df_last_year %>% filter(regten == "Propiedad") %>% pull(median_renthog),
    df_last_year %>% filter(regten == "Casero1") %>% pull(median_renthog),
    df_last_year %>% filter(regten == "Casero_Multi") %>% pull(median_renthog)
  ),
  riqueza = c(
    df_last_year %>% filter(regten == "Alquiler") %>% pull(median_riquezanet),
    df_last_year %>% filter(regten == "Propiedad") %>% pull(median_riquezanet),
    df_last_year %>% filter(regten == "Casero1") %>% pull(median_riquezanet),
    df_last_year %>% filter(regten == "Casero_Multi") %>% pull(median_riquezanet)
  )
)

edad <- data.frame(
  grupo = c("Menor de 35", "35-44 años", "45-54 años", "55-64 años", "65-74 años", "Mayor de 74"),
  renta = c(
    df_age_last_year %>% filter(bage == "Menor de 35 anos") %>% pull(median_renthog),
    df_age_last_year %>% filter(bage == "Entre 35 y 44 anos") %>% pull(median_renthog),
    df_age_last_year %>% filter(bage == "Entre 45 y 54 anos") %>% pull(median_renthog),
    df_age_last_year %>% filter(bage == "Entre 55 y 64 anos") %>% pull(median_renthog),
    df_age_last_year %>% filter(bage == "Entre 65 y 74 anos") %>% pull(median_renthog),
    df_age_last_year %>% filter(bage == "Mayores de 74 anos") %>% pull(median_renthog)
  ),
  riqueza = c(
    df_age_last_year %>% filter(bage == "Menor de 35 anos") %>% pull(median_riquezanet),
    df_age_last_year %>% filter(bage == "Entre 35 y 44 anos") %>% pull(median_riquezanet),
    df_age_last_year %>% filter(bage == "Entre 45 y 54 anos") %>% pull(median_riquezanet),
    df_age_last_year %>% filter(bage == "Entre 55 y 64 anos") %>% pull(median_riquezanet),
    df_age_last_year %>% filter(bage == "Entre 65 y 74 anos") %>% pull(median_riquezanet),
    df_age_last_year %>% filter(bage == "Mayores de 74 anos") %>% pull(median_riquezanet)
  )
)

tenencia$dimension <- "Régimen de tenencia"
edad$dimension <- "Grupo de edad"

df_renta <- bind_rows(
  tenencia %>% select(grupo, valor = renta, dimension),
  edad %>% select(grupo, valor = renta, dimension)
)
df_riqueza <- bind_rows(
  tenencia %>% select(grupo, valor = riqueza, dimension),
  edad %>% select(grupo, valor = riqueza, dimension)
)

niveles <- c(
  "Inquilinos", "Propietarios", "Arrendadores\n(una vivienda en alquiler)", "Multiarrendadores",
  "Menor de 35", "35-44 años", "45-54 años", "55-64 años", "65-74 años", "Mayor de 74"
)
df_renta$grupo <- factor(df_renta$grupo, levels = rev(niveles))
df_riqueza$grupo <- factor(df_riqueza$grupo, levels = rev(niveles))

colores <- c("Régimen de tenencia" = "#2E4057", "Grupo de edad" = "#4472C4")
fmt_euro <- function(x) paste0(format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE), " €")

g_renta_comp <- ggplot(df_renta, aes(x = valor, y = grupo, fill = dimension)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = fmt_euro(valor)), hjust = -0.1, size = 3.2, color = "grey30") +
  scale_fill_manual(values = colores, name = NULL) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.22)), labels = fmt_euro) +
  labs(
    title = "La relación con la vivienda divide más que la edad",
    subtitle = "Riqueza neta mediana según relación con la vivienda y según grupo de edad (2022)",
    x = NULL, y = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "bottom", panel.grid.major.y = element_blank(),
    plot.title = element_text(face = "bold", size = 12),
    plot.subtitle = element_text(color = "grey50", size = 9)
  )

print(g_renta_comp)
ggsave("out/informeMICO/tablas_y_graficos/grafico_renta_comparado.png", g_renta_comp, width = 8, height = 6, dpi = 300)

g_riqueza_comp <- ggplot(df_riqueza, aes(x = valor, y = grupo, fill = dimension)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = fmt_euro(valor)), hjust = -0.1, size = 3.2, color = "grey30") +
  scale_fill_manual(values = colores, name = NULL) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.22)), labels = fmt_euro) +
  labs(
    title = "Las diferencias asociadas a la posición en el mercado de la vivienda \nson muy superiores a las observadas entre grupos de edad",
    subtitle = "Riqueza neta mediana según relación con la vivienda y según grupo de edad (2022)",
    x = NULL, y = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "bottom", panel.grid.major.y = element_blank(),
    plot.title = element_text(face = "bold", size = 12),
    plot.subtitle = element_text(color = "grey50", size = 9)
  )

print(g_riqueza_comp)
ggsave("out/informeMICO/tablas_y_graficos/grafico_riqueza_comparado.png", g_riqueza_comp, width = 8, height = 6, dpi = 300)


# ==============================================================================
# BLOQUE 3 - Propietarios y edad
# ==============================================================================
df_homeownership <- read_csv("out/informeMICO/2-homeownership-age.csv") %>%
  mutate(
    bage = recode(bage,
      "Menor de 35 anos"   = "Menor de 35 años",
      "Entre 35 y 44 anos" = "Entre 35 y 44 años",
      "Entre 45 y 54 anos" = "Entre 45 y 54 años",
      "Entre 55 y 64 anos" = "Entre 55 y 64 años",
      "Entre 65 y 74 anos" = "Entre 65 y 74 años",
      "Mayores de 74 anos" = "Mayores de 74 años"
    ),
    bage = factor(bage, levels = c(
      "Menor de 35 años", "Entre 35 y 44 años", "Entre 45 y 54 años",
      "Entre 55 y 64 años", "Entre 65 y 74 años", "Mayores de 74 años"
    )),
    pct = round(homeownership_rate * 100, 1)
  )

# Grafico 2011 vs 2022
df_homeownership_comp <- df_homeownership %>%
  filter(year %in% c(2011, 2022)) %>%
  mutate(year = factor(year))

g_propietarios_evol <- ggplot(df_homeownership_comp, aes(x = bage, y = pct, fill = year)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = paste0(pct, "%")),
    position = position_dodge(width = 0.9), hjust = -0.1, size = 3.2
  ) +
  coord_flip() +
  scale_fill_manual(values = c("2011" = "#aed6f1", "2022" = "#1a5276")) +
  scale_y_continuous(
    labels = scales::percent_format(scale = 1),
    expand = expansion(mult = c(0, 0.15))
  ) +
  labs(
    title = NULL,
    subtitle = "Porcentaje de hogares propietarios de su vivienda principal por grupo de edad (2011 y 2022)",
    x = NULL, y = NULL, fill = NULL
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 12),
    plot.subtitle = element_text(color = "grey50", size = 9)
  )

print(g_propietarios_evol)
ggsave("out/informeMICO/tablas_y_graficos/grafico_propietarios_evol.png", g_propietarios_evol, width = 9, height = 5, dpi = 300)



# Grafico evolucion todos los años
g_propietarios_lineas <- ggplot(df_homeownership, aes(x = year, y = pct, color = bage)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = unique(df_homeownership$year)) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  scale_color_manual(values = c(
    "Menor de 35 años"    = "#aed6f1",
    "Entre 35 y 44 años"  = "#5dade2",
    "Entre 45 y 54 años"  = "#2e86c1",
    "Entre 55 y 64 años"  = "#1a5276",
    "Entre 65 y 74 años"  = "#154360",
    "Mayores de 74 años"  = "#0b2545"
  )) +
  labs(
    title = NULL,
    subtitle = "Porcentaje de hogares propietarios de su vivienda principal por grupo de edad (2002-2022)",
    x = NULL, y = NULL, color = NULL
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 12),
    plot.subtitle = element_text(color = "grey50", size = 9)
  )

print(g_propietarios_lineas)
ggsave("out/informeMICO/tablas_y_graficos/grafico_propietarios_lineas.png", g_propietarios_lineas, width = 9, height = 5, dpi = 300)



# ==============================================================================
# BLOQUE 4 - Riqueza inmobiliaria
# ==============================================================================

dt <- fread("out/informeMICO/IPC_2022/4-inmo-inequality-quantiles.csv")
dt_base <- dt[riquezainmo %in% c("riquezainmo.0.9", "riquezainmo.0.95", "riquezainmo.0.99")]

dt_top <- dt_base[, .(year, riquezainmo, share = 100 - share_riquezainmo)]
dt_top[riquezainmo == "riquezainmo.0.9", riquezainmo := "3. 10% superior"]
dt_top[riquezainmo == "riquezainmo.0.95", riquezainmo := "2. 5% superior"]
dt_top[riquezainmo == "riquezainmo.0.99", riquezainmo := "1. 1% superior"]

dt_bottom <- dt_base[
  riquezainmo == "riquezainmo.0.9",
  .(year, riquezainmo = "4. 90% inferior", share = share_riquezainmo)
]

dt_final <- rbind(dt_top, dt_bottom)
setorder(dt_final, year, riquezainmo)

total_riquezainmo <- fread("out/informeMICO/3-inequality-ratio50_1.csv")[, .(year, total_wealth_absolute)]
ipc <- fread("datasets/IPC/base2022.csv")[year %in% total_riquezainmo$year]
total_riquezainmo[ipc, total_riquezainmo_IPC := total_wealth_absolute * 100 / i.ipc, on = "year"]
dt_final[total_riquezainmo, riquezainmo_total_IPC := i.total_riquezainmo_IPC * (share / 100), on = "year"]

# Gráfico barras último año
df_inmo_select_last <- dt_final %>%
  filter(year == max(year)) %>%
  mutate(
    grupo = factor(riquezainmo, levels = c("4. 90% inferior", "3. 10% superior", "2. 5% superior", "1. 1% superior")),
    color_grupo = case_when(
      riquezainmo == "4. 90% inferior" ~ "inferior",
      riquezainmo == "3. 10% superior" ~ "top10",
      riquezainmo == "2. 5% superior" ~ "top5",
      riquezainmo == "1. 1% superior" ~ "top1"
    )
  )


# Gráfico 1: 90% vs 10%
df_g1 <- df_inmo_select_last %>%
  filter(grupo %in% c("4. 90% inferior", "3. 10% superior"))
g1 <- ggplot(df_g1, aes(
  x = factor(grupo, levels = c("3. 10% superior", "4. 90% inferior")),
  y = share, fill = color_grupo
)) +
  geom_col() +
  geom_text(aes(label = paste0(round(share, 1), "%")), vjust = -0.5, size = 5) +
  scale_fill_manual(values = c("inferior" = "#7fbfff", "top10" = "#1a5276")) +
  scale_x_discrete(labels = c("4. 90% inferior" = "90% inferior", "3. 10% superior" = "10% superior")) +
  scale_y_continuous(
    labels = scales::percent_format(scale = 1),
    expand = expansion(mult = c(0, 0.1))
  ) +
  labs(
    title = "El 10% de los hogares acumula el 42% de toda la riqueza en vivienda",
    subtitle = "Distribución de la riqueza en vivienda por grupos de patrimonio (2022)",
    x = "", y = NULL
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", size = 12),
    plot.subtitle = element_text(color = "grey50", size = 9)
  )

print(g1)
ggsave("out/informeMICO/tablas_y_graficos/grafico_concentracion_90_10.png", g1, width = 6, height = 5, dpi = 300)

# Gráfico 2: dentro del 10% superior
df_g2 <- df_inmo_select_last %>%
  filter(grupo %in% c("3. 10% superior", "2. 5% superior", "1. 1% superior"))

g2 <- ggplot(df_g2, aes(x = grupo, y = share, fill = color_grupo)) +
  geom_col() +
  geom_text(aes(label = paste0(round(share, 1), "%")), vjust = -0.5, size = 5) +
  scale_fill_manual(values = c("top10" = "#1a5276", "top5" = "#1f618d", "top1" = "#2980b9")) +
  scale_x_discrete(labels = c(
    "3. 10% superior" = "10% superior",
    "2. 5% superior" = "5% superior",
    "1. 1% superior" = "1% superior"
  )) +
  scale_y_continuous(
    labels = scales::percent_format(scale = 1),
    expand = expansion(mult = c(0, 0.1))
  ) +
  labs(
    title = "La concentración de riqueza en vivienda se intensifica en la cima",
    subtitle = "Distribución de la riqueza en vivienda dentro del 10% superior (2022)",
    x = "", y = NULL
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", size = 12),
    plot.subtitle = element_text(color = "grey50", size = 9)
  )

print(g2)
ggsave("out/informeMICO/tablas_y_graficos/grafico_concentracion_top10.png", g2, width = 6, height = 5, dpi = 300)



# Gráfico evolución 90% vs 10%
df_inmo_select_evol <- dt_final %>%
  filter(riquezainmo %in% c("4. 90% inferior", "3. 10% superior")) %>%
  mutate(grupo = recode(riquezainmo,
    "4. 90% inferior" = "90% inferior",
    "3. 10% superior" = "10% superior"
  ))

g_evol_2 <- ggplot(df_inmo_select_evol, aes(x = year, y = share, color = grupo)) +
  geom_line() +
  geom_point(size = 3) +
  scale_color_manual(values = c("90% inferior" = "#2980b9", "10% superior" = "#e74c3c")) +
  scale_x_continuous(breaks = unique(df_inmo_select_evol$year)) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  labs(
    title = "El 10% más rico gana cada vez más terreno:\nla riqueza en vivienda fluye hacia arriba desde la crisis",
    subtitle = "Evolución del porcentaje de riqueza en vivienda acumulado por el 10% superior y el 90% inferior (2002-2022)",
    x = NULL, y = NULL, color = NULL
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 12),
    plot.subtitle = element_text(color = "grey50", size = 9)
  )

print(g_evol_2)
ggsave("out/informeMICO/tablas_y_graficos/grafico_evolucion_90_10.png", g_evol_2, width = 9, height = 5, dpi = 300)

# Gráfico evolución 4 grupos
df_inmo_select_evol4 <- dt_final %>%
  filter(riquezainmo %in% c("4. 90% inferior", "3. 10% superior", "2. 5% superior", "1. 1% superior")) %>%
  mutate(
    grupo = recode(riquezainmo,
      "4. 90% inferior" = "90% inferior",
      "3. 10% superior" = "10% superior",
      "2. 5% superior"  = "5% superior",
      "1. 1% superior"  = "1% superior"
    ),
    grupo = factor(grupo, levels = c("90% inferior", "10% superior", "5% superior", "1% superior"))
  )

g_evol_4 <- ggplot(df_inmo_select_evol4, aes(x = year, y = share, color = grupo)) +
  geom_line() +
  geom_point(size = 3) +
  scale_color_manual(values = c(
    "90% inferior" = "#2980b9", "10% superior" = "#1a5276",
    "5% superior" = "#e74c3c", "1% superior" = "#7b241c"
  )) +
  scale_x_continuous(breaks = unique(df_inmo_select_evol4$year)) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  labs(
    title = "El 10% más rico gana cada vez más terreno:\nla riqueza en vivienda fluye hacia arriba desde la crisis",
    subtitle = "Evolución de la concentración de la riqueza en vivienda por grupos (2002-2022)",
    x = NULL, y = NULL, color = NULL
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 12),
    plot.subtitle = element_text(color = "grey50", size = 9)
  )

print(g_evol_4)
ggsave("out/informeMICO/tablas_y_graficos/grafico_evolucion_4grupos.png", g_evol_4, width = 9, height = 5, dpi = 300)

# ==============================================================================
# BLOQUE 5 - Desigualdad edad y vivienda
# ==============================================================================

df_cross_2022 <- read_csv("out/informeMICO/8-inequality-tenancy-age.csv") %>%
  filter(year == 2022, regten != "Cesion") %>%
  mutate(
    regten = recode(regten,
      "Alquiler"  = "Inquilinos",
      "Propiedad" = "Propietarios",
      "Casero"    = "Arrendadores"
    ),
    regten = factor(regten, levels = c("Arrendadores", "Propietarios", "Inquilinos")),
    bage = recode(bage,
      "Menor de 35 anos"   = "Menor de 35 años",
      "Entre 35 y 44 anos" = "Entre 35 y 44 años",
      "Entre 45 y 54 anos" = "Entre 45 y 54 años",
      "Entre 55 y 64 anos" = "Entre 55 y 64 años",
      "Entre 65 y 74 anos" = "Entre 65 y 74 años",
      "Mayores de 74 anos" = "Mayores de 74 años"
    ),
    bage = factor(bage, levels = c(
      "Menor de 35 años", "Entre 35 y 44 años", "Entre 45 y 54 años",
      "Entre 55 y 64 años", "Entre 65 y 74 años", "Mayores de 74 años"
    ))
  )

fmt_euro_full <- function(x) paste0(format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE), " €")

# Heatmap renta
g_heat_renta <- ggplot(df_cross_2022, aes(x = bage, y = regten, fill = median_renthog)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = fmt_euro_full(floor(median_renthog))), size = 3) +
  scale_fill_gradient(
    low = "#d6eaf8", high = "#1a5276",
    labels = fmt_euro_full
  ) +
  labs(
    title = "La renta depende más del régimen de tenencia que de la edad",
    subtitle = "Renta mediana anual por régimen de tenencia y grupo de edad (2022)",
    x = NULL, y = NULL, fill = "Renta mediana"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 30, hjust = 1),
    plot.title = element_text(face = "bold", size = 12),
    plot.subtitle = element_text(color = "grey50", size = 9),
    legend.position = "right"
  )

print(g_heat_renta)
ggsave("out/informeMICO/tablas_y_graficos/grafico_heat_renta.png", g_heat_renta, width = 10, height = 5, dpi = 300)

# Heatmap riqueza
g_heat_riqueza <- ggplot(df_cross_2022, aes(x = bage, y = regten, fill = median_riquezanet)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = fmt_euro_full(floor(median_riquezanet))), size = 3) +
  scale_fill_gradient(
    low = "#d6eaf8", high = "#1a5276",
    labels = fmt_euro_full
  ) +
  labs(
    title = "La riqueza depende más del régimen de tenencia que de la edad",
    subtitle = "Riqueza neta mediana por régimen de tenencia y grupo de edad (2022)",
    x = NULL, y = NULL, fill = "Riqueza mediana"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 30, hjust = 1),
    plot.title = element_text(face = "bold", size = 12),
    plot.subtitle = element_text(color = "grey50", size = 9),
    legend.position = "right"
  )

print(g_heat_riqueza)
ggsave("out/informeMICO/tablas_y_graficos/grafico_heat_riqueza.png", g_heat_riqueza, width = 10, height = 5, dpi = 300)

# ver si es estadísticamente significativo

# Importar
df_sample <- read_csv("out/informeMICO/9-precise-min-sample-size.csv")

# Ordenar factores
niveles_regten <- c("Alquiler", "Propiedad", "Casero", "Cesion")
niveles_edad <- c(
  "Menor de 35 anos", "Entre 35 y 44 anos", "Entre 45 y 54 anos",
  "Entre 55 y 64 anos", "Entre 65 y 74 anos", "Mayores de 74 anos"
)
labels_edad <- c(
  "Menor de 35 anos"   = "Menor de 35",
  "Entre 35 y 44 anos" = "35-44 años",
  "Entre 45 y 54 anos" = "45-54 años",
  "Entre 55 y 64 anos" = "55-64 años",
  "Entre 65 y 74 anos" = "65-74 años",
  "Mayores de 74 anos" = "Mayor de 74"
)

df_sample <- df_sample %>%
  mutate(
    regten = factor(regten, levels = niveles_regten),
    bage   = factor(bage, levels = niveles_edad)
  )

# Heatmap n_actual sin colores
ggplot(df_sample, aes(x = bage, y = regten, fill = n_actual)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = n_actual), size = 3.5, fontface = "bold") +
  scale_fill_gradient(low = "#f5f5f5", high = "#2c3e50") +
  scale_x_discrete(labels = labels_edad) +
  labs(
    title = "Tamaño muestral por régimen de tenencia y grupo de edad",
    subtitle = "Cuanto más oscuro, mayor tamaño muestral",
    x = NULL, y = NULL, fill = "N"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 30, hjust = 1),
    legend.position = "right",
    plot.title = element_text(face = "bold", size = 12),
    plot.subtitle = element_text(color = "grey50", size = 9)
  )


# ==============================================================================
# BLOQUE 6 - Desigualdad GINI
# ==============================================================================

df_gini <- read_csv("out/informeMICO/5-inequality-inmo-gini.csv") %>%
  mutate(ci_low = gini - 1.96 * se, ci_high = gini + 1.96 * se)

g_gini <- ggplot(df_gini, aes(x = year, y = gini)) +
  geom_line(color = "#5555aa", linewidth = 1) +
  geom_point(color = "#5555aa", size = 3) +
  geom_text(aes(label = round(gini, 3)), vjust = -0.8, size = 3.5, color = "#5555aa") +
  scale_x_continuous(breaks = unique(df_gini$year)) +
  labs(
    title = "Tras la crisis de 2008, la desigualdad en riqueza en vivienda no ha dejado de crecer",
    subtitle = "Índice de Gini de la distribución de la riqueza en vivienda entre los hogares españoles (2002-2022)",
    x = NULL, y = "Índice Gini"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 12),
    plot.subtitle = element_text(color = "grey50", size = 9)
  )

print(g_gini)
ggsave("out/informeMICO/tablas_y_graficos/grafico_gini.png", g_gini, width = 9, height = 5, dpi = 300)
