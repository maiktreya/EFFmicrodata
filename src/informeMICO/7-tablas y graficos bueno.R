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
  mutate(regten = factor(regten, levels = c("Alquiler", "Propiedad",
                                            "Casero1", "Casero_Multi", "Casero2", "Casero3", "Casero4+"))) %>%
  arrange(year, regten)

df_last_year <- df %>%
  filter(year == max(year), regten != "Cesion") %>%
  select(year, regten, median_renthog, median_riquezanet,
         ratio_median_income_alq, ratio_median_wealth_alq) %>%
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
        regten == "Alquiler"     ~ "Alquiler",
        regten == "Propiedad"    ~ "Propiedad",
        regten == "Casero1"      ~ "Casero (1 vivienda en alquiler)",
        regten == "Casero_Multi" ~ "Multiarrendadores",
        regten == "Casero2"      ~ "Dos viviendas en alquiler",
        regten == "Casero3"      ~ "Tres viviendas en alquiler",
        regten == "Casero4+"     ~ "Cuatro o más viviendas en alquiler"
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

# Gráfico tabla renta
g_tabla_renta <- ggplot(df_last_year %>% filter(regten != "Cesion"),
                        aes(x = factor(regten, levels = c("Casero4+", "Casero3", "Casero2",
                                                          "Casero_Multi", "Casero1",
                                                          "Propiedad", "Alquiler")),
                            y = median_renthog,
                            fill = regten %in% c("Casero2", "Casero3", "Casero4+"))) +
  geom_col() +
  geom_text(aes(label = paste0(format(median_renthog, big.mark = ".", decimal.mark = ","), " €")),
            hjust = -0.1, size = 3.2) +
  coord_flip() +
  scale_x_discrete(labels = c(
    "Alquiler"    = "Inquilinos",
    "Propiedad"   = "Propietarios",
    "Casero1"     = "Arrendadores \n(una vivienda en alquiler)",
    "Casero_Multi"= "Multiarrendadores",
    "Casero2"     = "Multiarrendadores\n(dos viviendas en alquiler)",
    "Casero3"     = "Multiarrendadores\n(tres viviendas en alquiler)",
    "Casero4+"    = "Multiarrendadores\n(cuatro o más viviendas en alquiler)"
  )) +
  scale_fill_manual(values = c("FALSE" = "#1a5276", "TRUE" = "#7fbfff")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.25))) +
  labs(title = "El mercado del alquiler transfiere renta de quienes menos tienen a quienes más acumulan",
       subtitle = "Renta mediana anual según régimen de tenencia (2022)",
       x = NULL, y = NULL) +
  theme_minimal() +
  theme(panel.grid.major.y = element_blank(),
        legend.position = "none",
        plot.title = element_text(face = "bold", size = 12),
        plot.subtitle = element_text(color = "grey50", size = 9))

print(g_tabla_renta)
ggsave("out/informeMICO/tablas_y_graficos/grafico_tabla_renta_regten.png", g_tabla_renta, width = 9, height = 5, dpi = 300)

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
g_tabla_riqueza <- ggplot(df_last_year %>% filter(regten != "Cesion"),
                          aes(x = factor(regten, levels = c("Casero4+", "Casero3", "Casero2",
                                                            "Casero_Multi", "Casero1",
                                                            "Propiedad", "Alquiler")),
                              y = median_riquezanet,
                              fill = regten %in% c("Casero2", "Casero3", "Casero4+"))) +
  geom_col() +
  geom_text(aes(label = paste0(format(median_riquezanet, big.mark = ".", decimal.mark = ","), " €")),
            hjust = -0.1, size = 3.2) +
  coord_flip() +
  scale_x_discrete(labels = c(
    "Alquiler"    = "Inquilinos",
    "Propiedad"   = "Propietarios",
    "Casero1"     = "Arrendadores\n(una vivienda en alquiler)",
    "Casero_Multi"= "Multiarrendadores",
    "Casero2"     = "Multiarrendadores\n(dos viviendas en alquiler)",
    "Casero3"     = "Multiarrendadores\n(tres viviendas en alquiler)",
    "Casero4+"    = "Multiarrendadores\n(cuatro o más viviendas en alquiler)"
  )) +
  scale_fill_manual(values = c("FALSE" = "#1a5276", "TRUE" = "#7fbfff")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.25)),
                     labels = function(x) ifelse(x >= 1e3, paste0(round(x/1e3, 0), "k€"), paste0(x, "€"))) +
  labs(title = "Los multiarrendadores acumulan 450 veces más riqueza que los inquilinos",
       subtitle = "Riqueza neta mediana según régimen de tenencia (2022)",
       x = NULL, y = NULL) +
  theme_minimal() +
  theme(panel.grid.major.y = element_blank(),
        legend.position = "none",
        plot.title = element_text(face = "bold", size = 12),
        plot.subtitle = element_text(color = "grey50", size = 9))

print(g_tabla_riqueza)
ggsave("out/informeMICO/tablas_y_graficos/grafico_tabla_riqueza_regten.png", g_tabla_riqueza, width = 9, height = 5, dpi = 300)

# Evolución régimen de tenencia
evo_ca <- read_csv("out/informeMICO/IPC_2022/1-inequality-tenancy_multi.csv")
evo_multi <- read_csv("out/informeMICO/IPC_2022/1-inequality-tenancy_multi2.csv")

evo_casero_multi <- evo_multi %>% filter(regten == "Casero_Multi")

evo <- bind_rows(evo_ca, evo_casero_multi) %>%
  mutate(regten = factor(regten, levels = c("Alquiler", "Propiedad",
                                            "Casero1", "Casero_Multi", "Casero2", "Casero3", "Casero4+"))) %>%
  arrange(year, regten)

df_evolution <- evo %>%
  filter(regten %in% c("Alquiler", "Propiedad", "Casero_Multi", "Casero1")) %>%
  select(year, regten, median_renthog_IPC, median_riquezanet_IPC)

colores_regten <- c("Alquiler" = "#3498db", "Propiedad" = "#2ecc71",
                    "Casero1" = "#e74c3c", "Casero_Multi" = "#9b59b6")
labels_regten  <- c("Alquiler" = "Alquiler", "Propiedad" = "Propiedad",
                    "Casero1" = "Arrendadores (una vivienda en alquiler)", "Casero_Multi" = "Multiarrendadores")

g_renta_evo <- ggplot(df_evolution, aes(x = year, y = median_renthog_IPC, color = regten)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  scale_x_continuous(breaks = unique(df_evolution$year)) +
  scale_y_continuous(labels = function(x) ifelse(x >= 1e3, paste0(round(x/1e3, 0), "k€"), paste0(x, "€"))) +
  scale_color_manual(values = colores_regten, labels = labels_regten) +
  labs(title = "Dos décadas de divergencia: la renta de los multiarrendadores se dispara\nmientras la de los inquilinos se estanca",
       subtitle = "Renta mediana anual según régimen de tenencia (2002-2022, euros constantes 2022)",
       x = NULL, y = NULL, color = NULL) +
  theme_minimal() +
  theme(panel.grid.minor = element_blank(),
        legend.position = "bottom",
        plot.title = element_text(face = "bold", size = 12),
        plot.subtitle = element_text(color = "grey50", size = 9))

print(g_renta_evo)
ggsave("out/informeMICO/tablas_y_graficos/grafico_renta_regten.png", g_renta_evo, width = 9, height = 5, dpi = 300)

print(g_renta_evo)
ggsave("out/informeMICO/tablas_y_graficos/grafico_renta_regten.png", g_renta_evo, width = 9, height = 5, dpi = 300)

g_riqueza_evo <- ggplot(df_evolution, aes(x = year, y = median_riquezanet_IPC, color = regten)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  scale_x_continuous(breaks = unique(df_evolution$year)) +
  scale_y_continuous(labels = function(x) ifelse(x >= 1e3, paste0(round(x/1e3, 0), "k€"), paste0(x, "€"))) +
  scale_color_manual(values = colores_regten, labels = labels_regten) +
  labs(title = "La brecha patrimonial entre inquilinos y arrendadores supera el millón de euros",
       subtitle = "Riqueza neta mediana según régimen de tenencia (2002-2022, euros constantes 2022)",
       x = NULL, y = NULL, color = NULL) +
  theme_minimal() +
  theme(panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold", size = 12),
        plot.subtitle = element_text(color = "grey50", size = 9))

print(g_riqueza_evo)
ggsave("out/informeMICO/tablas_y_graficos/grafico_riqueza_regten.png", g_riqueza_evo, width = 9, height = 5, dpi = 300)


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

niveles_edad <- c("Mayores de 74 anos", "Entre 65 y 74 anos", "Entre 55 y 64 anos",
                  "Entre 45 y 54 anos", "Entre 35 y 44 anos", "Menor de 35 anos")

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
  mutate(valor = paste0(format(median_renthog, big.mark = ".", decimal.mark = ",", scientific = FALSE), " €"),
         ratio = format(ratio_median_renthog, decimal.mark = ",")) %>%
  select(bage, valor, ratio) %>%
  flextable() %>%
  set_header_labels(bage = "", valor = "Renta mediana", ratio = "Ratio de desigualdad renta") %>%
  theme_booktabs() %>%
  autofit()

doc <- read_docx() %>% body_add_flextable(ft_age_renta)
print(doc, target = "out/informeMICO/tablas_y_graficos/tabla_age_renta.docx")

# Gráfico tabla renta edad
g_tabla_age_renta <- ggplot(df_age_last_year,
                            aes(x = factor(bage, levels = niveles_edad), y = median_renthog)) +
  geom_col(fill = "#2980b9") +
  geom_text(aes(label = paste0(format(median_renthog, big.mark = ".", decimal.mark = ","), " €")),
            hjust = -0.1, size = 3.2) +
  coord_flip() +
  scale_x_discrete(labels = labels_edad) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.25)),
    breaks = seq(0, 40000, by = 5000),
    labels = function(x) paste0(format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE), " €")
  ) +
  labs(title = "No existe brecha generacional en los ingresos",
       subtitle = "Renta mediana anual por tramos de edad (2022)",
       x = NULL, y = NULL) +
  theme_minimal() +
  theme(panel.grid.major.y = element_blank(),
        plot.title = element_text(face = "bold", size = 12),
        plot.subtitle = element_text(color = "grey50", size = 9))

print(g_tabla_age_renta)
ggsave("out/informeMICO/tablas_y_graficos/grafico_tabla_renta_edad.png", g_tabla_age_renta, width = 9, height = 5, dpi = 300)

# Tabla riqueza edad
ft_age_riqueza <- df_age_last_year %>%
  mutate(valor = paste0(format(median_riquezanet, big.mark = ".", decimal.mark = ",", scientific = FALSE), " €"),
         ratio = format(ratio_median_wealth, decimal.mark = ",")) %>%
  select(bage, valor, ratio) %>%
  flextable() %>%
  set_header_labels(bage = "", valor = "Riqueza neta mediana", ratio = "Ratio de desigualdad riqueza") %>%
  theme_booktabs() %>%
  autofit()

doc <- read_docx() %>% body_add_flextable(ft_age_riqueza)
print(doc, target = "out/informeMICO/tablas_y_graficos/tabla_age_riqueza.docx")

# Gráfico tabla riqueza edad
g_tabla_age_riqueza <- ggplot(df_age_last_year,
                              aes(x = factor(bage, levels = niveles_edad), y = median_riquezanet)) +
  geom_col(fill = "#2980b9") +
  geom_text(aes(label = paste0(format(median_riquezanet, big.mark = ".", decimal.mark = ","), " €")),
            hjust = -0.1, size = 3.2) +
  coord_flip() +
  scale_x_discrete(labels = labels_edad) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.25)),
    breaks = seq(0, 250000, by = 25000),
    labels = function(x) paste0(format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE), " €")
  ) +
  labs(title = "La desigualdad generacional es de patrimonio",
       subtitle = "Riqueza neta mediana según grupo de edad (2022)",
       x = NULL, y = NULL) +
  theme_minimal() +
  theme(panel.grid.major.y = element_blank(),
        plot.title = element_text(face = "bold", size = 12),
        plot.subtitle = element_text(color = "grey50", size = 9))

print(g_tabla_age_riqueza)
ggsave("out/informeMICO/tablas_y_graficos/grafico_tabla_riqueza_edad.png", g_tabla_age_riqueza, width = 9, height = 5, dpi = 300)


# ==============================================================================
# BLOQUE 3.5 - Desigualdad generacional vs. posición en el mercado
# ==============================================================================

tenencia <- data.frame(
  grupo = c("Inquilinos", "Propietarios", "Arrendadores\n(una vivienda en alquiler)", "Multiarrendadores"),
  renta = c(21335, 32120, 50959, 80375),
  riqueza = c(2217, 193919, 407975, 996826)
)

edad <- data.frame(
  grupo = c("Menor de 35", "35-44 años", "45-54 años", "55-64 años", "Mayor de 64"),
  renta = c(26900, 32326, 34200, 32822, 23628),
  riqueza = c(20069, 76932, 128172, 189872, 221840)
)

tenencia$dimension <- "Régimen de tenencia"
edad$dimension <- "Grupo de edad"

df_renta <- bind_rows(tenencia %>% select(grupo, valor = renta, dimension),
                      edad %>% select(grupo, valor = renta, dimension))
df_riqueza <- bind_rows(tenencia %>% select(grupo, valor = riqueza, dimension),
                        edad %>% select(grupo, valor = riqueza, dimension))

niveles <- c("Inquilinos", "Propietarios", "Arrendadores\n(una vivienda en alquiler)", "Multiarrendadores",
             "Menor de 35", "35-44 años", "45-54 años", "55-64 años", "Mayor de 64")
df_renta$grupo   <- factor(df_renta$grupo,   levels = rev(niveles))
df_riqueza$grupo <- factor(df_riqueza$grupo, levels = rev(niveles))

colores <- c("Régimen de tenencia" = "#2E4057", "Grupo de edad" = "#4472C4")
fmt_euro <- function(x) paste0(format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE), " €")

g_renta_comp <- ggplot(df_renta, aes(x = valor, y = grupo, fill = dimension)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = fmt_euro(valor)), hjust = -0.1, size = 3.2, color = "grey30") +
  scale_fill_manual(values = colores, name = NULL) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.22)), labels = fmt_euro) +
  labs(title = "La vivienda divide más que la generación",
       subtitle = "Renta mediana anual según régimen de tenencia y grupo de edad (2022)",
       x = NULL, y = NULL) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom", panel.grid.major.y = element_blank(),
        plot.title = element_text(face = "bold", size = 12),
        plot.subtitle = element_text(color = "grey50", size = 9))

print(g_renta_comp)
ggsave("out/informeMICO/tablas_y_graficos/grafico_renta_comparado.png", g_renta_comp, width = 8, height = 6, dpi = 300)

g_riqueza_comp <- ggplot(df_riqueza, aes(x = valor, y = grupo, fill = dimension)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = fmt_euro(valor)), hjust = -0.1, size = 3.2, color = "grey30") +
  scale_fill_manual(values = colores, name = NULL) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.22)), labels = fmt_euro) +
  labs(title = "La brecha de riqueza entre inquilinos y arrendadores es incomparablemente\nmayor que la brecha entre generaciones",
       subtitle = "Riqueza neta mediana según régimen de tenencia y grupo de edad (2022)",
       x = NULL, y = NULL) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom", panel.grid.major.y = element_blank(),
        plot.title = element_text(face = "bold", size = 12),
        plot.subtitle = element_text(color = "grey50", size = 9))

print(g_riqueza_comp)
ggsave("out/informeMICO/tablas_y_graficos/grafico_riqueza_comparado.png", g_riqueza_comp, width = 8, height = 6, dpi = 300)

# Gráfico propietarios por edad
df_propietarios <- data.frame(
  grupo = c("Menor de 35 años", "Entre 35 y 44 años", "Entre 45 y 54 años",
            "Entre 55 y 64 años", "Entre 65 y 74 años", "Mayor de 74 años"),
  pct = c(31.8, 61.8, 71.8, 78.8, 83.0, 84.0)
)
df_propietarios$grupo <- factor(df_propietarios$grupo,
                                levels = c("Menor de 35 años", "Entre 35 y 44 años", "Entre 45 y 54 años",
                                           "Entre 55 y 64 años", "Entre 65 y 74 años", "Mayor de 74 años"))

g_propietarios <- ggplot(df_propietarios, aes(x = grupo, y = pct)) +
  geom_col(fill = "#1a5276") +
  geom_text(aes(label = paste0(pct, "%")), vjust = -0.5, size = 3.5) +
  scale_y_continuous(labels = scales::percent_format(scale = 1),
                     expand = expansion(mult = c(0, 0.1))) +
  labs(title = "La brecha de riqueza entre generaciones refleja ante todo una brecha\nen el acceso a la propiedad",
       subtitle = "Porcentaje de hogares propietarios de su vivienda principal por grupo de edad (2022)",
       x = NULL, y = NULL) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1),
        plot.title = element_text(face = "bold", size = 12),
        plot.subtitle = element_text(color = "grey50", size = 9))

print(g_propietarios)
ggsave("out/informeMICO/tablas_y_graficos/grafico_propietarios_edad.png", g_propietarios, width = 9, height = 5, dpi = 300)
# ==============================================================================
# BLOQUE 3 - Riqueza inmobiliaria
# ==============================================================================

dt <- fread("out/informeMICO/IPC_2022/4-inmo-inequality-quantiles.csv")
dt_base <- dt[riquezainmo %in% c("riquezainmo.0.9", "riquezainmo.0.95", "riquezainmo.0.99")]

dt_top <- dt_base[, .(year, riquezainmo, share = 100 - share_riquezainmo)]
dt_top[riquezainmo == "riquezainmo.0.9",  riquezainmo := "3. 10% superior"]
dt_top[riquezainmo == "riquezainmo.0.95", riquezainmo := "2. 5% superior"]
dt_top[riquezainmo == "riquezainmo.0.99", riquezainmo := "1. 1% superior"]

dt_bottom <- dt_base[riquezainmo == "riquezainmo.0.9",
                     .(year, riquezainmo = "4. 90% inferior", share = share_riquezainmo)]

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
      riquezainmo == "2. 5% superior"  ~ "top5",
      riquezainmo == "1. 1% superior"  ~ "top1"
    )
  )

g_barras <- ggplot(df_inmo_select_last, aes(x = grupo, y = share, fill = color_grupo)) +
  geom_col() +
  geom_text(aes(label = paste0(round(share, 1), "%")), vjust = -0.5, size = 5) +
  scale_fill_manual(values = c("inferior" = "#7fbfff", "top10" = "#1a5276",
                               "top5" = "#1f618d", "top1" = "#2980b9")) +
  scale_x_discrete(labels = c("4. 90% inferior" = "90% inferior", "3. 10% superior" = "10% superior",
                              "2. 5% superior" = "5% superior", "1. 1% superior" = "1% superior")) +
  scale_y_continuous(labels = scales::percent_format(scale = 1),
                     expand = expansion(mult = c(0, 0.1))) +
  labs(title = "Una minoría concentra casi la mitad de toda la riqueza en vivienda de España",
       subtitle = "Distribución de la riqueza en vivienda por grupos de patrimonio (2022)",
       x = "", y = NULL) +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold", size = 12),
        plot.subtitle = element_text(color = "grey50", size = 9))

print(g_barras)
ggsave("out/informeMICO/tablas_y_graficos/grafico_concentracion_barras.png", g_barras, width = 8, height = 5, dpi = 300)

# Gráfico evolución 90% vs 10%
df_inmo_select_evol <- dt_final %>%
  filter(riquezainmo %in% c("4. 90% inferior", "3. 10% superior")) %>%
  mutate(grupo = recode(riquezainmo,
                        "4. 90% inferior" = "90% inferior",
                        "3. 10% superior" = "10% superior"))

g_evol_2 <- ggplot(df_inmo_select_evol, aes(x = year, y = share, color = grupo)) +
  geom_line() +
  geom_point(size = 3) +
  scale_color_manual(values = c("90% inferior" = "#2980b9", "10% superior" = "#e74c3c")) +
  scale_x_continuous(breaks = unique(df_inmo_select_evol$year)) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  labs(title = "El 10% más rico gana cada vez más terreno:\nla riqueza en vivienda fluye hacia arriba desde la crisis",
       subtitle = "Evolución del porcentaje de riqueza en vivienda acumulado por el 10% superior y el 90% inferior (2002-2022)",
       x = NULL, y = NULL, color = NULL) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 12),
        plot.subtitle = element_text(color = "grey50", size = 9))

print(g_evol_2)
ggsave("out/informeMICO/tablas_y_graficos/grafico_evolucion_90_10.png", g_evol_2, width = 9, height = 5, dpi = 300)

# Gráfico evolución 4 grupos
df_inmo_select_evol4 <- dt_final %>%
  filter(riquezainmo %in% c("4. 90% inferior", "3. 10% superior", "2. 5% superior", "1. 1% superior")) %>%
  mutate(grupo = recode(riquezainmo,
                        "4. 90% inferior" = "90% inferior",
                        "3. 10% superior" = "10% superior",
                        "2. 5% superior"  = "5% superior",
                        "1. 1% superior"  = "1% superior"),
         grupo = factor(grupo, levels = c("90% inferior", "10% superior", "5% superior", "1% superior")))

g_evol_4 <- ggplot(df_inmo_select_evol4, aes(x = year, y = share, color = grupo)) +
  geom_line() +
  geom_point(size = 3) +
  scale_color_manual(values = c("90% inferior" = "#2980b9", "10% superior" = "#1a5276",
                                "5% superior" = "#e74c3c", "1% superior" = "#7b241c")) +
  scale_x_continuous(breaks = unique(df_inmo_select_evol4$year)) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  labs(title = "El 10% más rico gana cada vez más terreno:\nla riqueza en vivienda fluye hacia arriba desde la crisis",
       subtitle = "Evolución de la concentración de la riqueza en vivienda por grupos (2002-2022)",
       x = NULL, y = NULL, color = NULL) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 12),
        plot.subtitle = element_text(color = "grey50", size = 9))

print(g_evol_4)
ggsave("out/informeMICO/tablas_y_graficos/grafico_evolucion_4grupos.png", g_evol_4, width = 9, height = 5, dpi = 300)


# ==============================================================================
# BLOQUE 4 - Desigualdad GINI
# ==============================================================================

df_gini <- read_csv("out/informeMICO/5-inequality-inmo-gini.csv") %>%
  mutate(ci_low = gini - 1.96 * se, ci_high = gini + 1.96 * se)

g_gini <- ggplot(df_gini, aes(x = year, y = gini)) +
  geom_ribbon(aes(ymin = ci_low, ymax = ci_high), fill = "#b3b3e6", alpha = 0.2) +
geom_line(color = "#5555aa", linewidth = 1) +
  geom_point(color = "#5555aa", size = 3) +
  
  geom_text(aes(label = round(gini, 3)), vjust = -0.8, size = 3.5, color = "#5555aa") +
  scale_x_continuous(breaks = unique(df_gini$year)) +
  labs(title = "Tras la crisis de 2008, la desigualdad en riqueza en vivienda no ha dejado de crecer",
       subtitle = "Índice de Gini de la distribución de la riqueza en vivienda entre los hogares españoles (2002-2022)",
       x = NULL, y = "Índice Gini") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 12),
        plot.subtitle = element_text(color = "grey50", size = 9))

print(g_gini)
ggsave("out/informeMICO/tablas_y_graficos/grafico_gini.png", g_gini, width = 9, height = 5, dpi = 300)