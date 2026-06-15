# analyze_yearly_data_gini.R
library(data.table)
library(survey)
library(mitools)
library(convey)
library(ggplot2)

# 1. Load Data
rm(list = ls())
eff <- fread("datasets/full_eff_refined.gz")

# 2. Vectorized Data Cleaning
eff[, facine3 := as.numeric(facine3)]
eff[, riquezainmo := as.numeric(riquezainmo)]

# 3. Handle Multiple Imputation
eff_list <- split(eff, by = "imputation")
mi_data <- imputationList(eff_list)

# 4. Create and Prep Survey Design
mi_design <- svydesign(ids = ~1, weights = ~facine3, data = mi_data)
# Apply convey_prep to the entire multiply-imputed design object at once
mi_design <- convey_prep(mi_design)

# 5. Gini Coefficient for Real Estate Wealth by Year ONLY
mi_ginis <- with(
    mi_design,
    svyby(~riquezainmo, ~year, design = .design, svygini, na.rm = TRUE)
)

# 6. Pool Results using Rubin's Rules
pooled_ginis <- MIcombine(mi_ginis)

# 7. Tidy the Gini Results Table
final_ginis <- data.table(
    year = names(coef(pooled_ginis)),
    gini = coef(pooled_ginis),
    se   = SE(pooled_ginis)
)
# Ensure year is numeric for easy merging/subsetting
final_ginis[, year := as.numeric(year)]

print(final_ginis)

# 8. Exporting the Lorenz Curves to PNG
plot_design <- mi_design$designs[[1]]
des_2002 <- subset(plot_design, year == 2002)
des_2022 <- subset(plot_design, year == 2022)
gini_2002 <- final_ginis[year == 2002, gini]
gini_2022 <- final_ginis[year == 2022, gini]


# Extraer puntos de la curva de Lorenz
lorenz_2002 <- svylorenz(~riquezainmo, design = des_2002,
                         quantiles = seq(0, 1, by = 0.05), ci = FALSE, plot = FALSE)
lorenz_2022 <- svylorenz(~riquezainmo, design = des_2022,
                         quantiles = seq(0, 1, by = 0.05), ci = FALSE, plot = FALSE)

df_lorenz <- rbind(
  data.frame(x = seq(0, 1, by = 0.05),
             y = as.numeric(lorenz_2002),
             año = paste0("2002 (Gini: ", round(gini_2002, 3), ")")),
  data.frame(x = seq(0, 1, by = 0.05),
             y = as.numeric(lorenz_2022),
             año = paste0("2022 (Gini: ", round(gini_2022, 3), ")"))
)

label_2002 <- paste0("2002 (Gini: ", round(gini_2002, 3), ")")
label_2022 <- paste0("2022 (Gini: ", round(gini_2022, 3), ")")

g_lorenz <- ggplot(df_lorenz, aes(x = x, y = y, color = año)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey60", linewidth = 0.8) +
  geom_line(linewidth = 1.2) +
  scale_color_manual(values = setNames(c("#2980b9", "#e74c3c"), c(label_2002, label_2022))) +
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Lo que era desigual en 2002 es aún más desigual en 2022:\nla vivienda como motor de desigualdad",
       subtitle = "Curva de Lorenz de la riqueza en vivienda, comparación 2002-2022",
       x = "Porcentaje acumulado de hogares",
       y = "Porcentaje acumulado de riqueza en vivienda",
       color = NULL) +
  theme_minimal() +
  theme(legend.position = "bottom",
        plot.title = element_text(face = "bold", size = 12),
        plot.subtitle = element_text(color = "grey50", size = 9))

print(g_lorenz)
ggsave("out/informeMICO/tablas_y_graficos/grafico_lorenz.png", g_lorenz, width = 8, height = 6, dpi = 300)
# Exportar tabla
fwrite(final_ginis, "out/informeMICO/5-inequality-inmo-gini.csv")