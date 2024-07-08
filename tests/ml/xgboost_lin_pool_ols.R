### WORKSPACE SETUP- MEMORY CLEAN AND PACKAGES IMPORT
`%>%` <- magrittr::`%>%` # nolint
options(scipen = 999)
c("survey", "data.table", "dineq", "xgboost") %>% sapply(library, character.only = T)
dt_eff <- "saves/eff-pool-2002-2020.csv" %>% fread() # Data table con microdatos anuales
dt_eff <- dt_eff[, c("rif_actreales", "riquezafin", "sex", "bage", "class", "renthog1", "homeowner", "sv_year")]
# Convert 'class' and 'bage' to dummy variables

dt_eff[, riquezafin := as.logical(riquezafin)]
for (level in unique(dt_eff$class)) dt_eff[, paste0(level) := as.integer(class == level)]
for (level in unique(dt_eff$bage)) dt_eff[, paste0(level) := as.integer(bage == level)]
# Remove the original 'class' variable
dt_eff[, class := NULL][, bage := NULL]

factor_cols <- colnames(dt_eff)[2:length(colnames(dt_eff))]

# Creating the model matrix
bst_model <- lm(rif_actreales ~ ., data = dt_eff)

# Feature Importance
importance_matrix <- bst_model$coefficients[-1] # %>% abs() %>% sort(decreasing = TRUE)

# PREVIEW PRELIMINARY RESULTS
sink("output/gradient-boost/xgboost/xgboost_linear_ols.txt")
bst_model %>% print()
"IMPORTANCE MATRIX:" %>% print()
importance_matrix %>% print()
sink()

######## 3 PLOTTING PARTIAL DEPENDENCE
jpeg(file = "output/gradient-boost/xgboost/xgboost_linear_ols.jpeg")
barplot(importance_matrix,
  main = "Joint Importance of Categorical Variables",
  ylab = "Importance", xlab = "Variable", las = 2, cex.names = 0.8, col = "skyblue"
)
dev.off()
