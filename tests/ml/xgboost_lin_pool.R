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
X <- model.matrix(rif_actreales ~ ., data = dt_eff)
y <- dt_eff$rif_actreales

# Create DMatrix
dtrain <- xgb.DMatrix(data = X, label = y)

# Train xgboost model
params <- list(
  objective = "reg:squarederror",
  booster = "gblinear",
  eta = 0.3, # learning rate. default 0.3
  alpha = 0, # default 0
  lambda = 0 # default 0 regularization term on weights
)

bst_model <- xgb.train(
  params = params,
  dtrain,
  nrounds = 500
)
# bst_model <- xgboost(data = X, label = y, nrounds = 100, objective = "reg:squarederror", booster = "gblinear")

# Feature Importance
importance_matrix <- xgb.importance(feature_names = colnames(X), model = bst_model)
importance_matrix_plot <- importance_matrix$Weight #  %>% abs() %>% sort(decreasing = T)
names(importance_matrix_plot) <- importance_matrix$Feature
# Coefficients (Note that in gblinear, raw dump includes coefficients)
coefs <- xgb.dump(bst_model, with_stats = TRUE, dump_format = "text")
# Extract the coefficients
coefs_values <- as.numeric(coefs[5:length(coefs)]) # assuming coefs contains numeric values starting from the second element

# Determine the indices for each categorical variable
factor_indices <- lapply(factor_cols, function(col) grep(col, colnames(X)))

# Calculate the sum of the absolute coefficients for each categorical variable
joint_importance <- sapply(factor_indices, function(indices) sum(abs(coefs_values[indices])))

# Combine with the variable names
joint_importance_named <- setNames(joint_importance, factor_cols)


# PREVIEW PRELIMINARY RESULTS
sink("output/gradient-boost/xgboost/xgboost_linear_loop.txt")
bst_model %>% print()
"IMPORTANCE MATRIX:" %>% print()
importance_matrix %>% print()
sink()

######## 3 PLOTTING PARTIAL DEPENDENCE
jpeg(file = "output/gradient-boost/xgboost/xgboost_linear_loop_joint.jpeg")
barplot(joint_importance_named,
  main = "Joint Importance of Categorical Variables",
  ylab = "Importance", las = 2, cex.names = 0.8, col = "skyblue"
)
dev.off()
jpeg(file = "output/gradient-boost/xgboost/xgboost_linear_loop.jpeg")
barplot(importance_matrix_plot,
  main = "Joint Importance of Categorical Variables",
  ylab = "Importance", las = 2, cex.names = 0.8, col = "skyblue"
)
dev.off()
