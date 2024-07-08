### WORKSPACE SETUP- MEMORY CLEAN AND PACKAGES IMPORT
`%>%` <- magrittr::`%>%` # nolint
options(scipen = 99)
rif_var <- "quantile"
c("survey", "data.table", "dineq", "xgboost") %>% sapply(library, character.only = T)
selected_variables <- c(
    "facine3", "renthog", "renthog1", "bage", "homeowner", "worker", "young", "sex", "class",
    "actreales", "riquezanet", "riquezafin", "educ", "auton", "class",
    "tipo_auton", "direc", "multipr", "useprop", "inherit"
)
dt_eff <- "saves/eff-pool-2002-2020.csv" %>% fread() # Data table con microdatos anuales
# Convert 'class' and 'bage' to dummy variables
dt_eff[, ..selected_variables]
dt_eff[is.na(dt_eff)] <- 0
final_dt <- data.table()
models_dt <- list()

cpi <- c(73.31, 80.44, 89.11, 93.35, 96.82, 97.98, 100) / 100
years <- c(2002, 2005, 2008, 2011, 2014, 2017, 2020)
dt_eff[homeowner == "", homeowner := "Non-Owner"]
dt_eff$class <- relevel(as.factor(dt_eff$class), ref = "self-employed")
dt_eff$bage <- relevel(as.factor(dt_eff$bage), ref = "0-34")
dt_eff$inherit <- relevel(as.factor(dt_eff$inherit), ref = "Non-inherit")
dt_eff$homeowner <- relevel(as.factor(dt_eff$homeowner), ref = "Non-Owner")
dt_eff$riquezafin <- factor(as.logical(dt_eff$riquezafin), levels = c(T, F), labels = c("Fin", "NonFin"))

# dt_eff <- fastDummies::dummy_cols(dt_eff, select_columns = c("class"))
dt_eff <- dt_eff[worker == "Worker"]
for (i in seq_along(years)) {
    dt_transform <- dt_eff[sv_year == years[i]]
    # Estimate RIF model
    dt_transform[riquezanet <= 0, riquezanet := 1]
    dt_transform[, riquezanet := log(riquezanet)]
    models_dt[[i]] <- lm(riquezanet ~ bage + sex + educ + riquezafin + inherit + direc + homeowner + multipr, weights = facine3, data = dt_transform)
    coefs <- coef(summary(lm(riquezanet ~ bage + sex + educ + riquezafin + inherit + direc + homeowner + multipr, weights = facine3, data = dt_transform))) %>% data.table()
    coefs <- as.data.frame(coefs)
    pre_dt <- c(rbind(coefs[, "Estimate"], coefs[, "Pr(>|t|)"]))
    final_dt <- cbind(final_dt, pre_dt)
}
previous_names <- row.names(coef(summary(models_dt[[1]])))
interleaved_names <- c(rbind(previous_names, rep("p-val", length(previous_names))))

final_dt <- cbind(interleaved_names, round(final_dt, 3))
colnames(final_dt) <- c("vars", years)

fwrite(final_dt, file = paste0("output/log-model/", rif_var, "_final_dt.csv"))



sink(paste0("output/log-model/", rif_var, "_temptative.txt"))

for (i in seq_along(years)) {
    print(paste0("###############", years[i], " ###############"))
    print(summary(models_dt[[i]]))
}
sink()
