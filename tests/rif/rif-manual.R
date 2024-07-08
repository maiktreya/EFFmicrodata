# Define your weights
### WORKSPACE SETUP- MEMORY CLEAN AND PACKAGES IMPORT
rm(list = ls()) # ENSURE ENVIROMENT IS CLEAN
`%>%` <- magrittr::`%>%` # nolint # ALLOW PIPE  MULTI-LOADING WITHOUT MAGRITTR
c("magrittr", "survey", "dineq", "data.table", "oaxaca", "gtsummary", "xtable") %>% sapply(library, character.only = T)

# PARAMETERS AND VARIABLES TO INITIALIZE
sel_year <- c(2002, 2020) # selected survey year
dtlist <- list()

        # DATA LOADING AND VARIABLE MANIPULATION
for (i in seq_along(sel_year)) {
        dt_eff <- paste0(".datasets/", sel_year[i], "-EFF.microdat.csv") %>% fread() # Data table con microdatos anuales
        dt_eff[is.na(p6_81)]$p6_81 <- 2 # set unassigned to non-worker
        dt_eff$young <- dt_eff$bage # create a variable for binary age
        dt_eff[young != 1]$young <- 2 # set above 35 to non-young
        setnames(dt_eff,
                old = c("nsitlabdom", "p6_81", "np2_1", "np2_5"),
                new = c("class", "worker", "homeowner", "mainres_val")
        )
        # create a categorical income variable
        dt_eff[renthog < 20000, renthog1 := "a"][renthog > 20000, renthog1 := "b"][renthog > 80000, renthog1 := "c"]
        dt_eff[renthog1 == "a", renthog1 := 1][renthog1 == "b", renthog1 := 2][renthog1 == "c", renthog1 := 3]
        dt_eff[, worker1 := as.numeric(worker) - 1] # create a 0,1 numeric variable for Oaxaca package

        # DEFINITION OF CATEGORICAL VARIABLES, ALL BINARY BUT RENTHOG 1 WHICH IS USED TO DIVIDE BETWEEN GROUPS
        dt_eff$renthog1 <- factor(dt_eff$renthog1, levels = c(1, 2, 3), labels = c("Low", "Middle", "High"))
        dt_eff$sex <- factor(dt_eff$sex, levels = c(1, 2), labels = c("Man", "Women"))
        dt_eff$class <- factor(dt_eff$class, levels = c(1, 2, 3, 4, 5, 6), labels = c("worker", "capitalist", "self-employed", "inactive", "retired", "manager"))
        dt_eff$bage <- factor(dt_eff$bage, levels = c(1, 2, 3, 4, 5, 6), labels = c("0-34", "35-44", "45-54", "54-65", "65-75", "75"))
        dt_eff$young <- factor(dt_eff$young, levels = c(1, 2), labels = c("Young", "Not-Young"))
        dt_eff$worker <- factor(dt_eff$worker, levels = c(1, 2), labels = c("Worker", "Non-Worker"))
        dt_eff$homeowner <- factor(dt_eff$homeowner, levels = c(0, 1), labels = c("Non-Owner", "Homeowner"))
        dt_eff$RIF_actreales <- rif(dt_eff$actreales, method = "quantile", quantile = 0.5)

        dtlist[[i]] <- dt_eff # assign to list a given year survey
}
# SELECT NEEDED VARIABLES AND MERGE THE TWO SURVEYS FOR OAXACA PACKAGE
dt_effA <- dtlist[[1]][, c("facine3", "renthog", "renthog1", "bage", "homeowner", "worker", "young", "sex", "class", "actreales", "RIF_actreales")][, identif := 0]
dt_effB <- dtlist[[2]][, c("facine3", "renthog", "renthog1", "bage", "homeowner", "worker", "young", "sex", "class", "actreales", "RIF_actreales")][, identif := 1]
dt_eff <- rbind(dt_effA, dt_effB)

# Fit weighted linear models for each group
model1 <- lm(RIF_actreales ~ bage + class + sex + homeowner, data = dt_effA, weights = facine3)
model2 <- lm(RIF_actreales ~ bage + class + sex + homeowner, data = dt_effB, weights = facine3)

# Extract coefficients
coef1 <- coef(model1)
coef2 <- coef(model2)

# Calculate mean differences in predictors
mean_diff <- colMeans(model.matrix(model1)) - colMeans(model.matrix(model2))

# Decompose effects by regressor
interaction_effect <- (coef1 - coef2) * mean_diff
endowment_effect <- mean_diff * coef1 - interaction_effect
coefficient_effect <- (coef1 - coef2) * colMeans(model.matrix(model1)) - interaction_effect

# Print results
results <- data.frame(
  endowment = endowment_effect,
  coefficient = coefficient_effect,
  interaction = interaction_effect
)
results_tot <- data.frame(
  endowment = sum(endowment_effect),
  coefficient = sum(coefficient_effect),
  interaction = sum(interaction_effect)
)

sink("output/rif/rif_manual.txt")
print("############### 1. RIF 2002 ###############")
model1 %>% summary() %>% print()
print("############### 2. RIF 2022 ###############")
model2 %>% summary() %>% print()
print("############### 3. WEIGHTED OAXACA DECOMPOSITION (TOTAL) ###############")
results_tot %>% print()
print("############### 4. WEIGHTED OAXACA DECOMPOSITION (COVARIATES) ###############")
results %>% print()
sink()
