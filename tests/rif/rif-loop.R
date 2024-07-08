# Define your weights
### WORKSPACE SETUP- MEMORY CLEAN AND PACKAGES IMPORT
rm(list = ls()) # ENSURE ENVIROMENT IS CLEAN
`%>%` <- magrittr::`%>%` # nolint # ALLOW PIPE  MULTI-LOADING WITHOUT MAGRITTR
c("magrittr", "survey", "dineq", "data.table", "oaxaca", "gtsummary", "xtable") %>% sapply(library, character.only = T)

# PARAMETERS AND VARIABLES TO INITIALIZE
sel_year <- c(2002, 2005, 2008, 2011, 2014, 2017, 2020) # selected survey year
selected_variables <- c(
        "facine3", "renthog", "renthog1", "bage", "homeowner", "worker", "young", "sex", "class",
        "actreales", "riquezanet", "riquezafin", "rif_actreales", "educ", "auton", "rents",
        "tipo_auton", "direc", "multipr", "useprop", "viaprop"
)
models <- list()
final_dt <- data.table()

# LOOP OVER ALL SURVEYS TO CREATED SUBSET OF NEEDED VARIABLES FOR EACH YEAR AND RUN MODELS
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


        dt_eff <- dt_eff[, ..selected_variables][, identif := i]
        models[[i]] <- lm(RIF_actreales ~ bage + class + sex + homeowner, data = dt_eff, weights = facine3)
}

## LOOP OVER RETURNED MODELS TO CREATE A JOINT COEFFICIENTS TABLE
final_dt[, categories := models[[1]]$coefficients %>% names()]
for (i in seq_along(models)) final_dt[, as.character(sel_year[i]) := unname(models[[i]]$coefficients)]

## GENERATE OUTPUT AND EXPORT IT
sink("output/rif/tables/rif_loop.csv")
print("############### 1. EXTRACTED COEFS FROM DISTINCT RIF YEARS ###############")
final_dt %>% print()
for (i in seq_along(models)) {
        print(paste0("############### 1. MEDIAN RIF ", sel_year[i], "###############"))
        models[[i]] %>%
                summary() %>%
                print()
}
sink()

final_dt %>% fwrite(file = "output/rif/tables/rif_loop.csv")
