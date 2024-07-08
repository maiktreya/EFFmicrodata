### WORKSPACE SETUP- MEMORY CLEAN AND PACKAGES IMPORT
rm(list = ls()) # ENSURE ENVIROMENT IS CLEAN
`%>%` <- magrittr::`%>%` # nolint # ALLOW PIPE  MULTI-LOADING WITHOUT MAGRITTR
c("magrittr", "survey", "dineq", "data.table", "oaxaca", "decr") %>% sapply(library, character.only = T)

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
        dt_eff$RIF_riquezanet <- rif(dt_eff$riquezanet, weights = dt_eff$facine3, method = "quantile", quantile = 0.5)

        dtlist[[i]] <- dt_eff # assign to list a given year survey
}
# SELECT NEEDED VARIABLES AND MERGE THE TWO SURVEYS FOR OAXACA PACKAGE
dt_effA <- dtlist[[1]][, c("facine3", "renthog", "renthog1", "bage", "homeowner", "worker", "young", "sex", "class", "riquezanet", "RIF_riquezanet")][, identif := 0]
dt_effB <- dtlist[[2]][, c("facine3", "renthog", "renthog1", "bage", "homeowner", "worker", "young", "sex", "class", "riquezanet", "RIF_riquezanet")][, identif := 1]
dt_eff <- rbind(dt_effA, dt_effB)


# RESAMPLING WITH WHEIGTS
dt_eff$facine31 <- dt_eff$facine3 / sum(dt_eff$facine3)
set.seed(123) # For reproducibility
dt_eff_rew <- dt_eff[sample(seq_len(nrow(dt_eff)), size = nrow(dt_eff), replace = TRUE, prob = dt_eff$facine31), ]

# RIF REGRESSION
rif_results1 <- lm(RIF_riquezanet ~ bage + class + sex + homeowner, data = dt_effA, weights = facine3)
rif_results2 <- lm(RIF_riquezanet ~ bage + class + sex + homeowner, data = subset(dt_eff_rew, identif == 0))
rif_results3 <- lm(RIF_riquezanet ~ bage + class + sex + homeowner, data = dt_effB, weights = facine3)
rif_results4 <- lm(RIF_riquezanet ~ bage + class + sex + homeowner, data = subset(dt_eff_rew, identif == 1))

# OAXACA BLINDER METHOD
oaxaca_results <- oaxaca(RIF_riquezanet ~ bage + class + sex + homeowner | identif, data = dt_eff_rew)
oaxaca_results_decr <- reweight_strata_all2(data = dt_eff,
  treatment = "identif",
  variables = c("bage", "class", "sex", "homeowner"),
  y = "RIF_riquezanet",
  weights = "facine3")
d01 <- dec_quantile(oaxaca_results_decr, probs = 0.5)
d01_p50_AB <- dec_(d01, counterfactual = "AB")

# PREVIEW PRELIMINARY RESULTS
"output/rif/rif-oaxaca-new.txt" %>% sink()
"############### METHOD: RIF REGRESSION dineq R ###############" %>% print()
rif_results1 %>% summary() %>% print()
rif_results2 %>% summary() %>% print()
rif_results3 %>% summary() %>% print()
rif_results4 %>% summary() %>% print()

"############### METHOD: OAXACA DECOMPOSITION oaxaca R ###############" %>% print()
oaxaca_results %>% print()
"############### METHOD: OAXACA DECOMPOSITION decr R ###############" %>% print()
oaxaca_results_decr %>% print()
d01 %>% print()
d01_p50_AB %>% print()
sink()
jpeg(file = "output/rif/img/oaxaca.jpeg")
# Create a scatter plot of the first two dimensions
plot.oaxaca(oaxaca_results) %>% print()
dev.off()
