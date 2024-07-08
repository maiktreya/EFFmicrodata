## CREATE A JOIN DATATABLE FOR A SELECTION OF VARIABLES OF ALL PUBLISHED EFF SURVEYS
`%>%` <- magrittr::`%>%` # nolint # ALLOW PIPE  MULTI-LOADING WITHOUT MAGRITTR
c("magrittr", "data.table", "dineq") %>% sapply(library, character.only = T)
# PARAMETERS AND VARIABLES TO INITIALIZE
sel_year <- c(2002, 2005, 2008, 2011, 2014, 2017, 2020) # selected survey year
selected_variables <- c(
        "facine3", "renthog", "renthog1", "bage", "homeowner", "worker", "young", "sex", "class",
        "actreales", "riquezanet", "riquezafin", "rif_actreales", "educ", "auton", "rents",
        "tipo_auton", "direc", "multipr", "useprop", "inherit"
)
final_dt <- data.table()

# LOOP OVER ALL SURVEYS TO CREATED SUBSET OF NEEDED VARIABLES FOR EACH YEAR AND RUN MODELS
for (i in seq_along(sel_year)) {
        dt_eff <- paste0(".datasets/", sel_year[i], "-EFF.microdat.csv") %>% fread() # Data table con microdatos anuales
        dt_eff[is.na(p6_81)]$p6_81 <- 2 # set unassigned to non-worker
        dt_eff$young <- dt_eff$bage # create a variable for binary age
        dt_eff[young != 1]$young <- 2 # set above 35 to non-young
        setnames(dt_eff,
                old = c("p6_81", "np2_1", "np2_5", "p2_42_1", "p2_35_1"),
                new = c("worker", "homeowner", "mainres_val", "useprop", "inherit")
        )
        dt_eff[direc == 1, class := 6]
        dt_eff[inherit == 2.6, inherit := 3]
        dt_eff[inherit == 2.2, inherit := 2]
        dt_eff[inherit == 2.4, inherit := 2]
        dt_eff[inherit != 2, inherit := 0]
        dt_eff[inherit == 2, inherit := 1]
        dt_eff[is.na(inherit), inherit := 0]
        # create a categorical income variable
        dt_eff[renthog < 20000, renthog1 := "a"][renthog > 20000, renthog1 := "b"][renthog > 80000, renthog1 := "c"]
        dt_eff[renthog1 == "a", renthog1 := 1][renthog1 == "b", renthog1 := 2][renthog1 == "c", renthog1 := 3]
        dt_eff[, worker1 := as.numeric(worker) - 1] # create a 0,1 numeric variable for Oaxaca package
        # DEFINITION OF CATEGORICAL VARIABLES, ALL BINARY BUT RENTHOG 1 WHICH IS USED TO DIVIDE BETWEEN GROUPS
        dt_eff$renthog1 <- factor(dt_eff$renthog1, levels = c(1, 2, 3), labels = c("Low", "Middle", "High"))
        dt_eff$sex <- factor(dt_eff$sex, levels = c(1, 2), labels = c("Man", "Women"))
        dt_eff$class <- factor(dt_eff$nsitlabdom, levels = c(1, 2, 3, 4, 5, 6), labels = c("worker", "capitalist", "self-employed", "inactive", "retired", "manager"))
        dt_eff$bage <- factor(dt_eff$bage, levels = c(1, 2, 3, 4, 5, 6), labels = c("0-34", "35-44", "45-54", "54-65", "65-75", "75"))
        dt_eff$young <- factor(dt_eff$young, levels = c(1, 2), labels = c("Young", "Not-Young"))
        dt_eff$worker <- factor(dt_eff$worker, levels = c(1, 2), labels = c("Worker", "Non-Worker"))
        dt_eff$homeowner <- factor(dt_eff$homeowner, levels = c(0, 1), labels = c("Non-Owner", "Homeowner"))
        dt_eff$inherit <- factor(dt_eff$inherit, levels = c(0, 1), labels = c("Non-inherit", "Inheritance"))
        dt_eff$rif_actreales <- rif(dt_eff$actreales, method = "quantile", quantile = 0.5)
        dt_eff <- dt_eff[, ..selected_variables][, sv_year := sel_year[i]]
        final_dt <- rbind(final_dt, dt_eff)
}
final_dt %>% fwrite(file = "saves/eff-pool-2002-2020.csv")


for (i in sel_year) {
        final_dt[worker == "Worker" & sv_year == i] %>%
                nrow() %>%
                print()
}
