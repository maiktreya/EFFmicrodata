### PLOTTING EMPIRICAL DISTRIBUTION FUNCTIONS FROM DIFFERENT ANNUAL ISSUES OF ENCUESTA FINANCIERA DE FAMILIA ###
rm(list = ls()) # ensure enviroment is clean
options(scipen = 999) # force to avoid if possible scientific notation in output
`%>%` <- magrittr::`%>%` # nolint # ALLOW PIPE  MULTI-LOADING WITHOUT MAGRITTR
c("magrittr", "survey", "dineq", "data.table", "oaxaca", "gtsummary", "xtable", "convey") %>% sapply(library, character.only = T)

# PARAMETERS AND VARIABLES TO INITIALIZE
sel_year <- c(2002, 2020) # selected survey year
cpi <- 0.7331
dt <- data.table()
gini_w <- gini_c <- data.table()

# loop over needed survey annual issues
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
        dt_eff$RIF_riquezanet <- rif(dt_eff$riquezanet, method = "quantile", quantile = 0.5)

        # subset needed variables and create survey object
        dt_eff <- dt_eff [, c("facine3", "renthog", "renthog1", "bage", "homeowner", "worker", "young", "sex", "class", "riquezanet", "RIF_riquezanet")]
        if (sel_year[i] == 2020) dt_eff[, riquezanet := riquezanet * cpi]
        sv_eff <- svydesign(ids = ~1, data = as.data.frame(dt_eff), weights = ~ dt_eff$facine3)

        # set the bound to avoid extreme ocurrences or not greater than 0
        up_bound <- svyquantile(~riquezanet, sv_eff, quantiles = c(0.99))[1]$riquezanet[, "quantile"]
        lo_bound <- svyquantile(~riquezanet, sv_eff, quantiles = c(0.01))[1]$riquezanet[, "quantile"]

        # get empirical distribution functions
        cap_s <- svysmooth(~riquezanet, subset(sv_eff, riquezanet < up_bound & class %in% "capitalist" & riquezanet > lo_bound), na.rm = T)[[1]]
        wor_s <- svysmooth(~riquezanet, subset(sv_eff, riquezanet < up_bound & class %in% "worker" & riquezanet > lo_bound), na.rm = T)[[1]]
        gini_w <- cbind(gini_w, c(sel_year[i], svygini(~riquezanet, convey_prep(subset(sv_eff, class %in% "worker")), na.rm = T)))
        gini_c <- cbind(gini_c, c(sel_year[i], svygini(~riquezanet, convey_prep(subset(sv_eff, class %in% "capitalist")), na.rm = T)))

        # pipe edf into existing data.table columns identifying by year
        dt <- dt[, as.character(paste0("cap_s_y", sel_year[i])) := cap_s$y][,
                   as.character(paste0("wor_s_y", sel_year[i])) := wor_s$y][,
                   as.character(paste0("cap_s_x", sel_year[i])) := cap_s$x][,
                   as.character(paste0("wor_s_x", sel_year[i])) := wor_s$x]
}
names(gini_c) <- names(gini_w) <- sapply(sel_year, as.character)

######## plot
jpeg(file = "output/rif/img/gini_lorentz..jpeg")
svylorenz(~riquezanet, convey_prep(sv_eff), na.rm = T)
dev.off()

jpeg(file = "output/rif/img/cdf.jpeg")
svycdf(~riquezanet, subset(sv_eff, riquezanet < up_bound & riquezanet > lo_bound & class == "capitalist"), na.rm = T)[[1]] %>%
        plot(col = "cyan", main = "Group comparison of wealth quantiles 2020")
svycdf(~riquezanet, subset(sv_eff, riquezanet < up_bound & riquezanet > lo_bound & class == "worker"), na.rm = T)[[1]] %>%
        lines(col = "blue")
svycdf(~riquezanet, subset(sv_eff, riquezanet < up_bound & riquezanet > lo_bound), na.rm = T)[[1]]  %>%
        lines(col = "red")
legend("bottomright", legend = c("Employers", "Workforce", "Total Pop."), col = c("cyan", "blue", "red"), lty = 1)
dev.off()

jpeg(file = "output/rif/img/histogram.jpeg")
svyhist(~riquezanet, subset(sv_eff, riquezanet < up_bound & riquezanet > lo_bound), na.rm = T)
dev.off()

# empirical distributions for workers and capitalist in 2002 and 2020
x_ran <- dt[, colnames(dt) %like% "x", with = F] %>% unlist() %>% as.numeric() %>% range()
y_ran <- dt[, colnames(dt) %like% "y", with = F] %>% unlist() %>% as.numeric() %>% range()
bottom_tit_c <- c(paste0("Employers02-GINI: ", round(gini_c[, "2002"][2], digits = 3), " - Employers20-GINI: ", round(gini_c[, "2020"][2], digits = 3)))
bottom_tit_w <- c(paste0("Workers02-GINI: ", round(gini_w[, "2002"][2], digits = 3), " - Workers20-GINI: ", round(gini_w[, "2020"][2], digits = 3)))

# plot the comparison of empirical distributions
jpeg(file = "output/rif/img/emp_histogram.jpeg")
par(mfrow = c(2, 1))

# capitalists
plot(dt$cap_s_x2002, dt$cap_s_y2002, type = "l", col = "cyan", xlim = x_ran, ylim = y_ran, main = "EDF", xlab = as.character(bottom_tit_c), ylab = "Rel. Freq.")
lines(dt$cap_s_x2020, dt$cap_s_y2020, col = "blue")
legend("topright", legend = c("Employers02", "Employers20"), col = c("cyan", "blue"), lty = 1)

# workers
plot(dt$wor_s_x2002, dt$wor_s_y2002, type = "l", col = "green", xlim = x_ran, ylim = y_ran, main = "EDF", xlab = as.character(bottom_tit_w), ylab = "Rel. Freq.")
lines(dt$wor_s_x2020, dt$wor_s_y2020, col = "#010f03")
legend("topright", legend = c("Workers02", "Workers20"), col = c("green", "#010f03"), lty = 1)
dev.off()
