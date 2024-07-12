## Final Revision 072024 for article "Spain, society of owners" @Miguel-Duch
library(data.table)
library(magrittr)
library(survey)


obj_list <- full_mean <- nueva <- df_sv <- list()
years <- c(2008, 2011, 2014, 2017, 2020) # You can expand this later if needed
refin <- refin_w <- c()
# for incorporating new variables from
for (i in years) {
    year_data <- list()

    for (j in 1:5) {
        df <- fread(paste0("EFFSpain/full/", i, "/otras_secciones_", i, "_imp", j, ".csv"))
        selection <- colnames(df)[colnames(df) %like% "p2_9b_"]
        year_data[[j]] <- df[, ..selection]
    }

    obj_list[[as.character(i)]] <- year_data
}

# to get the previously created datasets
# P29b % population
for (i in seq_along(years)) {
    full_mean[[i]] <- fread(paste0("EFFSpain/datasets/", years[i], "-EFF.microdat.csv"))
    for (j in 1:5) {
        obj_list[[i]][[j]][is.na(obj_list[[i]][[j]])] <- 0
        obj_list[[i]][[j]][, sum := as.numeric(p2_9b_1) + as.numeric(p2_9b_2) + as.numeric(p2_9b_3) + as.numeric(p2_9b_4)]
    }
    nueva[[i]] <- obj_list[[i]][[1]]$sum + obj_list[[i]][[2]]$sum + obj_list[[i]][[3]]$sum + obj_list[[i]][[4]]$sum + obj_list[[i]][[5]]$sum
    part <- data.table(nueva[[i]] / 5)
    full_mean[[i]][, p2_9b := part]
    transf <- full_mean[[i]] %>% data.frame()
    df_sv[[i]] <- svydesign(
        ids = ~1,
        data = transf,
        weights = ~ transf$facine3
    )
    refin[i] <- prop.table(svytable(~p2_9b, df_sv[[i]]))[2] %>% round(3)
    refin_w[i] <- prop.table(svytable(~p2_9b, subset(df_sv[[i]], nsitlabdom == 1)))[2] %>% round(3)
}


#------------------EFF REFINANCING RESULTS-----------------------------------------------------------#
#
results_refin <- cbind(years, refin, refin_w)
fwrite(results_refin, "refin.csv")