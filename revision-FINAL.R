## Final Revision 072024 for article "Spain, society of owners" @Miguel-Duch
library(data.table)
library(magrittr)
library(survey)

years <- c(2008, 2011, 2014, 2017, 2020) # You can expand this later if needed
obj_list <- list()
full_mean <- list()
ecv_mean <- list()

# for incorporating new variables from
for (i in years) {
    year_data <- list()

    for (j in 1:5) {
        df <- fread(paste0("EFFSpain/full/", i, "/otras_secciones_", i, "_imp", j, ".csv"))
        ec <- fread(paste0("EFF-ECVSpain/ECV/output/filtered_data", i, ".csv"))
        selection <- colnames(df)[colnames(df) %like% "p2_9b_"]
        year_data[[j]] <- df[, ..selection]
        ecv_mean[[j]] <- data.table(ec)
    }

    obj_list[[as.character(i)]] <- year_data
}

# to get the previously created datasets
for (p in 1:5) full_mean[[p]] <- fread(paste0("EFFSpain/datasets/", years[p], "-EFF.microdat.csv"))

#------------------CHECK LENGTH OF OBJECTS-----------------------------------------------------------#

# Working class subsample size
value <- c()
for (z in seq_along(years)) {
    value[z] <- nrow(full_mean[[z]][nsitlabdom == 1, 1]) / nrow(full_mean[[z]][, 1])
    print(value[z])
}

# Young people subsample size
value2 <- c()
for (z in seq_along(years)) {
    value2[z] <- nrow(full_mean[[z]][bage == 1 & nsitlabdom == 1, 1]) / nrow(full_mean[[z]][, 1])
    print(value2[z])
}

#-----------------------------------------------------------------------------#


# HY090G1, PL031
ec_sv <- list()
df_sv <- list()

for (p in 1:5) {
    transf <- ecv_mean[[p]] %>% data.frame()
    ec_sv[[p]] <- svydesign(
        ids = ~1,
        data = transf,
        weights = ~ transf$PB040
    )
}

# P29b % population
nueva <- list()
for (i in 1:5) {
    for (j in 1:5) {
        obj_list[[i]][[j]][is.na(obj_list[[i]][[j]])] <- 0
        obj_list[[i]][[j]][, sum := as.numeric(p2_9b_1) + as.numeric(p2_9b_2) + as.numeric(p2_9b_3) + as.numeric(p2_9b_4)]
    }
    nueva[[i]] <- obj_list[[i]][[1]]$sum + obj_list[[i]][[1]]$sum + obj_list[[i]][[1]]$sum + obj_list[[i]][[1]]$sum + obj_list[[i]][[1]]$sum
    part <- data.table(nueva[[i]] / 5)
    full_mean[[i]][, p2_9b := part]
}

for (p in 1:5) {
    full_mean[[p]][, homeowner := as.factor(np2_1)][, young_homeowner := homeowner == 1 & bage == 1]
    transf <- full_mean[[p]] %>% data.frame()
    df_sv[[p]] <- svydesign(
        ids = ~1,
        data = transf,
        weights = ~ transf$facine3
    )
}

#-----------------------------------------------------------------------------#


# Young people homeownership
years <- c(2002, 2005, 2008, 2011, 2014, 2017, 2020) # You can expand this later if
resu <- resu2 <- c()
for (p in seq_along(years)) {
    full_mean[[p]] <- fread(paste0("EFFSpain/datasets/", years[p], "-EFF.microdat.csv"))
    full_mean[[p]][, homeowner := as.factor(np2_1)]
    full_mean[[p]][, young_homeowner := 0][homeowner == 1 & bage == 1, young_homeowner := 1]
    full_mean[[p]][, young_w_homeowner := 0][young_homeowner == 1 & nsitlabdom == 1, young_w_homeowner := 1]
    transf <- full_mean[[p]] %>% data.frame()
    df_sv[[p]] <- svydesign(
        ids = ~1,
        data = transf,
        weights = ~ transf$facine3
    )
    resu[[p]] <- prop.table(svytable(~young_homeowner, subset(df_sv[[p]], bage == 1)))[2]
    resu2[[p]] <- prop.table(svytable(~young_w_homeowner, subset(df_sv[[p]], bage == 1)))[2]
}
print(resu2)



filtered_data <- ecv_mean[[1]]

filtered_data[filtered_data$PL031 %in% c(2, 5), "PL031"] <- 1
filtered_data[filtered_data$PL031 %in% c(3, 4), "PL031"] <- 2
filtered_data[filtered_data$PL040 %in% c(2), "PL031"] <- 3
filtered_data[filtered_data$PL051 %in% c(1, 11, 12, 13, 14, 15, 16, 17, 18, 19), "PL031"] <- 4
filtered_data[filtered_data$PL031 %in% c(7), "PL031"] <- 5
filtered_data[filtered_data$PL031 %in% c(6, 8, 9, 10, 11), "PL031"] <- 6
filtered_data$PL031 <- factor(filtered_data$PL031,
    levels = c(1, 2, 3, 4, 5, 6),
    labels = c("worker", "capitalist", "self-employed", "manager", "retired", "inactive")
)

filtered_data <- filtered_data %>% data.frame()
filtered_data_sv <- svydesign(
    ids = ~1,
    data = filtered_data,
    weights = ~ filtered_data$PB040
)

# Share of income of working class rents income
ft_ren <- svyby(~HY040N, ~PL031, filtered_data_sv, svymean, keep.names = F, keep.var = F) # RENTAL ALQ. V
ft_hog <- as.data.frame(svyby(~HY040N, ~PL031, filtered_data_sv, svymean, keep.names = F, keep.var = F))[-1]
