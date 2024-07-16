## Final Revision 072024 for article "Spain, society of owners" @Miguel-Duch
library(data.table)
library(magrittr)
library(survey)


ft_hog <- ft_ren <- ft_med <- ft_per <- ft_ratio <- ecv_mean <- list()
working_class_size <- sample_size <- working_class_ratio <- c()
years <- c(2005, 2008, 2011, 2014, 2017, 2020) # You can expand this later if
for (p in seq_along(years)) {
    ec <- fread(paste0("EFF-ECVSpain/ECV/output/filtered_data", years[p], ".csv"))
    ecv_mean[[p]] <- data.table(ec)

    filtered_data <- ecv_mean[[p]]

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
    filtered_data[, per_ral := 0][HY040N > 0, per_ral := 1][, per_ral := factor(per_ral)]
    working_class_size[p] <- nrow(filtered_data[PL031 == "worker", 1])
    sample_size[p] <- nrow(filtered_data[, 1])
    working_class_ratio[p] <- working_class_size[p] / sample_size[p]


    filtered_data <- filtered_data %>% data.frame()
    filtered_data_sv <- svydesign(
        ids = ~1,
        data = filtered_data,
        weights = ~ filtered_data$PB040
    )

    # Share of income of working class rents income
    ft_ren[[p]] <- cbind(svyby(~HY040N, ~PL031, subset(filtered_data_sv, HY040N > 0), svymean, keep.names = F, keep.var = F), years[p]) %>% data.table() # RENTAL ALQ. V
    ft_hog[[p]] <- cbind(svyby(~HY020, ~PL031, subset(filtered_data_sv, HY040N > 0), svymean, keep.names = F, keep.var = F), years[p])[, 2] %>% data.table()
    ft_per[[p]] <- svyby(~per_ral, ~PL031, filtered_data_sv, svymean, keep.names = F, keep.var = F)[, 2] %>% data.table()
    ft_med[[p]] <- svyby(~HY040N, ~PL031, subset(filtered_data_sv, HY040N > 0), svyquantile, quantiles = .5)[, 2] %>% data.table()
    ft_ratio[[p]] <- ft_ren[[p]]$statistic / ft_hog[[p]]$statistic
}


#------------------ECV RESULTS-----------------------------------------------------------#

## ECV sample size
sample_result_z <- cbind(years, sample_size, working_class_size, working_class_ratio)
print(sample_result_z)

# rental income for workers
worker_rental_z <- cbind(rbindlist(ft_ren), rbindlist(ft_med), rbindlist(ft_per), rbindlist(ft_hog))
colnames(worker_rental_z) <- c("class", "mean", "median", "%", "year", "renta")
print(worker_rental_z)
fwrite(worker_rental_z, "rental-income-ECV-updated.csv")
