### GENERAL USAGE FUNCTIONS

###############################################
replicator <- function(z, serie_length) {
  as.data.frame(
    lapply(
      z,
      function(i) {
        replicate(serie_length, i)
      }
    )
  )
}

#########################################
replicator_dummy <- function(z, serie_length) {
  as.data.frame(
    sapply(
      z,
      function(i) {
        as.numeric(c(replicate(i, 0), replicate(serie_length - i, 1)))
      }
    )
  )
}

#############################################
rates_to_levels <- function(z, sample_length) {
  serie_length <- length(z[, 1]) + 1
  final_rates_dum <- data.frame(matrix(NA, serie_length, sample_length))

  for (r in 1:sample_length) {
    growth_rates <- c(100, rep(NA, serie_length - 1))
    thirlwall_tot_c <- z[, r]

    for (p in 2:serie_length) {
      growth_rates[p] <- growth_rates[p - 1] * (1 + thirlwall_tot_c[p - 1])
    }

    final_rates_dum[r] <- growth_rates
  }

  return(final_rates_dum)
}

############################################
levels_to_rates <- function(dataset = NA) {
  sapply(dataset, function(i) diff(log(i)))
}

################################################
ave_growth_rate <- function(z, sample_length) {
  sapply(
    z,
    function(i) {
      as.numeric(
        (log(tail(i, n = 1)) -
          log(head(i, n = 1))) /
          sample_length
      )
    }
  )
}

##################################################
growth_rate_log <- function(z, sample_length) {
  return(
    as.numeric(
      (tail(z, n = 1) -
        head(z, n = 1)) /
        sample_length
    )
  )
}

########################
growth_rate_prom <- function(z) {
  sapply(
    z,
    function(i) {
      mean(i)
    }
  )
}

##########################################################
create_dashboard <- function(title, folder) {
  folder_path <- paste0(as.character(folder))
  dash_title <- paste0(folder_path, as.character(title), ".Rmd")

  return(
    rmarkdown::draft(
      dash_title,
      template = "flex_dashboard",
      package = "flexdashboard"
    )
  )
}

###########################################################
missing_cols <- function(data, cname) {
  add <- cname[!cname %in% names(data)]
  if (length(add) != 0) data[add] <- 0

  return(data)
}

###########################################################
create_table <- function(fill = NA, rows, cols, as_sql = FALSE) {
  table <- as.data.frame(matrix(fill, rows, cols))
  if (as_sql == TRUE) table <- data.table::as.data.table(table)
  return(table)
}

###########################################################
is_less <- function(x, limit = 100) {
  return(
    length(x) < limit
  )
}

###########################################################
encontrar_nombre <- function(x, y) {
  return(
    x[which(names(x) %in% y)]
  )
}

###########################################################
add_stars <- function(pvalue = NA, stat = NA, num = NA, top_down = "top") {
  n_row <- nrow(pvalue)
  if (top_down == "top") {
    cv_cutpoints <- c(1, 0.999, 0.99, 0.95, 0.9, 0)
  } else if (top_down == "down") {
    cv_cutpoints <- c(0, 0.001, 0.01, 0.05, 0.1, 1)
  }
  stars <- symnum(as.matrix(stat), corr = FALSE, na = FALSE, cutpoints = cv_cutpoints, symbols = c("***", "**", "*", ".", " "))
  for (r in 1:num) {
    pvalue[1:n_row, r] <- paste0(pvalue[1:n_row, r], as.table(stars)[1:n_row, r])
  }

  return(pvalue)
}

###########################################################
seq_along_row <- function(dataset = NA) {
  result <- seq_along(row.names(dataset))

  return(result)
}

###########################################################
mix_by_col <- function(head = NA, tail = NA) {
  mixed_result <- do.call(
    "rbind", Map(
      "rbind",
      split(head, seq_along_row(head)),
      split(tail, seq_along_row(tail))
    )
  )

  return(mixed_result)
}

#################################
add_quotes <- function(stat = NA, num = NA) {
  for (i in 1:num) {
    stat[, i] <- paste0("(", stat[, i], ")")
  }

  return(stat)
}

##########################################################
get_pvalue <- function(dataset = NA, first_col = 1, last_col = NA, obs = NA) {
  p_values <- round(
    2 * pt(abs(as.matrix(
      dataset[first_col:last_col]
    )),
    df = obs,
    lower.tail = FALSE
    ),
    3
  )
  return(p_values)
}

########################################
supergazer <- function(dataset = NA, file = NA, foot = NA, title = NA, format = "latex", separation = "10pt", skin = "qje") {
  comments <- list(
    sig_down = c(
      "(a)  P-value in brackets under each statistic. Signification: CV: 0.01 ***, 0.05 **, 0.1 *"
    ),
    sig_up = c(
      "(a)  P-value in brackets under each statistic. Signification: CV: 0.99 ***, 0.95 **, 0.9 *"
    ),
    ardl_test = c(
      "(1)  Breuch-Godfrey LM test for Autocorrelation in errors",
      "(2)  Breuch-Pagan LM test for Heteroskedasticity)",
      "(3)  Shapiro-Wilk test for Normality",
      "(4)  Ramsey RESET F test for Funcional Form",
      "(a)  P-value in brackets under each statistic. CV: 0.99 ***, 0.95 **, 0.9 *"
    ),
    thirlwall = c(
      "(1) Annualized estimations are computed generating annual predictions by interpollation",
      "(a) Average growth rates are calculated afterwards using this interpollated values"
    ),
    f_bound = c(
      "(1)  F Bound Test for Cointegration (Pesaran and Shin, 2001)",
      "(a)  P-value in brackets under each statistic. Signification: CV: 0.01 ***, 0.05 **, 0.1 *"
    ),
    s_break = c(
      "(1-2) OLS-CUSUM test on residuals for a break in the cummulative process. ",
      "(3-4) Nº of structural breaks. Associated beakdate.",
      "(5-6) Chow-test: Ho= There is a break at obs. 14 (i.e.: 2008)."
    ),
    adf = c(
      "(1)  Augmented Dickey-Fuller test for Stationarity over differenced variables",
      "Ho= The serie has a unit root",
      "P-values for no intercept/no trend cases. CV: -2.62*** -1.95** -1.61* (0.01, 0.05, 0.1)",
      "(2)  KPSS Unit Root / Cointegration Test",
      "Ho= Stationary variables are I(0) and therefore, stationary",
      "P-values for no intercept/no trend cases. CV: 0.347*** 0.463**  0.739* (0.01, 0.05, 0.1)"
    )
  )
  prenotes <- comments[foot]
  result <- stargazer::stargazer(dataset,
    title = title,
    summary = FALSE,
    type = format,
    header = FALSE,
    column.sep.width = separation,
    style = skin,
    notes = as.character(unlist(prenotes)),
    notes.align = "l",
    out = paste0(getwd(), "/SAVES/", file)
  )
}

#########################################################
percent <- function(primitive, length = 2, format = 2) {
  z <- round(as.matrix(primitive), length)
  x <- create_table(rows = nrow(z), cols = ncol(z))
  for (i in seq_len(ncol(z))) {
    x[i] <- scales::label_percent(suffix = "%")(z[, i])
  }

  x <- format(x, nsmall = format)
  return(x)
}

###########################################################
t_tester <- function(reg, coefnum, val) {
  co <- summary(reg)
  coefi <- co$coefficients[coefnum, 1]
  tstat <- (co$coefficients[coefnum, 1] - val) / co$coefficients[coefnum, 2]
  prob <- 2 * pt(abs(tstat), reg$df.residual, lower.tail = FALSE)

  return(round(c(tstat, prob, coefi, val), 3))
}

############################################################
growth_rate_pos_neg <- function(dataframe, isarray = FALSE) {
  if (isarray == TRUE) {
    cross_section <- 1
    sample_size <- length(dataframe) - 1
  } else {
    cross_section <- length(dataframe[1, ])
    sample_size <- length(dataframe[, 1]) - 1
  }

  rates <- create_table(0, sample_size, cross_section)

  for (i in 1:cross_section) {
    data <- as.numeric(dataframe[, i])

    for (z in 1:sample_size) {
      tester_A <- (data[z + 1] / data[z])
      tester_B <- (data[z + 1] - data[z]) / data[z]
      tester_C <- (data[z + 1] - data[z]) / abs(data[z])
      tester_D <- abs(data[z + 1]) / data[z]

      if (data[z] > 0 && data[z + 1] > 0) rates[z, i] <- tester_A - 1
      if (data[z] > 0 && data[z + 1] < 0) rates[z, i] <- tester_B
      if (data[z] < 0 && data[z + 1] > 0) rates[z, i] <- tester_C - 1
      if (data[z] < 0 && data[z + 1] < 0) rates[z, i] <- tester_D + 1
    }
  }

  return(rates)
}
################################################################### 33
#--Produces a data.frame with the Source Data+Training Data, Fitted Values+Forecast Values, forecast data Confidence Intervals
funggcast <- function(dn, fcast) {
  require(zoo) # needed for the 'as.yearmon()' function
  en <- max(time(fcast$mean)) # extract the max date used in the forecast
  # Extract Source and Training Data
  ds <- as.data.frame(window(dn, end = en))
  names(ds) <- "observed"
  ds$date <- as.Date(time(window(dn, end = en)))
  # Extract the Fitted Values (need to figure out how to grab confidence intervals)
  dfit <- as.data.frame(fcast$fitted)
  dfit$date <- as.Date(time(fcast$fitted))
  names(dfit)[1] <- "fitted"
  ds <- merge(ds, dfit, all.x = TRUE) # Merge fitted values with source and training data
  # Exract the Forecast values and confidence intervals
  dfcastn <- as.data.frame(fcast)
  dfcastn$date <- as.Date(as.yearmon(row.names(dfcastn)))
  names(dfcastn) <- c("forecast", "lo80", "hi80", "lo95", "hi95", "date")
  pd <- merge(ds, dfcastn, all.x = TRUE) # final data.frame for use in ggplot

  return(pd)
}
##############################################################
week_to_date <- function(start_year = NA, week = NA) {
  start_date <- as.Date(paste0(start_year, "-01-01"))
  first.day <- as.numeric(format(start_date, "%w"))
  converted_date <- start_date + week * 7 - first.day

  return(converted_date)
}
############################################################
mean_logit_sample <- function(sample = NA, formula = "mean") {
  return(
    sapply(sample, na.rm = TRUE, formula) %>% round(3)
  )
}
############################################################
mean_logit_sample1 <- function(sample = NA) {
  return(
    sapply(sample, function(i) {
      quantile(i, na.rm = TRUE, probs = seq(.10, .90, by = .20))
    }) %>% round(3)
  )
}
######################################################## 3
perc_with <- function(selection = NA, limit = 0) {
  results <- array()

  for (i in 1:length(colnames(selection))) {
    results[i] <- round(count(filter(selection[i], selection[i] > 0)) / count(selection[i]), digits = 4)
  }
  names(results) <- colnames(selection)
  return(results)
}
########################################################
set_lists_to_chars <- function(x) {
  if (class(x) == "list") {
    y <- paste(unlist(x[1]), sep = "", collapse = ", ")
  } else {
    y <- x
  }
  return(y)
}

##########################################################3
cbind.fill <- function(...) {
  nm <- list(...)
  nm <- lapply(nm, as.matrix)
  n <- max(sapply(nm, nrow))
  do.call(cbind, lapply(nm, function(x) {
    rbind(x, matrix(, n - nrow(x), ncol(x)))
  }))
}