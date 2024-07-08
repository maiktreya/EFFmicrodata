# WORKSPACE SETUP- MEMORY CLEAN AND PACKAGES IMPORT
rm(list = ls())
`%>%` <- magrittr::`%>%` # nolint
c("magrittr", "survey", "data.table", "dineq") %>% sapply(library, character.only = T)

# PARAMETERS AND VARIABLES TO INITIALIZE
quantile_cuts <- c(.25, .5, .75, .9, .99, .999) # default cuts for estimated proportions
sel_year <- 2020 # selected survey year
dt_eff <- paste0(".datasets/", sel_year, "-EFF.microdat.csv") %>% fread() # Data table con microdatos anuales
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

##################################### GRADIENT BOOSTING (WITH CROSS-VALIDATION) #############################################
library(gbm)
library(caret)
library(pdp)

# Specify the cross-validation method (5-fold cross-validation)
cv <- trainControl(method = "cv", number = 5)

# Fit the gradient boosting model with cross-validation
gbm_model_cv <- train(
  riquezanet ~ sex + bage + renthog1 + class,
  data = dt_eff,
  method = "gbm",
  trControl = cv,
  verbose = FALSE
)
# Access average accuracy from cross-validation
av_accuracy <-  paste("Average Accuracy (Classification):", mean(gbm_model_cv$results$Accuracy))

##################################### FINAL MODEL FIT AND EVALUATION #############################################

# Split data into training (80%) and test (20%) sets
set.seed(123)
train_indices <- sample(1:nrow(dt_eff), nrow(dt_eff) * 0.8)
train_set <- dt_eff[train_indices, ]
test_set <- dt_eff[-train_indices, ]

# Fit the final gradient boosting model on the training set
final_gbm_model <- gbm(
  riquezanet ~ sex + bage + renthog1 + class,
  data = train_set,
  distribution = "gaussian",
  n.trees = 500,
  interaction.depth = 4,
  shrinkage = 0.01,
  weights = facine3
)

# Predict probabilities on test set
pred_prob <- predict(final_gbm_model, newdata = test_set, n.trees = 500, type = "response")
# Generate class predictions using a threshold of 0.5
predictions <- ifelse(pred_prob > 0.5, 1, 0)
# Confusion matrix
confusion_matrix <- table(Predicted = predictions, Actual = test_set$homeowner)
accuracy <- paste("Accuracy (Classification):", sum(diag(confusion_matrix)) / sum(confusion_matrix))

# Partial Dependence Plot for 'sex'
sex.pdp <- partial(final_gbm_model, pred.var = "sex", plot = TRUE, n.trees = 500)
# Partial Dependence Plot for 'bage'
bage.pdp <- partial(final_gbm_model, pred.var = "bage", plot = TRUE, n.trees = 500)
# Partial Dependence Plot for 'renthog'
renthog.pdp <- partial(final_gbm_model, pred.var = "renthog1", plot = TRUE, n.trees = 500)
# Partial Dependence Plot for 'class'
class.pdp <- partial(final_gbm_model, pred.var = "class", plot = TRUE, n.trees = 500)


# PREVIEW PRELIMINARY RESULTS
sink("output/gradient-boost/clas_cv/test_gradient-boost_clas_cv.txt")
gbm_model_cv %>% print()
gbm_model_cv %>%
        summary() %>%
        print()
av_accuracy %>% print()
confusion_matrix %>% print()
accuracy %>% print()
sink()


########3 PLOTTING PARTIAL DEPENDENCE
jpeg(file = "output/gradient-boost/clas_cv/sex.jpeg")
sex.pdp %>% print()
dev.off()
jpeg(file = "output/gradient-boost/clas_cv/bage.jpeg")
bage.pdp %>% print()
dev.off()
jpeg(file = "output/gradient-boost/clas_cv/renthog.jpeg")
renthog.pdp %>% print()
dev.off()
jpeg(file = "output/gradient-boost/clas_cv/class.jpeg")
class.pdp %>% print()
dev.off()
