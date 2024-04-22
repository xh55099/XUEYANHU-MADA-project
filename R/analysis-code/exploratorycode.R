## ---- packages --------
#load needed packages. make sure they are installed.
library(here) #for data loading/saving
library(dplyr)
library(skimr)
library(ggplot2)

## ---- loaddata --------
#Path to data. Note the use of the here() package and not absolute paths
data_location <- here::here("starter-analysis-exercise","data","processed-data","processeddata.rds")
#load data
mydata <- readRDS(data_location)

## ---- logistic --------
# Fit multinomial logistic regression model
#library(nnet)
#multinom_model <- multinom(FAF ~ MTRANS, data = mydata)

# Summary of the model
#summary(multinom_model)


## ---- lm1 --------
# linear model of BMI by water and FAF
lm1 <- lm(BMI ~ Water + FAF + Water * FAF, data = mydata)
lm1_result <- summary(lm1)

# save the result
lm_file = here("results", "tables", "lm1table.rds")
saveRDS(lm1_result, file = lm_file)


## ---- lm2 --------
mydata$Alcohol <- as.factor(mydata$Alcohol)
lm2 <- lm(BMI ~ Alcohol, data = mydata)
lm2_result <- summary(lm2)

# save the result
lm_file = here("results", "tables", "lm2table.rds")
saveRDS(lm2_result, file = lm_file)

## ---- lm3 --------
mydata1 <- mydata %>%
  select(History, Water, Alcohol, FAF, MTRANS, BMI)
lm3 <- lm(BMI ~ ., data = mydata1)
lm3_result <- summary(lm3)

# save the result
lm_file = here("results", "tables", "lm3table.rds")
saveRDS(lm3_result, file = lm_file)


## ---- lm4 --------
lm4 <- lm(BMI ~ Gender * Age, data = mydata)
lm4_result <- summary(lm4)

# save the result
lm_file = here("results", "tables", "lm4table.rds")
saveRDS(lm4_result, file = lm_file)


## ---- fitting linear models --------
# Split data into training and testing sets
set.seed(123)  # Set seed for reproducibility
trainIndex <- createDataPartition(mydata$BMI, p = 0.8, list = FALSE)
trainData <- mydata[trainIndex, ]
testData <- mydata[-trainIndex, ]
# Create indices for 10-fold cross-validation
cv_folds <- createFolds(trainData$BMI, k = 10)

# predictors excluding interactions first
# Define predictors
predictors <- c("Gender", "Age", "History", "Water", "Alcohol", "FAF", "MTRANS")

# Iterate through all possible combinations of predictors
for (i in 1:length(predictors)) {
  subsets1 <- combn(predictors, i)
  for (j in 1:ncol(subsets1)) {
    predictors_subset1 <- subsets1[, j]
    
    # Initialize variable to store RMSE for current combination of predictors
    total_rmse_lm1 <- 0
    
    # Perform cross-validation
    for (fold in 1:10) {
      # Extract indices for training and validation data for current fold
      train_indices <- unlist(cv_folds[-fold])
      validation_indices <- cv_folds[[fold]]
      
      # Fit model using subset of predictors
      formula_lm1 <- paste("BMI ~", paste(predictors_subset1, collapse = " + "))
      model_lm1 <- lm(formula_lm1, data = trainData[train_indices, ])
      
      # Make predictions on validation set
      predictions_lm1 <- predict(model_lm1, newdata = trainData[validation_indices, ])
      
      # Calculate RMSE for current fold
      fold_rmse_lm1 <- sqrt(mean((trainData$BMI[validation_indices] - predictions_lm1)^2))
      
      # Add RMSE for current fold to total RMSE
      total_rmse_lm1 <- total_rmse_lm1 + fold_rmse_lm1
    }
    
    # Calculate average RMSE across all folds
    avg_rmse_lm1 <- total_rmse_lm1 / 10
    
    # Update minRMSE and bestSubset if average RMSE is lower
    if (avg_rmse_lm1 < minRMSE1) {
      minRMSE1 <- avg_rmse_lm1
      bestSubset1 <- predictors_subset1
    }
  }
}

# Print results
print(paste("Best subset of predictors:", paste(bestSubset1, collapse = ", ")))
print(paste("Lowest RMSE:", minRMSE1))

# calculate null model RMSE
# Initialize variable to store total RMSE across all folds
total_rmse_null <- 0

# Perform cross-validation
for (fold in 1:10) {
  # Extract indices for training and validation data for current fold
  train_indices <- unlist(cv_folds[-fold])
  validation_indices <- cv_folds[[fold]]
  
  # Compute the mean BMI from the training data
  mean_bmi_train <- mean(trainData$BMI[train_indices])
  
  # Create a vector of predicted values equal to the mean BMI from training data
  predicted_bmi_validation <- rep(mean_bmi_train, length(validation_indices))
  
  # Compute RMSE (Root Mean Squared Error) on validation data for current fold
  rmse_null_validation <- sqrt(mean((trainData$BMI[validation_indices] - predicted_bmi_validation)^2))
  
  # Add RMSE for current fold to total RMSE
  total_rmse_null <- total_rmse_null + rmse_null_validation
}

# Calculate average RMSE across all folds
avg_rmse_null <- total_rmse_null / 10

# Print the results
print(paste("Null Model RMSE using Cross-Validation:", avg_rmse_null))

# interactions included in predictors
# Define predictors and response variable
predictors <- c("Gender", "Age", "History", "Water", "Alcohol", "FAF", "MTRANS")
interaction_terms <- c("Gender_Age", "FAF_Water")  # Define interaction terms
response <- "BMI"

# Create interaction terms in training and testing data
trainData$Gender_Age <- trainData$Gender * trainData$Age
trainData$FAF_Water <- trainData$FAF * trainData$Water

testData$Gender_Age <- testData$Gender * testData$Age
testData$FAF_Water <- testData$FAF * testData$Water

# Update list of predictors to include interaction terms
predictors_updated <- c(predictors, "Gender_Age", "FAF_Water")

# Initialize variables to store results
minRMSE2 <- Inf
bestSubset2 <- NULL

# Iterate through all possible combinations of predictors
for (i in 1:length(predictors_updated)) {
  subsets2 <- combn(predictors_updated, i)
  for (j in 1:ncol(subsets2)) {
    predictors_subset2 <- subsets2[, j]
    
    # Initialize variable to store RMSE for current combination of predictors
    total_rmse_lm2 <- 0
    
    # Perform cross-validation
    for (fold in 1:10) {
      # Extract indices for training and validation data for current fold
      train_indices <- unlist(cv_folds[-fold])
      validation_indices <- cv_folds[[fold]]
      
      # Fit model using subset of predictors
      formula_lm2 <- paste("BMI ~", paste(predictors_subset2, collapse = " + "))
      model_lm2 <- lm(formula_lm2, data = trainData[train_indices, ])
      
      # Make predictions on validation set
      predictions_lm2 <- predict(model_lm2, newdata = trainData[validation_indices, ])
      
      # Calculate RMSE for current fold
      fold_rmse_lm2 <- sqrt(mean((trainData$BMI[validation_indices] - predictions_lm2)^2))
      
      # Add RMSE for current fold to total RMSE
      total_rmse_lm2 <- total_rmse_lm2 + fold_rmse_lm2
    }
    
    # Calculate average RMSE across all folds
    avg_rmse_lm2 <- total_rmse_lm2 / 10
    
    # Update minRMSE and bestSubset if average RMSE is lower
    if (avg_rmse_lm2 < minRMSE2) {
      minRMSE2 <- avg_rmse_lm2
      bestSubset2 <- predictors_subset2
    }
  }
}

# Print results
print(paste("Best subset of predictors (including interactions) using cross-validation:", paste(bestSubset2, collapse = ", ")))
print(paste("Lowest RMSE (including interactions) using cross-validation:", minRMSE2))

# Initialize variables to store results
min_rmse_lasso <- Inf
best_subset_lasso <- NULL

# Perform LASSO regression with cross-validation
lasso_model <- cv.glmnet(as.matrix(trainData[, predictors_updated]), trainData$BMI, alpha = 1, nfolds = 10)

# Extract the optimal lambda value
optimal_lambda <- lasso_model$lambda.min

# Fit the LASSO model using the optimal lambda
lasso_model_optimal <- glmnet(as.matrix(trainData[, predictors_updated]), trainData$BMI, alpha = 1, lambda = optimal_lambda)

# Compute RMSE on test data using the optimal LASSO model
predictions_lasso <- predict(lasso_model_optimal, newx = as.matrix(testData[, predictors_updated]))
rmse_lasso <- sqrt(mean((testData$BMI - predictions_lasso)^2))

# Extract coefficients of the LASSO model
lasso_coefficients <- as.matrix(coef(lasso_model_optimal))

# Print results
cat("Optimal lambda:", optimal_lambda, "\n")
cat("RMSE with LASSO:", rmse_lasso, "\n")

# Print non-zero coefficients (selected predictors)
selected_predictors <- rownames(lasso_coefficients)[lasso_coefficients != 0, drop = FALSE]
print("Selected predictors by LASSO:")
print(selected_predictors)

# Create formula with response and all predictors
predictors_updated <- c(predictors, "Gender_Age", "FAF_Water")
formula_rf <- as.formula(paste(response, "~", paste(predictors_updated, collapse = " + ")))

# Initialize variables to store results
best_rmse_rf <- Inf
best_predictors_rf <- NULL

# Forward selection with cross-validation
for (fold in seq_along(cv_folds)) {
  # Extract training and validation indices for current fold
  train_indices <- unlist(cv_folds[-fold])
  validation_indices <- cv_folds[[fold]]
  
  # Initialize variables to store results for current fold
  best_rmse_fold <- Inf
  best_predictors_fold <- NULL
  
  # Forward selection
  for (i in seq_along(all_predictors)) {
    predictors_subset_rf <- all_predictors[1:i]
    formula_subset_rf <- as.formula(paste(response, "~", paste(predictors_subset_rf, collapse = " + ")))
    
    # Fit random forest model using the training data for the current fold
    model_rf <- randomForest(formula = formula_subset_rf, data = mydata[train_indices, ])
    
    # Make predictions on the validation data for the current fold
    predictions_rf <- predict(model_rf, newdata = mydata[validation_indices, ])
    
    # Calculate RMSE for the current fold
    rmse_rf <- sqrt(mean((mydata$BMI[validation_indices] - predictions_rf)^2))
    
    # Check if current RMSE is better than the best RMSE so far for this fold
    if (rmse_rf < best_rmse_fold) {
      best_rmse_fold <- rmse_rf
      best_predictors_fold <- predictors_subset_rf
    }
  }
  
  # Check if the best RMSE for this fold is better than the best RMSE so far
  if (best_rmse_fold < best_rmse_rf) {
    best_rmse_rf <- best_rmse_fold
    best_predictors_rf <- best_predictors_fold
  }
}

# Print the best combination of predictors
print("Best combination of predictors for random forest model:")
print(best_predictors_rf)
print(best_rmse_rf)


## ---- fitting logistic models --------
# Define the outcome variable
outcome <- "Obesity"

# Define predictor variables
predictors_updated <- c(predictors, "Gender_Age", "FAF_Water")

# Initialize variables to store results
best_subset_DA <- NULL
best_accuracy_DA <- 0

# Create cross-validation folds
cv_fold1 <- createFolds(trainData$Obesity, k = 10)

# Iterate through all possible combinations of predictors
for (i in 1:length(predictors_updated)) {
  subsets <- combn(predictors_updated, i)
  for (j in 1:ncol(subsets)) {
    predictors_subset_DA <- subsets[, j]
    
    # Initialize variable to store accuracy for each fold
    fold_accuracies <- numeric(length(cv_fold1))
    
    # Perform cross-validation
    for (fold_index in seq_along(cv_fold1)) {
      # Extract training and validation indices for current fold
      train_indices <- unlist(cv_fold1[-fold_index])
      validation_indices <- cv_fold1[[fold_index]]
      
      # Fit the DA model using the subset of predictors on the training data for the current fold
      formula_DA <- as.formula(paste(outcome, "~", paste(predictors_subset_DA, collapse = " + ")))
      model_DA <- lda(formula_DA, data = mydata[train_indices, ])
      
      # Make predictions on the validation set
      predicted_DA <- predict(model_DA, newdata = trainData[validation_indices, ])
      
      # Calculate accuracy for the current fold
      fold_accuracies[fold_index] <- mean(predicted_DA$class == mydata$Obesity[validation_indices])
    }
    
    # Calculate average accuracy across all folds
    average_accuracy <- mean(fold_accuracies)
    
    # Update best subset and accuracy if current accuracy is higher
    if (average_accuracy > best_accuracy_DA) {
      best_subset_DA <- predictors_subset_DA
      best_accuracy_DA <- average_accuracy
    }
  }
}

# Print the best subset of predictors and accuracy
print(paste("Best subset of predictors:", paste(best_subset_DA, collapse = ", ")))
print(paste("Best average accuracy:", best_accuracy_DA))

# Calculate the frequency of each category in the outcome variable
outcome_frequency <- table(trainData$Obesity)

# Find the most frequent category
most_frequent_category <- names(outcome_frequency)[which.max(outcome_frequency)]

# Create a vector of predicted values with the most frequent category
predicted_outcome <- rep(most_frequent_category, nrow(trainData))

# Calculate the accuracy of the null model
accuracy_null_model <- sum(predicted_outcome == trainData$Obesity) / length(trainData$Obesity)

# Print the accuracy
print(paste("Accuracy of the null model:", accuracy_null_model))

# Define outcome variable
outcome <- "Obesity"

# Create formula
formula_mlr <- as.formula(paste(outcome, "~", paste(predictors_updated, collapse = " + ")))

# Initialize variables to store results
best_accuracy_mlr <- 0
best_predictors_mlr <- NULL

# Perform cross-validation
for (fold in 1:10) {
  # Extract indices for training and validation data for current fold
  train_indices <- unlist(cv_fold1[-fold])
  validation_indices <- cv_fold1[[fold]]
  
  # Fit the Multinomial Logistic Regression model
  model_mlr <- multinom(formula_mlr, data = trainData[train_indices, ])
  
  # Make predictions on the validation set
  predicted_mlr <- predict(model_mlr, newdata = trainData[validation_indices, ], type = "class")
  
  # Calculate accuracy
  accuracy_mlr <- mean(predicted_mlr == trainData$Obesity[validation_indices])
  
  # Update best_accuracy and best_predictors if current accuracy is higher
  if (accuracy_mlr > best_accuracy_mlr) {
    best_accuracy_mlr <- accuracy_mlr
    best_predictors_mlr <- predictors_updated
  }
}

# Print the best predictor combination and highest accuracy
print(paste("Best predictor combination:", paste(best_predictors_mlr, collapse = ", ")))

print(paste("Highest Accuracy:", best_accuracy_mlr))


## ---- test models --------
# Define formula for random forest
formula_rf_test <- as.formula(paste("BMI ~", paste(predictors_updated, collapse = " + ")))

# Fit random forest model using the entire training data
model_rf_test <- randomForest(formula = formula_rf_test, data = trainData)

# Make predictions on the test data
predictions_rf_test <- predict(model_rf_test, newdata = testData)

# Calculate RMSE on test data
rmse_rf_test <- sqrt(mean((testData$BMI - predictions_rf_test)^2))

# Print RMSE on test data
print(rmse_rf_test)

# Null model of test data
# Compute the mean BMI from the training data
mean_bmi_train <- mean(trainData$BMI)

# Create a vector of predicted values equal to the mean BMI from training data for the test data
predicted_bmi_test <- rep(mean_bmi_train, nrow(testData))

# Calculate RMSE (Root Mean Squared Error) on test data
rmse_null_test <- sqrt(mean((testData$BMI - predicted_bmi_test)^2))

# Print the results
print(paste("Null Model RMSE on Test Data:", rmse_null_test))

# Create a data frame for plotting
plot_data_rf <- data.frame(Observed = testData$BMI, 
                           Predicted_RF = predictions_rf_test,
                           Predicted_Null = predicted_bmi_test)

# Plot using ggplot
p_rf <- ggplot(plot_data_rf, aes(x = Observed, y = Predicted_RF)) +
  geom_point(color = "blue", size = 2, shape = 16) +
  geom_point(aes(y = Predicted_Null), color = "green", size = 2, shape = 16) +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  labs(x = "Observed BMI", y = "Predicted BMI", title = "Observed vs Predicted BMI") +
  theme_minimal() +
  theme(legend.position = "top") +
  scale_shape_manual(values = c(16, 16)) +
  scale_color_manual(values = c("blue", "green")) +
  guides(color = guide_legend(title = "Model"))

plot(p_rf)

# save the plot
figure_file = here("results","figures","Observed-Predicted .png")
ggsave(filename = figure_file, plot=p_rf) 

# Define formula for mlr model
formula_mlr_test <- as.formula(paste("Obesity ~", paste(predictors_updated, collapse = " + ")))

# Fit the Multinomial Logistic Regression model using the entire training data
model_mlr_test <- multinom(formula_mlr_test, data = trainData)

# Make predictions on the test data
predicted_mlr_test <- predict(model_mlr_test, newdata = testData, type = "class")

# Calculate accuracy on the test data
accuracy_mlr_test <- mean(predicted_mlr_test == testData$Obesity)

# Print the accuracy
print(paste("Accuracy of the MLR model on test data:", accuracy_mlr_test))

# Compute the mean BMI from the training data
mean_bmi_training <- mean(trainData$BMI)

# Create a vector of predicted values equal to the mean BMI from training data
predicted_bmi_test <- rep(mean_bmi_train, nrow(testData))

# Calculate RMSE on test data
rmse_null_test <- sqrt(mean((testData$BMI - predicted_bmi_test)^2))

# Print the RMSE
print(paste("Null Model RMSE on Test Data:", rmse_null_test))

# Initialize variable to store predictions on the test set
predictions_lm1_test <- rep(NA, nrow(testData))

# Update the formula to include Age, History, Alcohol, and MTRANS as predictors
formula_lm1_test <- paste("BMI ~ Age + History + Alcohol + MTRANS")

# Fit the linear regression model using the updated formula and the training data
model_lm1_test <- lm(formula_lm1_test, data = trainData)

# Make predictions on the test set
predictions_lm1_test <- predict(model_lm1_test, newdata = testData)

# Calculate RMSE on the test data
RMSE_lm1_test <- sqrt(mean((testData$BMI - predictions_lm1_test)^2))

# Print the RMSE on the test data
print(paste("RMSE on test data:", RMSE_lm1_test))

# Define formula for linear model 2
formula_lm2_test <- as.formula(paste("BMI ~", paste(predictors_updated, collapse = " + ")))

# Fit linear model 2 model using the entire training data
model_lm2_test <- randomForest(formula = formula_lm2_test, data = trainData)

# Make predictions on the test data
predictions_lm2_test <- predict(model_lm2_test, newdata = testData)

# Calculate RMSE on test data
rmse_lm2_test <- sqrt(mean((testData$BMI - predictions_lm2_test)^2))

# Print RMSE on test data
print(rmse_lm2_test)

# Fit the LASSO model using the optimal lambda
lasso_model_optimal <- glmnet(as.matrix(trainData[, predictors_updated]), trainData$BMI, alpha = 1, lambda = 0.0011)

# Make predictions on the test data using the optimal LASSO model
predictions_lasso_test <- predict(lasso_model_optimal, newx = as.matrix(testData[, predictors_updated]))

# Calculate RMSE on the test data
rmse_lasso_test <- sqrt(mean((testData$BMI - predictions_lasso_test)^2))

# Print RMSE on test data
print(rmse_lasso_test)

# Create a vector of predicted values with the most frequent category for testData
predicted_outcome_test <- rep(most_frequent_category, nrow(testData))

# Calculate the accuracy of the null model on testData
accuracy_null_model_test <- sum(predicted_outcome_test == testData$Obesity)/ length(testData$Obesity)

# Print the accuracy
print(paste("Accuracy of the null model on test data:", accuracy_null_model_test))

# Define formula for DA model
formula_DA_test <- as.formula(paste("Obesity ~", paste(predictors_updated, collapse = " + ")))

# Fit the DA model using the entire training data
model_DA_test <- lda(formula_DA_test, data = trainData)

# Make predictions on the test data
predicted_DA_test <- predict(model_DA_test, newdata = testData)

# Calculate accuracy on the test data
accuracy_DA_test <- mean(predicted_DA_test$class == testData$Obesity)

# Print the accuracy
print(paste("Accuracy of the DA model on test data:", accuracy_DA_test))
```