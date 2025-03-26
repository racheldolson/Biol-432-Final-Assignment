#Winson Huang
#March 25th, 2025
#Training different machine learning models to predict whether a sample is benign or malignant

# Read data
Data <- read.csv("data.csv")

# Load required libraries
library(caret)
library(randomForest)
library(ggplot2)
library(tidyr)
library(pROC)
library(dplyr)

# Get top predictors from regression tree
top_predictors <- row.names(importance)
TopPred <- diagData[, top_predictors, drop = FALSE]

# Combine with diagnosis
data <- cbind(diagnosis = diagData$diagnosis, TopPred)
data$diagnosis <- as.factor(data$diagnosis)

# Split data
set.seed(123)
train_index <- createDataPartition(data$diagnosis, p = 0.8, list = FALSE)
train_data <- data[train_index, ]
test_data <- data[-train_index, ]

# RDA Model
rda_model <- train(diagnosis ~ ., data = train_data, method = "rda",
                   trControl = trainControl(method = "cv", number = 5))
rda_pred <- predict(rda_model, test_data)
confusionMatrix(rda_pred, test_data$diagnosis)

# Random Forest
rf_model <- randomForest(diagnosis ~ ., data = train_data, ntree = 500, importance = TRUE)
rf_pred <- predict(rf_model, test_data)
confusionMatrix(rf_pred, test_data$diagnosis)

# Plot RF feature importance
varImpPlot(rf_model, main = "Random Forest Variable Importance")

# Top 10 features
rf_importance <- importance(rf_model)
rf_importance_df <- as.data.frame(rf_importance)
rf_importance_df$Feature <- rownames(rf_importance_df)

top_features <- rf_importance_df %>%
  arrange(desc(MeanDecreaseAccuracy)) %>%
  slice(1:10)

# Making bar graph for the top 10 features 
ggplot(top_features, aes(x = reorder(Feature, MeanDecreaseAccuracy), 
                         y = MeanDecreaseAccuracy)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Top 10 Most Important Features",
       x = "Feature", y = "Mean Decrease in Accuracy") +
  theme_minimal(base_size = 14)

# LDA Model
lda_model <- train(diagnosis ~ ., data = train_data, method = "lda",
                   trControl = trainControl(method = "cv", number = 5))
lda_pred <- predict(lda_model, test_data)
confusionMatrix(lda_pred, test_data$diagnosis)

# SVM Model
svm_model <- train(diagnosis ~ ., data = train_data, method = "svmRadial",
                   trControl = trainControl(method = "cv", number = 5), prob.model = TRUE)
svm_pred <- predict(svm_model, test_data)
confusionMatrix(svm_pred, test_data$diagnosis)

# Confusion Matrix Comparison
conf_matrix_results <- data.frame(
  Model = c("RDA", "RF", "LDA", "SVM"),
  Correct = c(110, 109, 110, 112),
  Incorrect = c(3, 4, 3, 1)
)

# Convert to long format
conf_matrix_results_long <- conf_matrix_results %>%
  pivot_longer(cols = c("Correct", "Incorrect"),
               names_to = "Prediction Type",
               values_to = "Count")

# Custom order of models
conf_matrix_results_long$Model <- factor(conf_matrix_results_long$Model, 
                                         levels = c("LDA", "RDA", "RF", "SVM"))

# Making bar graph comparisons of the confusion matrices
ggplot(conf_matrix_results_long, aes(x = Model, y = Count, fill = `Prediction Type`)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  geom_text(aes(label = Count),
            position = position_dodge(width = 0.7),
            vjust = -0.5, size = 4.5, fontface = "bold") +
  scale_fill_manual(values = c("Correct" = "#1b7837", "Incorrect" = "#d73027")) +
  labs(x = "Machine Learning Model",
       y = "Number of Predictions",
       fill = "Prediction Type") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 16),
    legend.position = "top",
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 11),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 13)
  )

# ROC Curve and AUC Comparison
actual <- ifelse(test_data$diagnosis == "M", 1, 0)
lda_prob <- predict(lda_model, test_data, type = "prob")[, 2]
rf_prob <- predict(rf_model, test_data, type = "prob")[, 2]
svm_prob <- predict(svm_model, test_data, type = "prob")[, 2]
rda_prob <- predict(rda_model, test_data, type = "prob")[, 2]

lda_roc <- roc(actual, lda_prob, levels = c(0, 1), direction = "<")
rf_roc <- roc(actual, rf_prob, levels = c(0, 1), direction = "<")
svm_roc <- roc(actual, svm_prob, levels = c(0, 1), direction = "<")
rda_roc <- roc(actual, rda_prob, levels = c(0, 1), direction = "<")

ggplot() +
  geom_line(aes(x = 1 - lda_roc$specificities, y = lda_roc$sensitivities, color = "LDA")) +
  geom_line(aes(x = 1 - rf_roc$specificities, y = rf_roc$sensitivities, color = "Random Forest")) +
  geom_line(aes(x = 1 - svm_roc$specificities, y = svm_roc$sensitivities, color = "SVM")) +
  geom_line(aes(x = 1 - rda_roc$specificities, y = rda_roc$sensitivities, color = "RDA")) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray") +
  labs(title = "ROC Curve Comparison", x = "1 - Specificity", y = "Sensitivity") +
  scale_color_manual(name = "Model",
                     values = c("LDA" = "blue", "Random Forest" = "green", "SVM" = "red", "RDA" = "black")) +
  theme_minimal()

# Print AUC values
lda_auc <- auc(actual, lda_prob)
rf_auc <- auc(actual, rf_prob)
svm_auc <- auc(actual, svm_prob)
rda_auc <- auc(actual, rda_prob)

auc_results <- data.frame(
  Model = c("LDA", "Random Forest", "SVM", "RDA"),
  AUC = c(lda_auc, rf_auc, svm_auc, rda_auc)
)
print(auc_results)