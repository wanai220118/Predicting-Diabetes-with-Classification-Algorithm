#install required packages
install.packages(c("caret", "randomForest", "class","e1071"))

#load libraries
library(caret)
library(randomForest)
library(class)
library(e1071)


#load the csv file
data <- read.csv("C:/Users/user/OneDrive/SEM6/Data Mining/Group Project/Dataset of Diabetes .csv")

#drop irrelevant columns
data <- subset(data, select = -c(ID, No_Pation, Cr, LDL, HDL, VLDL, TG))

#convert genderto numeric, M=1, F=0
data$Gender <- ifelse(data$Gender == "M", 1,0)

#convert class to binary : Y+P = 1, N = 0
data$CLASS <- ifelse(data$CLASS %in% c("Y", "P"), 1,0)

#check for missing values
print(sum(is.na(data)),
      
#impute missing values using median
if(sum(is.na(data))>0){
  preprocess <- preProcess(data, method = "medianImpute")
  data <- predict(preprocess, data)
}

#split data 30-70
set.seed(123)
split_index <- createDataPartition(data$CLASS, p = 0.3, list = FALSE)
train_data <- data[split_index,]
test_data <- data[-split_index,]

train_x <- train_data[, -which(names(train_data) == "CLASS")]
train_y <- train_data$CLASS
test_x <- test_data[, -which(names(test_data) == "CLASS")]
test_y <- test_data$CLASS

#THREE ALGORITHMS

#Logistic Regression
log_model<- glm(CLASS ~ ., data = train_data, family = binomial)
log_pred_prob <- predict(log_model, test_data, type = "response")
log_pred <- ifelse(log_pred_prob > 0.5,1,0)


#Random Forest
rf_model <- randomForest(as.factor(CLASS) ~., data = train_data, ntree =100)
rf_pred <- predict(rf_model, test_data)

#k-Nearest Neighbors (k=5)
knn_pred <- knn(train = train_x, test = test_x, cl = train_y, k=5)


#Evaluate Function and metrics
evaluate_model <- function(pred, actual) {
  cm <- confusionMatrix(as.factor(pred), as.factor(actual), positive = "1")
  precision <- cm$byClass["Precision"]
  recall <- cm$byClass["Recall"]
  f1 <- 2 * (precision * recall) / (precision + recall)
  accuracy <- cm$overall["Accuracy"]
  error_rate <- 1 - accuracy
  metrics <- c(
    Accuracy = accuracy,
    Error_Rate = error_rate,
    Precision = precision,
    Recall = recall,
    F1_Score = f1
  )
  return(metrics)
}



#evaluate all models

log_metrics <- evaluate_model(log_pred, test_y)
rf_metrics <- evaluate_model(rf_pred, test_y)
knn_metrics <- evaluate_model(knn_pred, test_y)

#Display Results

results <- rbind (
  Logistic_Regression = log_metrics,
  Random_Forest = rf_metrics,
  KNN = knn_metrics
)

print("Evaluation Metrics for each model:")
print(round(results,4))


