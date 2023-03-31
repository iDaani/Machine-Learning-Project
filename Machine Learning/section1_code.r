setwd("C:/Users/Asus/Desktop/AI Assignment")

# Intalling caTools library because of sample.split

library(caTools)

# Installing InformationValue library because of confusionMatrix in r

library(InformationValue)



library(modelr)

# Installing MLevel, ROCR library for ROC curve

library(MLeval)

library(ROCR)


set.seed(42)

features <- read.csv(file = './40280938_features.csv', header = TRUE)
features
nrow(features)



# 1.1


# we will add a separate column of 'isLetter' with two values
# 1 representing is Letter
# 2 representing is Not Letter

# Helpful for logistic regression 

features[80,]

features$isLetter <- sample(100, size = nrow(features), replace = TRUE)

features$isLetter[1:80] = 1
features$isLetter[81:140] = 0


features

# Splitting data into training set and test set

i_split<- sample.split(row.names(features), 0.8)
train<-features[i_split, ]
test<-features[!i_split, ]

nrow(train)
nrow(test)

# Performing the logistic regression model

logistic <- glm(isLetter ~ nr_pix + aspect_ratio, data = train, family="binomial")

logistic


# Predict probabilities first 

probability = predict(logistic, test, type="response")
probability

# Confusion matrix for threshold of 0.5


confusion_matrix <- confusionMatrix(test$isLetter, probability, threshold = 0.5)
confusion_matrix



# Accuracy of data


accuracy <- (10+3)/28
accuracy

# True positive rate

true_positive_rate <- sensitivity(test$isLetter, probability)
true_positive_rate

# False positive rate

false_positive_rate <- (5/8)
false_positive_rate

# Precision

precision <- (10/15)
precision

# Recall
# Recall = TruePositives / (TruePositives + FalseNegatives)

recall <- 10/ (10+10)
recall

# F1-score

f1_score <- (2*precision*recall)/(precision+recall)
f1_score



# 1.2

# Loading caret library for trainControl to work

library(caret)


# Performing cross-validation

ctrl <- trainControl(method = "cv", number = 5, savePredictions = T)


# Training the model

model <- train(isLetter ~ nr_pix + aspect_ratio, data = features, method = "lm", trControl = ctrl)

model

model$finalModel

# Predicting the probabilities

probability_cross_validated_data <- predict(model, features)

# Making confusion matrix 

# unloading caret library for confusionMatrix to work

detach("package:caret", unload = TRUE)

confusionMatrix(features$isLetter, probability_cross_validated_data, threshold = 0.5)


# Accuracy of data

accuracy_cross_validated_data <- (62+20)/140
accuracy_cross_validated_data

# True positive rate

true_positive_rate_cross_validated_data <- sensitivity(features$isLetter, probability_cross_validated_data)
true_positive_rate_cross_validated_data

# False positive rate

false_positive_rate_cross_validated_data <- (18/38)
false_positive_rate_cross_validated_data

# Precision

precision_cross_validated_data <- (62/80)
precision_cross_validated_data


# Recall
# Recall = TruePositives / (TruePositives + FalseNegatives)

recall_cross_validated_data <- 62/ (62+40)
recall_cross_validated_data

# F1-score


f1_score_cross_validated_data <- (2*precision_cross_validated_data*recall_cross_validated_data)/(precision_cross_validated_data+recall_cross_validated_data)
f1_score_cross_validated_data




# 1.3




# Plotting ROC curve for 1.2, linear regression model with corss-validation.

rocr_pred = prediction(probability_cross_validated_data, features$isLetter)

rocr_performance <- performance(rocr_pred, "tpr", "fpr")

plot(rocr_performance)

plot(rocr_performance, colorize = TRUE)

