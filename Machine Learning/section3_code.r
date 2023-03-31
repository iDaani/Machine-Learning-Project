setwd("C:/Users/Asus/Desktop/AI Assignment")

# Directory listing the CSV files
imagesDir<-"./Images"


imagesDir

allData <- read.table("./Images/all_features.csv")
allData

# Setting column names of the data

colnames(allData) <- c("label", "index", "nr_pix", "rows_with_1", "cols_with_1",
                       "rows_with_3p", "cols_with_3p", "aspect_ratio", "neigh_1",
                       "no_neigh_above", "no_neigh_below", "no_neigh_left", "no_neigh_right",
                       "no_neigh_horiz", "no_neigh_vert", "connected_areas", "eyes",
                       "custom")


# Making label as categorical factor so everything is classified


 levels = c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "sad", "smiley", "xclaim")

 labels = c("letter","letter","letter","letter","letter","letter",
          "letter","letter","letter","letter", "sad", "smiley", "xclaim" )


allData$label <- factor(allData$label,
                        levels = levels, labels = labels)


str(allData)



# The training data here chooses 80 values of each type of data and the rest in test data
# Also, custom feature  column is removed as asked in the assignment

training_data <- allData[allData$index<80, 1:17]

training_data

test_data <- allData[allData$index >= 80, 1:17]

test_data



# 3.1


library(caret)
library(randomForest)


train_control = trainControl(method="cv", number = 5, search = "grid", savePredictions = T)

set.seed(42)

tuningGrid <- expand.grid(mtry=c(2,4,6,8))

  

# For number of trees = 25

model_25 = train(label~., data=training_data, method="rf", trControl = train_control,
              tuneGrid = tuningGrid, ntree = 25)

model_25

model_25$finalModel$ntree


# For number of trees = 75

model_75 = train(label~., data=training_data, method="rf", trControl = train_control,
              tuneGrid = tuningGrid, ntree = 75)

model_75

model_75$finalModel$ntree


# For number of trees = 125

model_125 = train(label~., data=training_data, method="rf", trControl = train_control,
              tuneGrid = tuningGrid, ntree = 125)

model_125

model_125$finalModel$ntree


# For number of trees = 175

model_175 = train(label~., data=training_data, method="rf", trControl = train_control,
              tuneGrid = tuningGrid, ntree = 175)

model_175

model_175$finalModel$ntree

# For number of trees = 225

model_225 = train(label~., data=training_data, method="rf", trControl = train_control,
              tuneGrid = tuningGrid, ntree = 225)

model_225

model_225$finalModel$ntree

# For number of trees = 275

model_275 = train(label~., data=training_data, method="rf", trControl = train_control,
              tuneGrid = tuningGrid, ntree = 275)

model_275

model_275$finalModel$ntree


# For number of trees = 325

model_325 = train(label~., data=training_data, method="rf", trControl = train_control,
              tuneGrid = tuningGrid, ntree =325)

model_325

model_325$finalModel$ntree


# For number of trees = 375

model_375 = train(label~., data=training_data, method="rf", trControl = train_control,
              tuneGrid = tuningGrid, ntree = 375)

model_375
model_375$finalModel$ntree

# Highest accuracy of the model


model_highest_accuracy = 00.8942308


# Plotting the model

plot.train(model_325)



# 3.2



# Setting the cross-validation to 5-fold

train_control_refitted = trainControl(method="cv", number = 5, search = "grid", savePredictions = T)



tuningGrid_refitted <- expand.grid(mtry=c(2,4,6,8))


# All the best cross-validated accuracies of the models


all_accuracies <- c()

all_accuracies

# Refitting the model 15 times

for (i in seq(1,15,1)) {

model_325 = train(label~., data=training_data, method="rf", trControl = train_control,
                  tuneGrid = tuningGrid, ntree = 325)

all_accuracies <- append(all_accuracies, max(model_325$results$Accuracy))


}

print(all_accuracies)


# Mean of all accuracies

mean_of_accuracies <- mean(all_accuracies)
mean_of_accuracies

# Standard deviation of all accuracies

standard_deviation_of_accuracies <- sd(all_accuracies)
standard_deviation_of_accuracies


# T-test

t.test(all_accuracies)








