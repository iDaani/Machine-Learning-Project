setwd(setwd("C:/Users/Asus/Desktop/AI Assignment"))
set.seed(42)


# Installed class library for knn model

library(class) # for `knn`

features <-  read.csv(file = './40280938_features.csv', header = TRUE)


# Choosing only a, j and all non-letter 

a_letters <- features[1:8,]
j_letters <- features[73:80,]
all_non_letters <- features[81:140,]


# Adding all the data required for 2.1 in a separate dataframe

new_features <- rbind(a_letters, j_letters, all_non_letters)


# Making label as categorical factor so everything is classified


levels = c("a","j", "sad", "smiley", "xclaim")

labels = c("letter","letter","sad", "smiley", "xclaim" )


new_features$label <- factor(new_features$label,
                        levels = levels, labels = labels)


str(new_features)


nrow(new_features)


set.seed(42)

# Printing out summary and sample head

summary(new_features)
head(new_features, 10)



# shuffling data
# ~~~~~~~~~~~~~~~~


new_features <- new_features[sample(nrow(new_features)),]

new_features

head(new_features, 10)


# Splicing data
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# We use 1.0 as we have to use the entire training data and no separate test data

new_features_fold <- sample(1:nrow(new_features),size=nrow(new_features)*1,replace = FALSE) #random selection of 100% data.


# Classification
# ~~~~~~~~~~~~~~

# features for classification:
fs = c('nr_pix', 'aspect_ratio', 'rows_with_1', 'cols_with_1')




# 2.1

# Accuracy of all knn classification models

accs_2_1=c()

# Seperate k values to go through

knn_values <- c(1, 3, 5, 7, 9, 11, 13)

# Performing knn classification and printing it

for (i in knn_values) {
  
  train_items_this_fold  = new_features[new_features_fold,] 
  validation_items_this_fold = new_features[new_features_fold,]
  
  # fit knn model on this fold
  
  predictions = knn(train_items_this_fold[,fs], 
                    validation_items_this_fold[,fs], 
                    train_items_this_fold$label, k=i)
  predictions
  
  correct_list = predictions == validation_items_this_fold$label
  nr_correct = nrow(validation_items_this_fold[correct_list,])
  
  acc_rate = nr_correct/nrow(validation_items_this_fold)
  accs_2_1[i] = acc_rate
  print(acc_rate)
}

accs_2_1 <- accs_2_1[!is.na(accs_2_1)]
accs_2_1

# 2.2

# Seperate k values to go through

knn_values <- c(1, 3, 5, 7, 9, 11, 13)

# Accuracy of all cross validated knn models

accs_2_2=c()


library(caret)

for (i in knn_values) {
  
  # define training control
  train_control <- trainControl(method="cv", number=5)
  
  tune_grid <- expand.grid(k = i)
  
  # train the model
  model <- train(label~nr_pix+aspect_ratio+cols_with_1+rows_with_1, data=new_features, 
                 trControl=train_control, tuneGrid=tune_grid, method="knn")
  # summarize results
  print(model)
  
  # Store accuracy rate
  
  accs_2_2[i] = model$results$Accuracy
  
}


accs_2_2 <- accs_2_2[!is.na(accs_2_2)]

accs_2_2


# 2.3


# k value = 1 has the best k value with highest accuracy


# define training control
train_control_3 <- trainControl(method="cv", number=5)

tune_grid_3 <- expand.grid(k = 1)

# train the model
model_3 <- train(label~nr_pix+aspect_ratio+cols_with_1+rows_with_1, data=new_features, 
               trControl=train_control_3, tuneGrid=tune_grid_3, method="knn")
# summarize results
print(model_3)



confusionMatrix(model_3)





# 2.4


# Installing ggplot2 library

library(ggplot2)


# Making a dataframe of all the accuracies and the k-values

plotting_data_frame <- data.frame(accs_2_1,accs_2_2, knn_values)

plotting_data_frame

# Plotting the results

plot_2 <- ggplot()+
  geom_line(data=plotting_data_frame,aes(y=accs_2_1,x= 1/knn_values,colour="Training Accuracy Rate"),size=1, linetype="dashed" )+
  geom_line(data=plotting_data_frame,aes(y=accs_2_2,x= 1/knn_values,colour="Cross Validated Accuracy Rate"),size=1, linetype="dashed") +
  scale_color_manual(name = "Legend", values = c("Training Accuracy Rate" = "darkorange", "Cross Validated Accuracy Rate"= "darkgreen")) + labs(y= "Accuracy Rate", x = "1/K values")

plot_2









