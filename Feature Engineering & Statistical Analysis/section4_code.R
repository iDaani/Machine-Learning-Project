
# Adding stats to the library

library(stats)

# Adding MASS library 

library(MASS)

# Adding datasets library

library(datasets)

library(matrixStats)

# Adding library partitions for P function

library(partitions)

# Adding ggplot 2 library for graphing

library(ggplot2)

# Adding cowplot library for nice looking default in ggplot library

library(cowplot)


# Set working directory

setwd("./")

# Reading from the CSV file

data <- read.csv("./40280938_features.csv",
                 header = TRUE)
data

# --------
# 4.1
# --------

# Using AIC approach for feature selection first


# Now creating model of different combinations
model <- lm(aspect_ratio ~ nr_pix + rows_with_1 + cols_with_1 + rows_with_3p +
              cols_with_3p + neigh_1 + no_neigh_above + no_neigh_below + no_neigh_left +
              no_neigh_right + no_neigh_horiz + no_neigh_vert + custom, data = data)

summary(model)


# Running the model

stepAIC(model, direction = "backward")

# Regression model

result <- lm(aspect_ratio ~ nr_pix + neigh_1 + no_neigh_below + no_neigh_right + custom, data = data)

result

summary(result)



# --------
# 4.2
# --------

# Creating a separate dataframe for the logistic regression model

data_for_regression <- data

data_for_regression

# Categorical features are of different type, need to convert them to 'factor' type as done below

data_for_regression$cols_with_3p <- as.factor(data_for_regression$cols_with_3p)
data_for_regression$neigh_1 <- as.factor(data_for_regression$neigh_1)
data_for_regression$no_neigh_vert <- as.factor(data_for_regression$no_neigh_vert)
data_for_regression$aspect_ratio <- as.factor(data_for_regression$aspect_ratio)


logistic <- glm(aspect_ratio ~ cols_with_3p, data = data_for_regression, family="binomial")

summary(logistic)


# To draw graph, we start by creating a new data frame that contains the
# probabilities of predicted aspect ration with actual
# aspect _ratio

predicted_data <- data.frame(likely_aspect_ratio = logistic$fitted.values,
                             aspect_ratio = data_for_regression$aspect_ratio)

# then we sort data frame from low probabilities to high probabilities

predicted_data <- predicted_data[order(predicted_data$likely_aspect_ratio, decreasing =FALSE),]

# Then we add a new column to the data frame that has the rank of each sample, from low probability
# to high probability

predicted_data$rank <- 1:nrow(predicted_data)

ggplot(data=predicted_data, aes(x=rank, y=likely_aspect_ratio)) +
  geom_point(aes(color=aspect_ratio), alpha = 1, shape = 4, stroke = 2) +
  xlab("cols_with_3p") +
  ylab("Predicted values of aspect ratio")

ggplot(predicted_data, aes(x=rank, fill=as.factor(likely_aspect_ratio))) +
  geom_histogram(binwidth=.2, alpha=.5, position='identity')


# --------
# 4.3
# --------


# Calculating medians


median_of_nr_pixels <- colMedians(data.matrix(data[, 3]))
median_of_nr_pixels

median_of_aspect_ratio <- colMedians(data.matrix(data[, 8]))
median_of_aspect_ratio

median_of_neigh_1 <- colMedians(data.matrix(data[, 9]))

# Categorical variable for each

data$split1 <- as.factor(ifelse(data$nr_pix > median_of_nr_pixels, 1, 0))

data$split2 <- as.factor(ifelse(data$aspect_ratio > median_of_aspect_ratio, 1, 0))

data$split3 <- as.factor(ifelse(data$neigh_1 > median_of_neigh_1, 1, 0))

# Printing dataframe to check the categorical value

data

# Counting number of 1s in individual split variables

count_of_split_1 = 0

for (i in data$split1) {
  if (i == 1) {
    count_of_split_1 = count_of_split_1 + 1
  }
}


count_of_split_2 = 0

for (i in data$split2) {
  if (i == 1) {
    count_of_split_2 = count_of_split_2 + 1
  }
}


count_of_split_3 = 0

for (i in data$split3) {
  if (i == 1) {
    count_of_split_3 = count_of_split_3 + 1
  }
}


# Calculating proportions of letter, face, xclaim in each of the categorical variable

# for letters

letters_count_of_split_1 = 0 

for (i in data[1:80, 19]) {
  if (i == 1) {
    letters_count_of_split_1 = letters_count_of_split_1 + 1
  }
}

letters_count_of_split_1

letters_count_of_split_2 = 0 

for (i in data[1:80, 20]) {
  if (i == 1) {
    letters_count_of_split_2 = letters_count_of_split_2 + 1
  }
}

letters_count_of_split_2

letters_count_of_split_3 = 0 

for (i in data[1:80, 21]) {
  if (i == 1) {
    letters_count_of_split_3 = letters_count_of_split_3 + 1
  }
}

letters_count_of_split_3


letters_split_1_proportion = round(letters_count_of_split_1 / count_of_split_1, digits = 2)
letters_split_1_proportion

letters_split_2_proportion = round(letters_count_of_split_2 / count_of_split_2, digits = 2)
letters_split_2_proportion

letters_split_3_proportion = round(letters_count_of_split_3 / count_of_split_3, digits = 2)
letters_split_3_proportion


# For face


face_count_of_split_1 = 0 

for (i in data[81:120, 19]) {
  if (i == 1) {
    face_count_of_split_1 = face_count_of_split_1 + 1
  }
}

face_count_of_split_1

face_count_of_split_2 = 0 

for (i in data[81:120, 20]) {
  if (i == 1) {
    face_count_of_split_2 = face_count_of_split_2 + 1
  }
}

face_count_of_split_2

face_count_of_split_3 = 0 

for (i in data[81:120, 21]) {
  if (i == 1) {
    face_count_of_split_3 = face_count_of_split_3 + 1
  }
}

face_count_of_split_3


face_split_1_proportion = round(face_count_of_split_1 / count_of_split_1, digits = 2)
face_split_1_proportion

face_split_2_proportion = round(face_count_of_split_2 / count_of_split_2, digits = 2)
face_split_2_proportion

face_split_3_proportion = round(face_count_of_split_3 / count_of_split_3, digits = 2)
face_split_3_proportion



# For exclamatory 


xclaim_count_of_split_1 = 0 

for (i in data[121:140, 19]) {
  if (i == 1) {
    xclaim_count_of_split_1 = xclaim_count_of_split_1 + 1
  }
}

xclaim_count_of_split_1



xclaim_count_of_split_2 = 0 

for (i in data[81:120, 20]) {
  if (i == 1) {
    xclaim_count_of_split_2 = xclaim_count_of_split_2 + 1
  }
}

xclaim_count_of_split_2

xclaim_count_of_split_3 = 0 

for (i in data[81:120, 21]) {
  if (i == 1) {
    xclaim_count_of_split_3 = xclaim_count_of_split_3 + 1
  }
}

xclaim_count_of_split_3


xclaim_split_1_proportion = round(xclaim_count_of_split_1 / count_of_split_1, digits = 2)
xclaim_split_1_proportion

xclaim_split_2_proportion = round(xclaim_count_of_split_2 / count_of_split_2, digits = 2)
xclaim_split_2_proportion

xclaim_split_3_proportion = round(xclaim_count_of_split_3 / count_of_split_3, digits = 2)
xclaim_split_3_proportion


#  Making a table of all the proportions

result <- matrix(c(letters_split_1_proportion, letters_split_2_proportion, letters_split_3_proportion,
                  face_split_1_proportion, face_split_2_proportion, face_split_3_proportion,
                  xclaim_split_1_proportion, xclaim_split_2_proportion, xclaim_split_3_proportion),
                ncol = 3, nrow = 3)

colnames(result) <- c("Split1", "Split2", "Split3")
rownames(result) <- c("Letters", "Faces", "Exclamation Marks")

result

final_result <- as.table(result)
final_result



# --------
# 4.4
# --------


# Not done
