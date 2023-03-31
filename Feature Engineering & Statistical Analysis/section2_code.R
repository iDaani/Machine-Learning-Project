setwd("./")


# PLEASE RUN THIS CODE AFTER DELETING 40280938_features.csv FILE. Or else it will append on the existing CSV file.




# Downloading plyr package for count function for feature 1

library(plyr)
detach(package:plyr)



# We install zoo plugin for rowsum and colSums to work

library(zoo)


# Directory listing the CSV files
dir<-"./Images/CSVImages"

# Making a list of all the images

allfiles <- list.files(path = dir, pattern = NULL,
                       full.names = TRUE, recursive = TRUE, ignore.case = TRUE)

allfiles



# Looping through each individual image named as 'fileName'

for (fileName in allfiles) {
  
  # Reading from file
  
  img <- read.csv(fileName)
  names(img) <- NULL
  img
  
  # Changing the dataframe class from reading CSV to matrix
  
  class(img)
  imageMatrix = as.matrix(img)
  imageMatrix
  
  
  # Feature 0
  # Label feature
  
  # Using grep function to match symbol with file name
  # Difficulty that I had over here
  
  label <- "a"
  
  if (grepl("40280938_a", fileName, fixed = TRUE)) {
    label <- "a"
  }
  
  else if (grepl("40280938_b", fileName, fixed = TRUE)) {
    label <- "b"
  }
  
  else if (grepl("40280938_c", fileName, fixed = TRUE)) {
    label <- "c"
  }
  
  else if (grepl("40280938_d", fileName, fixed = TRUE)) {
    label <- "d"
  }
  
  else if (grepl("40280938_e", fileName, fixed = TRUE)) {
    label <- "e"
  }
  
  else if (grepl("40280938_f", fileName, fixed = TRUE)) {
    label <- "f"
  }
  
  else if (grepl("40280938_g", fileName, fixed = TRUE)) {
    label <- "g"
  }
  
  else if (grepl("40280938_h", fileName, fixed = TRUE)) {
    label <- "h"
  }
  
  else if (grepl("40280938_i", fileName, fixed = TRUE)) {
    label <- "i"
  }
  
  else if (grepl("40280938_j", fileName, fixed = TRUE)) {
    label <- "j"
  }
  
  else if (grepl("40280938_sad", fileName, fixed = TRUE)) {
    label <- "sad"
  }
  
  else if (grepl("40280938_smiley", fileName, fixed = TRUE)) {
    label <- "smiley"
  }
  
  else if (grepl("40280938_xclaim", fileName, fixed = TRUE)) {
    label <- "xclaim"
  }
  
  
  # Feature 0
  # Index feature 
  # Issue faced - when saving index as integer variable, 01 was saving as 1
  # to tackle this, saving the index value as string to then convert into integer or use directly
  
  
  
  index <- "0"
  
  if (grepl("_01", fileName, fixed = TRUE)) {
    index <- "01"
  }
  
  else if (grepl("_02", fileName, fixed = TRUE)) {
    index <- "02"
  }
  
  else if (grepl("_03", fileName, fixed = TRUE)) {
    index <- "03"
  }
  
  else if (grepl("_04", fileName, fixed = TRUE)) {
    index <- "04"
  }
  
  else if (grepl("_05", fileName, fixed = TRUE)) {
    index <- "05"
  }
  
  else if (grepl("_06", fileName, fixed = TRUE)) {
    index <- "06"
  }
  
  else if (grepl("_07", fileName, fixed = TRUE)) {
    index <- "07"
  }
  
  else if (grepl("_08", fileName, fixed = TRUE)) {
    index <- "08"
  }
  
  else if (grepl("_09", fileName, fixed = TRUE)) {
    index <- "09"
  }
  
  else if (grepl("_10", fileName, fixed = TRUE)) {
    index <- "10"
  }
  
  else if (grepl("_11", fileName, fixed = TRUE)) {
    index <- "11"
  }
  
  else if (grepl("_12", fileName, fixed = TRUE)) {
    index <- "12"
  }
  
  else if (grepl("_13", fileName, fixed = TRUE)) {
    index <- "13"
  }
  
  else if (grepl("_14", fileName, fixed = TRUE)) {
    index <- "14"
  }
  
  else if (grepl("_15", fileName, fixed = TRUE)) {
    index <- "15"
  }
  
  else if (grepl("_16", fileName, fixed = TRUE)) {
    index <- "16"
  }
  
  else if (grepl("_17", fileName, fixed = TRUE)) {
    index <- "17"
  }
  
  else if (grepl("_18", fileName, fixed = TRUE)) {
    index <- "18"
  }
  
  else if (grepl("_19", fileName, fixed = TRUE)) {
    index <- "19"
  }
  
  else if (grepl("_20", fileName, fixed = TRUE)) {
    index <- "20"
  }
  
  
  # Feature 1
  # nr_pix feature
  # Counting number of black pixels
  
  nr_pix <- sum(imageMatrix == 1)
  nr_pix
  
  # Feature 2
  # rows_with_1 feature
  # Counting number of rows with exactly black pixels
  
  # From StackOverFLow
  
  rows_with_1 <- sum(rowSums(imageMatrix == 1) == 1)
  rows_with_1
  
  # Feature 3
  # cols_with_1 feature
  
  cols_with_1 <- sum(colSums(imageMatrix == 1) == 1)
  cols_with_1
  
  # Feature 4 
  # rows_with_3p
  
  rows_with_3p <- sum(rowSums(imageMatrix == 1) >= 3)
  rows_with_3p
  
  # Feature 5
  # cols_with_3p
  
  cols_with_3p <- sum(colSums(imageMatrix == 1) >= 3)
  cols_with_3p
  
  
  # Feature 6
  # aspect ratio
  
  # Center pixel row
  center_pixel_row <- round((nrow(imageMatrix) / 2))
  center_pixel_row
  
  # Center pixel column
  center_pixel_col <- round((ncol(imageMatrix) / 2))
  center_pixel_col
  
  # Checking the value of the center pixel
  imageMatrix[center_pixel_row, center_pixel_col]
  
  
  # Right most black pixel column from the center pixel 
  right_most_pixel = max(which(imageMatrix == 1, arr.ind=TRUE)[8,])
  
  # Left most black pixel column from the center pixel 
  left_most_pixel = min(which(imageMatrix == 1, arr.ind=TRUE)[8,])
  
  # Horizontal difference between the pixels (width)
  
  width = right_most_pixel - left_most_pixel
  
  aspect_ratio = width
  aspect_ratio
  
  
  
  # Feature 7
  # neigh_1 - Using counter to count the number of black pixels
  
  counter = 0
  
  # Here a new matrx was created out of the original one with paddings added to it to make it
  
  imageNewMatrix <- imageMatrix
  
  nrow(imageMatrix)
  ncol(imageMatrix)
  
  imageNewMatrixa <- cbind(imageNewMatrix, 0)
  imageNewMatrixB <- rbind(imageNewMatrixa, 0)
  imageNewMatrixC <- cbind(imageNewMatrixB, 0)
  imageNewMatrixD <- rbind(imageNewMatrixC, 0)
  nrow(imageNewMatrixD)
  ncol(imageNewMatrixD)
  
  for (row in 2:19) {
    for (col in 2:19) {
      if (imageNewMatrixD[row,col] == 1) {
        
        # Get entry of the left pixel
        
        pixel_to_left = as.numeric(imageNewMatrixD[row, col-1])
        
        # Get entry of the right pixel
        
        pixel_to_right = as.numeric(imageNewMatrixD[row, col+1])
        
        # Get entry of the pixel at top
        
        pixel_at_top = as.numeric(imageNewMatrixD[row-1, col])
        
        # Get entry of the pixel at top-left
        
        pixel_at_top_left = as.numeric(imageNewMatrixD[row-1, col-1])
        
        # Get entry of the pixel at top right
        
        pixel_at_top_right = as.numeric(imageNewMatrixD[row-1, col+1])
        
        # Get entry of the pixel at bottom
        
        pixel_at_bottom = as.numeric(imageNewMatrixD[row+1, col])
        
        # Get entry of the pixel at bottom left
        
        pixel_at_bottom_left = as.numeric(imageNewMatrixD[row+1, col-1])
        
        # Get entry of the pixel at bottom right
        
        pixel_at_bottom_right = as.numeric(imageNewMatrixD[row+1, col+1])
        
        
        pixel_sum = pixel_to_left + pixel_to_right + pixel_at_top +
          pixel_at_top_left + pixel_at_top_right + pixel_at_bottom +
          pixel_at_bottom_left + pixel_at_bottom_right
        
        # Using isTrue as one of the pixels is empty and isTrue converts logical(0) to FALSE
        
        if (isTRUE(pixel_sum == 1)) {
          counter = counter + 1
        } 
        
        
        
      }
    }
    
  }
  
  neigh_1 = counter
  
  
  # Feature 8
  # no_neigh_above 
  
  counter_1 = 0
  
  for (row in 2:19) {
    for (col in 2:19) {
      if (imageNewMatrixD[row,col] == 1) {
        
        # Get entry of the pixel at top
        
        pixel_at_top = as.numeric(imageNewMatrixD[row-1, col])
        
        # Get entry of the pixel at top-left
        
        pixel_at_top_left = as.numeric(imageNewMatrixD[row-1, col-1])
        
        # Get entry of the pixel at top right
        
        pixel_at_top_right = as.numeric(imageNewMatrixD[row-1, col+1])
        
        
        pixel_sum = pixel_at_top + pixel_at_top_left + pixel_at_top_right
        
        if (isTRUE(pixel_sum == 0)) {
          counter_1 = counter_1 + 1
        } 
        
        
        
        
      }
    }
  }
  
  no_neigh_above = counter_1
  
  # Feature 9
  # no_neigh_below - WIP
  
  counter_2 = 0
  
  for (row in 2:19) {
    for (col in 2:19) {
      if (imageNewMatrixD[row,col] == 1) {
        
        # Get entry of the pixel at bottom
        
        pixel_at_bottom = as.numeric(imageNewMatrixD[row+1, col])
        
        # Get entry of the pixel at bottom left
        
        pixel_at_bottom_left = as.numeric(imageNewMatrixD[row+1, col-1])
        
        # Get entry of the pixel at bottom right
        
        pixel_at_bottom_right = as.numeric(imageNewMatrixD[row+1, col+1])
        
        
        pixel_sum = pixel_at_bottom + pixel_at_bottom_left + pixel_at_bottom_right
        
        if (isTRUE(pixel_sum == 0)) {
          counter_2 = counter_2 + 1
        } 
        
        
        
        
      }
    }
  }
  
  no_neigh_below = counter_2
  
  
  # Feature 10
  # no_neigh_left 
  
  counter_3 = 0
  
  
  for (row in 2:19) {
    for (col in 2:19) {
      if (imageNewMatrixD[row,col] == 1) {
        
        # Get entry of the pixel at left
        
        pixel_to_left = as.numeric(imageNewMatrixD[row, col-1])
        
        # Get entry of the pixel at bottom left
        
        pixel_at_bottom_left = as.numeric(imageNewMatrixD[row+1, col-1])
        
        # Get entry of the pixel at top left 
        
        pixel_at_top_left = as.numeric(imageNewMatrixD[row-1, col-1])
        
        
        
        pixel_sum = pixel_to_left + pixel_at_bottom_left + pixel_at_bottom_right
        
        if (isTRUE(pixel_sum == 0)) {
          counter_3 = counter_3 + 1
        } 
        
        
        
        
      }
    }
  }
  
  no_neigh_left = counter_3
  
  
  # Feature 11
  # no_neigh_right 
  
  
  counter_4 = 0
  
  
  for (row in 2:19) {
    for (col in 2:19) {
      if (imageNewMatrixD[row,col] == 1) {
        
        # Get entry of the pixel at right
        
        pixel_to_right = as.numeric(imageNewMatrixD[row, col+1])
        
        # Get entry of the pixel at bottom right
        
        pixel_at_bottom_right = as.numeric(imageNewMatrixD[row+1, col+1])
        
        # Get entry of the pixel at top right
        
        pixel_at_top_right = as.numeric(imageNewMatrixD[row-1, col+1])
        
        
        
        pixel_sum = pixel_to_right + pixel_at_bottom_right + pixel_at_top_right
        
        if (isTRUE(pixel_sum == 0)) {
          counter_4 = counter_4 + 1
        } 
        
        
        
        
      }
    }
  }
  
  no_neigh_right = counter_4
  
  
  # Feature 12
  # no_neigh_horiz 
  
  
  counter_5 = 0
  
  
  for (row in 2:19) {
    for (col in 2:19) {
      if (imageNewMatrixD[row,col] == 1) {
        
        # Get entry of the pixel at left
        
        pixel_to_left = as.numeric(imageNewMatrixD[row, col-1])
        
        # Get entry of the pixel at right
        
        pixel_to_right = as.numeric(imageNewMatrixD[row, col+1])
        
        
        
        pixel_sum = pixel_to_left + pixel_to_right
        
        if (isTRUE(pixel_sum == 0)) {
          counter_5 = counter_5 + 1
        } 
        
        
        
        
      }
    }
  }
  
  no_neigh_horiz = counter_5
  
  
  # Feature 13
  # no_neigh_vert - WIP
  
  
  counter_6 = 0
  
  
  for (row in 2:19) {
    for (col in 2:19) {
      if (imageNewMatrixD[row,col] == 1) {
        
        # Get entry of the pixel at top
        
        pixel_at_top = as.numeric(imageNewMatrixD[row-1, col])
        
        # Get entry of the pixel at bottom 
        
        pixel_at_bottom_ = as.numeric(imageNewMatrixD[row+1, col])
        
        
        
        
        pixel_sum = pixel_at_top + pixel_at_bottom
        
        if (isTRUE(pixel_sum == 0)) {
          counter_6 = counter_6 + 1
        } 
        
        
        
        
      }
    }
  }
  
  no_neigh_vert = counter_6
  
  
  # Feature 14
  # connected_areas - WIP
  
  connected_areas <- sample(1:100,1)
  connected_areas
  
  # Feature 15
  # eyes - WIP
  
  eyes <- sample(1:100,1)
  eyes
  
  # Feature 16
  # Custom feature
  # rows_with_no_black_pixel
  # Calculating number of rows with no black pixels
  
  custom <- sum(rowSums(imageMatrix == 1) == 0)
  custom
  
  
  
  
  # Here I added all the features to a vector
  # Then I created a CSV file using write.table and added the feature to the CSV file
  # without their column names (feature names)
  # Since I couldn't append to the CSV file without appending the column names each time as well
  # I had to think of something else
  
  # So I decided to just read from the CSV file, convert it to a dataframe with the actual column (feature) names
  # and then write the dataframe to the same CSV file, this time overwriting it and allowing column names
  
  # this ensured the column names would remain at the top and as new features were appended, their column names wont be appended as well
  
  feature <- c(label,index, nr_pix, rows_with_1, cols_with_1,
               rows_with_3p, cols_with_3p, aspect_ratio,
               neigh_1, no_neigh_above, no_neigh_below,
               no_neigh_left, no_neigh_right, no_neigh_horiz,
               no_neigh_vert, connected_areas, eyes, custom)
  
  feature
  
  
  
  write.table(matrix(feature, nrow=1), file = "./40280938_features.csv",
              append = TRUE, sep = ',',
              row.names = FALSE,
              col.names = FALSE)
  
  
}


# Reading from the CSV file with all the data present and then overwriting it to add the column names to it

final_data <- read.csv("./40280938_features.csv",
                       header = FALSE,
                       col.names = c('label','index', 'nr_pix', 'rows_with_1', 'cols_with_1',
                                     'rows_with_3p', 'cols_with_3p', 'aspect_ratio',
                                     'neigh_1', 'no_neigh_above', 'no_neigh_below',
                                     'no_neigh_left', 'no_neigh_right', 'no_neigh_horiz',
                                     'no_neigh_vert', 'connected_areas', 'eyes', 'custom'))

# Sorting data alphabetically and then by index

sorted_data_by_label <- final_data[order(final_data$label),]


write.table(sorted_data_by_label, file = "./40280938_features.csv",
            append = FALSE, sep = ',',
            row.names = FALSE)

