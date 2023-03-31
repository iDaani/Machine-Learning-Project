setwd("./")

# Directory listing the images
dir<-"./Images"

# Making a list of all the images

files <- list.files(path = dir, pattern = NULL,
                    full.names = TRUE, recursive = TRUE, ignore.case = TRUE)

# Printing the list of all images name

files

# Looping through each individual image named as 'img'

for (img in files) {
  
  
  # Reading through individual image
  
  indImage <- read.table(img,
                         header = FALSE,
                         skip = 4,
                         sep = ",",
                         stringsAsFactors = FALSE,
                         
                         
  )
  
  # changing pixel values
  
  indImage[indImage < 128] <- 1
  indImage[indImage >= 128] <- 0
  
  # Removing column name
  
  colnames(indImage) <- NULL
  
  # printing out just to check
  
  indImage
  
  # Turning to matrix
  
  imageMatrix <- as.matrix(indImage, nrow=18, ncol = 18)
  
  # Checking if its matrix
  
  is.matrix(imageMatrix)
  
  # setting column names to null
  
  colnames(imageMatrix) <- NULL
  
  # Printing out just to check
  
  imageMatrix
  
  # making another matrix and setting column names to null
  
  finalMatrix <- matrix(imageMatrix, nrow = 18, ncol = 18, byrow = TRUE)
  
  colnames(finalMatrix) <- NULL
  
  
  # Below, we store the CSV file in the same directory as image for easier
  # visibility. 
  # For that, we remove the '.txt' from the image name and concatenate
  # the string with '.csv' to store it as a CSV file.
  
  newfilepath <- substr(img,1,nchar(img)-4)

  csvtext <- ".csv"
  finalPath <- paste(newfilepath, csvtext, sep="")
  
  write.table(finalMatrix,
              file = finalPath,
              sep = ",",
              col.names = FALSE,
              row.names = FALSE,
              )
  
  
}
