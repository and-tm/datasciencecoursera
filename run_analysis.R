get_tidy_data <- function(){
  ##read files
  x_test <- read.table("test/X_test.txt")
  y_test <- read.table("test/y_test.txt")
  x_train <- read.table("train/X_train.txt")
  y_train <- read.table("train/y_train.txt")
  test_sub <- read.table("test/subject_test.txt")
  train_sub <- read.table("train/subject_train.txt")
  features <- read.table("features.txt")
    
  ##add columns with subject id and activity id to tables
  x_test <- cbind(y_test,x_test)
  x_test <- cbind(test_sub,x_test)
  x_train <- cbind(y_train,x_train)
  x_train <- cbind(train_sub,x_train)
  
  ##write the column names
  ##features[,2] is factor so write it as character
  names(x_test) <- c("subject", "labels" , as.character(features[,2]))
  names(x_train) <- c("subject", "labels" , as.character(features[,2]))
  
  ##merges the training and the test sets
  x <- rbind(x_test, x_train)
  
  ##delete invalid characters in column names
  column_names <- make.names(names=names(x), unique=TRUE, allow_ = TRUE)
  names(x) <- column_names
  
  ##sort by subject id and labels id
  library(plyr)
  x<-arrange(x,subject,labels)
  
  ##keep only the measurements on the mean and standard deviation for each measurement
  library(dplyr)
  tidy_data <- select(x, subject, labels, contains("mean.."), contains("std.."))
  
  ##create data with the average of each variable for each activity and each subject.
  group_tidy_data <- tidy_data %>% group_by(subject, labels) %>% summarise_each(funs(mean))
  
  ##return
  group_tidy_data
  
}
