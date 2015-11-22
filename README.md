# datasciencecoursera

#####
How the script works:

        1. Read test and training data, labels and subject and read features
        2. Add columns with subject id and activity id to test and training data set
        3. Write the column names from features
        4. Merges the training and the test data sets
        5. Delete invalid characters in column names(like "(", ")", "-")
        6. Sort data set by subject id and labels id
        7. Keep only the measurements on the mean and standard deviation for each measurement
        8. Create data with the average of each variable for each activity and each subject.



DATA DICTIONARY


subject

    Each row identifies the subject who performed the activity for each window sample. Its range is from 1 to 30. 
  
labels

    Type of activity:
    
      1 WALKING
      
      2 WALKING_UPSTAIRS
      
      3 WALKING_DOWNSTAIRS
      
      4 SITTING
      
      5 STANDING
      
      6 LAYING
  

tBodyAcc.XYZ

tGravityAcc.XYZ

tBodyAccJerk.XYZ

tBodyGyro.XYZ

tBodyGyroJerk.XYZ

tBodyAccMag

tGravityAccMag

tBodyAccJerkMag

tBodyGyroMag

tBodyGyroJerkMag

fBodyAcc.XYZ

fBodyAccJerk.XYZ

fBodyGyro.XYZ

fBodyAccMag

fBodyAccJerkMag

fBodyGyroMag

fBodyGyroJerkMag
 
 
        The features selected for this database come from the accelerometer and gyroscope 3-axial raw signals tAcc.XYZ and tGyro.XYZ. These time domain signals (prefix 't' to denote time) were captured at a constant rate of 50 Hz. Then they were filtered using a median filter and a 3rd order low pass Butterworth filter with a corner frequency of 20 Hz to remove noise. Similarly, the acceleration signal was then separated into body and gravity acceleration signals (tBodyAcc.XYZ and tGravityAcc.XYZ) using another low pass Butterworth filter with a corner frequency of 0.3 Hz. 
        
        Subsequently, the body linear acceleration and angular velocity were derived in time to obtain Jerk signals (tBodyAccJerk.XYZ and tBodyGyroJerk.XYZ). Also the magnitude of these three-dimensional signals were calculated using the Euclidean norm (tBodyAccMag, tGravityAccMag, tBodyAccJerkMag, tBodyGyroMag, tBodyGyroJerkMag). 
        
        Finally a Fast Fourier Transform (FFT) was applied to some of these signals producing fBodyAcc.XYZ, fBodyAccJerk.XYZ, fBodyGyro.XYZ, fBodyAccJerkMag, fBodyGyroMag, fBodyGyroJerkMag. (Note the 'f' to indicate frequency domain signals). 
        
        These signals were used to estimate variables of the feature vector for each pattern:  
        '.XYZ' is used to denote 3-axial signals in the X, Y and Z directions.
  
The set of variables that were estimated from these signals are:   

mean

        Mean value
    
std

        Standard deviation
