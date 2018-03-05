# Introduction

The purpose of this project is to demonstrate the ability to collect, transform and clean a data set. The goal is to prepare tidy data that can be used for later analysis. 

One of the most exciting areas in all of data science right now is wearable computing . Companies like Fitbit, Nike, Microsoft and Jawbone Up are racing to develop the most advanced algorithms to attract new users. Like these other companies, the Samsung Galaxy S smartphone can also collect data.  See the data link below.

The site where the data was obtained:

http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones

The data for the project:

https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip

This document describes:

* **The Data** from the Samsung Galaxy S smartphone
* walks through **The Code** while describing the variables
* explains the **Data Transformations** to tidy the data

> When you review the R Code you will see the code is heavily commented for your convenience.

#The Data
##Where Did It Come From?

The experiments were carried out with a group of 30 volunteers within an age bracket of 19-48 years. Each person performed six activities (WALKING, WALKING UPSTAIRS, WALKING DOWNSTAIRS, SITTING, STANDING, LAYING) wearing a smartphone (Samsung Galaxy S II) on the waist. Using its embedded accelerometer and gyroscope, 3-axial linear acceleration and 3-axial angular velocity at a constant rate of 50Hz data was captured. The experiments were video-recorded to label the data manually. The obtained dataset has been randomly partitioned into two sets, where 70% of the volunteers was selected for generating the training data and 30% the test data.

## The Data Set

The dataset includes the following files that were used in the project:

- features.txt: List of all features.
     + Triaxial acceleration from the accelerometer (total acceleration) and the estimated body acceleration. 
     + Triaxial Angular velocity from the gyroscope. 
     + A 561-feature vector with time and frequency domain variables. 

````
> head(features)
  V1                V2
1  1 tBodyAcc-mean()-X
2  2 tBodyAcc-mean()-Y
3  3 tBodyAcc-mean()-Z
4  4  tBodyAcc-std()-X
5  5  tBodyAcc-std()-Y
6  6  tBodyAcc-std()-Z
> 

````

- activity_labels.txt: Links the class labels with their activity name

````
> head(activitylabels)
  V1                 V2
1  1            WALKING
2  2   WALKING_UPSTAIRS
3  3 WALKING_DOWNSTAIRS
4  4            SITTING
5  5           STANDING
6  6             LAYING
> 
````

- train/X_train.txt: Training set

````
> head(trainxdata)
         V1          V2         V3         V4         V5         V6
1 0.2885845 -0.02029417 -0.1329051 -0.9952786 -0.9831106 -0.9135264
2 0.2784188 -0.01641057 -0.1235202 -0.9982453 -0.9753002 -0.9603220
3 0.2796531 -0.01946716 -0.1134617 -0.9953796 -0.9671870 -0.9789440
4 0.2791739 -0.02620065 -0.1232826 -0.9960915 -0.9834027 -0.9906751
5 0.2766288 -0.01656965 -0.1153619 -0.9981386 -0.9808173 -0.9904816
6 0.2771988 -0.01009785 -0.1051373 -0.9973350 -0.9904868 -0.9954200
````

- train/y_train.txt: Training labels

````
> head(trainydata)
  V1
1  5
2  5
3  5
4  5
5  5
6  5
> 
````

- test/X_test.txt: Test set
     + Data same format as Training Set above

- test/y_test.txt: Test labels
     + Data same format as Training Labels above

- subject_test.txt: Each row identifies the subject who performed the activity for each window sample. Its range is from 1 to 30.

````
> head(trainsubject) # The value of 1 represents the 1st of 30 participants in the study.
  V1
1  1
2  1
3  1
4  1
5  1
6  1
> 
````

- subject_train.txt: Each row identifies the subject who performed the activity for each window sample. Its range is from 1 to 30.
     + Data same format as Subject_test.txt above

# The Code

## Directory Structure

The solution builds a directory or folder structure off your working directory:

Your Working Directory/data/Class3Project

If these directories do not exist, they will be created. THe data files will be copied and unzipped by the code.

## Read the Data Files

Each of the needed data files will then be read into the variables noted below:

Data Table Name     | Varible Name in Code
------------------- | --------------------
features.txt        | features
activity_labels.txt | activtylabels
subject_train.txt   | trainsubject
x_train.txt         | trainxdata
y_train.txt         | trainydata
subject_test.txt    | testsubject
x_test.txt          | testxdata
y_test.txt          | testydata

## Class Project Objective 1:  Merge Test and Train Files
The first requirement for the project was to merge Test and Train files
````
xdatacombined <- rbind(trainxdata, testxdata)
ydatacombined <- rbind(trainydata, testydata)
subjectdatacombined <- rbind(trainsubject, testsubject)
````
> I decided to solve part of the assigment early

I thought it would now be a good time to replace the factors in ydatacombined with the named activities.  Recall that the y_train.txt and y_test.txt data looked like this:

````
> head(trainydata)
  V1
1  5
2  5
3  5
4  5
5  5
6  5
> 
````

Let's make this more understnadable.  We will replace the numbers with the actual activities they numbers stabd for using the activitylablels content:

````
> head(activitylabels)
  V1                 V2
1  1            WALKING
2  2   WALKING_UPSTAIRS
3  3 WALKING_DOWNSTAIRS
4  4            SITTING
5  5           STANDING
6  6             LAYING
> 
````

Before completing the dataset, let's change the ydatacombined to hold not numbers (factors) but descriptive text:
````
activitylabels[, 2] = gsub("_", "", tolower(as.character(activitylabels[, 2])))
ydatacombined[,1] = activitylabels[ydatacombined[,1],2]
names(ydatacombined) <- "activity"
````

Now when we merge xdatacombined, ydatacombined and subjectdatacombined, we end up with a data set that starts to make sense.  Rather than values of 1 6 six, the numbers are placed with their actual name like walking, sitting, standing.

completedataset <- cbind(subjectdatacombined, ydatacombined, xdatacombined) 
(Order is important so subject and activity are first two columns)

completedataset looks like this (note the values in the 2nd column):

````
> head(completedataset)
  V1 activity        V1          V2         V3         V4         V5         V6
1  1 standing 0.2885845 -0.02029417 -0.1329051 -0.9952786 -0.9831106 -0.9135264
2  1 standing 0.2784188 -0.01641057 -0.1235202 -0.9982453 -0.9753002 -0.9603220
3  1 standing 0.2796531 -0.01946716 -0.1134617 -0.9953796 -0.9671870 -0.9789440
4  1 standing 0.2791739 -0.02620065 -0.1232826 -0.9960915 -0.9834027 -0.9906751
5  1 standing 0.2766288 -0.01656965 -0.1153619 -0.9981386 -0.9808173 -0.9904816
6  1 standing 0.2771988 -0.01009785 -0.1051373 -0.9973350 -0.9904868 -0.9954200
````

## Class Project Objective 2: Extract the mean and st.dev from the completed data set
In the complete data set, there are many columns in addition to mean and st dev like energy, iqr, correlation, entropy
We will remove all columns other than those containing mean() or std()

````
onlymean <- grep("mean()", colnames(completedataset)) # Collect all the colum data with "mean()" in the name
onlystdev <- grep("std()", colnames(completedataset)) # Collect all the colum data with "std()" in the name

meanstddevcolumnindex <- c(onlymean, onlystdev)  # Build an index of the columns collected
meanstddevcolumnindexsorted <- sort(meanstddevcolumnindex) # Sort the columns since we collected mean() and std() independently
FinalDataSet <- completedataset[, c(1,2,meanstddevcolumnindexsorted)] # Complete the assignment in a variable FinalDataSet
````

````
> head(FinalDataSet)
  subject activity tBodyAcc-mean()-X tBodyAcc-mean()-Y tBodyAcc-mean()-Z tBodyAcc-std()-X
1       1 standing         0.2885845       -0.02029417        -0.1329051       -0.9952786
2       1 standing         0.2784188       -0.01641057        -0.1235202       -0.9982453
3       1 standing         0.2796531       -0.01946716        -0.1134617       -0.9953796
4       1 standing         0.2791739       -0.02620065        -0.1232826       -0.9960915
5       1 standing         0.2766288       -0.01656965        -0.1153619       -0.9981386
6       1 standing         0.2771988       -0.01009785        -0.1051373       -0.9973350
````
## Class Project Objective 3:  Use descriptive names to name the activities in the dataset
We accomplished this earlier.  See above for detail

## Class Project Objective 4: Appropriately labels the data set with descriptive variable names
With the data you have seen so far, have you gotten cofused by labels like: tBodyAcc-mean()-X, tBodyGyroJerk-std()-X or fBodyBodyAccJerkMag-meanFreq()?  I certainly did.  So let's make these descriptions a bit easier to understand:

````
names(FinalDataSet)<-gsub("^t", "Time", names(FinalDataSet)) 
names(FinalDataSet)<-gsub("^f", "Frequency", names(FinalDataSet))
names(FinalDataSet)<-gsub("Acc", "Accelerometer", names(FinalDataSet))
names(FinalDataSet)<-gsub("Gyro", "Gyroscope", names(FinalDataSet))
names(FinalDataSet)<-gsub("Mag", "Magnitude", names(FinalDataSet))
names(FinalDataSet)<-gsub("([Bb]ody[Bb]ody|[Bb]ody)", "Body", names(FinalDataSet))
names(FinalDataSet)<-gsub("-std", "StdDev", names(FinalDataSet))
names(FinalDataSet)<-gsub("-mean", "Mean", names(FinalDataSet))
names(FinalDataSet)<-gsub("([Gg]ravity)", "Gravity", names(FinalDataSet))
names(FinalDataSet)<-gsub("([Bb]odyaccjerkmag)", "BodyAccJerkMagnitude", names(FinalDataSet))
````

````
> head(FinalDataSet)
  subject activity TimeBodyAccelerometerMean()-X TimeBodyAccelerometerMean()-Y
1       1 standing                     0.2885845                   -0.02029417
2       1 standing                     0.2784188                   -0.01641057
3       1 standing                     0.2796531                   -0.01946716
4       1 standing                     0.2791739                   -0.02620065
5       1 standing                     0.2766288                   -0.01656965
6       1 standing                     0.2771988                   -0.01009785
````
Hopefully TimeBodyAccelerometerMean is easier to interpret than tBodyAcc-mean.

## Class Project Objective 5: Creates an independent tidy data set with the average of each variable for each activity and each subject.
See this for good explanation on how to do this:
http://www.r-bloggers.com/aggregate-function-in-r-making-your-life-easier-one-mean-at-a-time/

````
tidydataset<-aggregate(. ~subject + activity, FinalDataSet, mean)
tidydataset<-tidydataset[order(tidydataset$subject,tidydataset$activity),]
````
The final data set looks like this:

````
> head(tidydataset)
    subject          activity  TimeBodyAccelerometerMean()-X TimeBodyAccelerometerMean()-Y
1         1            laying                      0.2215982                  -0.040513953
31        1           sitting                      0.2612376                  -0.001308288
61        1          standing                      0.2789176                  -0.016137590
91        1           walking                      0.2773308                  -0.017383819
121       1 walking downstairs                     0.2891883                  -0.009918505
151       1   walking upstairs                     0.2554617                  -0.023953149
````

# Write the results to a file
The last required pirce to finish the Class Project is to write the last data set to a file:

````
write.table(tidydataset, file = "./data/Class3Project/tidyData.txt",row.names=FALSE);
````

>I hope you found this codebook helpful.

Enter file contents here
