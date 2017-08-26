#PART 1 OF ASSIGNMENT

library(dplyr)
library(data.table)
library(tidyr)
library(reshape2)

#makes variables out of the list in each of the files
xtrain <- read.table('./UCI HAR Dataset/train/X_train.txt')
ytrain <- read.table('./UCI HAR Dataset/train/y_train.txt')
subjecttrain <- read.table('./UCI HAR Dataset/train/subject_train.txt')
xtest <- read.table('./UCI HAR Dataset/test/X_test.txt')
ytest <- read.table('./UCI HAR Dataset/test/y_test.txt')
subjecttest <- read.table('./UCI HAR Dataset/test/subject_test.txt')

#combine test & train variables into a data table
xdata <- rbind(xtest, xtrain) #features
ydata <- rbind(ytest, ytrain) #activities
subjectdata <- rbind(subjecttest, subjecttrain)
#remove now-unneeded variables to save memory
rm(ytest, ytrain, xtest, xtrain, subjecttest, subjecttrain)

#assign name for subjectdata column, activity column, and all of the feature columns
names(subjectdata) <-"SubjectID"
names(ydata) <- "activity"
featureIDs <- read.table('./UCI HAR Dataset/features.txt')
   names(xdata) <- featureIDs$V2
rm(featureIDs) #to save space
   
#combine above datasets into one dataset
tidytable <- cbind(subjectdata,ydata,xdata)
rm(subjectdata, ydata, xdata) #to save space

#PART 2 OF ASSIGNEMENT

# make TRUE/FALSE array corresponding to the mean & standard deviation columns we wish to keep
trimtidytable <- grepl("mean", names(tidytable)) |  grepl("std", names(tidytable))
trimtidytable[1:2] <- TRUE #this line keeps subject & activity columns

# equate tidytable dataframe to trimtidytable T/F array, therefore only keeping colums which we marked "TRUE"
tidytable <- tidytable[, trimtidytable]
rm(trimtidytable) #to save space

#PART 3 OF ASSIGNMENT

#replace numbers in activity column with activity names
tidytable$activity[tidytable$activity==1] <- "walking"
tidytable$activity[tidytable$activity==2] <- "walking upstairs"
tidytable$activity[tidytable$activity==3] <- "walking downstairs"
tidytable$activity[tidytable$activity==4] <- "sitting"
tidytable$activity[tidytable$activity==5] <- "standing"
tidytable$activity[tidytable$activity==6] <- "laying"

#PART 4 OF ASSIGNEMENT

#replace variable names with more descriptive names
names(tidytable) <- gsub("^f","Frequency Domain ", names(tidytable))
names(tidytable) <- gsub("^t","Time Domain ", names(tidytable))
names(tidytable) <- gsub("Acc","Linear Acceleration ", names(tidytable))
names(tidytable) <- gsub("-mean", "Mean", names(tidytable))
names(tidytable) <- gsub("mean", "Mean", names(tidytable))
names(tidytable) <- gsub("-std", "Standard Deviation", names(tidytable))
names(tidytable) <- gsub("Gyro", "Angular Velocity ", names(tidytable))
names(tidytable) <- gsub("Body", "Body ", names(tidytable))
names(tidytable) <- gsub("Gravity", "Gravitational ", names(tidytable))
names(tidytable) <- gsub("Jerk", "Jerk ", names(tidytable))
names(tidytable) <- gsub("Mag", "Magnitude ", names(tidytable))
names(tidytable) <- gsub("MeanFreq", "Mean Frequency", names(tidytable))
names(tidytable) <- gsub("\\()", "", names(tidytable))
#renames first two columns to their original (Yeah, it's a workaround, but it gives the right result)
names(tidytable)[1] <- "SubjectID"
names(tidytable)[2] <- "Activity"

#PART 5 OF ASSIGNMENT

#group by subject ID and by activity for each subject, giving each variable and value for each grouping
grouped <- melt(tidytable, id=c("SubjectID", "Activity"))
#use mean function to 
#create additional dataset with average of each variable, by subject and by activity
tidy_bymean <- dcast(grouped, SubjectID+Activity ~ variable, mean)
   rm(grouped) #to save space

#this would create instead two datasets, one with averages by subject id, and one with averages by activity
#(included it since the assignment could be a little abiguous)
   #tidysubject <- dcast(melted, SubjectID ~ variable, mean)
   #tidyactivity <- dcast(melted, Activity ~ variable, mean)

#create txt file for dataset decribed in assignment question 5
write.table(tidy_bymean, "tidy_bymean.txt", row.names = FALSE)
