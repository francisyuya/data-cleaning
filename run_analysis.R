rm(list = ls())
library(dplyr)

setwd("D:/MOOC/Coursera/data-cleaning")

#========== read all data
features = read.table("UCI HAR Dataset/features.txt", col.names = c("n","functions"))
activities = read.table("UCI HAR Dataset/activity_labels.txt", col.names = c("code", "activity"))
subject_test = read.table("UCI HAR Dataset/test/subject_test.txt", col.names = "subject")
x_test = read.table("UCI HAR Dataset/test/X_test.txt", col.names = features$functions)
y_test = read.table("UCI HAR Dataset/test/y_test.txt", col.names = "code")
subject_train = read.table("UCI HAR Dataset/train/subject_train.txt", col.names = "subject")
x_train = read.table("UCI HAR Dataset/train/X_train.txt", col.names = features$functions)
y_train = read.table("UCI HAR Dataset/train/y_train.txt", col.names = "code")


#========== 1. Merges the training and the test sets to create one data set.
#merge x data
X <- rbind(x_train, x_test)
#merge y data
Y <- rbind(y_train, y_test)
#merge subject data
all_Subject = rbind(subject_train, subject_test)

data_merged = cbind(all_Subject, X, Y)


#========== 2. Extracts only the measurements on the mean and standard deviation for each measurement.
data = select(data_merged, subject, code, contains("mean"), contains("sd"))


#========== 3. Uses descriptive activity names to name the activities in the data set
data$activity = activities[data$code, c("activity")]


#========== 4. Appropriately labels the data set with descriptive variable names.
names(data)[2] = "activity"
names(data)<-gsub("Acc", "Accelerometer", names(data))
names(data)<-gsub("Gyro", "Gyroscope", names(data))
names(data)<-gsub("BodyBody", "Body", names(data))
names(data)<-gsub("Mag", "Magnitude", names(data))
names(data)<-gsub("^t", "Time", names(data))
names(data)<-gsub("^f", "Frequency", names(data))
names(data)<-gsub("tBody", "TimeBody", names(data))
names(data)<-gsub("-mean()", "Mean", names(data), ignore.case = TRUE)
names(data)<-gsub("-std()", "STD", names(data), ignore.case = TRUE)
names(data)<-gsub("-freq()", "Frequency", names(data), ignore.case = TRUE)
names(data)<-gsub("angle", "Angle", names(data))
names(data)<-gsub("gravity", "Gravity", names(data))


#========== 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
result <- data %>%
  group_by(subject, activity) %>%
  summarise_all(funs(mean))
write.table(result, "result.txt", row.name=FALSE)

