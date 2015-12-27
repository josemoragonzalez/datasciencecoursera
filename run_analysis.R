# You should create one R script called run_analysis.R that does the following. 
# 1. Merges the training and the test sets to create one data set.
# Load data X_Train 
data_train <- read.table("~/UCI HAR Dataset/train/X_train.txt")

# Load data Y_Train 
data_train_label <- read.table("~/UCI HAR Dataset/train/y_train.txt")

# Load data Subject Train
data_train_subject <- read.table("~/UCI HAR Dataset/train/subject_train.txt")

# Load data X_Test
data_test <- read.table("~/UCI HAR Dataset/test/X_test.txt")

# Load data Y_Test
data_test_label <- read.table("~/UCI HAR Dataset/test/y_test.txt") 

# Load data Subject Test
testSubject <- read.table("~/UCI HAR Dataset/test/subject_test.txt")

# Join datasets
join_Data <- rbind(data_train, data_test)
join_Label <- rbind(data_train_label, data_test_label)
join_Subject <- rbind(data_train_subject, testSubject)

# 2. Extracts only the measurements on the mean and standard deviation for each measurement. 

features <- read.table("~/UCI HAR Dataset/features.txt")
meanStdIndx <- grep("mean\\(\\)|std\\(\\)", features[, 2])
join_Data <- join_Data[, meanStdIndx]
names(join_Data) <- gsub("\\(\\)", "", features[meanStdIndx, 2]) # remove "()"
names(join_Data) <- gsub("mean", "Mean", names(join_Data)) # capitalize M
names(join_Data) <- gsub("std", "Std", names(join_Data)) # capitalize S
names(join_Data) <- gsub("-", "", names(join_Data)) # remove "-" in column names 

# 3. Uses descriptive activity names to name the activities in the data set
activity <- read.table("~/UCI HAR Dataset/activity_labels.txt")
activity[, 2] <- tolower(gsub("_", "", activity[, 2]))
substr(activity[2, 2], 8, 8) <- toupper(substr(activity[2, 2], 8, 8))
substr(activity[3, 2], 8, 8) <- toupper(substr(activity[3, 2], 8, 8))
activityLabel <- activity[join_Label[, 1], 2]
join_Label[, 1] <- activityLabel
names(join_Label) <- "activity"


# 4. Appropriately labels the data set with descriptive variable names. 
names(join_Subject) <- "subject"
cleanedData <- cbind(join_Subject, join_Label, join_Data)
write.table(cleanedData, "merged_data.txt") # write out the 1st dataset

# 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
subjectLen <- length(table(join_Subject)) 
activityLen <- dim(activity)[1] 
columnLen <- dim(cleanedData)[2]
result <- matrix(NA, nrow=subjectLen*activityLen, ncol=columnLen) 
result <- as.data.frame(result)
colnames(result) <- colnames(cleanedData)
row <- 1
for(i in 1:subjectLen) {
  for(j in 1:activityLen) {
    result[row, 1] <- sort(unique(join_Subject)[, 1])[i]
    result[row, 2] <- activity[j, 2]
    bool1 <- i == cleanedData$subject
    bool2 <- activity[j, 2] == cleanedData$activity
    result[row, 3:columnLen] <- colMeans(cleanedData[bool1&bool2, 3:columnLen])
    row <- row + 1
  }
}
write.table(result, "tidy.txt", row.names = FALSE) 
