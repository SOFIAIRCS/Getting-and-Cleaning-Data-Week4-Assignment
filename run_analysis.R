# This R script does the following:

# 1. Merges the training and the test sets to create one data set:

train <- read.table("train/X_train.txt")
test <- read.table("test/X_test.txt")
X <- rbind(train, test)

train <- read.table("train/Y_train.txt")
test <- read.table("test/Y_test.txt")
Y <- rbind(train, test)

train <- read.table("train/subject_train.txt")
test <- read.table("test/subject_test.txt")
subject <- rbind(train, test)

# 2. Extracts only the measurements on the mean and standard deviation for each measurement:

features <- read.table("features.txt")
index <- grep("-mean\\(\\)|-std\\(\\)", features[, 2])
X <- X[, index]
names(X) <- features[index, 2]
names(X) <- gsub("\\(|\\)", "", names(X))
names(X) <- tolower(names(X))

# 3. Uses descriptive activity names to name the activities in the data set:

activity <- read.table("activity_labels.txt")
activity[, 2] = gsub("_", "", tolower(as.character(activity[, 2])))
Y[, 1] = activity[Y[, 1], 2]
names(Y) <- "activity"

# 4. Appropriately labels the data set with descriptive variable names:

names(subject) <- "subject"
clean <- cbind(subject, Y, X)
write.table(clean, "merged_clean_and_tidy_data.txt")

# 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject:

uniqueSubjects <- unique(subject)[, 1]
numSubjects <- length(unique(subject)[, 1])
numActivities <- length(activity[, 1])
numColumns <- dim(clean)[2]
result <- clean[1:(numSubjects*numActivities), ]

row <- 1
for (s in 1:numSubjects){
	for (a in 1:numActivities){
		result[row, 1] <- uniqueSubjects[s]
		result[row, 2] <- activity[a, 2]
		temp <- clean[clean$subject == s & clean$activity == activity[a, 2], ]
		result[row, 3:numColumns] <- colMeans(temp[, 3:numColumns])
		row <- row + 1
	}
}
write.table(result, "data_set_with_the_averages.txt")


