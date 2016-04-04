
#Merge the training and test data sets into a single data set

traindata <- read.table("./data/train/X_train.txt")
trainlabel <- read.table("./data/train/y_train.txt")
trainsubj <- read.table("./data/train/subject_train.txt")

testdata <- read.table("./data/test/X_test.txt")
testlabel <- read.table("./data/test/y_test.txt")
testsubj <- read.table("./data/test/subject_test.txt")

joined_data <- rbind(testdata, traindata)
joined_labels <- rbind(testlabel, trainlabel)
joined_subj <- rbind(testsubj, trainsubj)


#Step 2: extract mean and sd from each measurement
features <- read.table("./data/features.txt")
mean_and_std_indices <- grep("mean\\(\\)|std\\(\\)", features[,2]) #match anything of the form "mean()" or "std()"

#reduce joined data to only the indices containing mean and sd information (throw away everything else!)
joined_data <- joined_data[,mean_and_std_indices]

names(joined_data) <- gsub("-", "", features[mean_and_std_indices, 2]) #replace column names with the names from features.txt
names(joined_data) <- gsub("\\(\\)", "", names(joined_data)) #removes the parenethesis e.g. "mean()" becomes "mean"
names(joined_data) <- gsub("mean", "Mean", names(joined_data)) #capitalize Mean 
names(joined_data) <- gsub("std", "Std", names(joined_data)) #capitalize Std

#Replace current column names more meaningful/descriptive activity names

activity <- read.table("./data/activity_labels.txt")
activity[,2] <- tolower(gsub("_","", activity[,2])) #remove underscore characters
substr(activity[2,2], 8, 8) <- toupper(substr(activity[2,2],8,8))
substr(activity[3,2], 8, 8) <- toupper(substr(activity[3,2],8,8))
activitylabel <- activity[joined_labels[,1],2]
joined_labels[,1] <- activitylabel

#label the joined data set with descriptive the improved (meaningful/descriptive) activity names

names(joined_subj) <- "subject"

cleaned_up_data <- cbind(joined_subj, joined_labels, joined_data)
write.table(cleaned_up_data, "merged_data.txt") #write the clean data set to external file

# create a data set with the means of each variable / subject

subjectlen <- length(table(joined_subj))
activitylen <- dim(activity)[1] 
columnlen <- dim(cleaned_up_data)[2]
result <- matrix(NA, nrow=subjectlen*activitylen, ncol=columnlen) 
result <- as.data.frame(result)
colnames(result) <- colnames(cleaned_up_data)
row <- 1
for(i in 1:subjectlen) {
  for(j in 1:activitylen) {
    result[row, 1] <- sort(unique(joined_subj)[, 1])[i]
    result[row, 2] <- activity[j, 2]
    bool1 <- i == cleaned_up_data$subject
    bool2 <- activity[j, 2] == cleaned_up_data$activity
    result[row, 3:columnlen] <- colMeans(cleaned_up_data[bool1&bool2, 3:columnlen])
    row <- row + 1
  }
}

write.table(result, "data_with_means.txt") #write means of each variable / subject to external file
