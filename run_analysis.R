library(tidyverse)
#download the zip file from the URL given
zipURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
zipFILE <- "UCI HAR Dataset.zip"

if(!file.exists(zipFILE)) {
  download.file(zipURL, zipFILE, mode = "wb")
}

#unzip the downloaded zip file! 
data <- "UCI HAR Dataset"
if(!file.exists(data)) {
  unzip(zipFILE)
}

#read the data 
trainSubjects <- read.table(file.path(data, "train", "subject_train.txt"))
trainValues <- read.table(file.path(data, "train", "X_train.txt"))
trainActivity <- read.table(file.path(data, "train", "y_train.txt"))

testSubjects <- read.table(file.path(data, "test", "subject_test.txt"))
testValues <- read.table(file.path(data, "test", "X_test.txt"))
testActivity <- read.table(file.path(data, "test", "y_test.txt"))

#merge files here 
subjects <- rbind(trainSubjects, testSubjects)
values <- rbind(trainValues, testValues)
activity <- rbind(trainActivity, testActivity)

#rename files
colnames(subjects) <- c("Subjects")
#read the features file
features <- read.table(file.path(data, "features.txt"), as.is = TRUE) 
colnames(values) <- features[,2]

#read the activities file
Activity <- read.table(file.path(data, "activity_labels.txt"))
colnames(Activity) <- c("Activity", "Label")

#Step 1: Done with reading alllllll da files!! Now, merging files into one *tidy* dataset called..."dataset"
dataset <- cbind(subjects, values, activity)
datasetcols <- colnames(dataset)
datasetcols <- gsub("^f", "frequency ", datasetcols)
datasetcols <- gsub("^t", "time ", datasetcols)
datasetcols <- gsub("Acc", "accelerometer ", datasetcols)
datasetcols <- gsub("Gyro", "gyroscope ", datasetcols)
datasetcols <- gsub("Mag", "magnitude ", datasetcols)
datasetcols <- gsub("Freq", "frequency ", datasetcols)
datasetcols <- gsub("mean", "mean ", datasetcols)
datasetcols <- gsub("std", "standard deviation ", datasetcols)
datasetcols <- gsub("BodyBody", "Body ", datasetcols)
datasetcols <- gsub("V1", "Activity", datasetcols)

datasetcols <- gsub("[\\(\\)-]", "", datasetcols) #special character removal - got line from internet lolz
colnames(dataset) <- datasetcols

#Step 2: extracting only mean and SD from each measurement
extracted <- grepl("[Ss]ubject|[Aa]ctivity|[Mm]ean|[Ss]tandard deviation", colnames(dataset)) 
tidydata <- dataset[,extracted] 
tibble <- as_tibble(tidydata)

#Step 3: Use descriptive activity names to name the activities in the dataset
joinedtibble <- full_join(Activity, tibble, by = "Activity")

#Step 4 done in lines 42-52

#step 5: second independent tidy dataset with the average of each variable for each activity/subject
meantibble <- joinedtibble %>%
  group_by(Subjects, Label) %>%
  summarize_all(mean, na.rm = TRUE)

write.table(meantibble, "tidy_data.txt", row.names = FALSE, quote = FALSE)

