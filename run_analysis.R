##load packages
library(downloader)
library(data.table)
library(plyr)

#1. merge the training and the test sets to create one data set
##set working directory
setwd("./~")

##create data folder
if(!file.exists(paste0(getwd(),"/data"))){dir.create(paste0(getwd(),"/data"))}

##download & unzip data
url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"

download(url, dest=paste0(getwd(),"/data","/dataset.zip"), mode="wb")

unzip(paste0(getwd(),"/data","/dataset.zip"), exdir=paste0(getwd(),"/data"))

##create file path
path<-file.path(paste0(getwd(),"/data/UCI HAR Dataset"))

## load activity labels & features
features    <- fread(paste0(path, "/features.txt"), colClasses=c("numeric", "character"), encoding="UTF-8", header=FALSE) #import features.txt
activityType<- fread(paste0(path, "/activity_labels.txt"), encoding="UTF-8", header=FALSE) #import activity_labels.txt

## read in the training data
subjectTrain<- fread(paste0(path, "/train/subject_train.txt"), encoding="UTF-8", header=FALSE); #import subject_train.txt
xTrain      <- fread(paste0(path, "/train/x_train.txt"), encoding="UTF-8", header=FALSE) #import x_train.txt
yTrain      <- fread(paste0(path, "/train/y_train.txt"), encoding="UTF-8", header=FALSE) #import y_train.txt

## assigin column names to the training data
colnames(activityType)  <- c('activityId','activityType')
colnames(subjectTrain)  <- "subjectId"
colnames(xTrain)        <- unlist(features[,2], recursive <- TRUE, use.names <- TRUE)
colnames(yTrain)        <- "activityId"

## create the training data by cbinding yTrain, subjectTrain, and xTrain
trainingData <- cbind(yTrain,subjectTrain,xTrain)

## read in the test data
subjectTest<- fread(paste0(path, "/test/subject_test.txt"), header=FALSE); #import subject_test.txt
xTest      <- fread(paste0(path, "/test/x_test.txt"), header=FALSE); #import x_test.txt
yTest      <- fread(paste0(path, "/test/y_test.txt"), header=FALSE); #import y_test.txt

## assign column names to the test data
colnames(subjectTest) <- "subjectId"
colnames(xTest)       <- unlist(features[,2], recursive <- TRUE, use.names <- TRUE)
colnames(yTest)       <- "activityId"

## create the test data by cbinding the xTest, yTest and subjectTest data
testData <- cbind(yTest,subjectTest,xTest)

## merge training and test dataset 
mergedDataset <- rbind(trainingData,testData)

####################################################################################################

# 2. extract only the measurements on the mean and standard deviation for each measurement

## get only columns with mean() or std() in their names
mean_and_std <- grep("-(mean|std)\\(\\)", features[, 2])

## subset the desired columns
mergedDataset <- mergedDataset[, mean_and_std]

####################################################################################################

# 3. use descriptive activity names to name the activities in the data set

## Merge the mergedDataset set with the acitivityType table to include descriptive activity names
mergedDataset <- merge(mergedDataset,activityType,by='activityId',all.x=TRUE);

# Updating the colNames vector to include the new column names after merge
colNames  <- colnames(mergedDataset); 

# 4. appropriately label the data set with descriptive activity names. 

# Cleaning up the variable names
for (i in 1:length(colNames)) 
{
  colNames[i] <- gsub("\\()","",colNames[i])
  colNames[i] <- gsub("-std$","StdDev",colNames[i])
  colNames[i] <- gsub("-mean","Mean",colNames[i])
  colNames[i] <- gsub("^(t)","time",colNames[i])
  colNames[i] <- gsub("^(f)","freq",colNames[i])
  colNames[i] <- gsub("([Gg]ravity)","Gravity",colNames[i])
  colNames[i] <- gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",colNames[i])
  colNames[i] <- gsub("[Gg]yro","Gyro",colNames[i])
  colNames[i] <- gsub("AccMag","AccMagnitude",colNames[i])
  colNames[i] <- gsub("([Bb]odyaccjerkmag)","BodyAccJerkMagnitude",colNames[i])
  colNames[i] <- gsub("JerkMag","JerkMagnitude",colNames[i])
  colNames[i] <- gsub("GyroMag","GyroMagnitude",colNames[i])
};

# Reassigning the new descriptive column names to the mergedDataset set
colnames(mergedDataset) <- colNames;

# 5. Create a second, independent tidy data set with the average of each variable for each activity and each subject. 

# Create a new table, mergedDatasetNoActivityType without the activityType column
mergedDatasetNoActivityType  <- mergedDataset[,names(mergedDataset) != 'activityType'];

# Summarizing the mergedDatasetNoActivityType table to include just the mean of each variable for each activity and each subject
tidyData    <- aggregate(mergedDatasetNoActivityType[,names(mergedDatasetNoActivityType) != c('activityId','subjectId')],by=list(activityId=mergedDatasetNoActivityType$activityId,subjectId <- mergedDatasetNoActivityType$subjectId),mean);

# Merging the tidyData with activityType to include descriptive acitvity names
tidyData    <- merge(tidyData,activityType,by='activityId',all.x=TRUE);

# Export the tidyData set 
write.table(tidyData, './tidyData.txt',row.names=TRUE,sep='\t');


