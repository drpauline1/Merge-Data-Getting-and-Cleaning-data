# Merge-Data-Getting-and-Cleaning-data
#Step : 1 Merges the training and the test sets to create one data set
library(dplyr)
setwd("C:\\Users\\mbhadaur\\Downloads\\UCI HAR Dataset\\UCI HAR Dataset")
trainingdata<-read.table("./train/X_train.txt")
head(trainingdata)
str(trainingdata)
traininglabel<-read.table("./train/y_train.txt")
table(traininglabel)
trainingsubject<-read.table("./train/subject_train.txt")
str(trainingsubject)
testingdata<-read.table("./test/X_test.txt")
str(testingdata)
testinglabel<-read.table("./test/y_test.txt")
dim(testinglabel)
testingsubject<-read.table("./test/subject_test.txt")
dim(testingsubject)
str(testingsubject)
mergedata<-rbind(trainingdata,testingdata)
dim(mergedata)
mergelabel<-rbind(traininglabel,testinglabel)
dim(mergelabel)
mergesubject<-rbind(trainingsubject,testingsubject)
dim(mergesubject)
head(mergedata)

#Step : 2 Extracts only the measurements on the mean and standard deviation for each measurement.

features<-read.table("./features.txt")
dim(features)
meanAndstdCols<-grep("mean\\(\\)|std\\(\\)", features[,2])
str(meanAndstdCols)
mergedata<-mergedata[,meanAndstdCols]
head(mergedata)
dim(mergedata)

#Step : 3 Uses descriptive activity names to name the activities in the data set

names(mergedata) <- gsub("\\(\\)", "", features[meanAndstdCols, 2])
dim(mergedata)
head(mergedata)
activity<-read.table("./activity_labels.txt")
activityLabel <- activity[mergelabel[, 1], 2]
dim(activitylabel)
activitylabel <- activity[mergelabel[, 1], 2]
dim(activitylabel)
mergelabel[, 1] <- activitylabel
names(mergelabel)<-"activity"
head(mergelabel)

#Step : 4 Appropriately labels the data set with descriptive variable names. 

names(mergesubject) <- "subject"
head(mergesubject)
finaldata<-cbind(mergesubject,mergelabel,mergedata)
dim(finaldata)
head(finaldata)
write.table(finaldata,"merged_data.txt")

#Step : 5 From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

head(finaldata)
dim(activity)
dim(mergesubject)
subjectlength <- length(table(mergesubject))
activitylength <- dim(activity)[1]
columnlength <- dim(finaldata)[2]
finalresult <- matrix(NA, nrow=subjectlength*activitylength, ncol=columnlength) 
finalresult <- as.data.frame(finalresult)
colnames(finalresult) <- colnames(finaldata)
row <- 1
for(i in 1:subjectlength) {
for(j in 1:activitylength) {
finalresult[row, 1] <- sort(unique(mergesubject)[, 1])[i]
finalresult[row, 2] <- activity[j, 2]
bool1 <- i == finaldata$subject
bool2 <- activity[j, 2] == finaldata$activity
finalresult[row, 3:columnlength] <- colMeans(finaldata[bool1&bool2, 3:columnlength])
row <- row + 1
}
}
head(finalresult)
dim(finalresult)
write.table(finalresult, "data_with_averages.txt", row.name=FALSE) 


