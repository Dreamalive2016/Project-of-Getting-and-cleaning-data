## setting working directory
setwd("C:/Users/Ma/OneDrive/6 Coursera/Getting and cleaning data/Project")
getwd()
list.files()

## Prepare the packages
library(dplyr)
library(tidyr)
library(plyr)
library(reshape2)
library(stringr)
library(data.table)

## download and load the data file to R
url<-"https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(url, destfile="proj.zip")
filenames<-unzip("proj.zip")
filenames

body_acc_x_test<-read.table("./UCI HAR Dataset/test/Inertial Signals/body_acc_x_test.txt")
body_acc_y_test<-read.table("./UCI HAR Dataset/test/Inertial Signals/body_acc_y_test.txt")
body_acc_z_test<-read.table("./UCI HAR Dataset/test/Inertial Signals/body_acc_z_test.txt")

body_gyro_x_test<-read.table("./UCI HAR Dataset/test/Inertial Signals/body_gyro_x_test.txt")
body_gyro_y_test<-read.table("./UCI HAR Dataset/test/Inertial Signals/body_gyro_y_test.txt")
body_gyro_z_test<-read.table("./UCI HAR Dataset/test/Inertial Signals/body_gyro_z_test.txt")

total_acc_x_test<-read.table("./UCI HAR Dataset/test/Inertial Signals/total_acc_x_test.txt")
total_acc_y_test<-read.table("./UCI HAR Dataset/test/Inertial Signals/total_acc_y_test.txt")
total_acc_z_test<-read.table("./UCI HAR Dataset/test/Inertial Signals/total_acc_z_test.txt")

subject_test<-read.table("./UCI HAR Dataset/test/subject_test.txt")
x_test<-read.table("./UCI HAR Dataset/test/X_test.txt")
y_test<-read.table("./UCI HAR Dataset/test/y_test.txt")

subject_train<-read.table("./UCI HAR Dataset/train/subject_train.txt")
x_train<-read.table("./UCI HAR Dataset/train/X_train.txt")
y_train<-read.table("./UCI HAR Dataset/train/y_train.txt")

feature<- read.table("./UCI HAR Dataset/features.txt" )

activity_labels<- read.table("./UCI HAR Dataset/activity_labels.txt" )

head(activity_labels)
dim(activity_labels)

feature

dim(body_acc_x_test)
dim(body_acc_y_test)
dim(body_acc_z_test)

dim(body_gyro_x_test)
dim(body_gyro_y_test)
dim(body_gyro_z_test)

dim(total_acc_x_test)
dim(total_acc_y_test)
dim(total_acc_z_test)

dim(subject_test)
dim(x_test)
dim(y_test)

dim(subject_train)
dim(x_train)
dim(y_train)

### Rename the variables before merging
colnames(subject_test)[1]<-"sub_id"
colnames(subject_train)[1]<-"sub_id"
subject_test$type<-"test"
subject_train$type<-"train"
colnames(x_train)<-feature[,2] ## Replace variable labels
colnames(x_test)<-feature[,2]
colnames(y_train)<-"activity"
colnames(y_test)<-"activity"

## STEP1:Merge the training and test sets to create one data set
test<-bind_cols(subject_test, x_test, y_test)
train<-bind_cols(subject_train, x_train, y_train)
test_train<-bind_rows(test, train)

dim(test)
dim(train)
dim(test_train)
head(test_train)
colnames(test_train)

## STEP2:Extract only mean and standard deviation of each measurement
name_index<-grep("mean()|std()", colnames(test_train))
name_index
data_extr<-test_train[,c(1,2,name_index, 480) ] ## only keep the mean and std of each measurement
dim(data_extr)
head(data_extr)

## STEP3:Add labels to activities
table(data_extr$activity)
str(data_extr$activity)
data_extr$activityLabel<-factor(data_extr$activity, labels=activity_labels[,2])
data_extr1<-tbl_df(data_extr)
data_extr1<-select(data_extr1, -activity)

## STEP4:Label the data set with descriptive variable names
index<-sub("fBody", "", colnames(data_extr1)) ##Delete the repeated "fBody" in each variable
index
index1<-sub("()", "", index, fixed=TRUE) ## Delete the "()" in each variable
index2<-gsub("-","_", index1)
index2
index3<-tolower(index2) ## change variable names to lower cases
index3

colnames(data_extr1)<-index3
colnames(data_extr1)

## STEP5: Create a second, independent dataset
columnIndex<-grep("mean", colnames(data_extr1))

columnIndex
data2<-data_extr1[, c(1,2, columnIndex, 82)] ## Create the data set containing only the mean values
colnames(data_extr1)   ## check variable names
sum(is.na(data_extr1)) ## check missing values
dataColumns<-colnames(data_extr1)[3:81] ## columns to be calculated

data2<-ddply(data_extr1, .(sub_id, activitylabel), function(x) colMeans(x[dataColumns])) ## group summary
str(data2)

dim(data2)

write.table(data2, file="clean.txt", row.names=FALSE)