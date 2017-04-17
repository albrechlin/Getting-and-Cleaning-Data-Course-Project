## 1. Merges the training and the test sets to create one data set.
## 2. Extracts only the measurements on the mean and standard deviation for each measurement.
## 3. Uses descriptive activity names to name the activities in the data set
## 4. Appropriately labels the data set with descriptive variable names.
## 5. From the data set in step 4, creates a second, independent tidy data set with the average
## of each variable for each activity and each subject.

## download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip",
## "UCI HAR Dataset.zip")
## unzip("UCI HAR Dataset.zip")

run_analysis<-function(){
features<-read.table("~/UCI HAR Dataset/features.txt")
x_train<-read.table("~/UCI HAR Dataset/train/x_train.txt")
y_train<-read.table("~/UCI HAR Dataset/train/y_train.txt")
## Y_train<-y_train
## Y_train<-gsub(1,"WALKING",Y_train)
## Y_train<-gsub(2,"WALKING UPSTAIRS",Y_train)
## Y_train<-gsub(3,"WALKING DOWNSTAIRS",Y_train)
## Y_train<-gsub(4,"SITTING",Y_train)
## Y_train<-gsub(5,"STANDING",Y_train)
## Y_train<-gsub(6,"LAYING",Y_train)
subject_train<-read.table("~/UCI HAR Dataset/train/subject_train.txt")
X_train<-cbind(subject_train,y_train,x_train)
X_train_colnames<-c("Subject","Activity",as.character(features$V2))
colnames(X_train)<-X_train_colnames

x_test<-read.table("~/UCI HAR Dataset/test/x_test.txt")
y_test<-read.table("~/UCI HAR Dataset/test/y_test.txt")
## Y_test<-y_test
## Y_test<-gsub(1,"WALKING",Y_test)
## Y_test<-gsub(2,"WALKING UPSTAIRS",Y_test)
## Y_test<-gsub(3,"WALKING DOWNSTAIRS",Y_test)
## Y_test<-gsub(4,"SITTING",Y_test)
## Y_test<-gsub(5,"STANDING",Y_test)
## Y_test<-gsub(6,"LAYING",Y_test)
subject_test<-read.table("~/UCI HAR Dataset/test/subject_test.txt")
X_test<-cbind(subject_test,y_test,x_test)
X_test_colnames<-c("Subject","Activity",as.character(features$V2))
colnames(X_test)<-X_test_colnames

X_testtrain<-rbind(X_train,X_test)
X_testtrain_1<-X_testtrain[,c(TRUE,
                              TRUE,
                              ((grepl("mean()",features$V2)&(!grepl("Freq",features$V2)))|grepl("std()",features$V2)))]
X_testtrain_2<-data.frame(matrix(ncol=68,nrow=0))

for(i in 1:30){
  for(j in 1:6){
    X_testtrain_t<-X_testtrain_1[(X_testtrain_1$Subject==i & X_testtrain_1$Activity==j),]
    X_testtrain_tavg<-apply(X_testtrain_t,2,mean)
    ## print(X_testtrain_tavg)
    X_testtrain_2<-rbind(X_testtrain_2,X_testtrain_tavg)
  }
}
colnames(X_testtrain_2)<-X_test_colnames[c(TRUE,
                                           TRUE,
                                           ((grepl("mean()",features$V2)&(!grepl("Freq",features$V2)))|grepl("std()",features$V2)))]
X_testtrain$Activity[X_testtrain$Activity==1]<-"WALKING"
X_testtrain_1$Activity[X_testtrain_1$Activity==1]<-"WALKING"
X_testtrain$Activity[X_testtrain$Activity==2]<-"WALKING UPSTAIRS"
X_testtrain_1$Activity[X_testtrain_1$Activity==2]<-"WALKING UPSTAIRS"
X_testtrain$Activity[X_testtrain$Activity==3]<-"WALKING DOWNSTAIRS"
X_testtrain_1$Activity[X_testtrain_1$Activity==3]<-"WALKING DOWNSTAIRS"
X_testtrain$Activity[X_testtrain$Activity==4]<-"SITTING"
X_testtrain_1$Activity[X_testtrain_1$Activity==4]<-"SITTING"
X_testtrain$Activity[X_testtrain$Activity==5]<-"STANDING"
X_testtrain_1$Activity[X_testtrain_1$Activity==5]<-"STANDING"
X_testtrain$Activity[X_testtrain$Activity==6]<-"LAYING"
X_testtrain_1$Activity[X_testtrain_1$Activity==6]<-"LAYING"
X_testtrain_2$Activity[X_testtrain_2$Activity==1]<-"WALKING"
X_testtrain_2$Activity[X_testtrain_2$Activity==2]<-"WALKING UPSTAIRS"
X_testtrain_2$Activity[X_testtrain_2$Activity==3]<-"WALKING DOWNSTAIRS"
X_testtrain_2$Activity[X_testtrain_2$Activity==4]<-"SITTING"
X_testtrain_2$Activity[X_testtrain_2$Activity==5]<-"STANDING"
X_testtrain_2$Activity[X_testtrain_2$Activity==6]<-"LAYING"

X_testtrain_2colreplace<-c("Subject",
                           "Activity",
                           "Body Accelerometer: X-axis Time Domain Mean Average",
                           "Body Accelerometer: Y-axis Time Domain Mean Average",
                           "Body Accelerometer: Z-axis Time Domain Mean Average",
                           "Body Accelerometer: X-axis Time Domain Standard Deviation Average",
                           "Body Accelerometer: Y-axis Time Domain Standard Deviation Average",
                           "Body Accelerometer: Z-axis Time Domain Standard Deviation Average",
                           "Gravity Accelerometer: X-axis Time Domain Mean Average",
                           "Gravity Accelerometer: Y-axis Time Domain Mean Average",
                           "Gravity Accelerometer: Z-axis Time Domain Mean Average",
                           "Gravity Accelerometer: X-axis Time Domain Standard Deviation Average",
                           "Gravity Accelerometer: Y-axis Time Domain Standard Deviation Average",
                           "Gravity Accelerometer: Z-axis Time Domain Standard Deviation Average",
                           "Body Accelerometer: X-axis Jerk Time Domain Mean Average",
                           "Body Accelerometer: Y-axis Jerk Time Domain Mean Average",
                           "Body Accelerometer: Z-axis Jerk Time Domain Mean Average",
                           "Body Accelerometer: X-axis Jerk Time Domain Standard Deviation Average",
                           "Body Accelerometer: Y-axis Jerk Time Domain Standard Deviation Average",
                           "Body Accelerometer: Z-axis Jerk Time Domain Standard Deviation Average",
                           "Body Gyroscope: X-axis Time Domain Mean Average",
                           "Body Gyroscope: Y-axis Time Domain Mean Average",
                           "Body Gyroscope: Z-axis Time Domain Mean Average",
                           "Body Gyroscope: X-axis Time Domain Standard Deviation Average",
                           "Body Gyroscope: Y-axis Time Domain Standard Deviation Average",
                           "Body Gyroscope: Z-axis Time Domain Standard Deviation Average",
                           "Body Gyroscope: X-axis Jerk Time Domain Mean Average",
                           "Body Gyroscope: Y-axis Jerk Time Domain Mean Average",
                           "Body Gyroscope: Z-axis Jerk Time Domain Mean Average",
                           "Body Gyroscope: X-axis Jerk Time Domain Standard Deviation Average",
                           "Body Gyroscope: Y-axis Jerk Time Domain Standard Deviation Average",
                           "Body Gyroscope: Z-axis Jerk Time Domain Standard Deviation Average",
                           "Body Accelerometer: Magnitude Time Domain Mean Average",
                           "Body Accelerometer: Magnitude Time Domain Standard Deviation Average",
                           "Gravity Accelerometer: Magnitude Time Domain Mean Average",
                           "Gravity Accelerometer: Magnitude Time Domain Standard Deviation Average",
                           "Body Accelerometer: Jerk Magnitude Time Domain Mean Average",
                           "Body Accelerometer: Jerk Magnitude Time Domain Standard Deviation Average",
                           "Body Gyroscope: Magnitude Time Domain Mean Average",
                           "Body Gyroscope: Magnitude Time Domain Standard Deviation Average",
                           "Body Gyroscope: Jerk Magnitude Time Domain Mean Average",
                           "Body Gyroscope: Jerk Magnitude Time Domain Standard Deviation Average",
                           "Body Accelerometer: X-axis Frequency Domain Mean Average",
                           "Body Accelerometer: Y-axis Frequency Domain Mean Average",
                           "Body Accelerometer: Z-axis Frequency Domain Mean Average",
                           "Body Accelerometer: X-axis Frequency Domain Standard Deviation Average",
                           "Body Accelerometer: Y-axis Frequency Domain Standard Deviation Average",
                           "Body Accelerometer: Z-axis Frequency Domain Standard Deviation Average",
                           "Body Accelerometer: X-axis Jerk Frequency Domain Mean Average",
                           "Body Accelerometer: Y-axis Jerk Frequency Domain Mean Average",
                           "Body Accelerometer: Z-axis Jerk Frequency Domain Mean Average",
                           "Body Accelerometer: X-axis Jerk Frequency Domain Standard Deviation Average",
                           "Body Accelerometer: Y-axis Jerk Frequency Domain Standard Deviation Average",
                           "Body Accelerometer: Z-axis Jerk Frequency Domain Standard Deviation Average",
                           "Body Gyroscope: X-axis Frequency Domain Mean Average",
                           "Body Gyroscope: Y-axis Frequency Domain Mean Average",
                           "Body Gyroscope: Z-axis Frequency Domain Mean Average",
                           "Body Gyroscope: X-axis Frequency Domain Standard Deviation Average",
                           "Body Gyroscope: Y-axis Frequency Domain Standard Deviation Average",
                           "Body Gyroscope: Z-axis Frequency Domain Standard Deviation Average",
                           "Body Accelerometer: Magnitude Frequency Domain Mean Average",
                           "Body Accelerometer: Magnitude Frequency Domain Standard Deviation Average",
                           "Body Body Accelerometer: Jerk Magnitude Frequency Domain Mean Average",
                           "Body Body Accelerometer: Jerk Magnitude Frequency Domain Standard Deviation Average",
                           "Body Body Gyroscope: Magnitude Frequency Domain Mean Average",
                           "Body Body Gyroscope: Magnitude Frequency Domain Standard Deviation Average",
                           "Body Body Gyroscope: Jerk Magnitude Frequency Domain Mean Average",
                           "Body Body Gyroscope: Jerk Magnitude Frequency Domain Standard Deviation Average")

colnames(X_testtrain_2)<-X_testtrain_2colreplace
write.table(X_testtrain_2,file="~/GettingAndCleaningDataCourseProject.txt",row.name=FALSE)
}