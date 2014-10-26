# Section 0: Loading the data sets in R

setwd("C:/Users/hp Probook 4540s/Desktop/data cleaning/project")
#Read X_train data
train=read.table("X_train.txt", header=FALSE,sep = "")
#Read Subject data
subj1=read.table("subject_train.txt", header=FALSE,sep = "")
#Read Activity  data
act1=read.table("y_train.txt", header=FALSE,sep = "")

train1=cbind(train,subj1,act1)

#Read X_test data
test=read.table("X_test.txt", header=FALSE,sep = "")
#Read Subject data
subj2=read.table("subject_test.txt", header=FALSE,sep = "")
#Read Activity  data
act2=read.table("y_test.txt", header=FALSE,sep = "")

test1=cbind(test,subj2,act2)

#Read features data (names of variables)
feature=read.table("features.txt", header=FALSE,sep = "\t")

#SECTION 1: Merges the training and the test sets to create one data set
activi=rbind(train1,test1)
#Quick summarize of cativity data
summary(activi)

#SECTION 2: Extracts only the measurements on the mean and standard deviation for each measurement
#Load stringr library
library(stringr)
#Find variables names in features that contain mean
means=grep("mean()",feature$V1)
#Find variables names in features that contain std
stds=grep("std()",feature$V1)
#Get the names of variables that contain mean and std
varmean=feature[means,1]
varstd=feature[stds,1]
#Extracts only measurements on the mean and standard deviation
val=union(means,stds)
val=union(val,c(562,563))
#Construct the first tidy data set with mean and std
tidy1=activi[,val]

#SECTION 3: Uses descriptive activity names to name the activities in the data set
#Split the names of mean variables
splitmean=strsplit(as.character(varmean),"\\ ")
newnames0=sapply(splitmean, "[[", 2)

#Split the names of standard deviation variables
splitstd=strsplit(as.character(varstd),"\\ ")
newnames1=sapply(splitstd, "[[", 2)
newnames=union(newnames0,newnames1)
newnames=union(newnames,c("Subject","Activity"))
names(tidy1)=newnames

#Label Activity
tidy1$Activity=as.character(tidy1$Activity)
tidy1$Activity[tidy1$Activity=="1"]="WALKING"
tidy1$Activity[tidy1$Activity=="2"]="WALKING_UPSTAIRS"
tidy1$Activity[tidy1$Activity=="3"]="WALKING_DOWNSTAIRS"
tidy1$Activity[tidy1$Activity=="4"]="SITTING"
tidy1$Activity[tidy1$Activity=="5"]="STANDING"
tidy1$Activity[tidy1$Activity=="6"]="LAYING"

#Write the tidy1 data set into text file
write.table(tidy1,file="activity-mean-std.txt",row.names = FALSE)

#SECTION 5: Create independant tidy data
#Use aggregate function to compute average of each variable

tidydata=aggregate(tidy1[,-c(80,81)],by=list(tidy1$Subject,tidy1$Activity),FUN=mean,na.rm=TRUE)

#Write the tidydata data set into text file
write.table(tidydata,file="tidydata.txt",row.names = FALSE)

