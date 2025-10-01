# Import Needed Libraries
library(tidyr)
library(dplyr)

# Import the datasets we need to work with
subect_train_file<-"./UCI HAR Dataset/train/subject_train.txt"
x_train_raw_file<-"./UCI HAR Dataset/train/X_train.txt"
y_train_raw_file<-"./UCI HAR Dataset/train/y_train.txt"
subect_test_file<-"./UCI HAR Dataset/test/subject_test.txt"
x_test_raw_file<-"./UCI HAR Dataset/test/X_test.txt"
y_test_raw_file<-"./UCI HAR Dataset/test/y_test.txt"
activity_labels<-"./UCI HAR Dataset/activity_labels.txt"

# Read the files into dataframes
x_train_raw<-read.table(x_train_raw_file,sep="\n",fill=TRUE)
y_train_raw<-read.table(y_train_raw_file,sep=" ")
subject_train_raw<-read.table(subect_train_file,sep=" ")
x_test_raw<-read.table(x_train_raw_file,sep="\n",fill=TRUE)
y_test_raw<-read.table(y_train_raw_file,sep=" ")
subject_test_raw<-read.table(subect_train_file,sep=" ")
activity_labels<-read.table(activity_labels,sep=" ")

# Rename most of the columns for clarity in final output. 
# (X column is input only, so not renamed)
colnames(y_train_raw)<-'activity_id'
colnames(subject_train_raw)<-'subject_id'
colnames(y_test_raw)<-'activity_id'
colnames(subject_test_raw)<-'subject_id'
colnames(activity_labels)<-c('activity_id','activity')

# Creates two dataframes for each set of data, training and testing, and then 
# combines them
train_set<-data.frame(x_train_raw,y_train_raw,subject_train_raw)
test_set<-data.frame(x_test_raw,y_test_raw,subject_test_raw)
combined_frame<-rbind(train_set,test_set)
#Changes any double spaces into spaces, due to unclean data
combined_frame$V1<-gsub("  "," ",combined_frame$V1)

# Initializes a vector to help sort the text in X into usable rows
vect<-1:length(combined_frame$V1)
vect_char<-paste0("V",vect)
# Splits feature data into usable columns, one per observation
x_pre_agg<-separate_wider_delim(combined_frame,V1," ",names=vect_char,too_few='align_start')
# Removes the first column, as it's empy (data always starts with a space)
x_pre_agg<-x_pre_agg[2:length(x_pre_agg)]
# Changes all the strings into numerics so mean and std can be performed
x_pre_agg<-x_pre_agg %>%  mutate(across(starts_with("V"), as.numeric))
# Calculate mean of features
feature_mean<-apply(x_pre_agg,1,mean,na.rm=TRUE)
# Calculate std of features
feature_std<-apply(x_pre_agg,1,sd,na.rm=TRUE)

# Left join activity name for clarity/ more legible data
activity_frame<-left_join(combined_frame,activity_labels,by='activity_id')

# Final dataset including mean, std, activity, and subject_id
final<-data.frame(feature_mean,feature_std,activity_frame$activity,activity_frame$subject_id)
# Summary of dataset based on the subject and activity
summary<-final %>% group_by(activity_frame.subject_id, activity_frame.activity) %>% summarise(mean_mean=mean(feature_mean),std_mean=mean(feature_std))

write.table(final,'tidy_data.txt',sep = " ", row.names=TRUE)
write.table(summary,'summary_data.txt',sep = " ", row.names=TRUE)