### Coursera - Data Science - Getting and Cleaning Data - Course Project

# You should create one R script called run_analysis.R that does the following. 
# 1.Merges the training and the test sets to create one data set.
# 2.Extracts only the measurements on the mean and standard deviation for each measurement. 
# 3.Uses descriptive activity names to name the activities in the data set
# 4.Appropriately labels the data set with descriptive variable names. 
# 5.From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.



### (1) Reading data and merging training and test data sets

activity_labels <- read.table("./UCI HAR Dataset/activity_labels.txt")
features <- read.table("./UCI HAR Dataset/features.txt")

activity_labels
features

# train:
subject_train<- read.table("./UCI HAR Dataset/train/subject_train.txt")
X_train <- read.table("./UCI HAR Dataset/train/X_train.txt")
y_train <- read.table("./UCI HAR Dataset/train/y_train.txt")

train.list <- list(subject_train, X_train, y_train)
sapply(train.list, dim)

train <- cbind(subject_train, y_train, X_train)

# test:
subject_test<- read.table("./UCI HAR Dataset/test/subject_test.txt")
X_test <- read.table("./UCI HAR Dataset/test/X_test.txt")
y_test <- read.table("./UCI HAR Dataset/test/y_test.txt")

test.list <- list(subject_test, X_test, y_test)
sapply(test.list, dim)

test <- cbind(subject_test, y_test, X_test)
 
## Merge:

# test if columns possibly have different order in 'train' and 'test':
any(colnames(test) != colnames(train)) 

all <- rbind(train, test)
colnames(all) <- c("subject", "activity.num", as.character(features[,2])) # step (4)

dim(all)
length(unique(all$subject))

### (2) extract mean and sd measurements (=columns)

any(is.na(all)) #test for NA values

library(tidyr)

# column indexes of relevant columns:
ms.i <- grep("mean()|std()", names(all)) 

length(ms.i)
names(all)[ms.i]

# filter out "meanFreq()" columns
f.i <- grep ("meanFreq()", names(all)[ms.i])

names(all)[ms.i][f.i]
names(all)[ms.i][-f.i] # 66 columns containing "mean()" and "std()"

all_ms <- all[,c(1:2,ms.i[-f.i])]

dim(all_ms) # 2+66=68 columns -> looks allright

all_ms[1:20,1:5]


### (3)
# define factor-vector of labels according to existing coding (y_train, y_test)
activity <- cut(all[,2], breaks=6, labels=activity_labels[,2]) 

# replace old numeric activity labels with new factors
all[,2] <- activity
colnames(all)[2] <- "activity"


### (4)

# done already in step 1, see:
names(all_ms)


### (5) built tidy data set: 
#         rows: subjects (30) * activity (6)
#         columns: average of every variable (66)

## summarize subjects:

# aggregate all values for subjects (30) and all activities (6), result should therefor have 180 rows
all_agg <- aggregate(all_ms[3:length(all_ms)], by=list(all_ms[,1], all_ms[,2]), FUN=function(x) mean(x, na.rm=T))

# check dimensions and colnames:
dim(all_agg)
names(all_agg)[1:3]
head(all_agg[,1:3])

names(all_agg)[1:2] <- c("subject", "activity")

# save final data set as txt file:
?write.table
write.table(all_agg, "Samsung tidy.txt", row.names=F)




