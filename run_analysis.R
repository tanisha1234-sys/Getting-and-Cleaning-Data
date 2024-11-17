#******************************************************************
# Step 0: Downloading and unzipping the dataset
#******************************************************************

# Create a directory for data if it doesn't exist
if (!dir.exists("./data")) {
  dir.create("./data")
}

# Specify the dataset URL and download the file
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileUrl, destfile = "./data/Dataset.zip")

# Unzip the dataset into the /data directory
unzip(zipfile = "./data/Dataset.zip", exdir = "./data")

#******************************************************************
# Step 1: Merging the training and the test sets to create one data set.
#******************************************************************

# 1.1 Reading data files
# 1.1.1 Training data
x_train <- read.table("./data/UCI HAR Dataset/train/X_train.txt")
y_train <- read.table("./data/UCI HAR Dataset/train/y_train.txt")
subject_train <- read.table("./data/UCI HAR Dataset/train/subject_train.txt")

# 1.1.2 Testing data
x_test <- read.table("./data/UCI HAR Dataset/test/X_test.txt")
y_test <- read.table("./data/UCI HAR Dataset/test/y_test.txt")
subject_test <- read.table("./data/UCI HAR Dataset/test/subject_test.txt")

# 1.1.3 Features and activity labels
features <- read.table('./data/UCI HAR Dataset/features.txt')
activity_labels <- read.table('./data/UCI HAR Dataset/activity_labels.txt')

# Check the first few rows of the data to confirm they are loaded correctly
print(head(x_train))
print(head(y_train))
print(head(subject_train))
print(head(features))
print(head(activity_labels))

# 1.2 Assigning column names
colnames(x_train) <- features[, 2]
colnames(y_train) <- "activityId"
colnames(subject_train) <- "subjectId"

colnames(x_test) <- features[, 2]
colnames(y_test) <- "activityId"
colnames(subject_test) <- "subjectId"

colnames(activity_labels) <- c("activityId", "activityType")

# Check the column names to ensure they're correctly assigned
print(colnames(x_train))
print(colnames(y_train))
print(colnames(subject_train))

# 1.3 Merging training and test sets
train_set <- cbind(y_train, subject_train, x_train)
test_set <- cbind(y_test, subject_test, x_test)
full_data <- rbind(train_set, test_set)

# Check the structure and dimensions of the merged dataset
print(dim(full_data))
print(head(full_data))

#******************************************************************
# Step 2: Extracting mean and standard deviation measurements
#******************************************************************

# 2.1 Extract column names
colnames_full_data <- colnames(full_data)

# 2.2 Defining a logical vector for selecting mean and std columns
selected_columns <- (grepl("activityId", colnames_full_data) |
                       grepl("subjectId", colnames_full_data) |
                       grepl("mean", colnames_full_data) |
                       grepl("std", colnames_full_data))

# Check the selected columns to ensure we're selecting the correct ones
print(sum(selected_columns))  # Should print the number of selected columns
print(colnames_full_data[selected_columns])  # Print the selected column names

# 2.3 Subset data based on selected columns
subset_data <- full_data[, selected_columns]

# Check the structure of the subset data
print(dim(subset_data))
print(head(subset_data))

#******************************************************************
# Step 3: Replacing activity IDs with descriptive activity names
#******************************************************************

# Merge with activity labels to add activity type names
data_with_activity_names <- merge(subset_data, activity_labels, 
                                  by = "activityId", all.x = TRUE)

# Check the structure and a sample of the merged data
print(dim(data_with_activity_names))
print(head(data_with_activity_names))

#******************************************************************
# Step 4: Labeling the data set with descriptive variable names
#******************************************************************
# The labeling was done in previous steps when setting column names
# Here, you can double-check that the column names are descriptive

#******************************************************************
# Step 5: Creating a second tidy data set with averages for each subject and activity
#******************************************************************

# 5.1 Create a second tidy dataset with the mean of each variable for each subject and activity
# Explicitly use the base::mean function to avoid any namespace conflicts
tidy_data <- aggregate(. ~ subjectId + activityId, data_with_activity_names, FUN = base::mean)

# Check the structure of the tidy data
print(dim(tidy_data))
print(head(tidy_data))

# 5.2 Write the second tidy data set to a text file
write.table(tidy_data, "tidy_data.txt", row.names = FALSE)

# Confirmation message
cat("Tidy data set created and written to tidy_data.txt\n")
