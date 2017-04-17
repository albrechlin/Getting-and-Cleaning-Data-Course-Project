# Getting-and-Cleaning-Data-Course-Project

# The run_analysis script unites the text files comprising the test and train data sets, first creating a new data frame for both test and train, then combining the two into a new data set with both original data sets.

# The new data set is then subsetted to only contain the mean() and std() of each measurement, along with the information for subject and activity.

# A "for" loop calculates the average of the mean() and std() for each measurement of each subject and activity, and writes them into a new data frame.

# The new data frame containing the averages is then tidied, with the activity names replacing their numbers and the variables replaced with descriptive names.

# The data set is then written out using write.table() to create the text file.