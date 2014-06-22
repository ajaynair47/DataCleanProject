
## Routine to do the following:

## Load Training data set
## Append subject and activity columns
## Append Test data set to training data set
## Extract a new data set with only mean and std columns
## Finally create a new data set that with the average of 
## each variable for each activity and each subject

RunAnalysis <- function() {
        
        ## Load features 
        features <- read.delim("features.txt", header = FALSE, sep = "")
        ## Load Activity Labels
        activity_labels <- read.delim("activity_labels.txt", header = FALSE, sep = "")
        
        ## Load training data set
        x_train <- read.delim("./train/x_train.txt", header = FALSE, sep = "")
        ## set column names of data set based on features
        colnames(x_train) <- features[,2]
        ## Load subjects
        subject_train <- read.delim("./train/subject_train.txt", header = FALSE, sep = "")
        ## Append subjects to data set as new column
        x_train <- cbind(x_train,subject_train)
        colnames(x_train)[ncol(x_train)] = "Subject"
        ## Load activities
        y_train <- read.delim("./train/y_train.txt", header = FALSE, sep = "")
        ## Append activities to data set as new column
        x_train <- cbind(x_train,y_train)
        colnames(x_train)[ncol(x_train)] = "ActivityNumber"
        ## Convert Activity code to Activity Name
        x_train <- merge(x_train, activity_labels, by.x = "ActivityNumber", by.y = colnames(activity_labels)[1], all.x = TRUE)
        colnames(x_train)[ncol(x_train)] = "Activity"
        
        ## Load test data set
        x_test <- read.delim("./test/x_test.txt", header = FALSE, sep = "")
        ## set column names of data set based on features
        colnames(x_test) <- features[,2]
        ## Load subjects
        subject_test <- read.delim("./test/subject_test.txt", header = FALSE, sep = "")
        ## Append subjects to data set as new column
        x_test <- cbind(x_test,subject_test)
        colnames(x_test)[ncol(x_test)] = "Subject"
        ## Load activities
        y_test <- read.delim("./test/y_test.txt", header = FALSE, sep = "")
        ## Append activities to data set as new column
        x_test <- cbind(x_test,y_test)
        colnames(x_test)[ncol(x_test)] = "ActivityNumber"
        ## Convert Activity code to Activity Name
        x_test <- merge(x_test, activity_labels, by.x = "ActivityNumber", 
                        by.y = colnames(activity_labels)[1], all.x = TRUE)
        colnames(x_test)[ncol(x_test)] = "Activity"
        
        ## Append test data set rows to training data set and create a new data set
        x_trainandtest <- rbind(x_train,x_test)
        
        ## Get list of column names with *mean* or *std*
        MeanStdCols <- colnames(x_trainandtest)[grep("*mean*|*std*", 
                                                     colnames(x_trainandtest))]
        
        ## create a new data set with only *mean* and *std* columns besides subject and
        ## activity
        x_meanstdonly <- x_trainandtest[ , 
                        names(x_trainandtest) %in% c(MeanStdCols, 
                                                "Subject", "Activity")] 
        
        ## use plyr library to get avg of every column from 1 through 79
        ## column 80 and 81 are subject and activity respectively
        ## grouped by activity and subject
        ## the new column names have a avg_ prefix
        bak <- colnames(x_meanstdonly)[1]
        colnames(x_meanstdonly)[1] <- "temp"
        x_final <- ddply(x_meanstdonly, c("Activity","Subject"), summarize, 
                         mean(temp, na.rm = TRUE) )
        colnames(x_meanstdonly)[1] <- bak
        colnames(x_final)[ncol(x_final)] <- 
                paste("avg",colnames(x_meanstdonly)[1],sep="_")
        
        for (i in 2:79)
        {
                bak <- colnames(x_meanstdonly)[i]
                colnames(x_meanstdonly)[i] <- "temp"
                x_final <- cbind(x_final, ddply(x_meanstdonly, 
                        c("Activity","Subject"), summarize, 
                        mean(temp, na.rm = TRUE) )[,3] )
                colnames(x_meanstdonly)[i] <- bak
                colnames(x_final)[ncol(x_final)] <- 
                        paste("avg",colnames(x_meanstdonly)[i],sep="_")                
        }        
        
        ## return the final data set
        x_final
}