run_analysis <- function(){
        
        library(plyr)
        library(stats)
        
        # read in activity labels
        activityLabel <- read.table("activity_labels.txt",col.names = c("labelNumber","activity"))    
        
        # read in feature labels
        featureLabel <- read.table("features.txt",col.names = c("index","featureName"))
        
        #read in 'test' data
        subjectTest <- read.table("test/subject_test.txt", col.names = "subject")
        ### satsifies #4 ###
        xTest <- read.table("test/X_test.txt",col.names = featureLabel$featureName)
        yTest <- read.table("test/Y_test.txt", col.names = "labelNumber")
        
        #append the 3 data tables in the following fashion: [subjectTest,yTest,xTest] 
        test <- cbind(subjectTest,yTest,xTest)

        #read in 'train' data
        subjectTrain <- read.table("train/subject_train.txt", col.names = "subject")
        ### satsifies #4 ###
        xTrain <- read.table("train/X_train.txt",col.names = featureLabel$featureName)
        yTrain <- read.table("train/Y_train.txt", col.names = "labelNumber")
       
        #append the 3 data tables in the following fashion: [subjectTrain,yTrain,xTrain] 
        train <- cbind(subjectTrain,yTrain,xTrain)
        
        #append both test and train
        ### satsifies #1 ###
        total <- rbind(test,train)

        
        #to make the data frame more useable, map labelNumbers to activities
        ### satsifies #3 ###
        total <- mutate(total,activity = activityLabel$activity[labelNumber])
        
        #Now, we need to pick out the mean and standard deviation data.
        #For this, I am assuming that this refers to data columns that contain
        #either "mean()" or "std()" in the name.  This concsiouly excludes meanFreq().
        featuresToInclude <- featureLabel$featureName[grep(pattern = "mean\\(\\)|std\\(\\)",featureLabel$featureName)]
        #convert dashes and parentheses to \\.
        featuresToInclude <- gsub("[\\-\\(\\)\\-]", "\\.",featuresToInclude)
        #append subject, and activity columns
        featuresToInclude <- c('subject', 'activity',featuresToInclude)
        #Now, select only the useful columns for our data frame.
        ### satisfies #2 ###
        columnsToChoose <- (names(total) %in% featuresToInclude)
        total <- total[,columnsToChoose]
        total$subject <- as.factor(total$subject)
        
        #now, create a new data frame that averages each variable for each subject and for each activity
        tempMean <- list()
        tempI <- list()
        tempJ <- list()

        for(i in levels(total$subject)){
                for(j in levels(total$activity)){
                        temp <- total[(total$subject == i & total$activity == j), ]
                        temp <- temp[,2:67]
                        tempMean <- rbind(tempMean,unlist(lapply(temp,mean)))
                        tempI <- rbind(tempI,i)
                        tempJ <- rbind(tempJ,j)
                }
        }
        colnames(tempMean) <- NULL
        colnames(tempI) <- NULL
        colnames(tempJ) <- NULL
        output <- data.frame(subject = tempI, activity = tempJ, data=tempMean)
        colnames(output) <- featuresToInclude
        # for whatever reason, the above logic is creating columns that are comprised of lists of length 1.
        #need to go through and unlist each column.
        for(i in seq(1,68)){
                output[,i] <- unlist(output[,i])
        }
        output$subject <- as.factor(output$subject)
        output$activity <- as.factor(output$activity)
        write.table(output, file="output.txt",row.name=FALSE)
        output

}

