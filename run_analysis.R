
## load data to data frame and tidy it up
loadData = function (subjectDataFile, labelDataFile, labelNameFile, dataSetFile, featureListFile)  {
        
    ## get subjects
    subjectVector <- read.table(subjectDataFile)[,1]
    
    ## get activity labels    
    labelNamesVector <- merge(read.table(labelDataFile), read.table(labelNameFile))[,2]
      
    ## read data and set meaningful columns
    dataFrame <- read.table(dataSetFile)    
    dataFeatureNames <- read.table(featureListFile)[,2] 
    colnames(dataFrame) <- dataFeatureNames
    
    
    dataFrame$subject <- subjectVector
    dataFrame$label <-labelNamesVector
    
    dataFrame    
}


##load test and train data and union it   
loadAndMergeTestTrainDataSets = function(dataDir) {
    subjectFileTest <- paste0(dataDir, "/test/subject_test.txt")
    labelFileTest <- paste0(dataDir, "/test/y_test.txt")
    dataSetFileTest <- paste0 (dataDir, "/test/X_test.txt")
    
    subjectFileTrain <- paste0(dataDir, "/train/subject_train.txt")
    labelFileTrain <- paste0(dataDir, "/train/y_train.txt")
    dataSetFileTrain <- paste0 (dataDir, "/train/X_train.txt")
    
    featuresFile <- paste0(dataDir, "/features.txt")
    activityLabelsFile <- paste0(dataDir, "/activity_labels.txt")

    
    dataFrameTest <- loadData(subjectFileTest, labelFileTest, activityLabelsFile, 
                              dataSetFileTest, featuresFile)
    
    dataFrameTrain <- loadData(subjectFileTrain, labelFileTrain, activityLabelsFile, 
                              dataSetFileTrain, featuresFile)
    
    
    ##union 2 data frames
    dataFrame <- rbind(dataFrameTest, dataFrameTrain) 
       
  
}

## create mean for all mean/std columns grouped by activity
calcMeanForMeanStdCols = function(dataFrame) {
    ##get features names that contain std or mean
    meanStdFeatureNames <- as.vector(Filter(function(x) {grepl("-mean\\(\\)",as.character(x)) | grepl("-std\\(\\)", as.character(x))}, 
                                            colnames(dataFrame)))
    ##add label and subject to columns
    coln = c("label", "subject", meanStdFeatureNames)
    
    ##project columns
    meanStdDataFrame <- dataFrame[, coln]
    
    ## group data frame by activity
    dfs <- split(meanStdDataFrame, f=meanStdDataFrame[, "label"])
    
    ##calculate mean for all columns in group
    res <- data.frame(sapply(dfs, function(x) {x$label<-NULL;x$subject<-NULL; colMeans(x)}))    
    
    res
}


##project main function
##input : dataDir with train and test data; output file name
main <- function (dataDir, outputFile) {
    dataFrame <- loadAndMergeTestTrainDataSets(dataDir)
    res <- calcMeanForMeanStdCols(dataFrame)
    write.table(res, outputFile, row.name=FALSE )
}
