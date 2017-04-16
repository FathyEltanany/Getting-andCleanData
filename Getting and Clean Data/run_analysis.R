 library(reshape2)
 library(data.table)
 path<-file.path(getwd())


  path<-file.path(getwd())
  list.files(path,recursive = TRUE)
  
  #read data
  subjectTrain <- fread(file.path(path, "train", "subject_train.txt"))
  subjectTest  <- fread(file.path(path, "test" , "subject_test.txt" ))
  print("done read")
  activityTrain <- fread(file.path(path, "train", "Y_train.txt"))
  activityTest  <- fread(file.path(path, "test" , "Y_test.txt" ))
  
  train_data<- read.table(file.path(path,"train","X_train.txt"))
  test_data<- read.table(file.path(path,"test","X_test.txt"))
  
  #merge subdata
  subject<-rbind(subjectTrain,subjectTest)
  activity<-rbind(activityTrain,activityTest)
  
  data<-rbind(train_data,test_data)
  print("done merge")
  
  #set names
  setnames(subject,"subject")
  setnames(activity,"activity")

  #merge subject and activity 
  subdata<-rbind(subject,activity)
  data<-cbind(subdata,data)
  
  print("done")
  summary(data)
  
  features<-fread(file.path(path,"features.txt"))
  meanstd <- grep("mean\\(\\)|std\\(\\)", features[, 2])
  data<-data[,meanstd]
  
  #use descripive 
  activity <- fread(file.path(path,"activity_labels.txt"))
  setnames(activity,names(activity),c("activityNum","activityName"))
  
  #make merged data
  dataLen<-length(table(data))
  activityLen<-dim(activity)[1]
  colLen<-dim(data)[2]
  
  tidyData<-matrix(NA,nrow = dataLen,ncol = colLen)
  tidyData<-as.data.frame(tidyData)
  colnames(tidyData)<-colnames(data)
  write.table(tidyData, "mergedData.txt")
  #make tidy dataset
  
  row <- 1
  for(i in 1:dataLen) {
    for(j in 1:activityLen) {
      tidyData[row, 1] <- sort(unique(data)[, 1])[i]
      tidyData[row, 2] <- activity[j, 2]
      bool1 <- i == data$subject
      bool2 <- activity[j, 2] == data$activity
      tidyData[row, 3:colLen] <- colMeans(data[bool1&bool2, 3:colLen])
      row <- row + 1
    }
  }
  
  write.table(tidyData, "AvgData.txt") 
  
