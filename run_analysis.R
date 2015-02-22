run_analysis <- function () {
  ### Combine the training data.
  s1 <- read.table("UCI-HAR-Dataset/train/subject_train.txt",sep="")
  a1 <- read.table("UCI-HAR-Dataset/train/y_train.txt")
  d1 <- read.table("UCI-HAR-Dataset/train/X_train.txt")
  f1 <- read.table("UCI-HAR-Dataset/features.txt")
  
  ### Now each data set above has 7231 lines.  So I will do a column
  ### bind of them, leaving a new data set of 563 columns by 7231 lines.
  train <- cbind(s1,a1,d1)
  
  message("Built training data")
  
  s1 <- read.table("UCI-HAR-Dataset/test/subject_test.txt",sep="")
  a1 <- read.table("UCI-HAR-Dataset/test/y_test.txt")
  d1 <- read.table("UCI-HAR-Dataset/test/X_test.txt")
  
  
  ### Now each data set above has 2947 lines.  So I will do a column
  ### bind of them, leaving a new data set of 563 columns by 2947 lines.
  test <- cbind(s1,a1,d1)
  
  message("Build test data.")
  
  ### Now I combine the test and train data sets using a row bind
  
  all <- rbind(train,test)
  
  message("Select mean and standard deviation colums after converting to unique names.")
  
  names(all) <- c("Subject","Activity",make.names(f1$V2,unique=TRUE))
  all2 <- select(all,matches("Subject|Activity|std|mean"))
  
  message("Convert Activity from numbers to readable labels.")
  
  all2$Activity <- as.character(all2$Activity)
  all2$Activity[all2$Activity == "1"] <- "WALKING"
  all2$Activity[all2$Activity == "2"] <- "WALKING_UPSTAIRS"
  all2$Activity[all2$Activity == "3"] <- "WALKING_DOWNSTAIRS"
  all2$Activity[all2$Activity == "4"] <- "SITTING"
  all2$Activity[all2$Activity == "5"] <- "STANDING"
  all2$Activity[all2$Activity == "6"] <- "LAYING"
  group_by(all2,Activity,Subject) %>% summarise_each(funs(mean))
}

