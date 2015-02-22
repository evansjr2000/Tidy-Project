Here is how the data was transformed.  It is based on the R code that actually does it.


run_analysis <- function () {

  ### Combine the training data. First I read it in from the data directory
  ### "UCI-HAR-Dataset" and its subdirectories.

  s1 <- read.table("UCI-HAR-Dataset/train/subject_train.txt",sep="")
  a1 <- read.table("UCI-HAR-Dataset/train/y_train.txt")
  d1 <- read.table("UCI-HAR-Dataset/train/X_train.txt")
  f1 <- read.table("UCI-HAR-Dataset/features.txt")
  
  ### Now each data set above has 7231 lines.  So I will do a column
  ### bind of them, leaving a new data set of 563 columns by 7231 lines.

  train <- cbind(s1,a1,d1)
  
  message("Built training data")
  
  ## Now I do the same for the test data as I did for the training data.
  ##
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
  
  ### The next section creates readable columns, note the use of
  ### the "make.names() function, which is the key.

  names(all) <- c("Subject","Activity",make.names(f1$V2,unique=TRUE))

  ### The following code extracts all relevant columns having
  ### "std" or "mean" in their variable name, along with
  ### the Subject and Activity columns

  all2 <- select(all,matches("Subject|Activity|std|mean"))
  
  message("Convert Activity from numbers to readable labels.")
  
  ### Here I convert the Activity column from integer to character
  ### as this is a necessary step before converting the "numbers"
  ### to decriptive labels of the Activity.

  all2$Activity <- as.character(all2$Activity)

  ### Here I do the conversion to readable activity descriptions.

  all2$Activity[all2$Activity == "1"] <- "WALKING"
  all2$Activity[all2$Activity == "2"] <- "WALKING_UPSTAIRS"
  all2$Activity[all2$Activity == "3"] <- "WALKING_DOWNSTAIRS"
  all2$Activity[all2$Activity == "4"] <- "SITTING"
  all2$Activity[all2$Activity == "5"] <- "STANDING"
  all2$Activity[all2$Activity == "6"] <- "LAYING"

  ### lastly I group everything by Activity and Subject, then
  ### calculate the mean of the data.  The function returns
  ### the "tidy" data.

  group_by(all2,Activity,Subject) %>% summarise_each(funs(mean))
}

