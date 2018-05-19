#------------------------------------------------------------------------
# Titanic Kaggle Entry
# By: Mark Erenberg
#------------------------------------------------------------------------

library(rpart)


test <- read.csv("C:/Users/marke/Downloads/Kaggle/Titanic Competition/test.csv",
                 header=TRUE, stringsAsFactors = FALSE)
train <- read.csv("C:/Users/marke/Downloads/Kaggle/Titanic Competition/train.csv",
                  header=TRUE, stringsAsFactors = FALSE)



train <- as.data.frame(train)
test <- as.data.frame(test)
N <- length(train[,1])



# ------------------------------------------------
# Explanatory Variables
# ------------------------------------------------

# Explanatory variables considered include:
# 1) pclass     (categorical)
# 2) sex        (categorical)
# 3) age        (continuous)
# 4) sibsp      (continuous)
# 5) parch      (continuous)
# 6) fare       (continuous)
# 7) cabin      (categorical)
# 8) embarked   (categorical)

# Ticket number was excluded for logical reasons. 


survived <- train$Survived
pclass <- train$Pclass
sex <- train$Sex
age <- train$Age
sibsp <- train$SibSp
parch <- train$Parch
fare <- train$Fare
cabin <- train$Cabin
embarked <- train$Embarked


# Since age has NAs, we will be using a subset of the data that does
# not have age listed as NA, so that we can properly consider age as 
# a continuous variable, and not a categorical one.

naindex <- which(is.na(age))

# Similarly, we will eliminate the observations that do not have an embarked
# value.

naindex <- c(naindex, which(embarked == ""))

train2 <- train[-naindex,]

survived <- train2$Survived
pclass <- train2$Pclass
sex <- train2$Sex
age <- train2$Age
sibsp <- train2$SibSp
parch <- train2$Parch
fare <- train2$Fare
cabin <- train2$Cabin
embarked <- train2$Embarked

N2 <- length(train2[,1])


# Convert cabin to a categorical variable:
cabincat <- c()
for(i in 1:N2){
  first <- substr(cabin[i],1,1)
  if(first == ""){
    cabincat[i] <- "N"
  }
  else {
    cabincat[i] <- first
  }
}


# Create dummy variables for categorical variables:
pclass <- factor(pclass)
sex <- factor(sex)
cabin <- factor(cabincat)
embarked <- factor(embarked)




# ------------------------------------------------
# Fitting The Model
# ------------------------------------------------


# We begin by fitting a model with all explanatory variables:

tree <- rpart(survived~pclass+sex+age+sibsp+parch+fare+cabin+embarked,
                    data=train2, method = "class")
pfit<- prune(tree, cp=tree$cptable[which.min(tree$cptable[,"xerror"]),"CP"])

printcp(tree)
printcp(pfit)

# Since the pruned tree has the same root node error as the original tree,
# we will proceed with the original tree.

# Missclassification Error
predict_train <- predict(tree, type="class")
length(which(predict_train != survived))/N2

# The cross-validated error rate is approximately 0.1516.


# ------------------------------------------------
# Making Predictions
# ------------------------------------------------

# Variables need to be redefined for test data set:
survived <- test$Survived
pclass <- test$Pclass
sex <- test$Sex
age <- test$Age
sibsp <- test$SibSp
parch <- test$Parch
fare <- test$Fare
embarked <- test$Embarked
passengerid <- test$PassengerId


cabincat <- c()
for(i in 1:nrow(test)){
  first <- substr(cabin[i],1,1)
  if(first == ""){
    cabincat[i] <- "N"
  }
  else {
    cabincat[i] <- first
  }
}

# Now a new data frame needs to be constructed for the predict function
variables <- cbind(pclass, sex, age, sibsp,parch, fare, 
                       cabincat,embarked)
variables <- as.data.frame(variables)
names(variables)[7]<-"cabin"

# Lastly the data types need to be realigned to match those of the train
# data set:
variables$age <- as.numeric(variables$age)
variables$sibsp <- as.integer(variables$sibsp)
variables$parch <- as.integer(variables$parch)
variables$fare <- as.numeric(variables$fare)

# The final predicted values are:
predict_test <- predict(tree, newdata = variables, type="class")


write.csv(predict_test, "C:/Users/marke/Documents/Kaggle/Predictions.csv")



