setwd("C:/Users/Nurseli/Desktop/dataa")
myData <- read.csv("data2.csv", header = TRUE)
View(myData)

#necessary packages downloaded using pacman
install.packages("pacman")
require(pacman)
library(pacman)
pacman:: p_load(pacman,dply,GGally,ggplot2,ggthemes, ggvis, httr, lubridate, plotly, rio, rmarkdown, shiny, string, tidyr)

#--------------------------------------------DATA PREPROCESSING-------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------------------------------------

#since dealing with NA values is better than dealing with NULL values, I will convert them to NA.
myData [myData == "NULL"] <- NA
summary(myData)

#since in some attributes we have lots of NA values and we don't need to use some attributes. So I will delete those attributes to have better results.
myData <- myData %>% select(c(-1,-2,-3, -5, -6, -7,-8, -9, -10, -18, -20, -21,-22, -23, -36,-37,-38, -39, -40, -41, -42,-43,-44,-45,-46,-47,-48,-49,-50,-51,-52,-53,-54,-55,
                              -56,-57,-58,-59,-60,-61))
summary(myData)

#in those numeric data we replace the NA values using the average of the coloumns

for(i in 2:12){
  myData[,i] = as.numeric(myData[,i])
}

for(i in 2:12){
  myData[,i] = ifelse(is.na(myData[,i]),ave(myData[,i], FUN = function(x) mean(x, na.rm = 'TRUE')),myData[,i])
}

print(sum(is.na(myData)))


myData[is.na(myData)] <- 0
summary(myData)

#In the data, there were some NU values, so I replaced them by 0.
myData[myData == "NU"] <- 0

summary(myData)
write.csv(myData, 'myData2.csv')

#-----------------------------------CLUSTERING-----------------------------------
---------------------------------------------------------------------------------
  
  
#-----------------------------------HCLUSTERS------------------------------------

scaled <- myData[-c(1,1)]
distance<- dist(scaled, method="euclidean")
hfit <- hclust(distance, method="ward")

plot(hfit)

group = cutree(hfit, k=5)
group

rect.hclust(hfit,k=5,border="red")



#-----------------------------------KMEANS---------------------------------------

set.seed(12345)

install.packages("rattle")
scaled <- myData[-c(1,1)]
kmeansdata <- kmeans(scaled, 8)
attributes(kmeansdata)

kmeansdata$cluster
c1 <- cbind(kmeansdata$cluster)
c1

library(cluster)
clusplot(scaled, kmeansdata$cluster, main="2D representation of Cluster", shade=TRUE, label=0)

data.matrix(myData)
test1 <- scale(na.omit(data.matrix(myData)[-1]))
head(myData)
wssplot <- function(test1, nc=20, seed=123){
  wss <- (nrow(test1)-1)*sum(apply(test1,2,var))
  for(i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(test1, centers=i)$withinss)
  }
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")
}

wssplot(test1, nc=20)

#------------------------------------------CLASSIFICATION----------------------------------------
#------------------------------------------------------------------------------------------------


# load libraries
library(mlbench)
install.packages("caret",dependencies = TRUE)
install.packages("e1071", dependencies=TRUE)
library(caret)
set.seed(12345)

myData$MAIN = as.factor(myData$MAIN)
head(myData)

inTrain = createDataPartition(y = myData$MAIN, p = .75, list = FALSE)
training = myData[inTrain,]
testing = myData[-inTrain,] 

#-------------------DECISION TREE ALGORITHM---------------------------------
#----------------------------------------------------------------------------

library(rpart)
myF <- MAIN ~ HCM2 + NUMBRANCH + SCH_DEG + PREDDEG + HIGHDEG + CCBASIC + CCUGPROF + CCSIZSET
myData_dtree <- rpart(myF, data = training, method="class")
summary( myData_dtree)

# check the prediction
pred<-predict(myData_dtree, training[, c(2,3,5,6,7,10,11,12)], type="class")

confusionMatrix(table (pred, training$MAIN))

library(rpart.plot)
plot(myData_dtree, main="Classification Tree ")
text(myData_dtree, use.n=TRUE, all=TRUE, cex=.7)


# predict on test data
testPred <- predict(myData_dtree, testing,type="class")
testPred

View(myData)
na.omit(myData)
confusionMatrix(table(testing$MAIN,testPred))


  
  
#---------------------------KNN CLASSIFICATION------------------------------
----------------------------------------------------------------------------
  
  
  #myData <- myData[]  #removes the 1. , 2. and 3. variables from the data set.   
  # it helps us to get the numbers of patients
  
  for(i in 1:20){
    if(!is.numeric(myData[,i]))
    {
      myData[,i] <- as.numeric(myData[,i])
    }
  }

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x))) }

myData_n <- as.data.frame(lapply(myData[,c(2,3,5,6,7,10,11,12)], normalize))

#divide the prc_n data frame into prc_train and prc_test data frames
myData_train <- myData_n[1:5000,]
myData_test <- myData_n[5001:7112,]
myData_train_labels <- myData[1:5000, 4]
myData_test_labels <- myData[5001:7112, 4]   

install.packages("class") #for knn
install.packages("Rtools")
library(class)
library(caret)

prc_test_pred <- knn(train = myData_train, test = myData_test,cl = myData_train_labels, k=20)



table( myData_test_labels,myData_test_labels)


  
  
  
  
  
  









