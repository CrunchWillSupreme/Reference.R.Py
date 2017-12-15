
## ------------------------------------------------------------------------
set.seed(12345)    #random number generator; 

## ------------------------------------------------------------------------
homeData <- read.table("HomeDataClean.csv", header=TRUE, row.names=1, sep=",",
                       comment.char="", colClasses=c("character", 
                       rep("factor",2), rep("numeric",4), rep("factor",3))) 

## ------------------------------------------------------------------------
senicData <- read.table("SENIC.csv", header=TRUE, row.names=1, sep=",",
                        colClasses=c(rep("numeric",7),rep("factor",2),
                        rep("numeric",9), rep("factor",2)))
senicData <- senicData[,-ncol(senicData)]


## ------------------------------------------------------------------------
load("lendingData.rda")

## ------------------------------------------------------------------------
priceSqFtlm <- lm(Price ~ Sq..Ft., data=homeData)
summary(priceSqFtlm)

## ------------------------------------------------------------------------
plot(homeData$Sq..Ft., homeData$Price, xlab="Square Footage", ylab="Price",
    pch=16, col="blue")
abline(priceSqFtlm, lwd=3)

## ------------------------------------------------------------------------
predict(priceSqFtlm)

## ------------------------------------------------------------------------
predict(priceSqFtlm, list(Sq..Ft. = c(1200,1400,2000,3500)))

## ------------------------------------------------------------------------
priceSqFtBathdf <- data.frame(homeData[,c("Sq..Ft.","Baths","Price")])
priceSqFtBathlm <- lm(Price ~ ., data=priceSqFtBathdf)    #"linear model using price against(~) all other variables (.) using the priceSQFtBathdf dataframe
summary(priceSqFtBathlm)

library(rgl)
plot3d(priceSqFtBathdf)
planes3d(a=priceSqFtBathlm$coefficients[2],b=priceSqFtBathlm$coefficients[3],
         c=-1.0, d=priceSqFtBathlm$coefficients[1], alpha=0.05)

## ------------------------------------------------------------------------
myMLRdf <- homeData[,c("Price", "Location", "Bedrooms", "Baths", "Sq..Ft.", 
                       "Realtor.Group")]
myMLRlm <- lm(Price ~ ., data=myMLRdf)
summary(myMLRlm)

## ------------------------------------------------------------------------
myMLRsummary <- summary(myMLRlm)
myMLRpredictors <- names(which(myMLRsummary$coefficients[,4] < 0.10))
myMLRpredictors

## ------------------------------------------------------------------------
myclassdf <- senicData[,c("Medical_School", "Age_years", "Num_Services")]

myMedlr <- glm(Medical_School ~ ., data = myclassdf, family=binomial("logit"))
summary(myMedlr)

## ------------------------------------------------------------------------
exp(coef(myMedlr))

## ------------------------------------------------------------------------
myMedPredict <- predict(myMedlr, myclassdf, type="response")
myMedPredictClass <- character(length(myMedPredict))
myMedPredictClass[myMedPredict < 0.5] <- "No"
myMedPredictClass[myMedPredict >= 0.5] <- "Yes"
myMedPredictClass
myMedcm <- table(myclassdf$Medical_School, myMedPredictClass)
myMedcm

library(MASS)
myMedlda <- lda(Medical_School ~ ., data=myclassdf)     #lda=linear discretion analysis
myMedlda

## ------------------------------------------------------------------------
myMedldapredict <- predict(myMedlda)
table(myclassdf$Medical_School,myMedldapredict$class)

## ------------------------------------------------------------------------
predict(myMedlda, list(Age_years=60, Num_Services=20))

install.packages("rpart")    ##for classification trees

## ------------------------------------------------------------------------
library(rpart)

## ------------------------------------------------------------------------
myWeights <- numeric(nrow(myclassdf))
myWeights[myclassdf$Medical_School == "Yes"] <- sum(myclassdf$Medical_School == "No")
myWeights[myclassdf$Medical_School == "No"] <- sum(myclassdf$Medical_School == "Yes")
data.frame(myWeights, myclassdf$Medical_School)
## ------------------------------------------------------------------------
myMedrpart <- rpart(Medical_School ~ ., data=myclassdf, weights=myWeights)

myMedrpartPredict <- predict(myMedrpart, newdata=myclassdf, type="class")
table(myclassdf$Medical_School, myMedrpartPredict)

## ------------------------------------------------------------------------
plot(myMedrpart)
text(myMedrpart)
myMedrpart

## ------------------------------------------------------------------------
myMedrpart$variable.importance    #HIGHER NUMBER IS MORE IMPORTANT THAN LOWER NUMBER

## TRAINING, VALIDATION, TESTING------------------------------------------------------------------------
senicNrow <- nrow(myclassdf)   #count number of rows in that dataframe
senicNrow
mysample <- sample(1:senicNrow) 
mysample
trainRows <- mysample[1:floor(0.5*senicNrow)]   #from 1 to half of the 113 samples.  "Floor" rounds down. make %50 of hospitals training data
validationRows <- mysample[(floor(0.5*senicNrow)+1):floor(0.75*senicNrow)] 
testRows <- mysample[(floor(0.75*senicNrow)+1):senicNrow] 

trainData <- myclassdf[trainRows,]    #turns the rows into data
validationData <- myclassdf[validationRows,]
testData <- myclassdf[testRows,]

## ------------------------------------------------------------------------
myWeights <- numeric(nrow(trainData))    #creates an empty list for which the data will go into
myWeights[trainData$Medical_School == "Yes"] <- sum(trainData$Medical_School == "No")
myWeights[trainData$Medical_School == "No"] <- sum(trainData$Medical_School == "Yes")

library(rpart)
myMedrpart <- rpart(Medical_School ~ ., data=trainData, weights=myWeights)
myMedrpartpredict <- predict(myMedrpart, newdata=testData, type="class")
myMedrpartpredict
table(testData$Medical_School, myMedrpartpredict)  #compare true values (Medical_School) from predicted/test (myMedrpartpredict)

## install.packages("ROCR")

library(ROCR)

## ------------------------------------------------------------------------
myMedlr <- glm(Medical_School ~ ., data = trainData, family=binomial("logit"),
               weights=myWeights)
mylrPredict <- predict(myMedlr, testData, type="response")  #gives us a probability of having a medical center.  0=not have medical center, 1=have medical center
mylrPredict
mylrPred <- prediction(mylrPredict, testData$Medical_School)
mylrPerf <- performance(mylrPred, "tpr", "fpr")

myMedrpart <- rpart(Medical_School ~ ., data=trainData, weights=myWeights)
myrpartPredict  <- predict(myMedrpart, testData, type="prob")
myrpartPredict
myrpartPred <- prediction(myrpartPredict[,2], testData$Medical_School)
myrpartPerf <- performance(myrpartPred, "tpr", "fpr")

## ------------------------------------------------------------------------
plot(mylrPerf, col=1)
plot(myrpartPerf, col=2, add=TRUE)
legend(0.7, 0.6, c("Log. Reg.", "Class. Tree"), col=1:2, lwd=3)

class(mylrPerf)
?plot.performance

## ------------------------------------------------------------------------
performance(mylrPred, "auc")
performance(myrpartPred, "auc")

