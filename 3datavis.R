
## ------------------------------------------------------------------------
senicData <- read.table("SENIC.csv", header=TRUE, row.names=1, sep=",",
                        colClasses=c(rep("numeric",7),rep("factor",2),
                        rep("numeric",9), rep("factor",2)))
senicData <- senicData[,-ncol(senicData)]

## ------------------------------------------------------------------------
load("lendingData.rda")

## ------------------------------------------------------------------------
hist(senicData$Length_stay)

## ------------------------------------------------------------------------
hist(senicData$Length_stay, col="red",border="blue", 
     xlab="Average Length of Stay", main="Histogram of Average Length of Stay") 

## dev.off()

## Saving plot as a pdf
pdf("lengthHist.pdf")
hist(senicData$Length_stay, col="red",border="blue", 
     xlab="Average Length of Stay", main="Histogram of Average Length of Stay") 
dev.off()

?pdf

## Saving plot as png
png("lengthHist.png")
hist(senicData$Length_stay, col="red",border="blue", 
     xlab="Average Length of Stay", main="Histogram of Average Length of Stay") 
dev.off()

?png


## ------------------------------------------------------------------------
## empirical cumulative distribution function- allows looking at many distributions at once
plot(ecdf(senicData$Length_stay[senicData$Region_Name == "NorthCentral"]), 
     verticals=TRUE, main="ECDF of Length of Stay", xlim=c(7,20))          ##xlim=x axis scale
lines(ecdf(senicData$Length_stay[senicData$Region_Name == "NorthEast"]), col=2, 
      verticals=TRUE)
lines(ecdf(senicData$Length_stay[senicData$Region_Name == "South"]), col=3, 
      verticals=TRUE)
lines(ecdf(senicData$Length_stay[senicData$Region_Name == "West"]), col=4, 
      verticals=TRUE)
legend("bottomright", c("NorthCentral", "NorthEast", "South", "West"), 
       col=c(1,2,3,4), pch=1)                             ##pch=plotting character, changes the legend markers

?par     ##plotting options

## ------------------------------------------------------------------------
regionCounts <- table(senicData$Region_Name)
regionCounts
barplot(regionCounts)

## ------------------------------------------------------------------------
barplot(regionCounts, xlab="Region", ylab="Frequency", 
        col=rainbow(unique(senicData$Region_Name)), main="Bar Plot of Region")

## ------------------------------------------------------------------------
boxplot(senicData$Length_stay ~ senicData$Region_Name)     #continuous variable, y-axis, first; categorical variable x-axis, second

## ------------------------------------------------------------------------
boxplot(senicData$Length_stay ~ senicData$Region_Name, xlab="Region", 
        ylab="Average Length of Stay", main = "Boxplot of Average 
        Length of Stay by Region", col=3, outline=FALSE)

## ------------------------------------------------------------------------
# senicData$Med_Sch[senicData$Med_Sch == 0] <- "No"       #Changing the "0" to "No"
# senicData$Med_Sch[senicData$Med_Sch == 1] <- "Yes"      #Changing the "1" to "Yes"
# senicData$Med_Sch <- as.factor(senicData$Med_Sch)       #changing the variable to factor
# summary(senicData$Med_Sch)
medschoolRegionTable <- table(senicData$Region_Name, senicData$Medical_School)
medschoolRegionTable
mosaicplot(medschoolRegionTable, color=c(1:2), xlab="Region",
           ylab="Medical School",main="")

## ------------------------------------------------------------------------
plot(senicData$Age_years, senicData$Length_stay)          #scatterplot

## ------------------------------------------------------------------------
par(bg="lightgray")
plot(senicData$Age_years, senicData$Length_stay, xlab="Average Age", 
     ylab="Average Length of Stay", col="blue", pch=16, 
     main="Plot of Average Length of Stay versus Average Age")

## ------------------------------------------------------------------------
plot(senicData$Age_years, senicData$Length_stay, type="n", xlab="Average Age",    #type="n" = dont plot anything?
     ylab="Average Length of Stay", main="Plot of Average Length of Stay 
     versus Average Age")
points(senicData$Age_years[senicData$Medical_School=="Yes"], 
       senicData$Length_stay[senicData$Medical_School == "Yes"], col=2, pch=2)
points(senicData$Age_years[senicData$Medical_School=="No"], 
       senicData$Length_stay[senicData$Medical_School == "No"], col=3, pch=3)

windows()    #call to open a window for editing

## x11()

## ------------------------------------------------------------------------
plot(senicData$Age_years, senicData$Length_stay, type="n", xlab="Average Age", 
     ylab="Average Length of Stay", main="Plot of Average Length of Stay 
           versus Average Age")
points(senicData$Age_years[senicData$Medical_School=="Yes"], 
       senicData$Length_stay[senicData$Medical_School == "Yes"], col=2, pch=2)
points(senicData$Age_years[senicData$Medical_School=="No"], 
       senicData$Length_stay[senicData$Medical_School == "No"], col=3, pch=3)
legend("topleft", title="Med School Affiliation?", legend=c("Yes", "No"), 
       col=c(2,3), pch=c(2,3)) 

install.packages("rgl")

## ------------------------------------------------------------------------
library(rgl)

plot3d(senicData$Length_stay, senicData$Age_years, senicData$Infection_pct, 
       col=as.numeric(senicData$Region_Name)+1, type="s",     #+1 = so we dont have any black color;type=s = plot as spheres
       xlab="Average Length of Stay",ylab="Average Age", 
       zlab="Average Prob. of Infection")
legend3d("topright", levels(senicData$Region_Name), 
         col=1:length(levels(senicData$Region_Name))+1, pch=16)

## ------------------------------------------------------------------------
hist(senicData$Culture_ratio, col="green", xlab="Culture Ratio", 
     ylab="Frequency", main="Histogram of Culture Ratio")

## ?boxplot

## ------------------------------------------------------------------------
boxplot(senicData$Infection_pct ~ senicData$Medical_School, 
        xlab="Medical School?", ylab="Infection percentage", main="", col=4)

## ------------------------------------------------------------------------
homeownerTermTable <- table(lendingData[,c("home_ownership", "term")])
mosaicplot(homeownerTermTable, color=rainbow(3), xlab="home ownership status", 
           ylab="term", main="", las=2)

## ------------------------------------------------------------------------
library(dplyr)
lendingData$issue_d <- as.POSIXct(lendingData$issue_d)
loansByMonth <- lendingData %>%
                 select(loan_amnt, issue_d) %>% 
                 group_by(issue_d) %>%
                 arrange(issue_d) %>%
                 summarise(sum(loan_amnt))

plot(loansByMonth, type="l", col="red", xlab="Month", ylab="Total Loans")

## ------------------------------------------------------------------------
boxplot(lendingData$funded_amnt ~ 
        lendingData$home_ownership, xlab="Home Ownership Status",
        ylab="Funded Amount", main="", col=5)

## install.packages("ggplot2")

library(ggplot2)  
library(reshape2) 
fundedData <- lendingData[, c("funded_amnt", "home_ownership", "term")]

## ------------------------------------------------------------------------
fundedMeltedData <- melt(fundedData)

## dev.off()

## ------------------------------------------------------------------------
myBoxPlot <- ggplot(fundedMeltedData, aes(x=home_ownership, y=value), group=term) + 
             geom_boxplot(aes(fill=term))
print(myBoxPlot)

## ------------------------------------------------------------------------
ggsave(myBoxPlot, file="fundedHomeTerm.jpg", width=10, height=5)


