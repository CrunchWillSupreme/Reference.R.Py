library("forecast")

Amtrak.data <- read.csv("C:\\DAPT\\Forecasting Methods\\Amtrak data.csv")

ridership.ts <- ts(Amtrak.data$Ridership, start = c(1991,1), end = c(2004, 3), freq = 12)

par(mfrow = c(1, 1))

plot(ridership.ts, xlab = "Time", ylab = "Ridership", ylim = c(1300, 2300), bty = "l")

seasonplot(ridership.ts, ylab="Ridership", 
           xlab="Year", main="Seasonal Plot", year.labels=TRUE)

monthplot(ridership.ts, ylab="Ridership", 
           xlab="Year", main="Seasonal Deviation Plot")

lag.plot(ridership.ts, lags=16)

tsdisplay(ridership.ts)
