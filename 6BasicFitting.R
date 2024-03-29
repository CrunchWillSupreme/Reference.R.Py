library("forecast")

Amtrak.data <- read.csv("C:\\DAPT\\Forecasting Methods\\Amtrak data.csv")

ridership.ts <- ts(Amtrak.data$Ridership, start = c(1991,1), end = c(2004, 3), freq = 12)

par(mfrow = c(3, 1))

plot(ridership.ts, xlab = "Time", ylab = "Ridership", ylim = c(1300, 2300), bty = "l")
ridership.lm <- tslm(ridership.ts ~ poly(trend, 1))
lines(ridership.lm$fitted, lwd = 2)

plot(ridership.ts, xlab = "Time", ylab = "Ridership", ylim = c(1300, 2300), bty = "l")
ridership.lm <- tslm(ridership.ts ~ poly(trend, 2))
lines(ridership.lm$fitted, lwd = 2)

plot(ridership.ts, xlab = "Time", ylab = "Ridership", ylim = c(1300, 2300), bty = "l")
ridership.lm <- tslm(ridership.ts ~ poly(trend, 3))
lines(ridership.lm$fitted, lwd = 2)

