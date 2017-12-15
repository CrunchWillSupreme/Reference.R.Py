install.packages("forecast")

library("forecast")

Amtrak.data <- read.csv("C:\\DAPT\\Forecasting Methods\\Amtrak data.csv")


ridership.ts <- ts(Amtrak.data$Ridership, start = c(1991,1), end = c(2004, 3), freq = 12)

par(mfrow = c(1, 1))

plot(ridership.ts, ylab = "Ridership", xlab = "Time", bty = "l",
     xaxt = "n", xlim = c(1991,2006.25), main = "", lty = 1)
axis(1, at = seq(1991, 2006, 1), labels = format(seq(1991, 2006, 1)))

par(mfrow = c(3, 3))

nValid <- 36
nTrain <- length(ridership.ts) - nValid
train.ts <- window(ridership.ts, 
                   start = c(1991, 1), end = c(1991, nTrain))
valid.ts <- window(ridership.ts, 
                   start = c(1991, nTrain+1), end = c(1991, nTrain+nValid))

ridership.lm1 <-  tslm(train.ts ~ poly(trend, 2))
ridership.lm1.pred <- forecast(ridership.lm1, h = nValid, level =c(80,95))
plot(ridership.lm1.pred, ylim = c(1300, 2600),  ylab = "Ridership", xlab = "Time", bty = "l",
     xaxt = "n", xlim = c(1991,2006.25), main = "", lty = 2)
axis(1, at = seq(1991, 2006, 1), labels = format(seq(1991, 2006, 1)))
lines(ridership.lm1$fitted, lwd = 2)
lines(valid.ts)

hist(ridership.lm1.pred$residuals, ylab = "Frequency", xlab = "Fit Error", bty = "l", main = "")
hist(valid.ts - ridership.lm1.pred$mean, ylab = "Frequency", xlab = "Forecast Error", bty = "l", main = "")

ridership.lm2.pred <- naive(train.ts, h = nValid)
plot(ridership.lm2.pred, ylim = c(1300, 2600),  ylab = "Ridership", xlab = "Time", bty = "l",
     xaxt = "n", xlim = c(1991,2006.25), main = "", lty = 2)
axis(1, at = seq(1991, 2006, 1), labels = format(seq(1991, 2006, 1)))
lines(valid.ts)

hist(ridership.lm2.pred$residuals, ylab = "Frequency", xlab = "Fit Error", bty = "l", main = "")
hist(valid.ts - ridership.lm2.pred$mean, ylab = "Frequency", xlab = "Forecast Error", bty = "l", main = "")

ridership.lm3.pred <- snaive(train.ts, h = nValid)
plot(ridership.lm3.pred, ylim = c(1300, 2600),  ylab = "Ridership", xlab = "Time", bty = "l",
     xaxt = "n", xlim = c(1991,2006.25), main = "", lty = 2)
axis(1, at = seq(1991, 2006, 1), labels = format(seq(1991, 2006, 1)))
lines(valid.ts)

hist(ridership.lm3.pred$residuals, ylab = "Frequency", xlab = "Fit Error", bty = "l", main = "")
hist(valid.ts - ridership.lm3.pred$mean, ylab = "Frequency", xlab = "Forecast Error", bty = "l", main = "")

accuracy(ridership.lm1.pred$mean, valid.ts)
accuracy(ridership.lm2.pred$mean, valid.ts)
accuracy(ridership.lm3.pred$mean, valid.ts)

