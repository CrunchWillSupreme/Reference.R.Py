library("forecast")

Amtrak.data <- read.csv("C:\\DAPT\\Forecasting Methods\\Amtrak data.csv")

ridership.ts <- ts(Amtrak.data$Ridership, start = c(1991,1), end = c(2004, 3), freq = 12)

par(mfrow = c(1, 1))

plot(ridership.ts, xlab = "Time", ylab = "Ridership", ylim = c(1300, 2300), bty = "l")

fit1 <- decompose(ridership.ts, type="additive")
plot(fit1)

fit2 <- stl(ridership.ts, t.window = 12, 
            s.window=12, robust=TRUE)
plot(fit2)

seasonality <- seasadj(fit2)
plot(seasonality)

forecast2 <- forecast(fit2, h=26, method="naive")
plot(forecast2)

