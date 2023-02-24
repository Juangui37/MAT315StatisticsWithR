x <- c(8,11,5,2,4)
y <- c(17,23,11,5,9)
plot(y~x)

cor(x,y)

x <- c(10,3,5,1,6)
y <- c(-30,-2,-10,6,-14)

plot(y~x)

x <- c(2,4,5,8,11)
y1 <- c(11.5,13.2,14,22.3,19.8)
y2 <- c(10.7,11.2,33,36.7,22.7)

plot(y1~x, xlim = c(0,15), ylim = c(0,40) )

plot(y2~x, xlim = c(0,15), ylim = c(0,40) )

cor(x,y1)
cor(x,y2)

DataLab6 <- read.csv(file.choose())
plot(DataLab6)
plot(DataLab6$G,DataLab6$T, xlab = "Gas", ylab = "Tempature", pch = 16, col = "blue", main = "Relationship between Tempature & Gas")


(GTModel <- lm(DataLab6$G ~ DataLab6$T) )

abline(GTModel)
plot(DataLab6$T,DataLab6$G,abline(GTModel),xlab = "Gas", ylab = "Tempature", pch = 16, col = "blue", main = "Relationship between Tempature & Gas")

GTModel$fitted.values
GTModel$residuals

plot(GTModel$residuals~GTModel$fitted.values,xlab = "Tempature", ylab = "Residuals",pch = 16, col = "blue",main = "Residual Plot")
abline(0,0)

sum(GTModel$residuals)
cor(DataLab6$G, DataLab6$T)
