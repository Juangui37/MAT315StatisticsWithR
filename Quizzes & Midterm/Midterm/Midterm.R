CANFREG <- read.csv(file.choose())
head(CANFREG)
hist(CANFREG$CO2,xlab = "CO2 Emissions",col = "royalblue",main = "CO2 Emissions")
boxplot(CANFREG$CO2,xlab = "CO2 Emissions",col = "pink",horizontal = TRUE,main = "Box Plot of CO2 Emissions")
summary(CANFREG$CO2)
mean(CANFREG$CO2)
sd(CANFREG$CO2)
IQR(CANFREG$CO2)


Exam <- read.csv(file.choose())
load(file.choose())
MidMTF$Region <- factor(MidMTF$Region,levels = c(1,2,3,4),
                        labels = c("NorthEast","North Central","South", "West"))

MidMTF$Sex <- factor(MidMTF$Sex, levels = c(1,2),
                     labels = c("Males, Females"))
MidMTF$Smoke <- factor(MidMTF$Smoke, levels = c(1,2,3),
                       labels = c("Never","Occ","Reg"))

save(MidMTF, file = "Exam.RData")                        



(Smoking_Table <- table(MidMTF$Smoke))
(Smoking_Table_Per <- prop.table(Smoking_Table)*100)
Smoking_BP <- barplot(Smoking_Table_Per, beside = TRUE, xlab = "Have You Ever Smoked Cigarettes?", ylab = "Percent",
                      col = c("slateblue","slateblue3","lightblue"), ylim = c(0,70), main = "Percentage of Students Who smoke Cigarettes")
text(Smoking_BP,68,round(Smoking_Table_Per),cex=.8,pos=2)

(Two_Way_Table <- table(MidMTF$Region,MidMTF$Smoke))

(Xsq <- chisq.test(Two_Way_Table, correct = FALSE))

(Two_Way_Table_Row_Perc <- prop.table(Two_Way_Table,1)*100)


pnorm(185, mean = 150, sd = 35) - pnorm(80, mean = 150, sd = 35)


CANFREG <- read.csv(file.choose())
head(CANFREG)
plot(CANFREG$FuelConsHwy, CANFREG$CO2)

plot(CANFREG$FuelConsHwy,CANFREG$CO2, xlab = "HighWay Fuel Consumption", ylab = "CO2", main = "Relationship between HighWay Fuel Consumption & CO2")
cor(CANFREG$FuelConsHwy,CANFREG$CO2)
(HighWayCO2Model <- lm(CANFREG$CO2~CANFREG$FuelConsHwy))

abline(HighWayCO2Model)
plot(CANFREG$FuelConsHwy,CANFREG$CO2, abline(HighWayCO2Model),  xlab = "HighWay Fuel Consumption", ylab = "CO2",
     main = "Relationship between HighWay Fuel Consumption & CO2")
summary(HighWayCO2Model)
summary(CANFREG$FuelConsHwy)
summary(CANFREG$CO2)


HighWayCO2ModelModel$residuals
HighWayCO2ModelModel$fitted.values

plot(HighWayCO2ModelModel$residuals~HighWayCO2ModelModel$fitted.values,xlab = "HighWay Fuel Consumption", ylab = "CO2",pch = 16, col = "blue",main = "Residual Plot")
abline(0,0)

plot(HighWayCO2ModelModel$residuals~CANFREG$HwyMPG,xlab = "HighWayFuel Consumption", ylab = "Residuals")
abline(0,0)
