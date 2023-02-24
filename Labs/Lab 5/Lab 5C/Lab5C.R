DataLab5C <- read.csv(file.choose())

DataLab5C$X2019_DATA <- factor(DataLab5C$X2019_DATA, levels = c(0,1), labels = c("No","Yes") )
DataLab5C$X2018_DATA <- factor(DataLab5C$X2018_DATA, levels = c(0,1), labels = c("No","Yes") )
DataLab5C$X2017_DATA <- factor(DataLab5C$X2017_DATA, levels = c(0,1), labels = c("No","Yes") )
DataLab5C$X2016_DATA <- factor(DataLab5C$X2016_DATA, levels = c(0,1), labels = c("No","Yes") )
DataLab5C$X2015_DATA <- factor(DataLab5C$X2015_DATA, levels = c(0,1), labels = c("No","Yes") )
DataLab5C$X2014_DATA <- factor(DataLab5C$X2014_DATA, levels = c(0,1), labels = c("No","Yes") )
DataLab5C$X2013_DATA <- factor(DataLab5C$X2013_DATA, levels = c(0,1), labels = c("No","Yes") )

save(DataLab5C, file = "DataLab5_3.RData")

load(file.choose())

Table_2019 <- table(DataLab5C$X2019_DATA)
(Table_2019_P <- prop.table(Table_2019)*100)


Table_2018 <- table(DataLab5C$X2018_DATA)
(Table_2018_P <- prop.table(Table_2018)*100)

Table_2017 <- table(DataLab5C$X2017_DATA)
(Table_2017_P <- prop.table(Table_2017)*100)

Table_2016 <- table(DataLab5C$X2016_DATA)
(Table_2016_P <- prop.table(Table_2016)*100)

Table_2015 <- table(DataLab5C$X2015_DATA)
(Table_2015_P <- prop.table(Table_2015)*100)

Table_2014 <- table(DataLab5C$X2014_DATA)
(Table_2014_P <- prop.table(Table_2014)*100)

Table_2013 <- table(DataLab5C$X2013_DATA)
(Table_2013_P <- prop.table(Table_2013)*100)

x = c(2013,2014,2015,2016,2017,2018,2019)

yNo = c(Table_2013_P[1],Table_2014_P[1],
        Table_2015_P[1],Table_2016_P[1],
        Table_2017_P[1],Table_2018_P[1],
        Table_2019_P[1])

yYes = c(Table_2013_P[2],Table_2014_P[2],
         Table_2015_P[2],Table_2016_P[2],
         Table_2017_P[2],Table_2018_P[2],
         Table_2019_P[2])

plot(x, yYes, type = "b", pch = 19, col = "blue", xlab = "Year", ylab = "Percent", main = "Percent of Drinking Responses From 2013-2019")

plot(x, yNo, type = "b", pch = 19, col = "red", xlab = "Year", ylab = "Percent", main = "Percent of Non-Drinking Responses From 2013-2019")

plot(x, yYes, type = "b", pch = 19, col = "blue",
     xlab = "Year",ylab = "Percent",main = "Percent Over Time",
     xlim = c(2012.5,2020.5),ylim = c(0,100))
par(new = TRUE)
plot(x, yNo, type = "b", pch = 19, col = "red",
     xlab = "Year",ylab = "Percent",main = "Percent Over Time",
     xlim = c(2012.5,2020.5),ylim = c(0,100))
par(new = TRUE)
legend("topleft",legend = c("Drinkers","Non-Drinkers"),fill = c("blue","red"))



plot(x, yYes, type = "b", pch = 19, col = "blue",
     xlab = "Year",ylab = "Percent",
     main = "Percent Over Time", xlim = c(2013, 2019),
     xaxt = "n",
     ylim = c(0,100))
par(new = TRUE)
plot(x, yNo, type = "b", pch = 19, col = "red",
     xlab = "Year",ylab = "Percent",
     main = "Percent Over Time", xlim = c(2013,2019),
     xaxt = "n",
     ylim = c(0,100))
par(new = FALSE)
legend("topleft",legend = c("Drinkers","Non-Drinkers"),fill = c("blue","red"))
axis(1, at = 2013:2019)


