Sex <- c("Female","Male","Female","Male","Female","Female")
Age <- c(53,64,59,61,26,44)
EmpStatus <- c("Employed","Retired","Employed","Employed","Employed","Employed")
SpendFood <- c(4,500,500,100,NA,120)
Food <- data.frame(Sex,Age,EmpStatus,SpendFood)
Food
str(Food)
Food$Sex
Table_Sex <- table(Food$Sex)
Table_Sex
barplot(Table_Sex)
pie(Table_Sex)
Table_Sex <- table(Food$Sex)
Table_Sex
(Table_Sex <- table(Food$Sex))
(Table_EmpStatus <- table(Food$EmpStatus))

FoodSurvey <- read.csv(file.choose())
head(FoodSurvey)
FoodSurvey$PESEX <- factor(FoodSurvey$PESEX,levels = c(1,2),labels = c("Male","Female"))
hist(FoodSurvey$PRTAGE,xlab = "Respondent's Age",col = "royalblue",main = "Histogram of Food Survey")
boxplot(FoodSurvey$PRTAGE,xlab = "Age of Respondents",col = "pink",horizontal = TRUE,main = "Box Plot of Food Survey")
qqnorm(FoodSurvey$PRTAGE)
qqline(FoodSurvey$PRTAGE,col = "red")
summary(FoodSurvey$HETS8OU)
FoodSurvey$HETS8OU[FoodSurvey$HETS8OU<0]=NA
hist(FoodSurvey$HETS8OU,xlab = "Amount respondants spent on food per week",col = "royalblue",main = "Histogram of Amount Respondants spent on food per week")
boxplot(FoodSurvey$HETS8OU,xlab = "$ Respondents spent on food per week",col = "pink",horizontal = TRUE,main = "amount of $ respondents spent on food per week")
summary(FoodSurvey$HETS8OU)
qqnorm(FoodSurvey$HETS8OU)
qqline(FoodSurvey$HETS8OU,col = "red")

head(FoodSurvey$HESSM3)
FoodSurvey$HESSM3 <- factor(FoodSurvey$HESSM3,levels = c(1,2),labels = c("Yes","No"))
Table_notlast <- table(FoodSurvey$HESSM3)
Table_notlast_percent <- prop.table(Table_notlast)*100
Table_notlast_percent
pie(Table_notlast_percent)
barplot(Table_notlast_percent)
Table_Sex_notlast <- table(FoodSurvey$PESEX, FoodSurvey$HESSM3)
(Table_Sex_notlast_RowP <- prop.table(Table_Sex_notlast,1)*100)
barplot(Table_Sex_notlast_RowP, xlab = "Response" , ylab = "Male Responses (grey) Female Responses (Black)" )
Table_GREG_notlast <- table(FoodSurvey$GEREG,FoodSurvey$GEREG) 
(Table_GEREG_notlast_RowP <- prop.table(Table_GREG_notlast,1)*100)
dimnames(Table_GEREG_notlast_RowP) <- list(Regions=c("Northeast", "Midwest", "South", "West"), Voted=c("Yes", "No", "Yes","No"))
Table_GEREG_notlast_RowP                                          
barplot(Table_GEREG_notlast_RowP, xlab = "Regions",ylab = "Percent")
