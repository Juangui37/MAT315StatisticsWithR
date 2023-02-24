Proj <- read.csv(file.choose())

Proj$Smoked_Before <- factor(Proj$Smoked_Before,levels = c(1,2,3,4,5),
                             labels = c("Never","1-2X","Occasionally","REG Past","REG NOW"))

Proj$Race <- factor(Proj$Race,levels = c(1,2,3), labels = c("Black","White","Hispanic"))

Proj$HS_Grade_AVG <- factor(Proj$HS_Grade_AVG,levels = c(1,2,3,4,5,6,7,8,9),
                             labels = c("D","C-","C","C+","B-","B","B+","A-","A"))

(Two_Way_smoke_Grade <- table(Proj$Smoked_Before, Proj$HS_Grade_AVG))
(Two_way_Smoke_race <- table(Proj$Smoked_Before,Proj$Race))

Xsq <- chisq.test(Two_way_Smoke_race, correct = FALSE)
Xsq

Xsq <- chisq.test(Two_Way_smoke_Grade, correct = FALSE)
Xsq

Grade <- vector(mode = "character", length = length(Proj$HS_Grade_AVG))
head(Grade)
Grade[Proj$HS_Grade_AVG=="D"] <- "D"
Grade[Proj$HS_Grade_AVG=="C-"] <- "C"
Grade[Proj$HS_Grade_AVG=="C"] <- "C"
Grade[Proj$HS_Grade_AVG=="C+"] <- "C"
Grade[Proj$HS_Grade_AVG=="B-"] <- "B"
Grade[Proj$HS_Grade_AVG=="B"] <- "B"
Grade[Proj$HS_Grade_AVG=="B+"] <- "B"
Grade[Proj$HS_Grade_AVG=="A-"] <- "A"
Grade[Proj$HS_Grade_AVG=="A"] <- "A"
Grade <- factor(Grade, level = c("D", "C","B","A"), ordered = TRUE)
table(Grade)

Proj <- cbind(Proj,Grade)

head(Proj)

Two_Way_smoked_Grade <- table(Proj$Smoked_Before,Proj$Grade)


Two_Way_smoked_Grade
