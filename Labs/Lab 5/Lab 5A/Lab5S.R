DataLab5 <- read.csv(file.choose())
DataLab5$Sex <- factor(DataLab5$Sex,levels = c(1,2),labels = c("Male","Female"))
DataLab5$Father_ED <- factor(DataLab5$Father_ED,levels = c(1,2,3,4,5,6,7),
                             labels = c("GRADE SCH","SOME HS", "HS GRAD", "SOME CLG", "CLG GRAD", "GRAD SCH", "DK"))
DataLab5$Mother_ED <- factor(DataLab5$Mother_ED,levels = c(1,2,3,4,5,6,7),
                             labels = c("GRADE SCH","SOME HS", "HS GRAD", "SOME CLG", "CLG GRAD", "GRAD SCH", "DK"))
DataLab5$Political_PRF <- factor(DataLab5$Political_PRF,levels = c(1,2,3,4,5,6,7,8),
                                 labels = c("STRG REP","MILD REP","MILD DOM","STRG DEMO","INDEP","NO PREF","Other","DK"))
DataLab5$Relgion_Imp <- factor(DataLab5$Relgion_Imp,levels = c(1,2,3,4),
                               labels = c("Not Impor", "Little Impor", "Pretty Impor", "Very Impor"))
DataLab5$Ticket <- factor(DataLab5$Ticket,levels = c(0,1,2,3,4),
                          labels = c("None","Once","Twice","3 Times","4+ Times"))


DataLab5$Drive <- factor(DataLab5$Drive, levels = c(1,2,3,4,5,6),
                         labels = c("No Mileage Driven","1-10 Miles","11-50 Miles","51-100 Miles","101-200 Miles",">200 Miles"))                         
save(DataLab5, file = "DataLab5_1.RData")

load(file.choose())

Sex_Table <- table(DataLab5$Sex)
Sex_Table
addmargins(Sex_Table)
Sex_Table_Per <- prop.table(Sex_Table)*100
Sex_Table_Per

Two_Way_Table_Sex_Ticket <- table(DataLab5$Sex,DataLab5$Ticket)
Two_Way_Table_Sex_Ticket

options(digits = 3)
Two_Way_Table_Sex_Ticket_Row_Percentage <- prop.table(Two_Way_Table_Sex_Ticket)
Two_Way_Table_Sex_Ticket_Row_Percentage*100

SexTicket_Row <- prop.table(Two_Way_Table_Sex_Ticket,1)*100

SexTicket_Column <- prop.table(Two_Way_Table_Sex_Ticket,2)*100
SexTicket_Row
SexTicket_Column

Two_Way_Table_Drive_Ticket <- table(DataLab5$Drive,DataLab5$Ticket)
Two_Way_Table_Drive_Ticket
addmargins(Two_Way_Table_Drive_Ticket)
Xsq <- chisq.test(Two_Way_Table_Drive_Ticket, correct = FALSE)
Xsq
options(digits = 4)
(Two_Way_Table_Drive_Ticket_Row_Percentage <- prop.table(Two_Way_Table_Drive_Ticket,1)*100)
options(digits = 4)
Two_Way_Table_Drive_Ticket_Row_Percentage*100
Two_Way_BP <- barplot(Two_Way_Table_Drive_Ticket_Row_Percentage, beside = TRUE, xlab = "Driving Distance", ylab = "Percent",
         col = c("slateblue","slateblue1","slateblue2","slateblue3","slateblue4","lightblue"))
legend("topright",legend = rownames(Two_Way_Table_Drive_Ticket_Row_Percentage),fill = c("slateblue","slateblue1","slateblue2","slateblue3","slateblue4","lightblue"),
       title = "Driving Distance on Tickets Recieved",cex = .8)
text(Two_Way_BP,4,round(Two_Way_Table_Drive_Ticket_Row_Percentage=0),cex=.7,pos=1)

Two_Way_BP <- barplot(Two_Way_Table_Drive_Ticket_Row_Percentage, beside = TRUE, xlab = "Driving Distance", ylab = "Percent",
                      col = c("slateblue","slateblue1","slateblue2","slateblue3","slateblue4","lightblue"),
                      main = "Conditional Distribution of Tickets by Driving Distance",ylim = c(0,100))
legend("topright",legend = rownames(Two_Way_Table_Drive_Ticket_Row_Percentage),fill = c("slateblue","slateblue1","slateblue2","slateblue3","slateblue4","lightblue"),title = "Driving Distance", cex = .8)
text(Two_Way_BP+1, Two_Way_Table_Drive_Ticket_Row_Percentage+2.5, paste(round(Two_Way_Table_Drive_Ticket_Row_Percentage,0),"%",sep=""),cex=.1)
