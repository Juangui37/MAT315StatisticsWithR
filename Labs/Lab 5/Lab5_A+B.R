DataLab5 <- read.csv(file.choose())
DataLab5$Sex <- factor(DataLab5$Sex,levels = c(1,2),labels = c("Male","Female"))
DataLab5$Father_ED <- factor(DataLab5$Father_ED,levels = c(1,2,3,4,5,6,7),
                             labels = c("GRADE SCH","SOME HS", "HS GRAD", "SOME CLG", "CLG GRAD", "GRAD SCH", "DK"))
DataLab5$Mother_ED <- factor(DataLab5$Mother_ED,levels = c(1,2,3,4,5,6,7),
                             labels = c("GRADE SCH","SOME HS", "HS GRAD", "SOME CLG", "CLG GRAD", "GRAD SCH", "DK"))
DataLab5$Political_PRF <- factor(DataLab5$Political_PRF,levels = c(1,2,3,4,5,6,7,8),
                                 labels = c("STRONG REPUBLICAN","MILD REPUBLICAN","MILD DEMOCRAT","STRONG DEMOCRAT","INDEPENDENT","NO PREFERENCE","Other","DK"))
DataLab5$Relgion_Imp <- factor(DataLab5$Relgion_Imp,levels = c(1,2,3,4),
                               labels = c("Not Impor", "Little Impor", "Pretty Impor", "Very Impor"))
DataLab5$Ticket <- factor(DataLab5$Ticket,levels = c(0,1,2,3,4),
                          labels = c("None","Once","Twice","3 Times","4+ Times"))


DataLab5$Drive <- factor(DataLab5$Drive, levels = c(1,2,3,4,5,6),
                         labels = c("No Mileage Driven","1-10 Miles","11-50 Miles","51-100 Miles","101-200 Miles",">200 Miles"))                         
save(DataLab5, file = "DataLab5_0.RData")

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


load(file.choose())

(Sex_Driving <- table(DataLab5$Sex,DataLab5$Drive))
addmargins(Sex_Driving)
Xsq <- chisq.test(Sex_Driving, correct = FALSE)
Xsq
(Sex_Driving_Row_Per <- prop.table(Sex_Driving,1)*100)
Sex_Drive_BP <- barplot(Sex_Driving_Row_Per, beside = TRUE, xlab = "Driving Distance", ylab = "Percent",
                        col = c("Blue","hotPink"),
                        main = "Conditional Distribution of Sex by Driving Distance",ylim = c(0,40))
legend("topright",legend = rownames(Sex_Driving_Row_Per),fill = c("Blue","Hotpink"),title = "Sex", cex = 1)
text(Sex_Drive_BP, Sex_Driving_Row_Per+1, paste(round(Sex_Driving_Row_Per, digits = 2),"%",sep=""),cex=1)

(POLITICS_RELIGION <- table(DataLab5$Political_PRF,DataLab5$Relgion_Imp))

(POLITICS_RELIGION_Row_Perc <- prop.table(POLITICS_RELIGION,2)*100)
POLITICS_RELIGION_BP <- barplot(POLITICS_RELIGION_Row_Perc, beside = TRUE,xlab = "Political Preference", ylab = "Perecent",
                                col = c("Gray","Maroon","White","NavyBlue"),
                                main = "Conditional Distribution of Religion Importance by Political Preference", ylim = c(0,50))
legend("topleft",legend = rownames(POLITICS_RELIGION_Row_Perc),fill = c("Gray","Maroon","White","NavyBlue"),title = "Religion Preference", cex = .7)
text(POLITICS_RELIGION_BP, POLITICS_RELIGION_Row_Perc+.5, paste(round(POLITICS_RELIGION_Row_Perc),"%",sep=""),cex=.8)

PPref <- vector(mode = "character", length = length(DataLab5$Political_PRF))
head(PPref)
PPref[DataLab5$Political_PRF=="STRONG REPUBLICAN"] <- "GOP"
PPref[DataLab5$Political_PRF=="MILD REPUBLICAN"] <- "GOP"
PPref[DataLab5$Political_PRF=="MILD DEMOCRAT"] <- "DEM"
PPref[DataLab5$Political_PRF=="STRONG DEMOCRAT"] <- "DEM"
PPref[DataLab5$Political_PRF=="INDEPENDENT"] <- "IND/OTH"
PPref[DataLab5$Political_PRF=="Other"] <- "IND/OTH"
PPref[DataLab5$Political_PRF=="NO PREFERENCE"] <- "NO PREF/DK"
PPref[DataLab5$Political_PRF=="DK"] <- "NO PREF/DK"

table(PPref)

PPref <- factor(PPref, level = c("GOP", "DEM","IND/OTH","NO PREF/DK"), ordered = TRUE)
table(PPref)


DataLab5 <- cbind(DataLab5, PPref)
head(DataLab5)

save(DataLab5, file = "Datalab5_01.RData")

load(file.choose())

DataLab5$PPref
DataLab5$Relgion_Imp

(religion_pprf <- table(DataLab5$PPref, DataLab5$Relgion_Imp))
(religion_pprf_Row <- prop.table(religion_pprf,1)*100)
religion_pprf_BP <- barplot(religion_pprf_Row, beside = TRUE, xlab = "Religion", ylab = "Percent",
                         col = c("Gray","Maroon","White","NavyBlue"),
                         main = "Conditional Distribution of Political Preference on Religion Importance",ylim = c(0,50))
legend("topright",legend = rownames(religion_pprf_Row),fill = c("Gray","Maroon","White","NavyBlue"),title = "Political Preference", cex = .8)
text(religion_pprf_BP, religion_pprf_Row+1.5, paste(round(religion_pprf_Row, digits = 2),"%",sep=""),cex=.8)

(pprf_religion <- table(DataLab5$Relgion_Imp,DataLab5$PPref))
(pprf_religion_Row <- prop.table(pprf_religion,2)*100)
pprf_religion_BP <- barplot(pprf_religion_Row, beside = TRUE, xlab = "Political Preference", ylab = "Percent",
                         col = c("Gray","Maroon","White","NavyBlue"),
                         main = "Conditional Distribution of Religion Importance on Political Preference",ylim = c(0,60))
legend("topright",legend = rownames(pprf_religion_Row),fill = c("Gray","Maroon","White","NavyBlue"),title = "Religion Importance", cex = .8)
text(pprf_religion_BP, pprf_religion_Row+3, paste(round(pprf_religion_Row),"%",sep=""),cex=.8)

