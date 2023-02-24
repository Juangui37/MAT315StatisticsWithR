load(file.choose())

DataLab5$Sex
DataLab5$Drive
DataLab5$Sex

(Sex_Driving <- table(DataLab5$Sex,DataLab5$Drive))
addmargins(Sex_Driving)
Xsq <- chisq.test(Sex_Driving, correct = FALSE)
Xsq
options(digits = 4)
(Sex_Driving_Row_Per <- prop.table(Sex_Driving,1)*100)
Sex_Drive_BP <- barplot(Sex_Driving_Row_Per, beside = TRUE, xlab = "Driving Distance", ylab = "Percent",
                        col = c("Blue","hotPink"),
                        main = "Conditional Distribution of Sex by Driving Distance",ylim = c(0,40))
legend("topright",legend = rownames(Sex_Driving_Row_Per),fill = c("Blue","Hotpink"),title = "Sex", cex = 1)
text(Sex_Drive_BP, Sex_Driving_Row_Per+1, paste(round(Sex_Driving_Row_Per, digits = 2),"%",sep=""),cex=1)

(Religion_Politics <- table(DataLab5$Relgion_Imp,DataLab5$Political_PRF))

(Religion_Politics_Row_Perc <- prop.table(Religion_Politics,1)*100)
Religion_Politics_BP <- barplot(Religion_Politics_Row_Perc, beside = TRUE,xlab = "Political Preference", ylab = "Perecent",
                                col = c("Gray","Maroon","White","NavyBlue"),
                                main = "Conditional Distribution of Religion Importance by Political Preference", ylim = c(0,30))
legend("topleft",legend = rownames(Religion_Politics_Row_Perc),fill = c("Gray","Maroon","White","NavyBlue"),title = "Religion Preference", cex = .7)
text(Religion_Politics_BP, Religion_Politics_Row_Perc+.5, paste(round(Religion_Politics_Row_Perc),"%",sep=""),cex=.8)

PPref <- vector(mode = "character", length = length(DataLab5$Political_PRF))
head(PPref)
PPref[DataLab5$Political_PRF=="STRG REP"] <- "GOP"
PPref[DataLab5$Political_PRF=="MILD REP"] <- "GOP"
PPref[DataLab5$Political_PRF=="MILD DOM"] <- "DEM"
PPref[DataLab5$Political_PRF=="STRG DOM"] <- "DEM"
PPref[DataLab5$Political_PRF=="INDEP"] <- "IND/OTH"
PPref[DataLab5$Political_PRF=="Other"] <- "IND/OTH"
PPref[DataLab5$Political_PRF=="NO PREF"] <- "NO PREF/DK"
PPref[DataLab5$Political_PRF=="DK"] <- "NO PREF/DK"

table(PPref)

PPref <- factor(PPref, level = c("GOP", "DEM","IND/OTH","NO PREF/DK"), ordered = TRUE)
table(PPref)


DataLab5 <- cbind(DataLab5, PPref)
head(DataLab5)

save(DataLab5, file = "Datalab5_2.RData")


DataLab5$PPref
DataLab5$Relgion_Imp

(PPref_Rimp <- table(DataLab5$PPref, DataLab5$Relgion_Imp))
(addmargins(PPref_Rimp))
(PPref_Rimp_Row <- prop.table(PPref_Rimp,2)*100)
PPref_Rimp_BP <- barplot(PPref_Rimp_Row, beside = TRUE, xlab = "Religion", ylab = "Percent",
                        col = c("Gray","Maroon","White","NavyBlue"),
                        main = "Conditional Distribution of Political Preference for Religious Importance",ylim = c(0,50))
legend("topright",legend = rownames(PPref_Rimp_Row),fill = c("Gray","Maroon","White","NavyBlue"),title = "Political Preference", cex = .5)
text(PPref_Rimp_BP, PPref_Rimp_Row+.9, paste(round(PPref_Rimp_Row, digits = 2),"%",sep=""),cex=.8)

(Rimp_PPRef <- table(DataLab5$Relgion_Imp,DataLab5$PPref))
(Rimp_PPRef_Row <- prop.table(Rimp_PPRef,2)*100)
Rimp_PPref_BP <- barplot(Rimp_PPRef_Row, beside = TRUE, xlab = "Political Preference", ylab = "Percent",
                         col = c("Gray","Maroon","White","NavyBlue"),
                         main = "Conditional Distribution of Religious Importance for Political Preference",ylim = c(0,40))
legend("topright",legend = rownames(Rimp_PPRef_Row),fill = c("Gray","Maroon","White","NavyBlue"),title = "Religion Importance", cex = .8)
text(Rimp_PPref_BP, Rimp_PPRef_Row+.9, paste(round(Rimp_PPRef_Row, digits = 2),"%",sep=""),cex=.8)

