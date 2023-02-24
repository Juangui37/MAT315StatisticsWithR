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
