LabQuiz3 <- read.csv(file.choose())
LabQuiz3$MoEd <- factor(LabQuiz3$MoEd,levels = c(1,2,3),labels = c("Not HS Grad","HS Grad","College Grad"))
LabQuiz3$SkipClass <- factor(LabQuiz3$SkipClass,levels = c(1,2,3,4),labels = c("None","1-2X","3-5X","6 Or More"))

(MoEdTable <- table(LabQuiz3$MoEd))
(SkipClassTable <- table(LabQuiz3$SkipClass))

(MoEd_Skip_Table <- table(LabQuiz3$SkipClass, LabQuiz3$MoEd))

Xsq <- chisq.test(MoEd_Skip_Table, correct = FALSE)
Xsq

(MoEd_Skip_Table_Row <- prop.table(MoEd_Skip_Table,2)*100)

MoEd_Skip_Table_BP <- barplot(MoEd_Skip_Table_Row, beside = TRUE, xlab = "Mother Educational Level", ylab = "Percent",
                        col = c("Grey","Blue","Maroon","White"),
                        main = "Conditional Distribution of # of Classes Skipped by Mother Educational Level",ylim = c(0,100))
legend("topright",legend = rownames(MoEd_Skip_Table_Row),fill = c("Grey","Blue","Maroon","White"),
       title = "# of Classes Skipped",cex = .8)
text(MoEd_Skip_Table_BP, MoEd_Skip_Table_Row+4, paste(round(MoEd_Skip_Table_Row,0),"%",sep=""),cex=.8)

