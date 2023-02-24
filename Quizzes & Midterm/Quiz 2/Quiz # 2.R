QuizTable <- as.table(rbind(c(140,192,671),c(106,125,732)))
QuizTable
dimnames(QuizTable) <- list(Gender = c("Girls","Boys"),Witnessed = c("Never","Once","More Than Once"))
QuizTable
addmargins(QuizTable)

QuizTable_Row_Perc <- prop.table(QuizTable,1)
(QuizTable_Row_Perc <- prop.table(QuizTable,1)*100)
(QuizTable_Row_Perc <- prop.table(QuizTable,2)*100)

Xsq <- chisq.test(QuizTable, correct = FALSE)
Xsq

barplot(QuizTable_Row_Perc, beside = TRUE, xlab = "Gender",ylab = "Percent",
        legend.text = rownames(QuizTable_Row_Perc),col = c("coral1","coral3"))
Xsq$expected
