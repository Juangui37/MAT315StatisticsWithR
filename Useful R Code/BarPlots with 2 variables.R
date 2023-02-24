TD <- as.table(rbind(c(151,476),c(212,673)))
TD
dimnames(TD) <- list(Class = c("Third","Crew"),Status = c("Saved","Lost"))
TD
TD_Margin <- addmargins(TD)
TD_Margin
TD_Prop <- prop.table(TD,1)
TD_Prop
options(digits = 3)
TD_Prop <- prop.table(TD,1)
TD_Prop
(Table_Row_Perecent <- TD_Prop*100)
barplot(TD_Prop)
barplot(TD_Prop, beside = TRUE)
barplot(TD_Prop, beside = TRUE,xlab = "Status",legend = row.names(TD),col = c("skyblue","pink"))
(Table_Column_Perecent <- prop.table(TD,2)*100)
(Table_Joint_Perecent <- prop.table(TD)*100)
