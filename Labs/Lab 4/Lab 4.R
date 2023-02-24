(TableLabQ1 <- as.table(rbind(c(57,64),c(227,163),c(271,93))))
dimnames(TableLabQ1) <- list(Asset_Size = c("Under $100","$101-$200","$201 Or More"),
                    Offer_RDC = c("Yes","No"))
TableLabQ1

addmargins(TableLabQ1)
Xsq <- chisq.test(TableLabQ1)
Xsq$expected
Xsq <- chisq.test(TableLabQ1, correct = FALSE)
Xsq

(TableLab4Q1_Row <- prop.table(R,1)*100)
barplot(TableLab4Q1_Row, beside = TRUE, xlab = "Offer RDC",ylab = "Percent",
        legend.text = rownames(TableLab4Q1_Row),col = c("coral1","coral3","coral4"),ylim = c(0,100))


(TableLabQ2 <- as.table(rbind(c(57,64),c(227,163),c(271,93),c(303,51))))
dimnames(TableLabQ2) <- list(Highest_Educational_Attainment = c("Not High School Graduate"
                                                       ,"High School Grad/No College","Some College/Ass. Deg","Bachelors Degree or Higher"),
                    Voted_Nov_2012 = c("Yes","No"))
TableLabQ2
addmargins(TableLabQ2)
Xsq <- chisq.test(TableLabQ2, correct = FALSE)
Xsq
(TableLabQ2_Row <-prop.table(TableLabQ2,1)*100 )
SchoolBarPlot <- barplot(TableLabQ2_Row, beside = TRUE , xlab = "Voted In Nov 2012", ylab = "Perecent",col = c("Blue","Grey","Maroon","White"),
                         main = "Conditional Distribution of Voting on Novemeber 2012 by Educational Attainment",
                         ylim = c(0,105))
legend("topright", legend = rownames(TableLabQ2_Row), fill = c("Blue","Grey","Maroon","White"), title = "Educational Attainment",cex = .8)
                
        
text(SchoolBarPlot+.05,TableLabQ2_Row+2.5, paste(round(TableLabQ2_Row,0),"%",sep=""),cex=.7)
