Vape <- GroupData$Ever_Vaped_Nic
head(Vape)
Vape[Vape<0]=NA
Vape=Vape-1
table(Vape)
Cig <- GroupData$Ever_Smoked
Cig[Cig<0]=NA
table(Cig)
CigVape <- Cig+Vape
table(CigVape)
Table_Vape_Cig <- table(Vape,Cig)
Table_Vape_Cig
Vape <- factor(Vape,levels=c(0,1,2,3,4,5),
               labels=c("No","Yes","Yes","Yes","Yes","Yes"))
table(Vape)
Cig <- factor(Cig,levels=c(0,1),labels=c("No","Yes"))
table(Cig)
table(Cig,Vape)
table(CigVape)
CigVape <- factor(CigVape,levels=c(0,1,2,3,4,5,6),
                  labels=c("No","Yes","Yes","Yes","Yes","Yes","Yes"))
table(CigVape)
