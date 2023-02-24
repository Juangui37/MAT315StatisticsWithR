MEL=GroupData$Mother_EDU_LVL
MEL[MEL<0]=NA
Table_MEL_CV <- table(MEL,CigVape)
(Xsq_MEL_CV <- chisq.test(Table_MEL_CV,correct=FALSE))
RI=GroupData$Relgion_IMP
RI[RI<0]=NA
Table_RI_CV <- table(RI,CigVape)
(Xsq_RI_CV <- chisq.test(Table_RI_CV,correct=FALSE))
PP=GroupData$Political_PREF
PP[PP<0]=NA
Table_PP_CV <- table(PP,CigVape)
(Xsq_PP_CV <- chisq.test(Table_PP_CV,correct=FALSE))
Race=GroupData$Race
Race[Race<0]=NA
Table_R_CV <- table(Race,CigVape)
(Xsq_R_CV <- chisq.test(Table_R_CV,correct=FALSE))
SC=GroupData$Skipped_Class
SC[SC<0]=NA
Table_SC_CV <- table(SC,CigVape)
(Xsq_SC_CV <- chisq.test(Table_SC_CV,correct=FALSE))
SC <- factor(SC,levels=c(1,2,3,4,5,6),labels=c("None","1-2","3-5","6-10","11-20","20+"))
PP <- factor(PP,levels=c(1,2,3,4,5,6,7,8),labels=c("Strong Republican","Mildly Republican","Mildly Dem","Strong Dem","Indep","N/P","Other","DK"))
RI <- factor(RI,levels=c(1,2,3,4),labels=c("Not","Little","Pretty","Very"))
Race <- factor(Race,levels=c(1,2,3),labels=c("Black","White","Hispanic"))
