### Package ####################################################################

  library(dtms)

### Load results ###############################################################

  load("Results/over_time.Rda")


### For Table 2 ################################################################

  # Parts of the table
  men_exp <- men_res[men_res[,"education"]%in%c(0,2),][c(3,6,9,12),c("education","time","impaired")]
  men_ris <- men_res[men_res[,"education"]%in%c(0,2),][c(3,6,9,12),c("education","time","risk")]
  
  women_exp <- women_res[women_res[,"education"]%in%c(0,2),][c(3,6,9,12),c("education","time","impaired")]
  women_ris <- women_res[women_res[,"education"]%in%c(0,2),][c(3,6,9,12),c("education","time","risk")]
  
  men_exp_low <- summary(men_boot)$`2.5%`[men_res[,"education"]%in%c(0,2),][c(3,6,9,12),c("education","time","impaired")]
  men_exp_hig <- summary(men_boot)$`97.5%`[men_res[,"education"]%in%c(0,2),][c(3,6,9,12),c("education","time","impaired")]
  
  men_ris_low <- summary(men_boot)$`2.5%`[men_res[,"education"]%in%c(0,2),][c(3,6,9,12),c("education","time","risk")]
  men_ris_hig <- summary(men_boot)$`97.5%`[men_res[,"education"]%in%c(0,2),][c(3,6,9,12),c("education","time","risk")]
  
  women_exp_low <- summary(women_boot)$`2.5%`[women_res[,"education"]%in%c(0,2),][c(3,6,9,12),c("education","time","impaired")]
  women_exp_hig <- summary(women_boot)$`97.5%`[women_res[,"education"]%in%c(0,2),][c(3,6,9,12),c("education","time","impaired")]
  
  women_ris_low <- summary(women_boot)$`2.5%`[women_res[,"education"]%in%c(0,2),][c(3,6,9,12),c("education","time","risk")]
  women_ris_hig <- summary(women_boot)$`97.5%`[women_res[,"education"]%in%c(0,2),][c(3,6,9,12),c("education","time","risk")]

  # Editing
  men_exp <- reshape(as.data.frame(men_exp),direction="wide",idvar="education",timevar="time",v.names="impaired")
  men_exp_low <- reshape(as.data.frame(men_exp_low),direction="wide",idvar="education",timevar="time",v.names="impaired")
  men_exp_hig <- reshape(as.data.frame(men_exp_hig),direction="wide",idvar="education",timevar="time",v.names="impaired")
  
  men_ris <- reshape(as.data.frame(men_ris),direction="wide",idvar="education",timevar="time",v.names="risk")
  men_ris_low <- reshape(as.data.frame(men_ris_low),direction="wide",idvar="education",timevar="time",v.names="risk")
  men_ris_hig <- reshape(as.data.frame(men_ris_hig),direction="wide",idvar="education",timevar="time",v.names="risk")
  
  women_exp <- reshape(as.data.frame(women_exp),direction="wide",idvar="education",timevar="time",v.names="impaired")
  women_exp_low <- reshape(as.data.frame(women_exp_low),direction="wide",idvar="education",timevar="time",v.names="impaired")
  women_exp_hig <- reshape(as.data.frame(women_exp_hig),direction="wide",idvar="education",timevar="time",v.names="impaired")
  
  women_ris <- reshape(as.data.frame(women_ris),direction="wide",idvar="education",timevar="time",v.names="risk")
  women_ris_low <- reshape(as.data.frame(women_ris_low),direction="wide",idvar="education",timevar="time",v.names="risk")
  women_ris_hig <- reshape(as.data.frame(women_ris_hig),direction="wide",idvar="education",timevar="time",v.names="risk")
  
  # Combining (men)
  names(men_exp_low) <- c("education","impaired.1_low","impaired.2_low")
  names(men_exp_hig) <- c("education","impaired.1_hig","impaired.2_hig")
  
  names(men_ris_low) <- c("education","risk.1_low","risk.2_low")
  names(men_ris_hig) <- c("education","risk.1_hig","risk.2_hig")

  men_total <- merge(men_exp,men_exp_low)
  men_total <- merge(men_total,men_exp_hig)
  men_total <- merge(men_total,men_ris)
  men_total <- merge(men_total,men_ris_low)
  men_total <- merge(men_total,men_ris_hig)
  
  varorder <- c("education",
                "impaired.1","impaired.1_low","impaired.1_hig",
                "risk.1","risk.1_low","risk.1_hig",
                "impaired.2","impaired.2_low","impaired.2_hig",
                "risk.2","risk.2_low","risk.2_hig")
  
  men_total <- men_total[,varorder]
  men_total <- round(men_total,digits=2)
  rownames(men_total) <- c("Men low","Men high")
  men_total <- men_total[,-1]
  
  # Women
  names(women_exp_low) <- c("education","impaired.1_low","impaired.2_low")
  names(women_exp_hig) <- c("education","impaired.1_hig","impaired.2_hig")
  
  names(women_ris_low) <- c("education","risk.1_low","risk.2_low")
  names(women_ris_hig) <- c("education","risk.1_hig","risk.2_hig")
  
  women_total <- merge(women_exp,women_exp_low)
  women_total <- merge(women_total,women_exp_hig)
  women_total <- merge(women_total,women_ris)
  women_total <- merge(women_total,women_ris_low)
  women_total <- merge(women_total,women_ris_hig)

  women_total <- women_total[,varorder]
  women_total <- round(women_total,digits=2)
  
  rownames(women_total) <- c("Women low","Women high")
  women_total <- women_total[,-1]
  
  # Combine
  total <- rbind(men_total,women_total)
  
  
### For supplementary materials ################################################
  
  # Parts of the table
  men_exp <- men_res[men_res[,"education"]%in%c(0,2),][c(3,6,9,12),c("education","time","not.impaired")]
  men_ris <- men_res[men_res[,"education"]%in%c(0,2),][c(3,6,9,12),c("education","time","TOTAL")]
  
  women_exp <- women_res[women_res[,"education"]%in%c(0,2),][c(3,6,9,12),c("education","time","not.impaired")]
  women_ris <- women_res[women_res[,"education"]%in%c(0,2),][c(3,6,9,12),c("education","time","TOTAL")]
  
  men_exp_low <- summary(men_boot)$`2.5%`[men_res[,"education"]%in%c(0,2),][c(3,6,9,12),c("education","time","not.impaired")]
  men_exp_hig <- summary(men_boot)$`97.5%`[men_res[,"education"]%in%c(0,2),][c(3,6,9,12),c("education","time","not.impaired")]
  
  men_ris_low <- summary(men_boot)$`2.5%`[men_res[,"education"]%in%c(0,2),][c(3,6,9,12),c("education","time","TOTAL")]
  men_ris_hig <- summary(men_boot)$`97.5%`[men_res[,"education"]%in%c(0,2),][c(3,6,9,12),c("education","time","TOTAL")]
  
  women_exp_low <- summary(women_boot)$`2.5%`[women_res[,"education"]%in%c(0,2),][c(3,6,9,12),c("education","time","not.impaired")]
  women_exp_hig <- summary(women_boot)$`97.5%`[women_res[,"education"]%in%c(0,2),][c(3,6,9,12),c("education","time","not.impaired")]
  
  women_ris_low <- summary(women_boot)$`2.5%`[women_res[,"education"]%in%c(0,2),][c(3,6,9,12),c("education","time","TOTAL")]
  women_ris_hig <- summary(women_boot)$`97.5%`[women_res[,"education"]%in%c(0,2),][c(3,6,9,12),c("education","time","TOTAL")]
  
  # Editing
  men_exp <- reshape(as.data.frame(men_exp),direction="wide",idvar="education",timevar="time",v.names="not.impaired")
  men_exp_low <- reshape(as.data.frame(men_exp_low),direction="wide",idvar="education",timevar="time",v.names="not.impaired")
  men_exp_hig <- reshape(as.data.frame(men_exp_hig),direction="wide",idvar="education",timevar="time",v.names="not.impaired")
  
  men_ris <- reshape(as.data.frame(men_ris),direction="wide",idvar="education",timevar="time",v.names="TOTAL")
  men_ris_low <- reshape(as.data.frame(men_ris_low),direction="wide",idvar="education",timevar="time",v.names="TOTAL")
  men_ris_hig <- reshape(as.data.frame(men_ris_hig),direction="wide",idvar="education",timevar="time",v.names="TOTAL")
  
  women_exp <- reshape(as.data.frame(women_exp),direction="wide",idvar="education",timevar="time",v.names="not.impaired")
  women_exp_low <- reshape(as.data.frame(women_exp_low),direction="wide",idvar="education",timevar="time",v.names="not.impaired")
  women_exp_hig <- reshape(as.data.frame(women_exp_hig),direction="wide",idvar="education",timevar="time",v.names="not.impaired")
  
  women_ris <- reshape(as.data.frame(women_ris),direction="wide",idvar="education",timevar="time",v.names="TOTAL")
  women_ris_low <- reshape(as.data.frame(women_ris_low),direction="wide",idvar="education",timevar="time",v.names="TOTAL")
  women_ris_hig <- reshape(as.data.frame(women_ris_hig),direction="wide",idvar="education",timevar="time",v.names="TOTAL")
  
  # Combining (men)
  names(men_exp_low) <- c("education","not.impaired.1_low","not.impaired.2_low")
  names(men_exp_hig) <- c("education","not.impaired.1_hig","not.impaired.2_hig")
  
  names(men_ris_low) <- c("education","TOTAL.1_low","TOTAL.2_low")
  names(men_ris_hig) <- c("education","TOTAL.1_hig","TOTAL.2_hig")
  
  men_total <- merge(men_exp,men_exp_low)
  men_total <- merge(men_total,men_exp_hig)
  men_total <- merge(men_total,men_ris)
  men_total <- merge(men_total,men_ris_low)
  men_total <- merge(men_total,men_ris_hig)
  
  varorder <- c("education",
                "not.impaired.1","not.impaired.1_low","not.impaired.1_hig",
                "TOTAL.1","TOTAL.1_low","TOTAL.1_hig",
                "not.impaired.2","not.impaired.2_low","not.impaired.2_hig",
                "TOTAL.2","TOTAL.2_low","TOTAL.2_hig")
  
  men_total <- men_total[,varorder]
  men_total <- round(men_total,digits=2)
  rownames(men_total) <- c("Men low","Men high")
  men_total <- men_total[,-1]
  
  # Women
  names(women_exp_low) <- c("education","not.impaired.1_low","not.impaired.2_low")
  names(women_exp_hig) <- c("education","not.impaired.1_hig","not.impaired.2_hig")
  
  names(women_ris_low) <- c("education","TOTAL.1_low","TOTAL.2_low")
  names(women_ris_hig) <- c("education","TOTAL.1_hig","TOTAL.2_hig")
  
  women_total <- merge(women_exp,women_exp_low)
  women_total <- merge(women_total,women_exp_hig)
  women_total <- merge(women_total,women_ris)
  women_total <- merge(women_total,women_ris_low)
  women_total <- merge(women_total,women_ris_hig)
  
  women_total <- women_total[,varorder]
  women_total <- round(women_total,digits=2)
  
  rownames(women_total) <- c("Women low","Women high")
  women_total <- women_total[,-1]
  
  # Combine
  total <- rbind(men_total,women_total)
  