### Packages ###################################################################

  library(dtms)
  library(tidyverse)
  library(lmtest)


### Load data ##################################################################

  load("Data/hrs_edited.Rda")


### Setup dtms #################################################################

  # Note: variable step length
  hrsdtms <- dtms(transient=c("not impaired","impaired"),
                  absorbing="dead",
                  timescale=seq(50,98,1),
                  timestep=1:3)


### Reshape and edit data some more ############################################    

  # Reshape 
  estdata <- dtms_format(data=hrs,
                         dtms=hrsdtms,
                         idvar="id",
                         timevar="age",
                         statevar="state")

  # Clean
  estdata <- dtms_clean(data=estdata,dtms=hrsdtms)

  # Edit/add variables
  estdata$time2 <- estdata$time^2
  estdata$education <- as.factor(estdata$education)


### Subsets by gender ##########################################################

  men <- estdata |> filter(gender==1)
  women <- estdata |> filter(gender==2)
  
  
### Sample size ################################################################
  
  # No of transitions
  dim(men)[1]+dim(women)[1]
  
  # No of individuals
  nmen <- men |> pull(id) |> unique() |> length()
  nwomen <- women |> pull(id) |> unique() |> length()
  nmen+nwomen

  
### Comparison full vs reduced #################################################

  # Controls
  controls <- c("time","time2")  
    
  # Model, reduced
  fit1m <- dtms_fit(data=men,controls=controls,package="mclogit")
  fit1w <- dtms_fit(data=women,controls=controls,package="mclogit")
  
  # Model, full
  full1m <- dtms_fullfit(data=men,controls=controls,package="mclogit")
  full1w <- dtms_fullfit(data=women,controls=controls,package="mclogit")
  
  # Likelihood ratio test, men
  lrtest(fit1m,full1m)
  
  # Likelihood ratio test, women
  lrtest(fit1w,full1w)

  
### General settings ###########################################################
  
  # Controls
  controls <- c("time","time2","education")  
  
  # DTMS for prediction
  hrspredict <- dtms(transient=c("not impaired","impaired"),
                     absorbing="dead",
                     timescale=seq(50,98,2))
  
  # Covariate values for prediction
  reused <- list(time=seq(50,98,2),
                 time2=seq(50,98,2)^2)
  
  low <- c(reused,
           list(education=factor("0",levels=c("0","1","2"))))
  
  med <- c(reused,
           list(education=factor("1",levels=c("0","1","2"))))
  
  hig <- c(reused,
           list(education=factor("2",levels=c("0","1","2"))))

  
### Function for analysis ######################################################

  bootfun <- function(data,dtms) {
    
    cat(".")
    
    # Split data
    first <- data |> filter(wave%in%1:8)
    secon <- data |> filter(wave%in%9:15)
    
    ### For expectancies
  
    # Model
    fit1 <- dtms_fullfit(data=first,controls=controls)
    fit2 <- dtms_fullfit(data=secon,controls=controls)
    
    # Predict probabilities
    probs1_low <- dtms_transitions(dtms=dtms,model=fit1,controls=low,se=F)
    probs1_med <- dtms_transitions(dtms=dtms,model=fit1,controls=med,se=F)
    probs1_hig <- dtms_transitions(dtms=dtms,model=fit1,controls=hig,se=F)
    probs2_low <- dtms_transitions(dtms=dtms,model=fit2,controls=low,se=F)
    probs2_med <- dtms_transitions(dtms=dtms,model=fit2,controls=med,se=F)
    probs2_hig <- dtms_transitions(dtms=dtms,model=fit2,controls=hig,se=F)
    
    # Transition matrices
    Tm1_low <- dtms_matrix(dtms=dtms,probs=probs1_low)
    Tm1_med <- dtms_matrix(dtms=dtms,probs=probs1_med)
    Tm1_hig <- dtms_matrix(dtms=dtms,probs=probs1_hig)
    Tm2_low <- dtms_matrix(dtms=dtms,probs=probs2_low)
    Tm2_med <- dtms_matrix(dtms=dtms,probs=probs2_med)
    Tm2_hig <- dtms_matrix(dtms=dtms,probs=probs2_hig)
    
    # Starting distribution (correct, but small sample sizes) 
    S1_low <- dtms_start(dtms=dtms,data=first,start_time=c(50:56),variables=list(education=factor("0",levels=c("0","1","2"))))
    S1_med <- dtms_start(dtms=dtms,data=first,start_time=c(50:56),variables=list(education=factor("1",levels=c("0","1","2"))))
    S1_hig <- dtms_start(dtms=dtms,data=first,start_time=c(50:56),variables=list(education=factor("2",levels=c("0","1","2"))))
    S2_low <- dtms_start(dtms=dtms,data=secon,start_time=c(50:56),variables=list(education=factor("0",levels=c("0","1","2"))))
    S2_med <- dtms_start(dtms=dtms,data=secon,start_time=c(50:56),variables=list(education=factor("1",levels=c("0","1","2"))))
    S2_hig <- dtms_start(dtms=dtms,data=secon,start_time=c(50:56),variables=list(education=factor("2",levels=c("0","1","2"))))
    
    # 1: Expectancies
    resexp1 <- rbind(
      data.frame(education=0,start=c("Not imp.","impaired","AVERAGE"),dtms_expectancy(dtms=dtms,matrix=Tm1_low,start_distr=S1_low)),
      data.frame(education=1,start=c("Not imp.","impaired","AVERAGE"),dtms_expectancy(dtms=dtms,matrix=Tm1_med,start_distr=S1_med)),
      data.frame(education=2,start=c("Not imp.","impaired","AVERAGE"),dtms_expectancy(dtms=dtms,matrix=Tm1_hig,start_distr=S1_hig))
    )
    
    # 1: Difference to high educated, AVERAGE
    resexp1$diff <- resexp1$impaired-resexp1$impaired[resexp1$start=="AVERAGE" & resexp1$education==2]

    # 2: Expectancies
    resexp2 <- rbind(
      data.frame(education=0,start=c("Not imp.","impaired","AVERAGE"),dtms_expectancy(dtms=dtms,matrix=Tm2_low,start_distr=S2_low)),
      data.frame(education=1,start=c("Not imp.","impaired","AVERAGE"),dtms_expectancy(dtms=dtms,matrix=Tm2_med,start_distr=S2_med)),
      data.frame(education=2,start=c("Not imp.","impaired","AVERAGE"),dtms_expectancy(dtms=dtms,matrix=Tm2_hig,start_distr=S2_hig))
    )
    
    # 2: Difference to high educated, AVERAGE
    resexp2$diff <- resexp2$impaired-resexp2$impaired[resexp2$start=="AVERAGE" & resexp2$education==2]
    
    ### Risk
    
    # Re-estimate everything to get lifetime risk
    first <- dtms_forward(data=first,state="impaired",dtms=dtms)
    secon <- dtms_forward(data=secon,state="impaired",dtms=dtms)

    # Model
    fit1 <- dtms_fullfit(data=first,controls=controls)
    fit2 <- dtms_fullfit(data=secon,controls=controls)

    # Predict probabilities
    probs1_low <- dtms_transitions(dtms=dtms,model=fit1,controls=low,se=F)
    probs1_med <- dtms_transitions(dtms=dtms,model=fit1,controls=med,se=F)
    probs1_hig <- dtms_transitions(dtms=dtms,model=fit1,controls=hig,se=F)
    probs2_low <- dtms_transitions(dtms=dtms,model=fit2,controls=low,se=F)
    probs2_med <- dtms_transitions(dtms=dtms,model=fit2,controls=med,se=F)
    probs2_hig <- dtms_transitions(dtms=dtms,model=fit2,controls=hig,se=F)

    # Transition matrices
    Tm1_low <- dtms_matrix(dtms=dtms,probs=probs1_low)
    Tm1_med <- dtms_matrix(dtms=dtms,probs=probs1_med)
    Tm1_hig <- dtms_matrix(dtms=dtms,probs=probs1_hig)
    Tm2_low <- dtms_matrix(dtms=dtms,probs=probs2_low)
    Tm2_med <- dtms_matrix(dtms=dtms,probs=probs2_med)
    Tm2_hig <- dtms_matrix(dtms=dtms,probs=probs2_hig)

    # Starting distribution (correct, but small sample sizes)
    S1_low <- dtms_start(dtms=dtms,data=first,start_time=c(50:56),variables=list(education=factor("0",levels=c("0","1","2"))))
    S1_med <- dtms_start(dtms=dtms,data=first,start_time=c(50:56),variables=list(education=factor("1",levels=c("0","1","2"))))
    S1_hig <- dtms_start(dtms=dtms,data=first,start_time=c(50:56),variables=list(education=factor("2",levels=c("0","1","2"))))
    S2_low <- dtms_start(dtms=dtms,data=secon,start_time=c(50:56),variables=list(education=factor("0",levels=c("0","1","2"))))
    S2_med <- dtms_start(dtms=dtms,data=secon,start_time=c(50:56),variables=list(education=factor("1",levels=c("0","1","2"))))
    S2_hig <- dtms_start(dtms=dtms,data=secon,start_time=c(50:56),variables=list(education=factor("2",levels=c("0","1","2"))))
    
    # 1: Risk
    tmp1 <- dtms_risk(risk="impaired",dtms=dtms,matrix=Tm1_low,start_distr=S1_low)
    tmp2 <- dtms_risk(risk="impaired",dtms=dtms,matrix=Tm1_med,start_distr=S1_med)
    tmp3 <- dtms_risk(risk="impaired",dtms=dtms,matrix=Tm1_hig,start_distr=S1_hig)
    resexp1$risk <- c(tmp1[1:3],tmp2[1:3],tmp3[1:3])
    resexp1$riskdiff <- resexp1$risk-resexp1$risk[resexp1$start=="AVERAGE" & resexp1$education==2]
    resexp1$time <- 1
    
    # 2: Risk
    tmp1 <- dtms_risk(risk="impaired",dtms=dtms,matrix=Tm2_low,start_distr=S2_low)
    tmp2 <- dtms_risk(risk="impaired",dtms=dtms,matrix=Tm2_med,start_distr=S2_med)
    tmp3 <- dtms_risk(risk="impaired",dtms=dtms,matrix=Tm2_hig,start_distr=S2_hig)
    resexp2$risk <- c(tmp1[1:3],tmp2[1:3],tmp3[1:3])
    resexp2$riskdiff <- resexp2$risk-resexp2$risk[resexp2$start=="AVERAGE" & resexp2$education==2]
    resexp2$time <- 2
    
    ### Differences over time
    
    resexp1$diffdiff <- 0
    resexp1$riskdiffdiff <- 0
    
    resexp2$diffdiff <- resexp2$diff-resexp1$diff
    resexp2$riskdiffdiff <- resexp2$riskdiff-resexp1$riskdiff
    
    # Combine
    resexp <- rbind(resexp1,resexp2)
    
    # Drop start variable
    resexp <- resexp |> select(!start)
    
    # Return
    return(as.matrix(resexp))
  
  }

  
### Main results ###############################################################  

  men_res <- bootfun(data=men,dtms=hrspredict)
  women_res <- bootfun(data=women,dtms=hrspredict)
  
  
### Bootstrap ##################################################################
  
  men_boot <- dtms_boot(data=men,dtms=hrspredict,fun=bootfun,rep=1000,method="block",verbose=T)
  women_boot <- dtms_boot(data=women,dtms=hrspredict,fun=bootfun,rep=1000,method="block",verbose=T)


### Save results ###############################################################
  
  save(list=c("men_res","women_res","men_boot","women_boot"),
       file="Results/over_time.Rda")
  
  # If running on workstation
  if(Sys.info()["nodename"]%in%c("HYDRA01","HYDRA02","HYDRA11")) {rm(list=ls());gc()}
  
  