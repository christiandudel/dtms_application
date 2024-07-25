### Packages ###################################################################

  library(dtms)
  library(tidyverse)


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

  men_1st <- estdata |> filter(gender==1 & wave%in%1:8)
  men_2nd <- estdata |> filter(gender==1 & wave%in%9:15)
  women_1st <- estdata |> filter(gender==2 & wave%in%1:8)
  women_2nd <- estdata |> filter(gender==2 & wave%in%9:15)
  
  
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
  
    # Model
    fit <- dtms_fit(data=data,controls=controls)
    
    # Predict probabilities
    probs_low <- dtms_transitions(dtms=dtms,model=fit,controls=low)
    probs_med <- dtms_transitions(dtms=dtms,model=fit,controls=med)
    probs_hig <- dtms_transitions(dtms=dtms,model=fit,controls=hig)
    
    # Transition matrices
    Tm_low <- dtms_matrix(dtms=dtms,probs=probs_low)
    Tm_med <- dtms_matrix(dtms=dtms,probs=probs_med)
    Tm_hig <- dtms_matrix(dtms=dtms,probs=probs_hig)
    
    # Starting distribution (correct, but small sample sizes) 
    S_low <- dtms_start(dtms=dtms,data=data,start_time=c(50:56),variables=list(education=factor("0",levels=c("0","1","2"))))
    S_med <- dtms_start(dtms=dtms,data=data,start_time=c(50:56),variables=list(education=factor("1",levels=c("0","1","2"))))
    S_hig <- dtms_start(dtms=dtms,data=data,start_time=c(50:56),variables=list(education=factor("2",levels=c("0","1","2"))))
    
    # Expectancies
    resexp <- rbind(
      data.frame(education=0,start=c("Not imp.","impaired","AVERAGE"),dtms_expectancy(dtms=dtms,matrix=Tm_low,start_distr=S_low)),
      data.frame(education=1,start=c("Not imp.","impaired","AVERAGE"),dtms_expectancy(dtms=dtms,matrix=Tm_med,start_distr=S_med)),
      data.frame(education=2,start=c("Not imp.","impaired","AVERAGE"),dtms_expectancy(dtms=dtms,matrix=Tm_hig,start_distr=S_hig))
    )
    
    # Difference to high educated, AVERAGE
    resexp$diff <- resexp$impaired-resexp$impaired[resexp$start=="AVERAGE" & resexp$education==2]
    
    # Lifetime risk
    # dtms_risk(risk="impaired",dtms=dtms,matrix=Tm_low,start_distr=S_low)
    # dtms_risk(risk="impaired",dtms=dtms,matrix=Tm_med,start_distr=S_med)
    # dtms_risk(risk="impaired",dtms=dtms,matrix=Tm_hig,start_distr=S_hig)
    return(resexp)
  
  }

### Quick results ##############################################################  

  men_res1st <- bootfun(data=men_1st,dtms=hrspredict)
  men_res2nd <- bootfun(data=men_2nd,dtms=hrspredict)
  women_res1st <- bootfun(data=women_1st,dtms=hrspredict)
  women_res2nd <- bootfun(data=women_2nd,dtms=hrspredict)
  


