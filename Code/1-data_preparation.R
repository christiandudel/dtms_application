### Load Packages ##############################################################

  library(tidyverse)
  library(readstata13)
  library(dtms)


### Load data ##################################################################

  # Data; can be obtained from https://hrs.isr.umich.edu
  filename <- "randhrs1992_2020v2.dta"

  # Load
  hrs <- read.dta13(file=paste0("Data/",filename),
                    convert.factors=FALSE) 


### Select variables ###########################################################

                       # ID, gender, death/birth year, education
  hrs <- hrs |> select(hhidpn,ragender,radyear,rabyear,raeduc,
                       # Wave status: Response indicator (1= in wave)
                       starts_with("inw"), 
                       # Interview status (5 & 6 = dead)
                       starts_with("r")&ends_with("iwstat"),
                       # Age in years at interview month
                       starts_with("r")&ends_with("agey_e")&!contains("respagey"),
                       # Word recall (total, no web interview for recent waves)
                       starts_with("r")&contains("tr20")&!contains("tr20w"))

  
### Education ##################################################################

  # Education, 3 levels, 0=low, 1=medium, 2=high
  hrs <- hrs |> mutate(education=case_match(raeduc,
                                            c(1,2)~0,
                                            c(3,4)~1,
                                            5~2))
  
  # Drop if education is missing (22 individuals, negligible)
  hrs <- hrs |> filter(!is.na(education))


### Rename vars for easier reshaping below #####################################

  # Wave status
  hrs <- hrs |> rename_with(~paste0("r",1:15,"inw"),starts_with("inw"))
  
  # Age
  hrs <- hrs |> rename_with(~paste0("r",1:15,"age"),ends_with("agey_e"))
  
  # Immediate recall
  hrs <- hrs |> rename('r2tr20'='r2atr20',
                       'r14tr20'='r14tr20p',
                       'r15tr20'='r15tr20p')
  
  # Empty vars for reshaping later (required by reshape function)
  hrs$r1tr20 <- NA

  # Change format of time varying variables (not a great solution, but works)
  hrsnames <- str_split_fixed(names(hrs),"r[[:digit:]]{1,2}",2)
  hrsnames <- apply(hrsnames,1,function(x) {paste0(x,collapse="")})
  hrsnumbers <- parse_number(names(hrs))
  hrswhich <- !is.na(hrsnumbers)
  hrsnames[hrswhich] <- paste(hrsnames[hrswhich],hrsnumbers[hrswhich],sep="_")
  names(hrs) <- hrsnames

  
### Reshape ####################################################################

  # Get names of longitudinal vars and their ordering right 
  repvars <- grepl("_",names(hrs))   
  repvars <- names(hrs)[repvars]
  repvars <- unique(unlist(lapply(strsplit(repvars,split="_"),function(x)x[1])))
  repvars <- paste(rep(repvars, each = length(1:15)), 1:15, sep = "_")

  # Reshape (pivot_longer is just not intuitive to me, sorry)
  hrs <- reshape(data=as.data.frame(hrs),
                 direction="long",
                 varying=repvars,
                 sep="_",
                 idvar="hhidpn",
                 #times=1:15,
                 timevar="wave")

  # Sort 
  hrs <- hrs |> arrange(hhidpn,wave)
  
  # Drop people after death, and when not (yet) in wave
  hrs <- hrs |> filter(iwstat%in%c(1,5))


### Age ########################################################################

  # Age is missing in the year of death, add
  hrs <- hrs |> mutate(age=ifelse(iwstat==5,radyear-rabyear,age))
  
  # Age is still missing for a few people with unknown birth year and/or unknown 
  # year of death; for the latter, we impute year of death as mid-interval,
  # and generate age based on that
  hrs <- hrs |> mutate(toedit=ifelse(is.na(radyear) & !is.na(rabyear) & iwstat==5 & is.na(age),1,0),
                       radyear=case_when(
                                toedit==1 & wave==2~1993,
                                toedit==1 & wave==3~1995,
                                toedit==1 & wave==4~1997,
                                toedit==1 & wave==5~1999,
                                toedit==1 & wave==6~2001,
                                toedit==1 & wave==7~2003,
                                toedit==1 & wave==8~2005,
                                toedit==1 & wave==9~2007,
                                toedit==1 & wave==10~2009,
                                toedit==1 & wave==11~2011,
                                toedit==1 & wave==12~2013,
                                toedit==1 & wave==13~2015,
                                toedit==1 & wave==14~2017,
                                toedit==1 & wave==15~2019,
                                .default=radyear
                       ),
                       age=ifelse(iwstat==5&is.na(age),radyear-rabyear,age))
  
  # Drop if age is missing
  hrs <- hrs |> filter(!is.na(age))


### Cognitive functioning/impairment ###########################################

  # Threshold (based on pop ages 50-69)
  tr20mean <- hrs |> filter(age%in%50:69) |> pull(tr20) |> mean(na.rm=T)
  tr20sd <- hrs |> filter(age%in%50:69) |> pull(tr20) |> sd(na.rm=T)
  tr20threshold <- tr20mean-1.5*tr20sd
  
  # Impairment indicator
  hrs <- hrs |> mutate(cogimpair=NA,
                       cogimpair=ifelse(tr20>tr20threshold,0,cogimpair),
                       cogimpair=ifelse(tr20<=tr20threshold,1,cogimpair))
  

### State variable (including death) ###########################################

  # State using ADL
  hrs <- hrs |> mutate(state=case_when(
                                cogimpair==0~"not impaired",
                                cogimpair==1~"impaired",
                                iwstat==5~"dead",
                                .default=NA))
  

### Limit data #################################################################

  # Limit variables
  hrs <- hrs |> select(hhidpn,ragender,education,wave,age,state)
  
  # Rename
  hrs <- hrs |> rename('gender'='ragender',
                       'id'='hhidpn')
  
  # Drop obs
  hrs <- na.omit(hrs)

  
### Saving #####################################################################

  save(hrs,file="Data/hrs_edited.Rda")

