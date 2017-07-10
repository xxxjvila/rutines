
###################################################################################################
########################     COMPUTE RISC EUPHORIC     ############################################
###################################################################################################


euphoric.risc<-function(FILTER,COUNTRY,GROSS=NA,LIFEEXP=NA,CVMORT=NA,
                              CATHLAB,CORSURG,UNIVERSI,
                              AGE,WOM,HTN,DIAB,CVHIST=NA) {


  # EXTRA INPUT TXT FILES:
  # coeffmodels.txt  : file where coefficients, random effects estimates, variances and AUC from each of 18 models (every model by column)
  # countchar.txt  :  table with the values of 31 sample countries and 3 variables (GROSS, LIFEEXP and CVMORT).
  
  COEFFMODELS<-read.table(file.path(RutinesLocals,"COEFFMODELS.txt"),header=TRUE,sep="\t",as.is=TRUE)
  
  COUNTCHAR<-read.table(file.path(RutinesLocals,"COUNTCHAR.txt"),header=TRUE,sep="\t",as.is=TRUE)
  
  
  #################################################
  #############    INPUTS         #################
  #################################################
  
  
  INPUT.ORIG<-c(FILTER,COUNTRY,GROSS,LIFEEXP,CVMORT,CATHLAB,CORSURG,UNIVERSI,AGE,WOM,HTN,DIAB,CVHIST)
  
  ########## A)   OUTCOME: in-hospital mortality  : fixed.   ######
  
  ########## B)   PROCEDURE OR MANAGEMENT (FILTER CODES): 'select-only-one option button' #####
    
    # Managements
      #1-Unstable Angina
      #2-Miocardial infarction
    # Procedures
      #3-PCI
      #4-Thrombolysis
      #5-Angiography (coronary angiography)
  
      #@ FILTER<-2
  
  
  
  ########## C)    OBLIGATORY INPUT: hospital name / code, to be identified in the output results.  ######
  
  
  
  ########## D)  INPUT VARIABLES ########
  
  
  ####  1 -  country  : 'select-only-one list box  (with sample countries. Add 'OTHER')'
  
    COUNTRY.LIST<-COUNTCHAR[,1]  # sample countries
  
    #@ COUNTRY<-"SPAIN"  
    
  
  
  #### 2 - country characteristics: (SOURCE: http://www.who.int/whosis/database/core/core_select_process.cfm)
  
    if (COUNTRY!='OTHER'){ # if the country is in the sample, the country characteristics input window must be disabled
      
      COUNTRY.ROW<-match(COUNTRY,COUNTRY.LIST) # search where is the selected country in the list.
      
      GROSS<-COUNTCHAR[COUNTRY.ROW,2]
      LIFEEXP<-COUNTCHAR[COUNTRY.ROW,3]
      CVMORT<-COUNTCHAR[COUNTRY.ROW,4]
    
    } else { # else, the country characteristics input window must be enabled.
    
      GROSS<-GROSS      # (OPTIONAL) 
      LIFEEXP<-LIFEEXP    # (OPTIONAL)
      CVMORT<-CVMORT    # (OPTIONAL)
    
    }
    
    #If there's some missing in country characteristics, the average of sample country is imputed.
    if (is.na(GROSS)) GROSS<-mean(COUNTCHAR[,2])
    if (is.na(LIFEEXP)) LIFEEXP<-mean(COUNTCHAR[,3])
    if (is.na(CVMORT)) CVMORT<-mean(COUNTCHAR[,4])
  
  
  #### 3- hospital characteristics (MANDATORY)
  
    #@ CATHLAB<-1
    #@ CORSURG<-1
    #@ UNIVERSI<-0
  
  
  #### 4- patient characteristics 
    #@ AGE<-65     # (MANDATORY)
    #@ WOM<-0.33    # (MANDATORY)
    #@ HTN<-0.54    # (MANDATORY)
    #@ DIAB<-0.23     # (MANDATORY)
    #@ CVHIST<-0.12   # (OPTIONAL)  and disabled when FILTER == 'Thrombolysis'
  
    # TOTAL NUMBER OF MODELS:   5 X 2  - 1 (FILTER=Thrombolysis)= 9 MODELS
    # when FILTER=='Thrombolysis', a dialog window must appear saying 'CVHIST not present in the model'. 
  
    
  ##### 5- percentiles of risk (OUTCOME RESULTS): fixed not modifiable by user.
    PERCENT<-c(0.05,0.1,0.25,0.5,0.75,0.9,0.95)
    # quantiles of a standard-normal distribution correspondign to PERCENT probabilities.
    QPERCENT<-c(-1.6448536,-1.2815516,-0.6744898, 0.0000000, 0.6744898, 1.2815516, 1.6448536)
  
  ################ END INPUTS ########################
  
  
  ################### MAIN ###########################
  
  
  if (FILTER==1){ # Unstable Angina (UA) (ADKILLIP not present in the model)
    if (is.na(CVHIST)) RES<-COEFFMODELS[,2]
    if (!is.na(CVHIST)) RES<-COEFFMODELS[,3]
  }
  if (FILTER==2){ # MI  (Miocardial infarction) (ADKILLIP not present in the model)
    if (is.na(CVHIST)) RES<-COEFFMODELS[,4]
    if (!is.na(CVHIST)) RES<-COEFFMODELS[,5]
  }
  if (FILTER==3){ # PCI
    if (is.na(CVHIST)) RES<-COEFFMODELS[,6]
    if (!is.na(CVHIST)) RES<-COEFFMODELS[,7]
  }
  if (FILTER==4){  # TX (Thrombolysis)  (CVHIST not present in the model
    RES<-COEFFMODELS[,8]
  }
  if (FILTER==5){  # CA (Coronary Angiography)
    if (is.na(CVHIST)) RES<-COEFFMODELS[,9]
    if (!is.na(CVHIST)) RES<-COEFFMODELS[,10]
  }
  
  BETAS<-RES[1:12]   # model coefficients (including intercept)
  BETASPAT<-RES[1:6] # model coefficients for patient level variables (including intercept)
  RANEFF<-RES[25:55] # country empirical random estimates
  VARCOUNT<-RES[56]  # country random effects variance
  VARHOSP<-RES[57]   # hospital random effects variance
  AUC<-RES[58]       # Area under the curve of the model
  AUCLOW<-RES[59]    # AUC 95%CI Lower bound
  AUCUP<-RES[60]     # AUC 95%CI Upper bound
  
  
  ### vector of patient, hospital and country characteristics.
  
    # replace missing values for CVHIST coefficient by 0.
    CVHIST<-if (is.na(CVHIST)) 0 else CVHIST 
    
    X<-c(1,AGE,WOM,HTN,DIAB,CVHIST,CATHLAB,CORSURG,UNIVERSI,GROSS,LIFEEXP,CVMORT)
    XPAT<-c(1,AGE,WOM,HTN,DIAB,CVHIST)
  
  ###### STEP 1: compute linear predictor (eta) mean and variance
    
    XBETAS<-sum(X*BETAS)  # fixed effects part of linear predictor
    XBETASPAT<-sum(XPAT*BETASPAT) # fixed effects part of linear predictor from patient level variables
  
  ## CASE A: the country is not in the sample used to fit the model
    if (COUNTRY=='OTHER' || is.na(RANEFF[COUNTRY.LIST==COUNTRY])){
      MEANETA<-XBETAS                # linear predictor mean
      VARETA<-VARHOSP+VARCOUNT       # linear predictor variance
    }
  ## CASE B: the country is in the sample used to fit the model.
    if (COUNTRY!='OTHER' && !is.na(RANEFF[COUNTRY.LIST==COUNTRY])){
      EMPBAYES<-RANEFF[COUNTRY.LIST==COUNTRY]  # empirical bayes estimate of random effect of the specified country.
      MEANETA<-XBETAS+EMPBAYES          # linear predictor mean
      VARETA<-VARHOSP                   # linear predictor variance
    }
  
  ###### STEP 2: compute linear predictor (eta) percentiles
  
    ETAPERC<-QPERCENT*sqrt(VARETA)+MEANETA     # eta percentiles
  
  
  ###### STEP 3: transform eta to probability scale
  
    PROBPERC<-1/(1+exp(-ETAPERC))   
    
    # name components of the result vector
    names(PROBPERC)<-paste(PERCENT*100,"%",sep="")
    
    # return the estimated probabilities, and AUC (as a vector).
    OUTPUT<-c(PROBPERC*100,"AUC"=AUC*100,"AUC.LOWER"=AUCLOW*100,"AUC.UPPER"=AUCUP*100)
  
    curvef<-function(x){
      res<-dnorm(log(x/(1-x)),MEANETA,sqrt(VARETA))/(x*(1-x))
      ifelse(res<0,0,res)
    }

  
    # print results in R-Console with 3 decimals
    list(caract=INPUT.ORIG,resultat=cbind(round(OUTPUT,3)),curvef=curvef,meaneta=MEANETA,vareta=VARETA,varhosp=VARHOSP,varcount=VARCOUNT,coeff=BETAS,xbetaspat=XBETASPAT)


}



