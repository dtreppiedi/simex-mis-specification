#####                 02_SIMEX_SOI_Nino34_Application              ###### 
# Dario Treppiedi  
# (https://github.com/dtreppiedi)  


rm(list = ls(all=TRUE))

set.seed(1234)


#### Packages  #####

library("stringr")
library('simex')


#### Path and Input Data #####

path_general = dirname(rstudioapi::getSourceEditorContext()$path)
path_general = sub("/[^/]+$", "", path_general)


path_data = paste(path_general, "Data", sep = "/")


###### SOI - NOAA ######

SOIHist_Mon=read.table(paste(path_data, "soi_standardized_Feb.txt", sep = "/"), header = T)
names(SOIHist_Mon)=c("YEAR", "MON")


###### SST ElNino3.4 - NMME GFDL-CM2p5-FLOR-B01###### 

NINO34_All_Lead_Mon=read.csv(paste(path_data, "NINO34_Feb_Lead05_35.csv", sep = "/"), header = T)



LeadTime=unique(NINO34_All_Lead_Mon$LeadTime)

perc_tr=60 #Percentage of data used for training
NTrain=ceiling(dim(SOIHist_Mon)[1]*(perc_tr/100)) #Number of years used for training 



#### Analysis  #####

for (lt in 1:length(LeadTime)) {
  
  NINO34_All_Lead_Mon_sub=NINO34_All_Lead_Mon[which(NINO34_All_Lead_Mon$LeadTime==(lt-0.5)),]
  
  # Training
  
  SOI_Tr=SOIHist_Mon[1:NTrain, ]
  NINO34_Tr=NINO34_All_Lead_Mon_sub[1:NTrain, ]
  
  
  lambdas=c(0.5, 1, 1.5, 2, 2.5, 3)
  
  y=SOI_Tr$MON
  w=NINO34_Tr$mean
  meas.err=mean(NINO34_Tr$sd)
  
  
  
  naive.model <- lm(y~w, x=TRUE)
  simex.model <- simex(model = naive.model, SIMEXvariable = "w", B=200, measurement.error=meas.err, 
                       lambda = lambdas)
  
  
  
  
  coeff_naive=naive.model$coefficients
  coeff_simex=simex.model$coefficients
  
  
  # Validation
  
  SOI_Val=SOIHist_Mon[(NTrain+1):dim(SOIHist_Mon)[1], ]
  NINO34_Val=NINO34_All_Lead_Mon_sub[(NTrain+1):dim(NINO34_All_Lead_Mon_sub)[1], ]
  
  SOI_Val$MON_Naive=NINO34_Val$mean*coeff_naive[2]+coeff_naive[1]
  SOI_Val$MON_SIMEX=NINO34_Val$mean*coeff_simex[2]+coeff_simex[1]
  
  SOI_Val$Err_Naive=SOI_Val$MON-SOI_Val$MON_Naive
  SOI_Val$Err_SIMEX=SOI_Val$MON-SOI_Val$MON_SIMEX
  
  SOI_Tr$LeadTime=paste("Lead", lt-0.5, sep = "  ")

  SOI_Val$LeadTime=paste("Lead", lt-0.5, sep = "  ")
  SOI_Val$RMSE_Naive=sqrt(mean((SOI_Val$MON - SOI_Val$MON_Naive)^2))
  SOI_Val$RMSE_SIMEX=sqrt(mean((SOI_Val$MON - SOI_Val$MON_SIMEX)^2))

  Reg_value_lead=data.frame(w=w, 
                            y=y,
                            Int_Naive=coeff_naive[1],
                            Slp_Naive=coeff_naive[2],
                            Int_SIMEX=coeff_simex[1],
                            Slp_SIMEX=coeff_simex[2], 
                            LeadTime=paste("Lead", lt-0.5, sep = "  "))
  
  
  if(lt==1){
    
    SOI_TRAIN_ALL=SOI_Tr
    SOI_VAL_ALL=SOI_Val
    Regress_ALL=Reg_value_lead
    
  } else {
    
    SOI_TRAIN_ALL=rbind(SOI_TRAIN_ALL, SOI_Tr)
    SOI_VAL_ALL=rbind(SOI_VAL_ALL, SOI_Val)
    Regress_ALL=rbind(Regress_ALL, Reg_value_lead)
    
  }
  
  

}


####  Results  #####

# The regression results are stored in Regress_ALL, while the RMSE values in SOI_VAL_ALL


