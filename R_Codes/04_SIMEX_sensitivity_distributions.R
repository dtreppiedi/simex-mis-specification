#####                 04_SIMEX_sensitivity_distributions              ###### 
# Dario Treppiedi  
# (https://github.com/dtreppiedi)  


rm(list=ls(all=TRUE))

set.seed(1234)


#### Packages  #####

library('simex')
library('jmuOutlier')
library("parallel")
library("foreach")
library("iterators")
library("doParallel")



#### Input Data and dataframe allocation  #####

params=cbind(c(0),
             c(0.8))

lambdas=c(0.5, 1, 1.5, 2, 2.5, 3)

s=0.3

n=c(25, 50, 100, 200, 400, 800) #sample size
iterat=1000



#### Analysis  #####

nCores <- detectCores(logical = FALSE)-1 # Number of cores used to parallelize the analysis


cl <- makeCluster(nCores) ##########
registerDoParallel(cl)



for(nnn in 1:length(n)){
  
  set.seed(1234)
  x=-1+2*runif(n[nnn])
  y=params[1, 1]+params[1, 2]*x+rnorm(n[nnn],0,0.1)
  
  print(paste("Sample Size: ", n[nnn], sep = " "))
  
  
  SIMEX_distrib <- foreach(it=1:iterat, .combine = rbind, .packages = c('simex', 'jmuOutlier')) %dopar% {  
    
    SIM_Iter_fun <-function (ii) {
      
      iter.cons=ii
      
      ### Normal
      
      err_gauss=rnorm(n[nnn],0,0.3)
      w_gauss=x+err_gauss  #X+U
      
      naive.model_gauss <- lm(y~w_gauss, x=TRUE)
      simex.model_gauss <- simex(model = naive.model_gauss, SIMEXvariable = "w_gauss", B=200, measurement.error= 0.3, 
                                 lambda = lambdas)
      
      temp_res_gauss=data.frame(simex.model_gauss[["SIMEX.estimates"]])
      names(temp_res_gauss)=c("lambda", "Intercept", "Slope")
      
      temp_res_gauss=cbind(Dist="U~Gaussian", s=s, B0=params[1, 1], B1=params[1, 2], temp_res_gauss)
      
      
      
      ### Log Normal
      
      err_lnorm=rlnorm(n[nnn], meanlog = 0, sdlog = 0.3) 
      err_lnorm=err_lnorm-1 
      
      w_lnorm=x+err_lnorm  #X+U
      
      naive.model_lnorm <- lm(y~w_lnorm, x=TRUE)
      simex.model_lnorm <- simex(model = naive.model_lnorm, SIMEXvariable = "w_lnorm", B=200, measurement.error= 0.3, 
                                 lambda = lambdas)
      
      temp_res_lnorm=data.frame(simex.model_lnorm[["SIMEX.estimates"]])
      names(temp_res_lnorm)=c("lambda", "Intercept", "Slope")
      
      temp_res_lnorm=cbind(Dist="U~Lognormal", s=s, B0=params[1, 1], B1=params[1, 2], temp_res_lnorm)
      
      
      
      ### Logistic
      
      err_logist=rlogis(n[nnn], location = 0, scale = sqrt(3*0.3^2/pi^2))
      w_logist=x+err_logist #X+U
      
      naive.model_logist <- lm(y~w_logist, x=TRUE)
      simex.model_logist <- simex(model = naive.model_logist, SIMEXvariable = "w_logist", B=200, measurement.error= 0.3, 
                                  lambda = lambdas)
      
      temp_res_logist=data.frame(simex.model_logist[["SIMEX.estimates"]])
      names(temp_res_logist)=c("lambda", "Intercept", "Slope")
      
      temp_res_logist=cbind(Dist="U~Logistic", s=s, B0=params[1, 1], B1=params[1, 2], temp_res_logist)
      
      
      
      ### Laplace
      
      err_lapla=rlaplace(n[nnn], 0, 0.3)
      w_lapla=x+err_lapla #X+U
      
      naive.model_lapla <- lm(y~w_lapla, x=TRUE)
      simex.model_lapla <- simex(model = naive.model_lapla, SIMEXvariable = "w_lapla", B=200, measurement.error= 0.3, 
                                 lambda = lambdas)
      
      temp_res_lapla=data.frame(simex.model_lapla[["SIMEX.estimates"]])
      names(temp_res_lapla)=c("lambda", "Intercept", "Slope")
      
      temp_res_lapla=cbind(Dist="U~Laplace", s=s, B0=params[1, 1], B1=params[1, 2], temp_res_lapla)
      
      
      
      ### Gamma
      
      err_gamma=rgamma(n[nnn], shape = (0.5/0.3)^2, rate = 0.5/0.3^2) 
      err_gamma=err_gamma-0.5 
      
      w_gamma=x+err_gamma #X+U
      
      naive.model_gamma <- lm(y~w_gamma, x=TRUE)
      simex.model_gamma <- simex(model = naive.model_gamma, SIMEXvariable = "w_gamma", B=200, measurement.error= 0.3, 
                                 lambda = lambdas)
      
      temp_res_gamma=data.frame(simex.model_gamma[["SIMEX.estimates"]])
      names(temp_res_gamma)=c("lambda", "Intercept", "Slope")
      
      temp_res_gamma=cbind(Dist="U~Gamma", s=s, B0=params[1, 1], B1=params[1, 2], temp_res_gamma)
      
      
      RES_TAB_iter=rbind(temp_res_gauss, temp_res_lnorm, temp_res_logist, temp_res_lapla, temp_res_gamma)
      RES_TAB_iter$iteration=iter.cons
      
      
      return(RES_TAB_iter)
      
    }
    
    SIM_Iter_fun(it)
    
    
  }
  
  
  SIMEX_distrib$Numer=n[nnn]


  if(nnn==1){
    
    RES_NUME=SIMEX_distrib
    
  } else {
    
    RES_NUME=rbind(RES_NUME, SIMEX_distrib)
    
  }
  
  
}



stopCluster(cl)



#### Results  #####

#Results are stored in RES_NUME dataframe


