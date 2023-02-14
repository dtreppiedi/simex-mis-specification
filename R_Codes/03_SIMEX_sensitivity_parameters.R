#####                 03_SIMEX_sensitivity_parameters              ###### 
# Dario Treppiedi  
# (https://github.com/dtreppiedi)  


rm(list=ls(all=TRUE))

set.seed(1234)

#### Packages  #####

library('simex')



#### Input Data and dataframe allocation  #####

params=cbind(c(0.8),   # true intercept B0
             c(0.8))   # true slope B1

mu_u=seq(0, 1, length.out=41)
sig_u=seq(0.3, 0.6, length.out=41)

lambdas=c(0.5, 1, 1.5, 2, 2.5, 3)

n=400

RES_TAB=data.frame(n=NA,
                   mu_u=NA,
                   sig_u=NA,
                   true_B0=NA,
                   true_B1=NA,
                   OLS_B0=NA,
                   OLD_B1=NA,
                   SIMEX_B0=NA,
                   SIMEX_B1=NA)



#### Analysis  #####

x=-1+2*runif(n)
y=params[1, 1]+params[1, 2]*x+rnorm(n,0,0.1)


for(m in 1:length(mu_u)){
  
  mu_cons=mu_u[m]
  
  cat('   Processing mu', mu_cons,'\n')
  
  for (s in 1:length(sig_u)) {
    
    sig_cons=sig_u[s]
    
    cat('   Processing sigma', sig_cons,'\n')
    
    
    err=rnorm(n, mean = mu_cons , sd = sig_cons) #standard Gaussian variate used to obtain U
    w=x+err  #X+U
    
    naive.model <- lm(y~w, x=TRUE)
    simex.model <- simex(model = naive.model, SIMEXvariable = "w", B=200, measurement.error= sig_cons, 
                         lambda = lambdas)
    
    temp_res=data.frame(simex.model[["SIMEX.estimates"]])
    names(temp_res)=c("lambda", "Intercept", "Slope")
    
    all_res=data.frame(n=n,
                       mu_u=mu_cons,
                       sig_u=sig_cons,
                       true_B0=params[1, 1],
                       true_B1=params[1, 2],
                       OLS_B0=temp_res[which(temp_res$lambda==0), "Intercept"],
                       OLD_B1=temp_res[which(temp_res$lambda==0), "Slope"],
                       SIMEX_B0=temp_res[which(temp_res$lambda==-1), "Intercept"],
                       SIMEX_B1=temp_res[which(temp_res$lambda==-1), "Slope"])
    
    RES_TAB=rbind(RES_TAB, all_res)
    
  }
  
  
}




####  Results  #####


RES_TAB=RES_TAB[-1,]

RES_TAB$B0_OLS_plot=(RES_TAB$OLS_B0-RES_TAB$true_B0)/RES_TAB$true_B1
RES_TAB$B0_SIMEX_plot=(RES_TAB$SIMEX_B0-RES_TAB$true_B0)/RES_TAB$true_B1

RES_TAB$B1_OLS_plot=100*(RES_TAB$true_B1-RES_TAB$OLD_B1)/RES_TAB$true_B1
RES_TAB$B1_SIMEX_plot=100*(RES_TAB$true_B1-RES_TAB$SIMEX_B1)/RES_TAB$true_B1


RES_TAB$sig_uncert=(RES_TAB$sig_u-sig_u[1])/sig_u[1]*100
RES_TAB$mu_uncert=(RES_TAB$mu_u-mu_u[1])/mu_u[1]*100





