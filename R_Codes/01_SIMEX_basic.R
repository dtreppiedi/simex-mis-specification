#####                 01_SIMEX_basic              ###### 
# Dario Treppiedi  
# (https://github.com/dtreppiedi)  


rm(list=ls(all=TRUE))

set.seed(1234)

#### Packages  #####

library('simex')
library('stringr')
library('ggplot2')


#### Input Data and dataframe allocation  #####


params=cbind(c(0),
             c(0.8))

lambdas=c(0.5, 1, 1.5, 2, 2.5, 3)

s=0.3 #level of noise 
n=400 #sample size

RES_TAB=data.frame(s=NA,
                   B0=NA,
                   B1=NA,
                   lambda=NA,
                   Intercept=NA,
                   Slope=NA)

EXTRAP_TAB=data.frame(s=NA,
                      B0=NA,
                      B1=NA,
                      lamvar=NA,
                      wvar=NA)               



#### Analysis  #####


for(cb in 1:dim(params)[1]){

  x=-1+2*runif(n)
  y=params[cb, 1]+params[cb, 2]*x+rnorm(n,0,0.1)
  
  err=rnorm(n,0,s) #standard Gaussian variate used to obtain U
  w=x+err  #X+U
  
  naive.model <- lm(y~w, x=TRUE)
  simex.model <- simex(model = naive.model, SIMEXvariable = "w", B=200, measurement.error= s, 
                       lambda = lambdas)
  
  temp_res=data.frame(simex.model[["SIMEX.estimates"]])
  names(temp_res)=c("lambda", "Intercept", "Slope")
  
  param_extr=simex.model[["extrapolation"]][["coefficients"]]
  param_extr=data.frame(t(param_extr[, "w"]))
  names(param_extr)=c("Extrap_Int", "Extrap_lam", "Extrap_lam2")
  
  
  temp_res=cbind(s=s, B0=params[cb, 1], B1=params[cb, 2], temp_res)
  
  temp_extr=cbind(s=s, B0=params[cb, 1], B1=params[cb, 2], 
                  lamvar=seq(-2, 4, 0.01), 
                  wvar=param_extr$Extrap_lam2 * seq(-2, 4, 0.01)^2 + 
                    param_extr$Extrap_lam * seq(-2, 4, 0.01) + 
                    param_extr$Extrap_Int)
  
  
  RES_TAB=rbind(RES_TAB, temp_res)
  EXTRAP_TAB=rbind(EXTRAP_TAB, temp_extr)
  
}


####  Results  #####


RES_TAB=RES_TAB[-1,]
EXTRAP_TAB=EXTRAP_TAB[-1,]


RES_TAB$param_header=str_c("(", RES_TAB$B0, ", ", RES_TAB$B1, ")", sep = "")
EXTRAP_TAB$param_header=str_c("(", EXTRAP_TAB$B0, ", ", EXTRAP_TAB$B1, ")", sep = "")


RES_TAB$SlopeSIMEX=NA
RES_TAB$SlopeSIMEX=ifelse(RES_TAB$lambda==-1, RES_TAB$Slope, NA)


EXTRAP_TAB_simex=EXTRAP_TAB[which(EXTRAP_TAB$lamvar>=0),]
EXTRAP_TAB_naive=EXTRAP_TAB[which(EXTRAP_TAB$lamvar<0),]



#### Plot  #####


PLOT=ggplot()+  
  geom_line(data=EXTRAP_TAB_simex, mapping=aes(x=lamvar,y=wvar))+
  geom_line(data=EXTRAP_TAB_naive, mapping=aes(x=lamvar,y=wvar), linetype="dashed")+
  annotate("rect", xmin = 0, xmax = 3.4, ymin = 0.25, ymax = 0.87, 
           alpha = .15)+
  geom_vline(xintercept = 0)+
  geom_vline(xintercept = -1, color="gray60")+
  geom_point(data=RES_TAB, mapping=aes(x=lambda,y=Slope), shape=21, size=4, fill="blue")+
  geom_point(data=RES_TAB, mapping=aes(x=lambda,y=SlopeSIMEX), shape=21, size=4, fill="red")+
  annotate("rect", xmin = -1.1, xmax = -0.9, 
           ymin = RES_TAB[which(RES_TAB$lambda==-1), "Slope"]-0.125, ymax = RES_TAB[which(RES_TAB$lambda==-1), "Slope"]-0.035, 
           alpha = 1, fill="white", color="black")+
  annotate("text", x = -1, y = RES_TAB[which(RES_TAB$lambda==-1), "Slope"]-0.08, label = "SIMEX", angle=-90, vjust=0.35)+
  annotate("rect", xmin = -0.1, xmax = 0.1, 
           ymin = RES_TAB[which(RES_TAB$lambda==0), "Slope"]-0.12, ymax = RES_TAB[which(RES_TAB$lambda==0), "Slope"]-0.04, 
           alpha = 1, fill="white", color="black")+
  annotate("text", x = 0, y = RES_TAB[which(RES_TAB$lambda==0), "Slope"]-0.08, label = "Na\u00efve", angle=-90, vjust=0.35)+
  annotate("text", x = -0.5, y = 0.3, size=4.5, label = "Extrapolation", vjust=0)+
  annotate("text", x = 1.5, y = 0.3,  size=4.5, label = "Simulation", vjust=0)+
  scale_y_continuous(name=bquote(Slope~beta[1]), breaks = seq(0, 2, 0.1), limits = c(0.25, 0.87), expand = c(0,0))+
  scale_x_continuous(name=bquote(zeta), breaks = seq(-1, 3, .5), limits = c(-1.4, 3.4), expand = c(0,0))+
  geom_hline(yintercept = 0.8, color="gray20")+
  annotate("text", x = -0.5, y = 0.81, size=4, label = "Target", vjust=0, fontface = "italic", color="gray20")+
  theme_bw()+
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5),
        axis.title =  element_text(size=14),
        axis.text = element_text(size=14),
        legend.title = element_text(angle = 0, size=14, face="bold"),
        legend.text = element_text(size=14),
        legend.title.align = 0.5,
        legend.spacing.y = unit(0.2, 'cm'),
        legend.key.size = unit(0.5, "cm"),
        legend.key.width = unit(0.5, "cm"),
        plot.margin = margin(.5, .5, .5, .5, "cm"),
        strip.text = element_text(size=14))
