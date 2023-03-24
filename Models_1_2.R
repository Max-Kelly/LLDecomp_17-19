library(rjags)
library(R2jags) 
library(xtable)
#library(kableExtra)
#library(Rmisc)
library(tidyverse)
library(dplyr)
#library(MCMCvis)
library(lubridate)


max_1719 <-  read.csv ("C:/Users/Max/Desktop/UGA/Pringle Lab/R/msDecomp/17-19Decomp.6.27.csv", fileEncoding = 'UTF-8-BOM')
dat<-max_1719
AFDM_1 <- dat[,c(1,2,3,4,5,9)]
AFDM_trt<-AFDM_1 [AFDM_1$Treatment != "Handling", ]  #347 observations

AFDM_trt$AFDMrem<-as.numeric(as.character(AFDM_trt$AFDMrem))  ###note - missing some data? - Yes, these values were never collected

## now set up obs and covariates for model below:
## we want 'treatment' = 1 if EC, 0 for C
AFDM_trt$EC<-ifelse(AFDM_trt$Treatment=="EC",1,0)
treatment<-AFDM_trt$EC # covariate vector for model

## we want variables for "2018" = 1 if 2018, 0 otherwise, and "2019"=1 if 2019, 0 otherwise; if neither, then it's 2017
AFDM_trt$y2017<-ifelse(AFDM_trt$Year==2017,1,0)
AFDM_trt$y2018<-ifelse(AFDM_trt$Year==2018,1,0)
AFDM_trt$y2019<-ifelse(AFDM_trt$Year==2019,1,0)
y2017<-AFDM_trt$y2017 # covariate vector for model
y2018<-AFDM_trt$y2018 # covariate vector for model
y2019<-AFDM_trt$y2019 # covariate vector for model

## we want 'stream' = 1 if PB, 0 in PA
AFDM_trt$stream<-ifelse(AFDM_trt$Stream=="PB",1,0)
PB<-AFDM_trt$stream # covariate vector for model

## we need a vector for 'day'
day<-AFDM_trt$Day  # covariate vector for model

## and we need a unique id for each pool - you did something like this earlier ... is this correct?
AFDM_trt$poolid<-0
#pb 1
AFDM_trt$poolid <- ifelse(AFDM_trt$Replicate == "1" & AFDM_trt$Stream == "PB", "1b", AFDM_trt$poolid )
#pb 2
AFDM_trt$poolid <- ifelse(AFDM_trt$Replicate == "2" & AFDM_trt$Stream == "PB", "3b", AFDM_trt$poolid )
#double check this one later
#pb 3 
AFDM_trt$poolid <- ifelse(AFDM_trt$Replicate == "3" & AFDM_trt$Stream == "PB", "11", AFDM_trt$poolid )
#pa 1 
AFDM_trt$poolid<- ifelse(AFDM_trt$Replicate == "1" & AFDM_trt$Stream == "PA", "1", AFDM_trt$poolid )
#pa 2
AFDM_trt$poolid<- ifelse(AFDM_trt$Replicate == "2" & AFDM_trt$Stream == "PA", "5a", AFDM_trt$poolid )
#pa 3
AFDM_trt$poolid <- ifelse(AFDM_trt$Replicate == "3" & AFDM_trt$Stream == "PA", "7", AFDM_trt$poolid )
#pa 5
AFDM_trt$poolid <- ifelse(AFDM_trt$Replicate == "5" & AFDM_trt$Stream == "PA", "5a", AFDM_trt$poolid) 

### 2019 
#pb,, 5, 11, 13 in 2019
AFDM_trt$poolid <- ifelse(AFDM_trt$Replicate == "5" & AFDM_trt$Stream == "PB", "5", AFDM_trt$poolid )
AFDM_trt$poolid <- ifelse(AFDM_trt$Replicate == "11" & AFDM_trt$Stream == "PB", "11", AFDM_trt$poolid )
AFDM_trt$poolid <- ifelse(AFDM_trt$Replicate == "13" & AFDM_trt$Stream == "PB", "13", AFDM_trt$poolid )
#pa, 4,6,7, in 2019
AFDM_trt$poolid <- ifelse(AFDM_trt$Replicate == "4" & AFDM_trt$Stream == "PA", "4", AFDM_trt$poolid )
AFDM_trt$poolid <- ifelse(AFDM_trt$Replicate == "6" & AFDM_trt$Stream == "PA", "6", AFDM_trt$poolid )
AFDM_trt$poolid <- ifelse(AFDM_trt$Replicate == "7" & AFDM_trt$Stream == "PA", "7", AFDM_trt$poolid )
## there would have been an easier way to do this! but model will want pools to have a number to be a random effect
AFDM_trt$pool<-0
AFDM_trt$pool<-ifelse(AFDM_trt$poolid=="1b",1,AFDM_trt$pool) #PB
AFDM_trt$pool<-ifelse(AFDM_trt$poolid=="3b",2,AFDM_trt$pool) #PB
AFDM_trt$pool<-ifelse(AFDM_trt$poolid=="11",3,AFDM_trt$pool)#PB
AFDM_trt$pool<-ifelse(AFDM_trt$poolid=="1",4,AFDM_trt$pool) #PA
AFDM_trt$pool<-ifelse(AFDM_trt$poolid=="5a",5,AFDM_trt$pool) #PA
AFDM_trt$pool<-ifelse(AFDM_trt$poolid=="7",6,AFDM_trt$pool) #PA
AFDM_trt$pool<-ifelse(AFDM_trt$poolid=="5",7,AFDM_trt$pool) #PB
AFDM_trt$pool<-ifelse(AFDM_trt$poolid=="13",8,AFDM_trt$pool) #PB
AFDM_trt$pool<-ifelse(AFDM_trt$poolid=="4",9,AFDM_trt$pool) #PA
AFDM_trt$pool<-ifelse(AFDM_trt$poolid=="6",10,AFDM_trt$pool) #PA
pool<-AFDM_trt$pool
stream<-c(1,1,1,0,0,0,1,1,0,0) #1 if pool is in PB
streamyr<-c(1,1,1,0,0,0,1,1,1,0,0,0,1,1,1,0,0,0,1,1,0,0) #1 if pool is in PB
nobs<-347 #total number afdm remaining observations!
npools<-10 #  this is the total number of unique pools  not all used in 2017 and 2018
AFDM<-AFDM_trt$AFDMrem  # the response variable
logAFDM<-log(AFDM)
###### pool and year specific random effects
AFDM_trt$poolyr<-0
AFDM_trt$poolyr<-ifelse(AFDM_trt$pool=="1" & AFDM_trt$Year=="2017",1,AFDM_trt$poolyr)
AFDM_trt$poolyr<-ifelse(AFDM_trt$pool=="2" & AFDM_trt$Year=="2017",2,AFDM_trt$poolyr)
AFDM_trt$poolyr<-ifelse(AFDM_trt$pool=="3" & AFDM_trt$Year=="2017",3,AFDM_trt$poolyr)
AFDM_trt$poolyr<-ifelse(AFDM_trt$pool=="4" & AFDM_trt$Year=="2017",4,AFDM_trt$poolyr)
AFDM_trt$poolyr<-ifelse(AFDM_trt$pool=="5" & AFDM_trt$Year=="2017",5,AFDM_trt$poolyr)
AFDM_trt$poolyr<-ifelse(AFDM_trt$pool=="6" & AFDM_trt$Year=="2017",6,AFDM_trt$poolyr)
AFDM_trt$poolyr<-ifelse(AFDM_trt$pool=="1" & AFDM_trt$Year=="2018",7,AFDM_trt$poolyr)
AFDM_trt$poolyr<-ifelse(AFDM_trt$pool=="2" & AFDM_trt$Year=="2018",8,AFDM_trt$poolyr)
AFDM_trt$poolyr<-ifelse(AFDM_trt$pool=="3" & AFDM_trt$Year=="2018",9,AFDM_trt$poolyr)
AFDM_trt$poolyr<-ifelse(AFDM_trt$pool=="4" & AFDM_trt$Year=="2018",10,AFDM_trt$poolyr)
AFDM_trt$poolyr<-ifelse(AFDM_trt$pool=="5" & AFDM_trt$Year=="2018",11,AFDM_trt$poolyr)
AFDM_trt$poolyr<-ifelse(AFDM_trt$pool=="6" & AFDM_trt$Year=="2018",12,AFDM_trt$poolyr)
AFDM_trt$poolyr<-ifelse(AFDM_trt$pool=="1" & AFDM_trt$Year=="2019",13,AFDM_trt$poolyr)
AFDM_trt$poolyr<-ifelse(AFDM_trt$pool=="2" & AFDM_trt$Year=="2019",14,AFDM_trt$poolyr)
AFDM_trt$poolyr<-ifelse(AFDM_trt$pool=="3" & AFDM_trt$Year=="2019",15,AFDM_trt$poolyr)
AFDM_trt$poolyr<-ifelse(AFDM_trt$pool=="4" & AFDM_trt$Year=="2019",16,AFDM_trt$poolyr)
AFDM_trt$poolyr<-ifelse(AFDM_trt$pool=="5" & AFDM_trt$Year=="2019",17,AFDM_trt$poolyr)
AFDM_trt$poolyr<-ifelse(AFDM_trt$pool=="6" & AFDM_trt$Year=="2019",18,AFDM_trt$poolyr)
AFDM_trt$poolyr<-ifelse(AFDM_trt$pool=="7" & AFDM_trt$Year=="2019",19,AFDM_trt$poolyr)
AFDM_trt$poolyr<-ifelse(AFDM_trt$pool=="8" & AFDM_trt$Year=="2019",20,AFDM_trt$poolyr)
AFDM_trt$poolyr<-ifelse(AFDM_trt$pool=="9" & AFDM_trt$Year=="2019",21,AFDM_trt$poolyr)
AFDM_trt$poolyr<-ifelse(AFDM_trt$pool=="10" & AFDM_trt$Year=="2019",22,AFDM_trt$poolyr)

poolyr<-AFDM_trt$poolyr
npoolyr<-22



########## Model II. Decomp differs between streams (and treatments) ###################
sink("treatment_and_stream.jags")    
cat("
    model { 
### specify how the data relate to variables; this is the ecological process 
    for(i in 1:nobs){ #  total number afdm remaining observations!
    
          logAFDM[i] ~ dnorm(mu[i], tau)  #each observation is a drawn from a distribution with mean AFDM remaining for that day, treatment and pool
          mu[i]<- int[i] + k[i]*day[i]
          int[i]<-alpha0
          
          k[i]~dnorm(mu2[i],precslope)
          
          mu2[i]<-b0*(1-treatment[i]) + b1*treatment[i] + 
          b2*PB[i]*(1-treatment[i]) + b3*PB[i]*treatment[i] + 
          random.effect[poolyr[i]]
          
    }

    c.decay.PA<-b0
    ec.decay.PA<-b1
    c.decay.PB<-b0+b2
    ec.decay.PB<-b1+b3
    
### define the priors for the unknowns in the model
alpha0 ~ dunif(4,5)  # intercept is between 55 and 148 %AFDM
 b0 ~ dnorm(0, 0.001)
 b1 ~ dnorm(0, 0.001) 
 b2 ~ dnorm(0, 0.001) 
 b3 ~ dnorm(0, 0.001) 

 
## we also have two variance terms, tau (variance aroung the mean on each date) and the random effect for pool
## tau (representing variance aroung the mean on each date) includes observation + other sources of effor
  tau <- 1 / sigma^2  # 1/variance (precision) = 1/SD-squared
  sigma ~ dunif(0,20) # this is the prior on the standard deviation among obs AFDM remaining on a given date, pool, treatement
  
  precslope<-1/sigma.k^2
  sigma.k~dunif(0, 1) #prior on uncertainty in any individual k estimate based on a leafpack
  
## random variation among pools -  
for(i in 1:npoolyr){ 
      random.effect[i] ~ dnorm(0,tau.pool) #tau.pool is unmodeled variance among pools
      }
  tau.pool <- 1 / sigma.pool^2  #tau is actually expressed a precision=1/variance, I.e 1/ sigma.pool^2
  sigma.pool ~ dunif(0, 1) # this is the prior on the standard deviation in r among pools

    }  ### this ends the code for the model
",fill = T)
sink() 


## now get all this into JAGS using a win.data statement
win.data <- list(logAFDM = logAFDM, npoolyr=npoolyr, nobs=nobs, day=day, treatment=treatment, poolyr=poolyr, PB=PB) 

## set starting points for the unknowns to be estimated in the regression
inits <- function(){list(alpha0=4.6,  b0 = 0.0001, b1 = 0, b2 = 0, b3 = 0, 
                         sigma = 10, sigma.pool = 1, sigma.k = 1)}

## specify the parameters for which we want posterior estimates - mean decay rate and treatment effect, and variances
## choose params - the second set gives output for plotting, in this case for 2017 only
params<-c("alpha0", "b0","b1","b2","b3", "sigma","sigma.pool",  "c.decay.PA", "c.decay.PB", "ec.decay.PA",
          "ec.decay.PB", "sigma.k")

# MCMC settings
ni <- 30000   ## number of iterations - want enough to converge on a stable answer
nt <- 3      ## this days use value from every third iteration, b/c the values are temporally dependent
nb <- 5000    ## this is the number of beginning iterations to discard
nc <- 3      ## this is the number of independent 'chains' to run at once

# Call JAGS from R (BRT 5 min)

out.treatment_and_stream<- jags(win.data, inits, params, "treatment_and_stream.jags", n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, working.directory = getwd())
print(out.treatment_and_stream, dig = 4)

CA<-out.treatment_and_stream$BUGSoutput$mean$c.decay.PA
CB<-out.treatment_and_stream$BUGSoutput$mean$c.decay.PB
EA<-out.treatment_and_stream$BUGSoutput$mean$ec.decay.PA
EB<-out.treatment_and_stream$BUGSoutput$mean$ec.decay.PB
lower_ci_ca<-quantile(out.treatment_and_stream$BUGSoutput$sims.list$c.decay.PA[ ,1 ], 0.025)
upper_ci_ca<-quantile(out.treatment_and_stream$BUGSoutput$sims.list$c.decay.PA[ ,1 ], 0.975)
lower_ci_cb<-quantile(out.treatment_and_stream$BUGSoutput$sims.list$c.decay.PB[ ,1 ], 0.025)
upper_ci_cb<-quantile(out.treatment_and_stream$BUGSoutput$sims.list$c.decay.PB[ ,1 ], 0.975)

lower_ci_ea<-quantile(out.treatment_and_stream$BUGSoutput$sims.list$ec.decay.PA[ ,1 ], 0.025)
upper_ci_ea<-quantile(out.treatment_and_stream$BUGSoutput$sims.list$ec.decay.PA[ ,1 ], 0.975)
lower_ci_eb<-quantile(out.treatment_and_stream$BUGSoutput$sims.list$ec.decay.PB[ ,1 ], 0.025)
upper_ci_eb<-quantile(out.treatment_and_stream$BUGSoutput$sims.list$ec.decay.PB[ ,1 ], 0.975)
global_size=12
Treatment<-c("Control", "Exclusion", "Control", "Exclusion")
k<--c(CA, EA, CB, EB)
lowCI<--c(upper_ci_ca, upper_ci_ea,upper_ci_cb, upper_ci_eb )
upCI<--c(lower_ci_ca, lower_ci_ea,lower_ci_cb, lower_ci_eb )
stream<-c("PA","PA","PB","PB")
datatoplot<-data.frame(Treatment,k,lowCI,upCI, stream)


model1_pub <- ggplot(datatoplot, aes(x=Treatment, y=k, group=Treatment))+
  geom_point(aes(fill=Treatment, shape=Treatment), color="black", size=2)+
  scale_shape_manual(values=c(16, 15))+
  geom_errorbar(aes(ymin=lowCI, ymax=upCI, linetype=Treatment), width=.2,
                position=position_dodge(0.05))+
  scale_linetype_manual(values = c("solid", "dashed")) +
  facet_wrap(~stream, strip.position = "bottom") +
  theme_bw(base_size = global_size) +
  theme(
    strip.background = element_blank(),
    strip.placement = "outside")+
  ylab(bquote('Mean decomposition rate'~italic (("-k"~ "day"^-1))))+
  xlab("Treament / stream")

setwd("C:/Users/Max/Desktop/UGA/Pringle Lab/R/msDecomp/Final_Models/FINAL_GOOD/Revision/figs_tables")
ggsave ("model1_pub.tiff", model1_pub, dpi=400, width=12.7, height=9, units="cm")

Model1_table <- xtable(out.treatment_and_stream$BUGSoutput$summary[, ])
write.csv(Model1_table, "C:/Users/Max/Desktop/UGA/Pringle Lab/R/msDecomp/Final_Models/Final_GOOD/Revision/model1table.2.8.csv")#if using on different computer will need to update this for location to work



#shrimp effects three years

sink("leafdecompallyrs.jags")    ## give it a name
cat("
    model { 
### specify how the data relate to variables; this is the ecological process 
    for(i in 1:nobs){ #  total number afdm remaining observations!
    
          logAFDM[i] ~ dnorm(mu[i], tau)  #each observation is a drawn from a distribution with mean AFDM remaining for that day, treatment and pool
          mu[i]<- int[i] + k[i]*day[i]
          int[i]<-alpha0 + alpha1*y2018[i] + alpha2*y2019[i]
          
          k[i]~dnorm(mu2[i],precslope)
          
          mu2[i]<-b0*(1-treatment[i]) + b1*treatment[i] + b2*y2018[i]*(1-treatment[i]) + b3*y2019[i]*(1-treatment[i]) + 
          b4*PB[i]*(1-treatment[i]) + b5*PB[i]*treatment[i] + b6*y2018[i]*treatment[i] +b7*y2019[i]*treatment[i]+
          b8*PB[i]*(1-treatment[i])*y2018[i] + b9*PB[i]*treatment[i]*y2018[i] + b10*PB[i]*(1-treatment[i])*y2019[i] +
          b11*PB[i]*(treatment[i])*y2019[i]+
          random.effect[poolyr[i]]
          
    }
##  This model lets each year have its own decay rate in c and ec, but assumes that the differences between PA and PB are constant across years   
## adding a 'treatment' or a '(1-treatment)' to each term allows c and ec to have their own coefficients
## let's derive the estimated treatment decay, each year and stream

    c.decay.2017A<-b0
    ec.decay.2017A<-b1
    c.decay.2018A<-b0+b2
    ec.decay.2018A<-b1+b6
    c.decay.2019A<-b0+b3
    ec.decay.2019A<-b1+b7
    
    c.decay.2017B<-b0 +b4   
    ec.decay.2017B<-b1 +b5
    c.decay.2018B<-b0+b2 +b4 + b8
    ec.decay.2018B<-b1+b6 +b5 + b9
    c.decay.2019B<-b0+b3 +b4 + b10
    ec.decay.2019B<-b1+b7 +b5 + b11

### define the priors for the unknowns in the model
alpha0 ~ dunif(4,5)  # intercept is between 55 and 148 %AFDM
alpha1~ dnorm(0, 0.001)
alpha2 ~ dnorm(0, 0.001)
 b0 ~ dnorm(0, 0.001)
 b1 ~ dnorm(0, 0.001) #this says we have no idea how the treatment affects decay rate
 b2 ~ dnorm(0, 0.001) # or how 2018 differs from 2017 in controls
 b3 ~ dnorm(0, 0.001) # or how 2019 differs from 2017 in controls
 b4 ~ dnorm(0, 0.001) # or how PB differs from PA in controls
 b5 ~ dnorm(0, 0.001) # or how PB differes from PA in exclusion
 b6 ~ dnorm(0, 0.001) # or how 2018 differs from 2017 in exlcusion
 b7 ~ dnorm(0, 0.001) # or how 2019 differs from 2017 in exlcusion
 b8 ~ dnorm(0, 0.001)
 b9 ~ dnorm(0, 0.001)
 b10 ~ dnorm(0, 0.001)
 b11~ dnorm(0, 0.001)
  b12~ dnorm(0, 0.001)


 
## we also have two variance terms, tau (variance aroung the mean on each date) and the random effect for pool
## tau (representing variance aroung the mean on each date) includes observation + other sources of effor
  tau <- 1 / sigma^2  # 1/variance (precision) = 1/SD-squared
  sigma ~ dunif(0,20) # this is the prior on the standard deviation among obs AFDM remaining on a given date, pool, treatement
  
  precslope<-1/sigma.k^2
  sigma.k~dunif(0, 1) #prior on uncertainty in any individual k estimate based on a leafpack
  
## random variation among pools -  
for(i in 1:npoolyr){ 
      random.effect[i] ~ dnorm(0,tau.pool) #tau.pool is unmodeled variance among pools
      }
  tau.pool <- 1 / sigma.pool^2  #tau is actually expressed a precision=1/variance, I.e 1/ sigma.pool^2
  sigma.pool ~ dunif(0, 1) # this is the prior on the standard deviation in r among pools

## effects?
exclusion_2017_A<-(b0-b1)/b0 #proportional decrease in k, A, 2017
exclusion_2018_A<-((b0+b2)-(b1+b6))/(b0+b2) ##proportional decrease in k, A, 2018
exclusion_2019_A<-((b0+b3)-(b1+b7))/(b0+b3) ##proportional decrease in k, A, 2019
exclusion_2017_B<-((b0+b4)-(b1+b5))/(b0+b4) #proportional decrease in k, B, 2017
exclusion_2018_B<-((b0+b2+b4+b8)-(b1+b6+b5+b9))/(b0+b2+b4+b8) ##proportional decrease in k, B, 2018
exclusion_2019_B<-((b0+b3+b4+b10)-(b1+b7+b5+b11))/(b0+b3+b4+b10) ##proportional decrease in k, B, 2019

    }  ### this ends the code for the model
",fill = T)
sink() 


## now get all this into JAGS using a win.data statement
win.data <- list(logAFDM = logAFDM, npoolyr=npoolyr, nobs=nobs, day=day, treatment=treatment, poolyr=poolyr,
                 y2018=y2018, y2019=y2019, PB=PB) #, streamyr=streamyr)

## set starting points for the unknowns to be estimated in the regression
inits <- function(){list(alpha0=4.6, alpha1=0, alpha2=0, b0 = 0.0001, b1 = 0, b2 = 0, b3 = 0, b4 = 0, b5 = 0, b6 = 0,
                         b7 = 0, b8 = 0, b9=0, b10=0, b11=0, sigma = 10, sigma.pool = 1, sigma.k = 1)}

## specify the parameters for which we want posterior estimates - mean decay rate and treatment effect, and variances
## choose params - the second set gives output for plotting, in this case for 2017 only
params<-c("alpha0","alpha1", "alpha2", "b0","b1","b2","b3","b4", "b5", "b6", "b7", "b8", "b9", "b10", "b11", "sigma","sigma.pool", "ec.decay.2017A",
          "ec.decay.2017B", "ec.decay.2018A", "ec.decay.2018B", "ec.decay.2019A", "ec.decay.2019B", "c.decay.2017A", "c.decay.2018A", "c.decay.2019A",
          "c.decay.2017B", "c.decay.2018B", "c.decay.2019B", "sigma.k",
          "exclusion_2017_A", "exclusion_2018_A", "exclusion_2019_A", "exclusion_2017_B", "exclusion_2018_B", "exclusion_2019_B")#, 
# "k_c_2017", "k_c_2018", "k_c_2019", "k_ec_2017", "k_ec_2018", "k_ec_2019")

#params<-c("int","b0","b1","b2","b3","b4", "b5", "b6", "b7", "b8", "b9", "b10", "b11", sigma","sigma.pool","random.effect")
# MCMC settings
ni <- 30000   ## number of iterations - want enough to converge on a stable answer
nt <- 3      ## this days use value from every third iteration, b/c the values are temporally dependent
nb <- 5000    ## this is the number of beginning iterations to discard
nc <- 3      ## this is the number of independent 'chains' to run at once

# Call JAGS from R (BRT 5 min)
#out.decay.1 uses first set of parameter
out.decay.global.sjw<- jags(win.data, inits, params, "leafdecompallyrs.jags", n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, working.directory = getwd())
print(out.decay.global.sjw, dig = 4)

#make table of output
Model1_table <- xtable(out.decay.global$BUGSoutput$summary[, ])
#option 1 export right to csv where you can make stylist changes and rename rows
write.csv(Model1_table, "C:/Users/Max/Desktop/UGA/Pringle Lab/R/msDecomp/Final_Models/Final_GOOD/Revision/Model_global_2.8.csv")#if using on different computer will need to update this for location to work

mean.c.2017.A<-mean(out.decay.global.sjw$BUGSoutput$sims.list$c.decay.2017A[ , ])
mean.ec.2017.A<-mean(out.decay.global.sjw$BUGSoutput$sims.list$ec.decay.2017A[ , ])
mean.c.2017.B<-mean(out.decay.global.sjw$BUGSoutput$sims.list$c.decay.2017B[ , ])
mean.ec.2017.B<-mean(out.decay.global.sjw$BUGSoutput$sims.list$ec.decay.2017B[ , ])

mean.c.2018.A<-mean(out.decay.global.sjw$BUGSoutput$sims.list$c.decay.2018A[ , ])
mean.ec.2018.A<-mean(out.decay.global.sjw$BUGSoutput$sims.list$ec.decay.2018A[,])
mean.c.2018.B<-mean(out.decay.global.sjw$BUGSoutput$sims.list$c.decay.2018B[,])
mean.ec.2018.B<-mean(out.decay.global.sjw$BUGSoutput$sims.list$ec.decay.2018B[ , ])

mean.c.2019.A<-mean(out.decay.global.sjw$BUGSoutput$sims.list$c.decay.2019A[ , ])
mean.ec.2019.A<-mean(out.decay.global.sjw$BUGSoutput$sims.list$ec.decay.2019A[ , ])
mean.c.2019.B<-mean(out.decay.global.sjw$BUGSoutput$sims.list$c.decay.2019B[ , ])
mean.ec.2019.B<-mean(out.decay.global.sjw$BUGSoutput$sims.list$ec.decay.2019B[ , ])

lower_ci_ca_17<-quantile(out.decay.global.sjw$BUGSoutput$sims.list$c.decay.2017A[ ,1 ], 0.025)
upper_ci_ca_17<-quantile(out.decay.global.sjw$BUGSoutput$sims.list$c.decay.2017A[ ,1 ], 0.975)
lower_ci_cb_17<-quantile(out.decay.global.sjw$BUGSoutput$sims.list$c.decay.2017B[ ,1 ], 0.025)
upper_ci_cb_17<-quantile(out.decay.global.sjw$BUGSoutput$sims.list$c.decay.2017B[ ,1 ], 0.975)

lower_ci_ea_17<-quantile(out.decay.global.sjw$BUGSoutput$sims.list$ec.decay.2017A[ ,1 ], 0.025)
upper_ci_ea_17<-quantile(out.decay.global.sjw$BUGSoutput$sims.list$ec.decay.2017A[ ,1 ], 0.975)
lower_ci_eb_17<-quantile(out.decay.global.sjw$BUGSoutput$sims.list$ec.decay.2017B[ ,1 ], 0.025)
upper_ci_eb_17<-quantile(out.decay.global.sjw$BUGSoutput$sims.list$ec.decay.2017B[ ,1 ], 0.975)

lower_ci_ca_18<-quantile(out.decay.global.sjw$BUGSoutput$sims.list$c.decay.2018A[ ,1 ], 0.025)
upper_ci_ca_18<-quantile(out.decay.global.sjw$BUGSoutput$sims.list$c.decay.2018A[ ,1 ], 0.975)
lower_ci_cb_18<-quantile(out.decay.global.sjw$BUGSoutput$sims.list$c.decay.2018B[ ,1 ], 0.025)
upper_ci_cb_18<-quantile(out.decay.global.sjw$BUGSoutput$sims.list$c.decay.2018B[ ,1 ], 0.975)

lower_ci_ea_18<-quantile(out.decay.global.sjw$BUGSoutput$sims.list$ec.decay.2018A[ ,1 ], 0.025)
upper_ci_ea_18<-quantile(out.decay.global.sjw$BUGSoutput$sims.list$ec.decay.2018A[ ,1 ], 0.975)
lower_ci_eb_18<-quantile(out.decay.global.sjw$BUGSoutput$sims.list$ec.decay.2018B[ ,1 ], 0.025)
upper_ci_eb_18<-quantile(out.decay.global.sjw$BUGSoutput$sims.list$ec.decay.2018B[ ,1 ], 0.975)

lower_ci_ca_19<-quantile(out.decay.global.sjw$BUGSoutput$sims.list$c.decay.2019A[ ,1 ], 0.025)
upper_ci_ca_19<-quantile(out.decay.global.sjw$BUGSoutput$sims.list$c.decay.2019A[ ,1 ], 0.975)
lower_ci_cb_19<-quantile(out.decay.global.sjw$BUGSoutput$sims.list$c.decay.2019B[ ,1 ], 0.025)
upper_ci_cb_19<-quantile(out.decay.global.sjw$BUGSoutput$sims.list$c.decay.2019B[ ,1 ], 0.975)

lower_ci_ea_19<-quantile(out.decay.global.sjw$BUGSoutput$sims.list$ec.decay.2019A[ ,1 ], 0.025)
upper_ci_ea_19<-quantile(out.decay.global.sjw$BUGSoutput$sims.list$ec.decay.2019A[ ,1 ], 0.975)
lower_ci_eb_19<-quantile(out.decay.global.sjw$BUGSoutput$sims.list$ec.decay.2019B[ ,1 ], 0.025)
upper_ci_eb_19<-quantile(out.decay.global.sjw$BUGSoutput$sims.list$ec.decay.2019B[ ,1 ], 0.975)

sd.c.2017.A<-sd(out.decay.global.sjw$BUGSoutput$sims.list$c.decay.2017A[ , ])
sd.ec.2017.A<-sd(out.decay.global.sjw$BUGSoutput$sims.list$ec.decay.2017A[ , ])
sd.c.2017.B<-sd(out.decay.global.sjw$BUGSoutput$sims.list$c.decay.2017B[ , ])
sd.ec.2017.B<-sd(out.decay.global.sjw$BUGSoutput$sims.list$ec.decay.2017B[ , ])

sd.c.2018.A<-sd(out.decay.global.sjw$BUGSoutput$sims.list$c.decay.2018A[ , ])
sd.ec.2018.A<-sd(out.decay.global.sjw$BUGSoutput$sims.list$ec.decay.2018A[,])
sd.c.2018.B<-sd(out.decay.global.sjw$BUGSoutput$sims.list$c.decay.2018B[,])
sd.ec.2018.B<-sd(out.decay.global.sjw$BUGSoutput$sims.list$ec.decay.2018B[ , ])

sd.c.2019.A<-sd(out.decay.global.sjw$BUGSoutput$sims.list$c.decay.2019A[ , ])
sd.ec.2019.A<-sd(out.decay.global.sjw$BUGSoutput$sims.list$ec.decay.2019A[ , ])
sd.c.2019.B<-sd(out.decay.global.sjw$BUGSoutput$sims.list$c.decay.2019B[ , ])
sd.ec.2019.B<-sd(out.decay.global.sjw$BUGSoutput$sims.list$ec.decay.2019B[ , ])

means_bar <- c(mean.c.2017.A, mean.ec.2017.A, mean.c.2017.B, mean.ec.2017.B, 
               mean.c.2018.A, mean.ec.2018.A, mean.c.2018.B, mean.ec.2018.B,
               mean.c.2019.A, mean.ec.2019.A, mean.c.2019.B, mean.ec.2019.B)
sd_bar <- c(sd.c.2017.A, sd.ec.2017.A, sd.c.2017.B, sd.ec.2017.B, 
            sd.c.2018.A, sd.ec.2018.A, sd.c.2018.B, sd.ec.2018.B,
            sd.c.2019.A, sd.ec.2019.A, sd.c.2019.B, sd.ec.2019.B)
lower<--c(upper_ci_ca_17, upper_ci_ea_17, upper_ci_cb_17, upper_ci_eb_17,
          upper_ci_ca_18, upper_ci_ea_18, upper_ci_cb_18, upper_ci_eb_18,
          upper_ci_ca_19, upper_ci_ea_19, upper_ci_cb_19, upper_ci_eb_19)
upper<--c(lower_ci_ca_17, lower_ci_ea_17, lower_ci_cb_17, lower_ci_eb_17,
          lower_ci_ca_18, lower_ci_ea_18, lower_ci_cb_18, lower_ci_eb_18,
          lower_ci_ca_19, lower_ci_ea_19, lower_ci_cb_19, lower_ci_eb_19)
wherewhat_bar<-c(rep ("c.2017.A", 1), rep ("ec.2017.A", 1), rep ("c.2017.B", 1), rep ("ec.2017.B", 1,),
                 rep ("c.2018.A", 1), rep ("ec.2018.A", 1), rep ("c.2018.B", 1), rep ("ec.2018.B", 1,), 
                 rep ("c.2019.A", 1), rep ("ec.2019.A", 1), rep ("c.2019.B", 1), rep ("ec.2019.B", 1,))
Treatment<-c(rep("Control",1), rep("Exclusion",1),rep("Control",1), rep("Exclusion",1),
             rep("Control",1), rep("Exclusion",1),rep("Control",1), rep("Exclusion",1),
             rep("Control",1), rep("Exclusion",1),rep("Control",1), rep("Exclusion",1) )
stream_bar<-c(rep("PA", 2), rep("PB",2),
              rep("PA", 2), rep("PB",2),
              rep("PA", 2), rep("PB",2))
year_bar <- c(rep("Exp 1", 4), rep("Exp 2", 4), rep("Exp 3", 4))


alltogether_bar<-data.frame(wherewhat_bar, Treatment, stream_bar, means_bar, sd_bar, year_bar, upper, lower)
alltogether_bar$mean_k<--alltogether_bar$means_bar

pd <- position_dodge(0.5) 
plot<-ggplot(alltogether_bar, aes(x=year_bar, y=mean_k, group=Treatment))+
  geom_point(position=pd, stat="identity", aes(shape=Treatment), size=3)+
  geom_errorbar(data=alltogether_bar, position=pd,aes(ymin= mean_k-sd_bar, ymax=mean_k+sd_bar, linetype=Treatment), width=0.3)+
  facet_wrap(~stream_bar) +
  theme_bw(base_size = global_size) +
  theme(
    strip.background = element_blank(),
    strip.text.x = element_blank()
  )+
  xlab(" ") +
  ylab(bquote(' Mean Decomposition Rate '~("k day" ^-1)))

plot+labs(x = c("PA                                 PB")) 

## with 95% credible intervals
plot<-ggplot(alltogether_bar, aes(x=year_bar, y=mean_k, group=Treatment))+
  geom_point(position=pd, stat="identity", aes(shape=Treatment), size=3)+
  geom_errorbar(data=alltogether_bar, position=pd,aes(ymin= lower, ymax=upper, linetype=Treatment), width=0.3)+
  facet_wrap(~stream_bar) +
  theme_bw(base_size = global_size) +
  theme(
    strip.background = element_blank(),
    strip.text.x = element_blank()
  )+
  xlab(" ") +
  ylab(bquote(' Mean Decomposition Rate '~("k day" ^-1)))

plot+labs(x = c("PA                                 PB")) 


model2_pub <- ggplot(alltogether_bar, aes(x=year_bar, y=mean_k, group=Treatment))+
  geom_point(position=pd, stat="identity", aes(shape=Treatment), size=2)+
  scale_shape_manual(values=c(16, 15))+
  geom_errorbar(data=alltogether_bar, position=pd,aes(ymin= lower, ymax=upper, linetype=Treatment), width=0.3)+
  scale_linetype_manual(values = c("solid", "dashed")) +
  facet_wrap(~stream_bar, strip.position = "bottom") +
  theme_bw(base_size = global_size) +
  theme(
    strip.background = element_blank(),
    strip.placement = "outside")+
  ylab(bquote('Mean decomposition rate'~italic (("-k"~ "day"^-1))))+
  xlab("Experiment / stream")
  
ggsave ("model2_pub.tiff", model2_pub, dpi=400, width=12.7, height=9, units="cm")
