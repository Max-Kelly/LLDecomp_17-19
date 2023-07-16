library(rjags)
library(R2jags) 
library(xtable)
library(tidyverse)
library(dplyr)
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



########## Model I. Decomp differs between streams and treatments ###################
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


########## Model II. Decomp differs between streams, treatments, and years) ###################

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
                 y2018=y2018, y2019=y2019, PB=PB)

## set starting points for the unknowns to be estimated in the regression
inits <- function(){list(alpha0=4.6, alpha1=0, alpha2=0, b0 = 0.0001, b1 = 0, b2 = 0, b3 = 0, b4 = 0, b5 = 0, b6 = 0,
                         b7 = 0, b8 = 0, b9=0, b10=0, b11=0, sigma = 10, sigma.pool = 1, sigma.k = 1)}

## specify the parameters for which we want posterior estimates - mean decay rate and treatment effect, and variances
params<-c("alpha0","alpha1", "alpha2", "b0","b1","b2","b3","b4", "b5", "b6", "b7", "b8", "b9", "b10", "b11", "sigma","sigma.pool", "ec.decay.2017A",
          "ec.decay.2017B", "ec.decay.2018A", "ec.decay.2018B", "ec.decay.2019A", "ec.decay.2019B", "c.decay.2017A", "c.decay.2018A", "c.decay.2019A",
          "c.decay.2017B", "c.decay.2018B", "c.decay.2019B", "sigma.k",
          "exclusion_2017_A", "exclusion_2018_A", "exclusion_2019_A", "exclusion_2017_B", "exclusion_2018_B", "exclusion_2019_B")#, 

# MCMC settings
ni <- 30000   ## number of iterations - want enough to converge on a stable answer
nt <- 3      ## this days use value from every third iteration, b/c the values are temporally dependent
nb <- 5000    ## this is the number of beginning iterations to discard
nc <- 3      ## this is the number of independent 'chains' to run at once

# Call JAGS from R (BRT 5 min)
out.decay.global.sjw<- jags(win.data, inits, params, "leafdecompallyrs.jags", n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, working.directory = getwd())
print(out.decay.global.sjw, dig = 4)

########## Model III. Decomp differs based upon visual counts of Shrimp abundance in 2019  ###################

max_1719 <-  read.csv ("C:/Users/Max/Desktop/UGA/Pringle Lab/R/msDecomp/17-19Decomp.6.27.csv", fileEncoding = 'UTF-8-BOM')

dat<-max_1719

AFDM_1 <- dat[,c(1,2,3,4,5,9)]
AFDM_trt<-AFDM_1 [AFDM_1$Treatment != "Handling", ]

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
AFDM_trt$poolid <- ifelse(AFDM_trt$Replicate == "1" & AFDM_trt$Stream == "PB", "1a", AFDM_trt$poolid )
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

### 2019 - not  sure I've assigned these to the correct pools:
#pb,, 5, 11, 13 in 2019
AFDM_trt$poolid <- ifelse(AFDM_trt$Replicate == "5" & AFDM_trt$Stream == "PB", "5", AFDM_trt$poolid )
AFDM_trt$poolid <- ifelse(AFDM_trt$Replicate == "11" & AFDM_trt$Stream == "PB", "11", AFDM_trt$poolid )
AFDM_trt$poolid <- ifelse(AFDM_trt$Replicate == "13" & AFDM_trt$Stream == "PB", "13", AFDM_trt$poolid )
#pa, 4,6,7, in 2019
AFDM_trt$poolid <- ifelse(AFDM_trt$Replicate == "4" & AFDM_trt$Stream == "PA", "4", AFDM_trt$poolid )
AFDM_trt$poolid <- ifelse(AFDM_trt$Replicate == "6" & AFDM_trt$Stream == "PA", "6", AFDM_trt$poolid )
AFDM_trt$poolid <- ifelse(AFDM_trt$Replicate == "7" & AFDM_trt$Stream == "PA", "7", AFDM_trt$poolid )
## there would have been an easier way to do this! but model will want pools to have a number to be a random effect I think
AFDM_trt$pool<-0
AFDM_trt$pool<-ifelse(AFDM_trt$poolid=="1a",1,AFDM_trt$pool)
AFDM_trt$pool<-ifelse(AFDM_trt$poolid=="3b",2,AFDM_trt$pool)
AFDM_trt$pool<-ifelse(AFDM_trt$poolid=="11",3,AFDM_trt$pool)
AFDM_trt$pool<-ifelse(AFDM_trt$poolid=="1",4,AFDM_trt$pool)
AFDM_trt$pool<-ifelse(AFDM_trt$poolid=="5a",5,AFDM_trt$pool)
AFDM_trt$pool<-ifelse(AFDM_trt$poolid=="7",6,AFDM_trt$pool)
AFDM_trt$pool<-ifelse(AFDM_trt$poolid=="5",7,AFDM_trt$pool)
AFDM_trt$pool<-ifelse(AFDM_trt$poolid=="13",8,AFDM_trt$pool)
AFDM_trt$pool<-ifelse(AFDM_trt$poolid=="4",9,AFDM_trt$pool)
AFDM_trt$pool<-ifelse(AFDM_trt$poolid=="6",10,AFDM_trt$pool)
pool<-AFDM_trt$pool

#load shrimp data
shrimp_dat <- read.csv ("C:/Users/Max/Desktop/UGA/Pringle Lab/R/msDecomp/2019_shrimpObs.csv", fileEncoding = "UTF-8-BOM")

means<- NULL

#get both day and night means
means <- shrimp_dat  %>%
  group_by(Site_ID, ToD ) %>%
  summarise_at(vars(Total), list(name = mean))

means$Time <- ifelse(means$ToD == "Diurnal", 1,0) ##1 is daytime  0 is nighttime


# create new data frames, one for day time obs one for night time obs
means_day <- subset(means, Time == 1)
QPA1_day <- as.numeric(means_day[1,3])
QPA4_day <- as.numeric(means_day[2,3])
QPA5_day <- as.numeric(means_day[3,3])
QPA6_day <- as.numeric(means_day[4,3])
QPA7_day <- as.numeric(means_day[5,3])
QPB1_day <- as.numeric(means_day[6,3])
QPB2_day <- as.numeric(means_day[9,3])
QPB5_day <- as.numeric(means_day[10,3])
QPB11_day <-as.numeric( means_day[7,3])
QPB13_day <- as.numeric(means_day[8,3])


#subset dataframe to just a 2019 dataframe
AFDM_trt_2019 <- subset(AFDM_trt, Year == 2019)

#add in daytime shrimp obs
AFDM_trt_2019$shrimpDay = 1 # make column

#use ifelse statements to put shrimp count into corresponding correct rows
AFDM_trt_2019$shrimpDay <- ifelse(AFDM_trt_2019$poolid == '1a',   QPB1_day,
                                  ifelse(AFDM_trt_2019$poolid == '3b', QPB2_day,
                                         ifelse(AFDM_trt_2019$poolid == '11', QPB11_day,
                                                ifelse(AFDM_trt_2019$poolid == '13', QPB13_day,
                                                       ifelse(AFDM_trt_2019$poolid == '1', QPA1_day,
                                                              ifelse(AFDM_trt_2019$poolid == '4', QPA4_day,
                                                                     ifelse(AFDM_trt_2019$poolid == '5a', QPA5_day,
                                                                            ifelse(AFDM_trt_2019$poolid == '6', QPA6_day,
                                                                                   ifelse(AFDM_trt_2019$poolid == '7', QPA7_day,
                                                                                          ifelse(AFDM_trt_2019$poolid == '5', QPB5_day, "ok"))))))))))



#create vector out of shrimp daytime counts
AFDM_trt_2019$shrimpDay <- as.numeric(AFDM_trt_2019$shrimpDay)
summarySE(data=AFDM_trt_2019, measurevar = "shrimpDay", groupvars = "poolid")
shrimp_day <-NULL
shrimp_day.2019<-as.numeric(AFDM_trt_2019$shrimpDay)

mean(shrimp_day.2019)
sd(shrimp_day.2019)

shrimp_day <- as.vector(scale(shrimp_day.2019))


## we want 'stream' = 1 if PB, 0 in PA
PB.2019<-AFDM_trt_2019$stream # covariate vector for model

## we need a vector for 'day'
day.2019<-AFDM_trt_2019$Day  # covariate vector for model

treatment.2019<-AFDM_trt_2019$EC # covariate vector for model



pool.2019<-AFDM_trt_2019$pool
nobs.2019<-155 #total number afdm remaining observations!
npools.2019<-10 # i think this is the total number of unique pools - right?  not all used in 2017 and 2018
AFDM.2019<-AFDM_trt_2019$AFDMrem  # the response variable

shrimp.for.plots <- as.vector (c( -1.25, -1, -0.75,
                                  -0.5, -0.25, 0, 0.25, 0.5, 0.75, 1,
                                  1.25, 1.5, 1.75, 2, 2.25, 2.5, 2.75))
stream<-c(1,1,1,0,0,0,1,1,0,0) #1 if pool is in PB
streamyr<-c(1,1,1,0,0,0,1,1,1,0,0,0,1,1,1,0,0,0,1,1,0,0) #1 if pool is in PB
nobs<-347 #total number afdm remaining observations!
npools<-10 #  this is the total number of unique pools  not all used in 2017 and 2018
AFDM<-AFDM_trt$AFDMrem  # the response variable
logAFDM<-log(AFDM)
###### pool and year specific random effects
AFDM_trt_2019$poolyr<-0

AFDM_trt_2019$poolyr<-ifelse(AFDM_trt_2019$pool=="1" & AFDM_trt_2019$Year=="2019",1,AFDM_trt_2019$poolyr)
AFDM_trt_2019$poolyr<-ifelse(AFDM_trt_2019$pool=="2" & AFDM_trt_2019$Year=="2019",2,AFDM_trt_2019$poolyr)
AFDM_trt_2019$poolyr<-ifelse(AFDM_trt_2019$pool=="3" & AFDM_trt_2019$Year=="2019",3,AFDM_trt_2019$poolyr)
AFDM_trt_2019$poolyr<-ifelse(AFDM_trt_2019$pool=="4" & AFDM_trt_2019$Year=="2019",4,AFDM_trt_2019$poolyr)
AFDM_trt_2019$poolyr<-ifelse(AFDM_trt_2019$pool=="5" & AFDM_trt_2019$Year=="2019",5,AFDM_trt_2019$poolyr)
AFDM_trt_2019$poolyr<-ifelse(AFDM_trt_2019$pool=="6" & AFDM_trt_2019$Year=="2019",6,AFDM_trt_2019$poolyr)
AFDM_trt_2019$poolyr<-ifelse(AFDM_trt_2019$pool=="7" & AFDM_trt_2019$Year=="2019",7,AFDM_trt_2019$poolyr)
AFDM_trt_2019$poolyr<-ifelse(AFDM_trt_2019$pool=="8" & AFDM_trt_2019$Year=="2019",8,AFDM_trt_2019$poolyr)
AFDM_trt_2019$poolyr<-ifelse(AFDM_trt_2019$pool=="9" & AFDM_trt_2019$Year=="2019",9,AFDM_trt_2019$poolyr)
AFDM_trt_2019$poolyr<-ifelse(AFDM_trt_2019$pool=="10" & AFDM_trt_2019$Year=="2019",10,AFDM_trt_2019$poolyr)

poolyr<-AFDM_trt_2019$poolyr
npoolyr<-10
npools<-10 #  this is the total number of unique pools  not all used in 2017 and 2018
AFDM.2019<-AFDM_trt_2019$AFDMrem  # the response variable
logAFDM<-log(AFDM.2019)
abundance<- as.numeric(scale(  AFDM_trt_2019$shrimpDay))
shrimp.for.plots <- as.vector (c( -1.25, -1, -0.75,
                                  -0.5, -0.25, 0, 0.25, 0.5, 0.75, 1,
                                  1.25, 1.5, 1.75, 2, 2.25, 2.5, 2.75))


##### Model III. ###############

sink("c_v_ec.jags")    ## give it a name
cat("
    model { 
### specify how the data relate to variables; this is the ecological process 
    for(i in 1:nobs.2019){ #  total number afdm remaining observations!
    
          logAFDM[i] ~ dnorm(mu[i], tau)  #each observation is a drawn from a distribution with mean AFDM remaining for that day, treatment and pool
          mu[i]<- int[i] + k[i]*day.2019[i]
          int[i]<-alpha0 
          
          k[i]~dnorm(mu2[i],precslope)
          
          mu2[i]<-b0*(1-treatment.2019[i]) + b1*treatment.2019[i] + b2*abundance[i]*(1- treatment.2019[i]) +random.effect[poolyr[i]]
     
     

    }
    
    #treatment effects
          predicted.k.control <- b0+b2*shrimp.for.plots
           shrimp.effect.control <- (1+b2)/(1+ b0)

##  This model lets each treatment have its own decay rate, with random variation in k among pool-yr combinations

### define the priors for the unknowns in the model
alpha0 ~ dunif(4,5)  # intercept is between 55 and 148 %AFDM

 b0 ~ dnorm(0, 0.001)
 b1 ~ dnorm(0, 0.001) #this says we have no idea how the treatment affects decay rate
 b2 ~ dnorm(0, 0.001)

 
## we also have three variance terms, tau (variance aroung the mean on each date), variance around k for each leaf pack,  and the random effect for pool
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
win.data <- list(logAFDM = logAFDM, npoolyr=npoolyr, nobs.2019=nobs.2019, day.2019=day.2019, treatment.2019=treatment.2019,
                 poolyr=poolyr, abundance=abundance, shrimp.for.plots=shrimp.for.plots)


## set starting points for the unknowns to be estimated in the regression
inits <- function(){list(alpha0=4.6, b0 = 0.0001, b1 = 0, b2=0, sigma = 10, sigma.pool = 1, sigma.k = 1)}

params<-c("alpha0", "b0","b1", "b2", "sigma.k", "sigma", "sigma.pool", "predicted.k.control", "shrimp.effect.control")

# MCMC settings
ni <- 30000   ## number of iterations - want enough to converge on a stable answer
nt <- 3      ## this days use value from every third iteration, b/c the values are temporally dependent
nb <- 5000    ## this is the number of beginning iterations to discard
nc <- 3      ## this is the number of independent 'chains' to run at once

# Call JAGS from R 
out.decay.2019.1<- jags(win.data, inits, params, "c_v_ec.jags", n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, working.directory = getwd())
print(out.decay.2019.1, dig = 4)