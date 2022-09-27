library(rjags)
library(R2jags) 
library(xtable)
library(kableExtra)
library(Rmisc)
library(tidyverse)
library(dplyr)
library(MCMCvis)
library(lubridate)


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
AFDM_trt$pool<-ifelse(AFDM_trt$poolid=="1b",1,AFDM_trt$pool)
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

nobs<-341 #total number afdm remaining observations!
npools<-10 #  this is the total number of unique pools  not all used in 2017 and 2018
AFDM<-AFDM_trt$AFDMrem  # the response variable


# model #1
#shrimp effects three years

sink("leafdecompallyrs.jags")    ## give it a name
cat("
    model { 
### specify how the data relate to variables; this is the ecological process 
    for(i in 1:nobs){ #  total number afdm remaining observations!
    
          AFDM[i] ~ dnorm(mu[i], tau)  #each observation is a drawn from a distribution with mean AFDM remaining for that day, treatment and pool
          log(mu[i])<- log(100) + (b0*(1-treatment[i]) + b1*treatment[i] + b2*y2018[i]*(1-treatment[i]) + b3*y2019[i]*(1-treatment[i]) + 
          b4*PB[i]*(1-treatment[i]) + b5*PB[i]*treatment[i] + b6*y2018[i]*treatment[i] +b7*y2019[i]*treatment[i]+
          
          random.effect[pool[i]])*day[i] 
          
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
    c.decay.2018B<-b0+b2 +b4
    ec.decay.2018B<-b1+b6 +b5
    c.decay.2019B<-b0+b3 +b4
    ec.decay.2019B<-b1+b7 +b5






#}

### define the priors for the unknowns in the model

 b0 ~ dnorm(0, 0.001)
 b1 ~ dnorm(0, 0.001) #this says we have no idea how the treatment affects decay rate
 b2 ~ dnorm(0, 0.001) # or how 2018 differs from 2017 in controls
 b3 ~ dnorm(0, 0.001) # or how 2019 differs from 2017 in controls
 b4 ~ dnorm(0, 0.001) # or how PB differs from PA in controls
 b5 ~ dnorm(0, 0.001) # or how PB differes from PA in exclusion
 b6 ~ dnorm(0, 0.001) # or how 2018 differs from 2017 in exlcusion
 b7 ~ dnorm(0, 0.001) # or how 2019 differs from 2017 in exlcusion
 
## we also have two variance terms, tau (variance aroung the mean on each date) and the random effect for pool
## tau (representing variance aroung the mean on each date) includes observation + other sources of effor
  tau <- 1 / sigma^2  # 1/variance (precision) = 1/SD-squared
  sigma ~ dunif(0,20) # this is the prior on the standard deviation among obs AFDM remaining on a given date, pool, treatement

## random variation among pools -  
for(i in 1:npools){ 
      random.effect[i] ~ dnorm(0,tau.pool) #tau.pool is unmodeled variance among pools
      }
  tau.pool <- 1 / sigma.pool^2  #tau is actually expressed a precision=1/variance, I.e 1/ sigma.pool^2
  sigma.pool ~ dunif(0, 1) # this is the prior on the standard deviation in r among pools

    }  ### this ends the code for the model
",fill = T)
sink()  #always need these last two statements


## now get all this into JAGS using a win.data statement
win.data <- list(AFDM = AFDM, npools=npools, nobs=nobs, day=day, treatment=treatment, pool=pool,
                 y2018=y2018, y2019=y2019, PB=PB)

## set starting points for the unknowns to be estimated in the regression
inits <- function(){list(b0 = 0, b1 = 0, b2 = 0, b3 = 0, b4 = 0, b5 = 0, b6 = 0,
                         b7 = 0, sigma = 10, sigma.pool = 1)}

## specify the parameters for which we want posterior estimates - mean decay rate and treatment effect, and variances
## choose params - the second set gives output for plotting, in this case for 2017 only
params<-c("b0","b1","b2","b3","b4", "b5", "b6", "b7", "sigma","sigma.pool", "ec.decay.2017A",
          "ec.decay.2017B", "ec.decay.2018A", "ec.decay.2018B", "ec.decay.2019A", "ec.decay.2019B", "c.decay.2017A", "c.decay.2018A", "c.decay.2019A",
          "c.decay.2017B", "c.decay.2018B", "c.decay.2019B"
          ) 

# MCMC settings
ni <- 30000   ## number of iterations - want enough to converge on a stable answer
nt <- 3      ## this days use value from every third iteration, b/c the values are temporally dependent
nb <- 5000    ## this is the number of beginning iterations to discard
nc <- 3      ## this is the number of independent 'chains' to run at once

# Call JAGS from R (BRT 5 min)
#out.decay.1 uses first set of parameter
out.decay.1<- jags(win.data, inits, params, "leafdecompallyrs.jags", n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, working.directory = getwd())
print(out.decay.1, dig = 4)


#make table of output
Model1_table <- xtable(out.decay.1$BUGSoutput$summary[, ])
#option 1 export right to csv where you can make stylist changes and rename rows
write.csv(Model1_table, "C:/Users/Max/Desktop/UGA/Pringle Lab/R/msDecomp/Final_Models/Final_GOOD/model1table.6.27.csv")#if using on different computer will need to update this for location to work




##for plot


sink("leafdecompallyrs.jags")    ## give it a name
cat("
    model { 
### specify how the data relate to variables; this is the ecological process 
    for(i in 1:nobs){ #  total number afdm remaining observations!
    
          AFDM[i] ~ dnorm(mu[i], tau)  #each observation is a drawn from a distribution with mean AFDM remaining for that day, treatment and pool
          log(mu[i])<- log(100) + (b0*(1-treatment[i]) + b1*treatment[i] + b2*y2018[i]*(1-treatment[i]) + b3*y2019[i]*(1-treatment[i]) + b4*PB[i]*(1-treatment[i]) + b5*PB[i]*treatment[i] + b6*y2018[i]*treatment[i] +b7*y2019[i]*treatment[i]+
          random.effect[pool[i]])*day[i] 
          
    }
##  This model lets each year have its own decay rate in c and ec, but assumes that the differences between PA and PB are the constant across years   
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
    c.decay.2018B<-b0+b2 +b4
    ec.decay.2018B<-b1+b6 +b5
    c.decay.2019B<-b0+b3 +b4
    ec.decay.2019B<-b1+b7 +b5

## now lets get posteriors for plotting
plot.day<-c(1,3,7,14,21,28,35,42,49)
for (j in 1:9){
c.2017.A[j]<-log(100) + c.decay.2017A*plot.day[j]
ec.2017.A[j]<-log(100)+ ec.decay.2017A*plot.day[j]
c.2017.B[j]<-log(100) + c.decay.2017B*plot.day[j]
ec.2017.B[j]<-log(100) + ec.decay.2017B*plot.day[j]

c.2018.A[j]<-log(100) + c.decay.2018A*plot.day[j]
ec.2018.A[j]<-log(100)+ ec.decay.2018A*plot.day[j]
c.2018.B[j]<-log(100) + c.decay.2018B*plot.day[j]
ec.2018.B[j]<-log(100) + ec.decay.2018B*plot.day[j]

c.2019.A[j]<-log(100) + c.decay.2019A*plot.day[j]
ec.2019.A[j]<-log(100)+ ec.decay.2019A*plot.day[j]
c.2019.B[j]<-log(100) + c.decay.2019B*plot.day[j]
ec.2019.B[j]<-log(100) + ec.decay.2019B*plot.day[j]


}

### define the priors for the unknowns in the model

 b0 ~ dnorm(0, 0.001)
 b1 ~ dnorm(0, 0.001) #this says we have no idea how the treatment affects decay rate
 b2 ~ dnorm(0, 0.001) # or how 2018 differs from 2017
 b3 ~ dnorm(0, 0.001) # or how 2019 differs from 2017
 b4 ~ dnorm(0, 0.001) # or how PB differs from PA
 b5 ~ dnorm(0, 0.001) # 
 b6 ~ dnorm(0, 0.001) # 
 b7 ~ dnorm(0, 0.001) # 
 
## we also have two variance terms, tau (variance aroung the mean on each date) and the random effect for pool
## tau (representing variance aroung the mean on each date) includes observation + other sources of effor
  tau <- 1 / sigma^2  # 1/variance (precision) = 1/SD-squared
  sigma ~ dunif(0,20) # this is the prior on the standard deviation among obs AFDM remaining on a given date, pool, treatement

## random variation among pools -  
for(i in 1:npools){ 
      random.effect[i] ~ dnorm(0,tau.pool) #tau.pool is unmodeled variance among pools
      }
  tau.pool <- 1 / sigma.pool^2  #tau is actually expressed a precision=1/variance, I.e 1/ sigma.pool^2
  sigma.pool ~ dunif(0, 1) # this is the prior on the standard deviation in r among pools

    }  ### this ends the code for the model
",fill = T)
sink()  #always need these last two statements


## now get all this into JAGS using a win.data statement
win.data <- list(AFDM = AFDM, npools=npools, nobs=nobs, day=day, treatment=treatment, pool=pool,
                 y2018=y2018, y2019=y2019, PB=PB)

## set starting points for the unknowns to be estimated in the regression
inits <- function(){list(b0 = 0, b1 = 0, b2 = 0, b3 = 0, b4 = 0, b5 = 0, b6 = 0,
                         b7 = 0, sigma = 10, sigma.pool = 1)}

## specify the parameters for which we want posterior estimates - mean decay rate and treatment effect, and variances
#choose params - the second set gives output for plotting, in this case for 2017 only
#params<-c("b0","b1","b2","b3","b4", "b5", "b6", "b7", "sigma","sigma.pool", 
#   "ec.decay.2017A",  "ec.decay.2017B",  "ec.decay.2018A", "ec.decay.2018B", "ec.decay.2019A", "ec.decay.2019B", 
# "c.decay.2017A", "c.decay.2017B", "c.decay.2018A", "c.decay.2018B", "c.decay.2019A", "c.decay.2019B") 

#this set is ordered in a way that i prefer for caterpillar plot
params<-c("b0","b1","b2","b3","b4", "b5", "b6", "b7", "sigma","sigma.pool", 
          "ec.decay.2017A", "c.decay.2017A", "ec.decay.2017B","c.decay.2017B",  "ec.decay.2018A", "c.decay.2018A", 
          "ec.decay.2018B",  "c.decay.2018B", "ec.decay.2019A", "c.decay.2019A", "ec.decay.2019B", "c.decay.2019B") 


#for plotting line graph
## params<-c("c.2017.A", "ec.2017.A", "c.2017.B", "ec.2017.B", "c.2018.A", "ec.2018.A", "c.2018.B", "ec.2018.B", 
#      "c.2019.A", "ec.2019.A", "c.2019.B", "ec.2019.B" )
# MCMC settings
ni <- 30000   ## number of iterations - want enough to converge on a stable answer
nt <- 3      ## this days use value from every third iteration, b/c the values are temporally dependent
nb <- 5000    ## this is the number of beginning iterations to discard
nc <- 3      ## this is the number of independent 'chains' to run at once

# Call JAGS from R (BRT 3 min)
out.decay.1.plot<- jags(win.data, inits, params, "leafdecompallyrs.jags", n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, working.directory = getwd())
print(out.decay.1.plot, dig = 4)

## using second set of parameters monitored above (line 82), get output to plot slopes

## here's a clunky way to do it: first extract means and 2.5% and 97.5% values from the posteriors 
## (these are the same values in the outpu)

mean.c.2017.A<-mean.ec.2017.A<-mean.c.2017.B<-mean.ec.2017.B<-as.array(NA)
mean.c.2018.A<-mean.ec.2018.A<-mean.c.2018.B<-mean.ec.2018.B<-as.array(NA)
mean.c.2019.A<-mean.ec.2019.A<-mean.c.2019.B<-mean.ec.2019.B<-as.array(NA)


for (i in 1:9){
  mean.c.2017.A[i]<-mean(out.decay.1.plot$BUGSoutput$sims.list$c.2017.A[ , i])
  mean.ec.2017.A[i]<-mean(out.decay.1.plot$BUGSoutput$sims.list$ec.2017.A[ , i])
  mean.c.2017.B[i]<-mean(out.decay.1.plot$BUGSoutput$sims.list$c.2017.B[ , i])
  mean.ec.2017.B[i]<-mean(out.decay.1.plot$BUGSoutput$sims.list$ec.2017.B[ , i])
  
  mean.c.2018.A[i]<-mean(out.decay.1.plot$BUGSoutput$sims.list$c.2018.A[ , i])
  mean.ec.2018.A[i]<-mean(out.decay.1.plot$BUGSoutput$sims.list$ec.2018.A[ , i])
  mean.c.2018.B[i]<-mean(out.decay.1.plot$BUGSoutput$sims.list$c.2018.B[ , i])
  mean.ec.2018.B[i]<-mean(out.decay.1.plot$BUGSoutput$sims.list$ec.2018.B[ , i])
  
  mean.c.2019.A[i]<-mean(out.decay.1.plot$BUGSoutput$sims.list$c.2019.A[ , i])
  mean.ec.2019.A[i]<-mean(out.decay.1.plot$BUGSoutput$sims.list$ec.2019.A[ , i])
  mean.c.2019.B[i]<-mean(out.decay.1.plot$BUGSoutput$sims.list$c.2019.B[ , i])
  mean.ec.2019.B[i]<-mean(out.decay.1.plot$BUGSoutput$sims.list$ec.2019.B[ , i])
}

#values for 95% credible intervals
lower.c.2017.A<-lower.ec.2017.A<-lower.c.2017.B<-lower.ec.2017.B<-as.array(NA)
lower.c.2018.A<-lower.ec.2018.A<-lower.c.2018.B<-lower.ec.2018.B<-as.array(NA)
lower.c.2019.A<-lower.ec.2019.A<-lower.c.2019.B<-lower.ec.2019.B<-as.array(NA)


for (i in 1:9){
  lower.c.2017.A[i]<-quantile(out.decay.1.plot$BUGSoutput$sims.list$c.2017.A[ , i], 0.025)
  lower.ec.2017.A[i]<-quantile(out.decay.1.plot$BUGSoutput$sims.list$ec.2017.A[ , i], 0.025)
  lower.c.2017.B[i]<-quantile(out.decay.1.plot$BUGSoutput$sims.list$c.2017.B[ , i], 0.025)
  lower.ec.2017.B[i]<-quantile(out.decay.1.plot$BUGSoutput$sims.list$ec.2017.B[ , i], 0.025)
  
  lower.c.2018.A[i]<-quantile(out.decay.1.plot$BUGSoutput$sims.list$c.2018.A[ , i], 0.025)
  lower.ec.2018.A[i]<-quantile(out.decay.1.plot$BUGSoutput$sims.list$ec.2018.A[ , i], 0.025)
  lower.c.2018.B[i]<-quantile(out.decay.1.plot$BUGSoutput$sims.list$c.2018.B[ , i], 0.025)
  lower.ec.2018.B[i]<-quantile(out.decay.1.plot$BUGSoutput$sims.list$ec.2018.B[ , i], 0.025)
  
  lower.c.2019.A[i]<-quantile(out.decay.1.plot$BUGSoutput$sims.list$c.2019.A[ , i], 0.025)
  lower.ec.2019.A[i]<-quantile(out.decay.1.plot$BUGSoutput$sims.list$ec.2019.A[ , i], 0.025)
  lower.c.2019.B[i]<-quantile(out.decay.1.plot$BUGSoutput$sims.list$c.2019.B[ , i], 0.025)
  lower.ec.2019.B[i]<-quantile(out.decay.1.plot$BUGSoutput$sims.list$ec.2019.B[ , i], 0.025)
}

upper.c.2017.A<-upper.ec.2017.A<-upper.c.2017.B<-upper.ec.2017.B<-as.array(NA)
upper.c.2018.A<-upper.ec.2018.A<-upper.c.2018.B<-upper.ec.2018.B<-as.array(NA)
upper.c.2019.A<-upper.ec.2019.A<-upper.c.2019.B<-upper.ec.2019.B<-as.array(NA)

for (i in 1:9){
  upper.c.2017.A[i]<-quantile(out.decay.1.plot$BUGSoutput$sims.list$c.2017.A[ , i], 0.975)
  upper.ec.2017.A[i]<-quantile(out.decay.1.plot$BUGSoutput$sims.list$ec.2017.A[ , i], 0.975)
  upper.c.2017.B[i]<-quantile(out.decay.1.plot$BUGSoutput$sims.list$c.2017.B[ , i], 0.975)
  upper.ec.2017.B[i]<-quantile(out.decay.1.plot$BUGSoutput$sims.list$ec.2017.B[ , i], 0.975)
  
  upper.c.2018.A[i]<-quantile(out.decay.1.plot$BUGSoutput$sims.list$c.2018.A[ , i], 0.975)
  upper.ec.2018.A[i]<-quantile(out.decay.1.plot$BUGSoutput$sims.list$ec.2018.A[ , i], 0.975)
  upper.c.2018.B[i]<-quantile(out.decay.1.plot$BUGSoutput$sims.list$c.2018.B[ , i], 0.975)
  upper.ec.2018.B[i]<-quantile(out.decay.1.plot$BUGSoutput$sims.list$ec.2018.B[ , i], 0.975)
  
  upper.c.2019.A[i]<-quantile(out.decay.1.plot$BUGSoutput$sims.list$c.2019.A[ , i], 0.975)
  upper.ec.2019.A[i]<-quantile(out.decay.1.plot$BUGSoutput$sims.list$ec.2019.A[ , i], 0.975)
  upper.c.2019.B[i]<-quantile(out.decay.1.plot$BUGSoutput$sims.list$c.2019.B[ , i], 0.975)
  upper.ec.2019.B[i]<-quantile(out.decay.1.plot$BUGSoutput$sims.list$ec.2019.B[ , i], 0.975)
}

#for sd 

sd.c.2017.A<-sd.ec.2017.A<-sd.c.2017.B<-sd.ec.2017.B<-as.array(NA)
sd.c.2018.A<-sd.ec.2018.A<-sd.c.2018.B<-sd.ec.2018.B<-as.array(NA)
sd.c.2019.A<-sd.ec.2019.A<-sd.c.2019.B<-sd.ec.2019.B<-as.array(NA)


wherewhat<-c(rep ("c.2017.A", 9), rep ("ec.2017.A", 9), rep ("c.2017.B", 9), rep ("ec.2017.B", 9,),
             rep ("c.2018.A", 9), rep ("ec.2018.A", 9), rep ("c.2018.B", 9), rep ("ec.2018.B", 9,), 
             rep ("c.2019.A", 9), rep ("ec.2019.A", 9), rep ("c.2019.B", 9), rep ("ec.2019.B", 9,))
meanvalues<-c(mean.c.2017.A, mean.ec.2017.A, mean.c.2017.B, mean.ec.2017.B, 
              mean.c.2018.A, mean.ec.2018.A, mean.c.2018.B, mean.ec.2018.B,
              mean.c.2019.A, mean.ec.2019.A, mean.c.2019.B, mean.ec.2019.B)
lowerci<-c(lower.c.2017.A, lower.ec.2017.A, lower.c.2017.B, lower.ec.2017.B,
           lower.c.2018.A, lower.ec.2018.A, lower.c.2018.B, lower.ec.2018.B,
           lower.c.2019.A, lower.ec.2019.A, lower.c.2019.B, lower.ec.2019.B)
upperci<-c(upper.c.2017.A, upper.ec.2017.A, upper.c.2017.B, upper.ec.2017.B,
           upper.c.2018.A, upper.ec.2018.A, upper.c.2018.B, upper.ec.2018.B,
           upper.c.2019.A, upper.ec.2019.A, upper.c.2019.B, upper.ec.2019.B)
which<-c(rep("c",9), rep("ec",9),rep("c",9), rep("ec",9),
         rep("c",9), rep("ec",9),rep("c",9), rep("ec",9),
         rep("c",9), rep("ec",9),rep("c",9), rep("ec",9) )
stream<-c(rep("A", 18), rep("B",18),
          rep("A", 18), rep("B",18),
          rep("A", 18), rep("B",18))
plot.day<-c(1,3,7,14,21,28,35,42,49)
plot.days<-rep(plot.day,12)
year <- c(rep("2017", 36), rep("2018", 36), rep("2019", 36))

alltogether<-data.frame(wherewhat, which, stream, plot.days, meanvalues, lowerci, upperci, year)

alltogether$meanAFDMrem<-exp(alltogether$meanvalues)
alltogether$lowerAFDMrem<-exp(alltogether$lowerci)
alltogether$upperAFDMrem<-exp(alltogether$upperci)

plot.2017<-ggplot(alltogether, aes(x=plot.days, y= meanAFDMrem, col=stream, linetype=which))+
  geom_line()

plot.2017

controls<-subset(alltogether, alltogether$which=="c")
plot.2017c<-ggplot(controls, aes(x=plot.days, y= meanAFDMrem, col=stream))+
  geom_line(size=2 ) +
  geom_ribbon(aes(ymin=lowerAFDMrem, ymax=upperAFDMrem, fill=stream, alpha=0.1))

plot.2017c



#credible intervals overlapping ??
ggplot(alltogether, aes(x=plot.days, y= meanAFDMrem, col=stream, linetype=which))+
  geom_ribbon(aes(ymin=lowerAFDMrem, ymax=upperAFDMrem, fill=stream, alpha=.1 )) +
  geom_line(size=2 )+
  facet_grid(year~stream) + 
  theme_bw()

ggplot(alltogether, aes(x=plot.days, y= meanvalues, col=stream, linetype=which))+
  geom_ribbon(aes(ymin=lowerci, ymax=upperci, fill=stream, alpha=.1 )) +
  geom_line(size=2 )+
  facet_grid(year~stream) + 
  theme_bw()




##plotting caterpillar plots 
library(MCMCvis)
cat.plot <- MCMCplot(out.decay.1.plot, 
                     params = c("c.decay.2017A", "ec.decay.2017A", "c.decay.2017B", "ec.decay.2017B", 
                                "c.decay.2018A", "ec.decay.2018A", "c.decay.2018B", "ec.decay.2018B",
                                "c.decay.2019A", "ec.decay.2019A", "c.decay.2019B", "ec.decay.2019B"),
                     # excl = c("deviance","sigma", "sigma.pool", "b1", "b0", "b2", "b3", "b4", "b5", "b6", "b7"),
                     ci = c(50, 95))

cat.plot

MCMCplot(out.decay.1.plot, 
         params = "all",
         excl = c("deviance","sigma", "sigma.pool", "b1", "b0", "b2", "b3", "b4", "b5", "b6", "b7"),
         ci = c(50, 95))


#bar plots for model 1 

mean.c.2017.A<-mean.ec.2017.A<-mean.c.2017.B<-mean.ec.2017.B<-as.array(NA)
mean.c.2018.A<-mean.ec.2018.A<-mean.c.2018.B<-mean.ec.2018.B<-as.array(NA)
mean.c.2019.A<-mean.ec.2019.A<-mean.c.2019.B<-mean.ec.2019.B<-as.array(NA)


for (i in 1:9){
  mean.c.2017.A<-mean(out.decay.1.plot$BUGSoutput$sims.list$c.decay.2017A[ , ])
  mean.ec.2017.A<-mean(out.decay.1.plot$BUGSoutput$sims.list$ec.decay.2017A[ , ])
  mean.c.2017.B<-mean(out.decay.1.plot$BUGSoutput$sims.list$c.decay.2017B[ , ])
  mean.ec.2017.B<-mean(out.decay.1.plot$BUGSoutput$sims.list$ec.decay.2017B[ , ])
  
  mean.c.2018.A<-mean(out.decay.1.plot$BUGSoutput$sims.list$c.decay.2018A[ , ])
  mean.ec.2018.A<-mean(out.decay.1.plot$BUGSoutput$sims.list$ec.decay.2018A[,])
  mean.c.2018.B<-mean(out.decay.1.plot$BUGSoutput$sims.list$c.decay.2018B[,])
  mean.ec.2018.B<-mean(out.decay.1.plot$BUGSoutput$sims.list$ec.decay.2018A[ , ])
  
  mean.c.2019.A<-mean(out.decay.1.plot$BUGSoutput$sims.list$c.decay.2019A[ , ])
  mean.ec.2019.A<-mean(out.decay.1.plot$BUGSoutput$sims.list$ec.decay.2019A[ , ])
  mean.c.2019.B<-mean(out.decay.1.plot$BUGSoutput$sims.list$c.decay.2019B[ , ])
  mean.ec.2019.B<-mean(out.decay.1.plot$BUGSoutput$sims.list$ec.decay.2019B[ , ])
}

sd.c.2017.A<-sd.ec.2017.A<-sd.c.2017.B<-sd.ec.2017.B<-as.array(NA)
sd.c.2018.A<-sd.ec.2018.A<-sd.c.2018.B<-sd.ec.2018.B<-as.array(NA)
sd.c.2019.A<-sd.ec.2019.A<-sd.c.2019.B<-sd.ec.2019.B<-as.array(NA)

for (i in 1:9){
  sd.c.2017.A<-sd(out.decay.1.plot$BUGSoutput$sims.list$c.decay.2017A[ , ])
  sd.ec.2017.A<-sd(out.decay.1.plot$BUGSoutput$sims.list$ec.decay.2017A[ , ])
  sd.c.2017.B<-sd(out.decay.1.plot$BUGSoutput$sims.list$c.decay.2017B[ , ])
  sd.ec.2017.B<-sd(out.decay.1.plot$BUGSoutput$sims.list$ec.decay.2017B[ , ])
  
  sd.c.2018.A<-sd(out.decay.1.plot$BUGSoutput$sims.list$c.decay.2018A[ , ])
  sd.ec.2018.A<-sd(out.decay.1.plot$BUGSoutput$sims.list$ec.decay.2018A[,])
  sd.c.2018.B<-sd(out.decay.1.plot$BUGSoutput$sims.list$c.decay.2018B[,])
  sd.ec.2018.B<-sd(out.decay.1.plot$BUGSoutput$sims.list$ec.decay.2018A[ , ])
  
  sd.c.2019.A<-sd(out.decay.1.plot$BUGSoutput$sims.list$c.decay.2019A[ , ])
  sd.ec.2019.A<-sd(out.decay.1.plot$BUGSoutput$sims.list$ec.decay.2019A[ , ])
  sd.c.2019.B<-sd(out.decay.1.plot$BUGSoutput$sims.list$c.decay.2019B[ , ])
  sd.ec.2019.B<-sd(out.decay.1.plot$BUGSoutput$sims.list$ec.decay.2019B[ , ])
}



#values for 95% credible intervals
lower.c.2017.A<-lower.ec.2017.A<-lower.c.2017.B<-lower.ec.2017.B<-as.array(NA)
lower.c.2018.A<-lower.ec.2018.A<-lower.c.2018.B<-lower.ec.2018.B<-as.array(NA)
lower.c.2019.A<-lower.ec.2019.A<-lower.c.2019.B<-lower.ec.2019.B<-as.array(NA)


means_bar <- c(mean.c.2017.A, mean.ec.2017.A, mean.c.2017.B, mean.ec.2017.B, 
               mean.c.2018.A, mean.ec.2018.A, mean.c.2018.B, mean.ec.2018.B,
               mean.c.2019.A, mean.ec.2019.A, mean.c.2019.B, mean.ec.2019.B)
sd_bar <- c(sd.c.2017.A, sd.ec.2017.A, sd.c.2017.B, sd.ec.2017.B, 
            sd.c.2018.A, sd.ec.2018.A, sd.c.2018.B, sd.ec.2018.B,
            sd.c.2019.A, sd.ec.2019.A, sd.c.2019.B, sd.ec.2019.B)
wherewhat_bar<-c(rep ("c.2017.A", 1), rep ("ec.2017.A", 1), rep ("c.2017.B", 1), rep ("ec.2017.B", 1,),
                 rep ("c.2018.A", 1), rep ("ec.2018.A", 1), rep ("c.2018.B", 1), rep ("ec.2018.B", 1,), 
                 rep ("c.2019.A", 1), rep ("ec.2019.A", 1), rep ("c.2019.B", 1), rep ("ec.2019.B", 1,))
which_bar<-c(rep("Control",1), rep("Exclusion",1),rep("Control",1), rep("Exclusion",1),
             rep("Control",1), rep("Exclusion",1),rep("Control",1), rep("Exclusion",1),
             rep("Control",1), rep("Exclusion",1),rep("Control",1), rep("Exclusion",1) )
stream_bar<-c(rep("PA", 2), rep("PB",2),
              rep("PA", 2), rep("PB",2),
              rep("PA", 2), rep("PB",2))
year_bar <- c(rep("2017 Exp.", 4), rep("2018 Exp.", 4), rep("2019 Exp.", 4))


alltogether_bar<-data.frame(wherewhat_bar, which_bar, stream_bar, means_bar, sd_bar, year_bar)

#bargraph
global_size = 20
#previous version
ggplot(data= alltogether_bar, aes(x= which_bar, y=-means_bar, group = stream_bar)) +
  geom_bar(width=1,  position="dodge", stat="identity", aes(fill = which_bar), color = "black") +
  geom_errorbar(data= alltogether_bar, aes(ymin = -means_bar-sd_bar, ymax=-means_bar+sd_bar), width=0.2, size=1,)+
  facet_grid(year_bar~stream_bar) +
  theme_bw(base_size = global_size) +
  theme(legend.position="none")+
  scale_fill_grey(start = 0.3, end = .7) +
  xlab(" ") +
  ylab(bquote(' Mean Decomposition Rate '~("k day" ^-1)))


#3.16 version
ggplot(data= alltogether_bar, aes(x= which_bar, y=-means_bar, group = stream_bar)) +
  geom_bar(width=1,  position="dodge", stat="identity", aes(fill = which_bar), color = "black") +
  geom_errorbar(data= alltogether_bar, aes(ymin = -means_bar-sd_bar, ymax=-means_bar+sd_bar), width=0.2, size=1,)+
  facet_grid(year_bar~stream_bar, space = 'free_x', scales = 'free_x', switch = "x") +
  theme_bw(base_size = global_size) +
  theme(legend.position="none", strip.placement = 'outside',
        strip.background.x = element_blank())+
  scale_fill_grey(start = 0.3, end = .7) +
  xlab("") +
  ylab(bquote(' Mean Decomposition Rate '~("k day" ^-1))) 

#this one is good
ggplot(data= alltogether_bar, aes(x= which_bar, y=-means_bar, group = stream_bar)) +
  geom_bar(width=1,  position="dodge", stat="identity", aes(fill = which_bar), color = "black") +
  geom_errorbar(data= alltogether_bar, aes(ymin = -means_bar-sd_bar, ymax=-means_bar+sd_bar), width=0.2, size=1,)+
  facet_grid(year_bar~stream_bar, space = 'free_x', scales = 'free_x', switch = "x") +
  theme_bw(base_size = global_size) +
  theme(legend.position="none", strip.placement = 'outside',
        strip.background.x = element_blank(), strip.text.y =  element_blank(),
        panel.spacing.y = unit(2, "lines"))+
  scale_fill_grey(start = 0.3, end = .7) +
  xlab("Stream / Treatment") +
  ylab(bquote(' Mean Decomposition Rate '~("k day" ^-1))) 

#this one also good
ggplot(data= alltogether_bar, aes(x= which_bar, y=-means_bar, group = stream_bar)) +
  geom_bar(width=1,  position="dodge", stat="identity", aes(fill = which_bar), color = "black") +
  geom_errorbar(data= alltogether_bar, aes(ymin = -means_bar-sd_bar, ymax=-means_bar+sd_bar), width=0.2, size=1,)+
  facet_grid(year_bar~stream_bar, space = 'free_x', scales = 'free_x', switch = "x") +
  theme_bw(base_size = global_size) +
  theme(legend.position="none", strip.placement = 'outside',
        strip.background.x = element_blank())+
  scale_fill_grey(start = 0.3, end = .7) +
  xlab("Stream/Treatment") +
  ylab(bquote(' Mean Decomposition Rate '~("k day" ^-1))) 

 

ggsave("meandecomp.tiff", figure, width=11,height=7, dpi=400 )
