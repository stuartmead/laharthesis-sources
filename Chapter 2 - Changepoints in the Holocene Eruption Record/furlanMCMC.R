rm(list=ls())
#Set u, the cutoff VEI (must be higher than this)
u  = 3.0
####Get data and load functions#####
setwd("./Chapter 2 - Changepoints in the Holocene Eruption Record")
load("ProcessedSet.RData")
source("furlanFunctions.R")
sortedData = sortedData2
regions = levels(sortedData2$Region)
countries = levels(sortedData2$Country)

#Loop through each region
for (levReg in 2:length(regions))
  {
    #Filter to region
    sortedData2 = subset(sortedData, sortedData$Region == regions[levReg])
    
    #Create a data frame containing just eruption year and VEI.
    historicaldata = data.frame(sortedData2$Start.Year, sortedData2$VEI)
    colnames(historicaldata) = c("Year", "VEI")
    
    #Filter out NA
    historicaldata = subset(historicaldata, is.na(historicaldata$VEI)==FALSE)
    historicaldata = subset(historicaldata, is.na(historicaldata$Year)==FALSE)
    
    #Select eruptions containing only these VEI's
    historicaldata = subset(historicaldata, historicaldata$VEI > u)

    #Translate years +10000 (to get positive values)
    historicaldata$Year = historicaldata$Year + 10000
    if (nrow(historicaldata)>3)
      {
        #####Initialisation params and chain######
        meanx = mean(historicaldata$VEI, na.rm = TRUE)
        iterations = 200000
        mu = 1.5
        mu.c = mu
        phi = 1.5
        phi.c = phi
        xi = -0.25
        xi.c = xi
        beta = 1.0
        beta.c = beta
        alpha = -3.5
        alpha.c = alpha
        k = quantile(historicaldata$Year, 0.25)
        k.c = k
        
        #####Priors#####
        muprior = 10
        phiprior = 10
        xiprior = 10
        alphaprior=1
        betaprior=1
        
        # Date prior, range of dates/n events
        kspread = (max(historicaldata$Year) - min(historicaldata$Year)/nrow(historicaldata))
        
        #####Transition Densities#####
        wmu = 0.1
        wphi = 0.1
        wxi = 0.1
        walpha = 0.25
        wbeta = 0.25
        
        #####Start Iteration Loop#####
        accepted = c(0, 0, 0, 0, 0, 0)
        ak.c = c()
        
        #Transform alpha
        alphat = alpha + beta*meanx
        alphat.c = alphat
        
        #Transform phi
        rho = 1 - exp( (-1/nrow(historicaldata)) * (1+(xi/phi)*(u-mu))^(-1/xi) )
        zeta = log(rho/(1-rho))
        zeta.c = zeta
        for (i in 1:iterations)
          {
            ##mu##
            mup = mu + rnorm(1, 0, wmu)
            amu = -furlike(mup, zeta, xi, alphat, beta, k, historicaldata, u ) + dnorm(mup, 0, muprior, log = TRUE) +
              dnorm(mu-mup, mean = 0, sd = wmu, log=TRUE) +
                      furlike(mu, zeta, xi, alphat, beta, k, historicaldata, u) - 
              dnorm(mu, 0, muprior, log = TRUE) - dnorm(mup-mu, mean = 0, sd = wmu, log=TRUE)
            
            if (log(runif(1)) < amu){
              mu = mup
              accepted[1] = accepted[1]+1
            }
            
            ##Zeta##
            zetap = zeta + rnorm(1,0,wphi)
            azeta = -furlike(mu, zetap, xi, alphat, beta, k, historicaldata, u) + dnorm(zetap, 0, phiprior, log = TRUE) + dnorm(zeta-zetap, mean = 0, sd = wphi, log=TRUE) +
                      furlike(mu, zeta, xi, alphat, beta, k, historicaldata, u) - dnorm(zeta, 0, phiprior, log = TRUE) - dnorm(zetap-zeta, mean = 0, sd = wphi, log=TRUE)
            if (log(runif(1))<azeta){
              zeta = zetap
              accepted[2] = accepted[2]+1
            }
            
            ##xi##
            xip = xi + rnorm(1, 0, wxi)
                        axi = -furlike(mu, zeta, xip, alphat, beta, k, historicaldata, u) + dnorm(xip, 0, xiprior, log = TRUE) + dnorm(xi-xip, mean = 0, sd = wxi, log=TRUE) +
                    furlike(mu, zeta, xi, alphat, beta, k, historicaldata, u) - dnorm(xi, 0, xiprior, log = TRUE) - dnorm(xip-xi, mean = 0, sd = wxi, log=TRUE)
            if (log(runif(1))<axi){
              xi = xip
              accepted[3] = accepted[3]+1
            }
            
            ##alpha##
            alphatp = alphat + rnorm(1, 0, walpha)
            aalpha = -furlike(mu, zeta, xi, alphatp, beta, k, historicaldata, u) + dnorm(alphatp, -3.5, alphaprior, log = TRUE) + dnorm(alphat-alphatp, mean = 0, sd = walpha, log=TRUE) +
                        furlike(mu, zeta, xi, alphat, beta, k, historicaldata, u) - dnorm(alphat, -3.5, alphaprior, log = TRUE) - dnorm(alphatp-alphat, mean = 0, sd = walpha, log=TRUE) 
            if (log(runif(1))<aalpha){
              alphat = alphatp
              accepted[4] = accepted[4]+1
            }
            
            ##beta##
            betap = beta + rnorm(1, 0, wbeta)
            abeta = -furlike(mu, zeta, xi, alphat, betap, k, historicaldata, u) + dnorm(betap, 1, betaprior, log = TRUE) + dnorm(beta-betap, mean = 0, sd = wbeta, log=TRUE) +
                      furlike(mu, zeta, xi, alphat, beta, k, historicaldata, u) - dnorm(beta, 1, betaprior, log = TRUE) - dnorm(betap-beta, mean = 0, sd = wbeta, log=TRUE)
            if (log(runif(1))<abeta){
              beta = betap
              accepted[5] = accepted[5]+1
            }
            
            ##K##
            kp = sample(size=1,x=(max(min(historicaldata$Year),k-kspread):min(k+kspread,max(historicaldata$Year))))
            ak = -furlike(mu, zeta, xi, alphat, beta, kp, historicaldata, u) + log(kp) + log(12013-kp) +
                    furlike(mu, zeta, xi, alphat, beta, k, historicaldata, u) - log(k) - log(12013-k)
            if(log(runif(1))<ak){
              k = kp
              accepted[6] = accepted[6]+1
            }

            if (i%%10 == 0){
              mu.c = c(mu.c, mu)
              zeta.c = c(zeta.c, zeta)
              xi.c = c(xi.c, xi)
              alphat.c = c(alphat.c, alphat)
              beta.c = c(beta.c, beta)
              ak.c = c(ak.c, ak)
              k.c = c(k.c, k)
            }
          }
        results = data.frame(mu.c,phi.c,xi.c,alpha.c,beta.c,k.c)
        attr(results,"Acceptance")<-accepted/iterations
        save(results, file = paste(regions[levReg],'.Rdata'))
      }
    }