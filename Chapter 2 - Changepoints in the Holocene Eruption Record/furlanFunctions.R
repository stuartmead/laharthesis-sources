#Furlans likelihood
furlike <- function(mu, zeta, xi, alphat, beta, k, historicaldata, u){
  
  #Transform Alpha_t back to normal alpha (Eq. 9 in Mead and Magill (2014))
  alpha<-alphat-beta*mean(historicaldata$VEI)   		
  
  #Transform sigma from Zeta (Eq. 8 in Mead and Magill (2014))
  sigma <-(xi*(u-mu))/
    ((nrow(historicaldata)*log(1+exp(zeta)))^(-xi)-1)	
  
  #Return a very low likeliood for values chosen that violate Eq. 5 in Mead
  # and Magill (2014) and where the distribution is bounded by a maximum less
  # than the cutoff VEI.
  if ((1 + ((xi*(u-mu))/sigma)) < 0 | sigma <= 0 |
         (mu - sigma/xi) < u | xi > -0.05)
    {
      l <- 10^6
    }
  else 
    {
      #Similarly, check that the VEI values do not result in a out-of-bounds
      # value.
      if (min(1+(xi/sigma)*(historicaldata$VEI-mu))<=0)
        {
          l <- 10^6			
        }
    else  
    {
      if(xi < 0)  
        {
          uu <- mu - sigma/xi	
        }
      else 
        {
          uu <- Inf
        }
      #Outer integral of Eq. 7 in Mead and Magill (2014)
      int <- integrate(intU, lower = u, upper = uu, 
                       mu = mu, sig = sigma, xi = xi, alpha=alpha, beta=beta)
      
      #Likelihood
      l <- (k/sigma)*int$value + 
        (12013-k)*( 1+(xi/sigma)*(u-mu))^(-1/xi) +
        sum( log((1+exp(-alpha-beta*subset(historicaldata, historicaldata$Year < k)[,'VEI']) ) ) ) + 
        nrow(historicaldata)*log(sigma) +
        (1/xi+1)*sum(log( 1+(xi/sigma)*(historicaldata$VEI-mu)) )	
    }
  }
  return(l)
}

intU <- function(x,mu,sig,xi,alpha,beta)
  {
    ((1+xi*(x-mu)/sig)^(-1/xi-1))*(1/(1+exp(-alpha-beta*x)))
  }