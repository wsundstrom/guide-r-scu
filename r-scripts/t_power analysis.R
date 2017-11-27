
#==============================================================================
#   Data Analysis Tutorial: Power analysis
#==============================================================================

# original by Michael Kevane 8/15/2017

# Description: basics of power analysis

#==============================================================================
#   1. Settings, packages, and options
#==============================================================================

#  Clear the working space
rm(list = ls())

# Power analysis in text of Guide to R
  # Let mu be the difference in means
  mu=0
  muactual=.13
  n1=18
  n2=19
  n=n1+n2
  stdev=.5
  se=sqrt((stdev^2)/n1 + (stdev^2)/n2)
  # p-value for test of difference in means = 0
  2*pnorm(-abs((muactual-mu)/(se)))
  # t-statistic of difference in means
  (muactual-mu)/(se)
  
  # Basic power analysis with a specified alternative hypothesis
  mualt=.2
  alpha=.05
  sided=2
  # critical t value for test, with normal or t distribution
  tcrit=abs(qnorm(alpha/sided))
  tcrit2=abs(qt(alpha/sided,df=n-2))
  left <- qt(0.025,df=pmin(n1,n2)-1)*se
  # implied critical value of diff in means for test, assuming Ho = mu
  Xcrit= tcrit*(se)+mu
  # Check that gives right critical p-value
  pnorm(-Xcrit/(se))
  # Power of t-test given assumptions about mualt, n, stdev, mu
  1-(pnorm((Xcrit-mualt)/(se))- pnorm((-Xcrit-mualt)/(se)))
  1-(pt((Xcrit-mualt)/(se), df=n-2)- pt((-Xcrit-mualt)/(se), df=n-2))
  # Another way to calculate power of test
  tcrit2x <- qt(0.975,df=pmin(n1,n2)-1)
  ncp <- (mualt)/se
  1-(pt(tcrit2x,df=pmin(n1,n2)-1,ncp=ncp)-pt(-tcrit2x,df=pmin(n1,n2)-1,ncp=ncp))

# Now change sample size to be 100, 50 in each
  # Let mu be the difference in means
  mu=0
  muactual=.13
  n1=50
  n2=50
  n=n1+n2
  stdev=.5
  se=sqrt((stdev^2)/n1 + (stdev^2)/n2)
  # p-value for test of difference in means = 0
  2*pnorm(-abs((muactual-mu)/(se)))
  # t-statistic of difference in means
  (muactual-mu)/(se)
  # Basic power analysis
  mualt=.2
  alpha=.05
  sided=2
  # critical t value for test, with normal or t distribution
  tcrit=abs(qnorm(alpha/sided))
  tcrit2=abs(qt(alpha/sided,df=n-2))
  left <- qt(0.025,df=pmin(n1,n2)-1)*se
  # implied critical value of diff in means for test, assuming Ho = mu
  Xcrit= tcrit*(se)+mu
  # Check that gives right critical p-value
  pnorm(-Xcrit/(se))
  # Power of t-test given assumptions about mualt, n, stdev, mu
  1-(pnorm((Xcrit-mualt)/(se))- pnorm((-Xcrit-mualt)/(se)))
  1-(pt((Xcrit-mualt)/(se), df=n-2)- pt((-Xcrit-mualt)/(se), df=n-2))
  # Another way to calculate power of test
  tcrit2x <- qt(0.975,df=pmin(n1,n2)-1)
  ncp <- (mualt)/se
  1-(pt(tcrit2x,df=pmin(n1,n2)-1,ncp=ncp)-pt(-tcrit2x,df=pmin(n1,n2)-1,ncp=ncp))

# Another example with different numbers
  # Let mu be the difference in means
  mu=0
  muactual=4.4
  n1=180
  n2=180
  n=n1+n2
  stdev=10.96
  se=sqrt((stdev^2)/n1 + (stdev^2)/n2)
  # p-value for test of difference in means = 0
  2*pnorm(-abs((muactual-mu)/(se)))
  # t-statistic of difference in means
  (muactual-mu)/(se)
  
  # Basic power analysis
  mualt=3.28
  alpha=.05
  sided=2
  # critical t value for test, with normal or t distribution
  tcrit=abs(qnorm(alpha/sided))
  tcrit2=abs(qt(alpha/sided,df=n-2))
  left <- qt(0.025,df=pmin(n1,n2)-1)*se
  # implied critical value of diff in means for test, assuming Ho = mu
  Xcrit= tcrit*(se)+mu
  # Check that gives right critical p-value
  pnorm(-Xcrit/(se))
  # Power of t-test given assumptions about mualt, n, stdev, mu
  1-(pnorm((Xcrit-mualt)/(se))- pnorm((-Xcrit-mualt)/(se)))
  1-(pt((Xcrit-mualt)/(se), df=n-2)- pt((-Xcrit-mualt)/(se), df=n-2))
  # Another way to calculate power of test
  tcrit2x <- qt(0.975,df=pmin(n1,n2)-1)
  ncp <- (mualt)/se
  1-(pt(tcrit2x,df=pmin(n1,n2)-1,ncp=ncp)-pt(-tcrit2x,df=pmin(n1,n2)-1,ncp=ncp))

# text discussion of example of one-sided ttest sample size
# for power = .80
#(.25*2)/(.5n) = [.25/(1.65 - qnorm(.2))]^2,
#(.5n) = (.25*2)/[.25/(1.65 - qnorm(.2))]^2,
n = (2*(.25*2))/(.25/(1.65 - qnorm(.2)))^2

1- ((pnorm((6.08-3.28)/(3.1)) - pnorm((-6.08-3.28)/(3.1))))
2*pnorm(-4.4/3.1)
# verification
power.t.test(n = 50, delta = .25, sd = .5, sig.level = 0.05,
             power = NULL,
             type = c("two.sample"),
             alternative = c("one.sided"),
             strict = FALSE, tol = .Machine$double.eps^0.25)

# Calculate power by simulation
  stdev=.7
  possible.ns <- seq(from=30, to=210, by=10)    # The sample sizes we'll be considering, possible n's
  sigs <- rep(NA, length(possible.ns))          # Empty object to collect simulation estimates
  pvals <- rep(NA, length(possible.ns))          # Empty object to collect simulation estimates

  sims <- 500                                  # Number of simulations to conduct for each N
  #### Outer loop to vary the number of subjects ####
  for (j in 1:length(possible.ns)){
    N <- possible.ns[j]               # Pick the jth value for N
    far.away <- rep(NA, sims)         # Empty object to count times far away from critical value
    pvalsig <- rep(NA, sims)         # Empty object to count times far away from critical value 
    #### Inner loop to conduct experiments "sims" times over for each N ####
    for (i in 1:sims){
      Y1 <-  rnorm(n=N/2, mean=7, sd=stdev) # group1
      Y2 <-  rnorm(n=N/2, mean=7.25, sd=stdev) # group1
      diff=mean(Y2)-mean(Y1)
      se=sqrt((stdev^2)/(N/2) + (stdev^2)/(N/2))
      # Determine Xbar critical with normal dist, qnorm is prob that critical value has 95% of draws below
      far.away[i] <- ((diff >= (qnorm(0.95,mean=0,sd=se))))  
      ttt=t.test(Y1,Y2,  alternative="less")
      pvalsig[i] <- (ttt[3] <=.05)
    }
    sigs[j] <- mean(far.away)       # store average success rate (power) for each N
    pvals[j] <- mean(pvalsig)
  }
  plot(possible.ns, sigs, ylim=c(0,1))
  plot(possible.ns, pvals, ylim=c(0,1))

# Calculate power by simulation (another example)
  stdev=.3
  possible.ns <- seq(from=30, to=80, by=10)    # The sample sizes we'll be considering, possible n's
  sigs <- rep(NA, length(possible.ns))          # Empty object to collect simulation estimates
  pvals <- rep(NA, length(possible.ns))          # Empty object to collect simulation estimates

  sims <- 5000                                  # Number of simulations to conduct for each N
  #### Outer loop to vary the number of subjects ####
  for (j in 1:length(possible.ns)){
    N <- possible.ns[j]               # Pick the jth value for N
    far.away <- rep(NA, sims)         # Empty object to count times far away from critical value
    pvalsig <- rep(NA, sims)         # Empty object to count times far away from critical value 
    #### Inner loop to conduct experiments "sims" times over for each N ####
    for (i in 1:sims){
      Y1 <-  rnorm(n=N/2, mean=7, sd=stdev) # group1
      Y2 <-  rnorm(n=N/2, mean=7.20, sd=stdev) # group1
      diff=mean(Y2)-mean(Y1)
      se=sqrt((stdev^2)/(N/2) + (stdev^2)/(N/2))
      far.away[i] <- ((diff >= (qnorm(0.025,mean=0,sd=se,lower.tail=FALSE)))|(diff <= (qnorm(0.025,mean=0,sd=se,lower.tail=TRUE))))  # Determine Xbar critical with normal dist
      ttt=t.test(Y1,Y2)
      pvalsig[i] <- (ttt[3] <=.05)
    }
    sigs[j] <- mean(far.away)       # store average success rate (power) for each N
    pvals[j] <- mean(pvalsig)
  }
  plot(possible.ns, sigs, ylim=c(0,1))
  plot(possible.ns, pvals, ylim=c(0,1))


# A simulation of multiple regressions  

  # Set effect size and variation for three outcomes 1,2,3
    efsize1=32.5
    sdact1=130
    efsize2=2
    sdact2=8
    efsize3=5
    sdact3=20
    
 # Set the sample sizes and objects to collect results
    possible.ns <- seq(from=100, to=700, by=50)      # The sample sizes we'll be considering
    powers1 <- rep(NA, length(possible.ns))           # Empty object to collect simulation estimates
    powers2 <- rep(NA, length(possible.ns))           # Empty object to collect simulation estimates
    powers3 <- rep(NA, length(possible.ns))           # Empty object to collect simulation estimates
    powersz2 <- rep(NA, length(possible.ns))          # Empty object to collect simulation estimates
    powersz3 <- rep(NA, length(possible.ns))          # Empty object to collect simulation estimates
    alpha <- 0.05                                    # Standard significance level
    sims <- 150                                       # Number of simulations to conduct for each N
   
    #### Outer loop to vary the number of subjects ####
    for (j in 1:length(possible.ns)){
      N <- possible.ns[j]                             # Pick the jth value for N
      significant.experiments1 <- rep(NA, sims)        # Empty object to count significant experiments
      significant.experiments2 <- rep(NA, sims)        # Empty object to count significant experiments
      significant.experiments3 <- rep(NA, sims)        # Empty object to count significant experiments
      significant.experimentsz2 <- rep(NA, sims)       # Empty object to count significant experiments
      significant.experimentsz3 <- rep(NA, sims)       # Empty object to count significant experiments      
      
      #### Inner loop to conduct experiments "sims" times over for each N ####
      for (i in 1:sims){
        Y0 <-  rnorm(n=N, mean=250, sd=sdact1)           # control potential outcome #1
        Y1 <- Y0 + efsize1                                 # treatment potential outcome
        T0 <-  rnorm(n=N, mean=4, sd=sdact2)           # control potential outcome #2
        T1 <- T0 + efsize2                                 # treatment potential outcome
        Q0 <-  rnorm(n=N, mean=10, sd=sdact3)           # control potential outcome #3
        Q1 <- Q0 + efsize3                                 # treatment potential outcome
        Z.sim <- rbinom(n=N, size=1, prob=.5)          # Do a random assignment
        Y.sim <- Y1*Z.sim + Y0*(1-Z.sim)               # Reveal outcomes according to assignment
        T.sim <- T1*Z.sim + T0*(1-Z.sim)               # Reveal outcomes according to assignment
        Q.sim <- Q1*Z.sim + Q0*(1-Z.sim)               # Reveal outcomes according to assignment
        fit1.sim <- lm(Y.sim ~ Z.sim)                   # Do analysis (Simple regression)
        fit2.sim <- lm(T.sim ~ Z.sim)                   # Do analysis (Simple regression)
        fit3.sim <- lm(Q.sim ~ Z.sim)                   # Do analysis (Simple regression)
        p1.value <- summary(fit1.sim)$coefficients[2,4]  # Extract p-values
        p2.value <- summary(fit2.sim)$coefficients[2,4]  # Extract p-values
        p3.value <- summary(fit3.sim)$coefficients[2,4]  # Extract p-values
        significant.experiments1[i] <- (p1.value <= alpha) # Determine significance according to p <= 0.05
        significant.experiments2[i] <- (p2.value <= alpha) # Determine significance according to p <= 0.05
        significant.experiments3[i] <- (p3.value <= alpha) # Determine significance according to p <= 0.05
        significant.experimentsz2[i] <- ((p1.value <= alpha) & (p2.value <= alpha)) # Determine significance according to p <= 0.05
        significant.experimentsz3[i] <- ((p1.value <= alpha) & (p2.value <= alpha) & (p3.value <= alpha)) # Determine significance according to p <= 0.05
      }
      powers1[j] <- mean(significant.experiments1)       # store average success rate (power) for each N
      powers2[j] <- mean(significant.experiments2)       # store average success rate (power) for each N
      powers3[j] <- mean(significant.experiments3)       # store average success rate (power) for each N
      powersz2[j] <- mean(significant.experimentsz2)       # store average success rate (power) for each N
      powersz3[j] <- mean(significant.experimentsz3)       # store average success rate (power) for each N
    }
    
    # Plot results proportion correctly reject- power of the test)
    # Note for multiple hypothesis have not done any corrections for multiple hypothesis testing
    plot(possible.ns, powers1, ylim=c(0,1))
    plot(possible.ns, powers2, ylim=c(0,1))
    plot(possible.ns, powers3, ylim=c(0,1))
    plot(possible.ns, powersz2, ylim=c(0,1))
    plot(possible.ns, powersz3, ylim=c(0,1))