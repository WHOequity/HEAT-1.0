# © Copyright World Health Organization (WHO) 2016.
# This file is part of the Health Equity Assessment Toolkit (HEAT).
# HEAT is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License Version 2 as published by
# the Free Software Foundation.
# 
# HEAT is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU General Public License for more details.
# You should have received a copy of the GNU General Public License
# along with HEAT. If not, see http://www.gnu.org/licenses/.

######### Absolute Concentration Index (ACI)
# Is a measures of the covariance between social rank and health. It measures the extent to which health/illness 
# is concentrated among groups on the absolute scale.  It may only be used with groups that have a natural ordering (p.9);
# e.g., income quintiles, levels of education, etc.
#
# Reference: Handbook on Health Inequality Monitoring, WHO (2013)
#########


wrap.aci <- function(x, w){
  
  prop.pop <- w/sum(w)  # Each groups proportion of the population
  mid.point <- midPointProp(w)  # cumulative mid point proportions ... Rank in the parlance of Ahmad Hosseinpoor
  
  # Calculate the ACI
  inequal.aci <- sum(prop.pop * (2*mid.point - 1) * x)
  #print(paste(Sys.time(), "out wrap.aci"))
  return(inequal.aci)
}


# Absolute Concentration Index (ACI)
aci <- function(dat, bs=FALSE){ 
  #dat<-x
  #original arguments: x, w=-1, se=-1, bs=F, rankorder
  x<-dat$r
  w<-dat$pop
  se <- dat$se
  rankorder <- dat$order
  # This function returns the Absolute Concentration Index of inequality:
  # Usage
  # x -- a vector of numbers
  # w -- the population size.  The default is unit weights, which is suitable for quintiles
  # se -- the vector of standard errors for each estimated x
  # rankorder -- a vector ordering the subgroup (e.g., quintiles of wealth) worst off (1) to best off (n>1)
  #
  #print(paste(Sys.time(), "begin aci"))
  
  
  if(any(is.na(c(x,w)))){
    return(list(inequal.aci=NA, se.aci.boot=NA,  se.aci.formula=NA))
  }
  
  if(!is.rank(rankorder)){
    return(list(inequal.aci=NA, se.aci.boot=NA,  se.aci.formula=NA))
  }
  
  if(any(is.na(w))) w <- -1

  
  if(any(is.na(se))) se <- -1
  
  if(!is.numeric(x) | !is.numeric(w) | !is.numeric(se)) 
    stop('This function operates on vector of numbers')

  
  if(length(w)==1 & w[1]==-1){  # i.e., if no population numbers are given assume each group has a weight of 1
    w <- rep(1, length(x))
  }
  
  if(length(se)==1)  se <- rep(0, length(x))
  
  if( !(length(x) == length(w)) | !(length(x)==length(se)) ){
    stop('the rates, population-size, and standard errors must all be of the same length')
  }
  
  if(all(w==0)){  # The calculation fails in midproppoint if their are no population numbers / this is a judgement call
    w <- rep(1, length(w))
  }
  
  # Make sure the vectors are ordered from most to least disadvantaged
  # Make sure the vectors are ordered from most to least disadvantaged
  x <- x[order(rankorder)]
  w <- w[order(rankorder)]
  se <- se[order(rankorder)]
  rankorder <- rankorder[order(rankorder)]
  
  if(length(w)==1 & w[1]==-1){
    # If there are no population weights, create a vector of unit weights
    w <- rep(1000, length(x))
  }
  #print(paste(Sys.time(), "before wrap aci"))
  inequal.aci <- wrap.aci(x, w)
  
  prop.pop <- w/sum(w)  # Each groups proportion of the population
  mid.point <- midPointProp(w)  # cumulative mid point proportions ... Rank in the parlance of Ahmad Hosseinpoor
  # Formula-based SE: provided by Ahmad Hosseinpoor (WHO, Geneva)
  se.formula <- sqrt(sum((prop.pop^2)*(2*midPointProp(w)-1)^2*se^2))
  
  # Bootstrap SE
  
  se.boot <- NA
  if(bs==T){
    se.boot <- c()  # Start with an empty vector of estimated ACIs
    for(i in 1:200){  # Run 200 bootstraps 
      nx <- rnorm(length(x), x, se)  # create a new set of estimates (y) varying as rnorm(mean=x, sd=se)
      se.boot <- c(se.boot, wrap.aci(nx, w))  # calculate the ACI on the new data
    } 
    se.boot <- sd(se.boot)  # Estimate the standard error of ACI as the SD of all the bootstrap MLDs 
  }
  #print(paste(Sys.time(), "outbootstrap bootstrap"))
  
  return(list(inequal.aci=inequal.aci, se.aci.boot=se.boot, se.aci.formula=se.formula))
}
