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

######### Between-Groups Variance (BGV)
# Reference: Harper & Lynch (p.8)

wrap.bgv <- function(x, w, national_est=NULL){
  # zev moved prop.pop out of the if/then
  prop.pop <- w/sum(w)  # Calculate proportion of the population in each group
  
  # Between Group Variance wrapper function
  if(is.null(national_est)){  # Calculate the population average from the data if a national average is unavailable
    meanx <- weighted.mean(x, prop.pop)  # Calculate the weighted mean 
  } else {
    
    meanx <- national_est
  }
  
  inequal.bgv <- sum(prop.pop * (x - meanx)^2)  # 
  
  return (inequal.bgv)  
}


bgv <- function(dat, bs=FALSE){
  x<-dat$r
  w<-dat$pop
  se<-dat$se
  national_est <-unique(dat$r_national)
  #print(national_est)
  #x, w=-1, se=-1, bs=F, national_est=NULL
  # This function returns the between group variance and is calculated as the square of the differences in group
  # rates from the population average, weighted by each groups population sizes:
  # Usage
  # x -- a vector of numbers
  # w -- the population size.  The default is unit weights, which is suitable for quintiles
  # returns the between group variance and its standard error
  #
  
  if(any(is.na(c(x,w)))){
    return(list(inequal.bgv=NA, se.bgv.boot=NA,  se.bgv.formula=NA))
  }
  
  if(any(is.na(w))){
    w <- -1
  }
  
  if(any(is.na(se))){
    se <- -1
  }
  
  if(!is.numeric(x) | !is.numeric(w) | !is.numeric(se)){
    stop('This function operates on vector of numbers')
  }
  
  if(length(w)==1 & w[1]==-1){  # i.e., if no population numbers are given assume each group has a n of 1,000
    w <- rep(1000, length(x))
  }
  
  if(length(se)==1){
    if(se==-1){  # i.e., if there are no standard errors provided, make the se's=0
      se <- rep(0, length(x))
    }
  }
  
  if( !(length(x) == length(w)) | !(length(x)==length(se)) ){
    stop('the rates, population-size, and standard errors must all be of the same length')
  }
  
  # Calculate the BGV
  inequal.bgv <- wrap.bgv(x, w, national_est)
  
  # Bootstrap SE
  # Bootstrap SE
  if(bs==T){
    bgv.boot <- c()  # Start with an empty vector of estimated BGVs
    for(i in 1:200){  # Run 1000 bootstraps
      nx <- rnorm(length(x), x, se)  # create a new set of estimates (x) varying as rnorm(mean=x, sd=se)
      bgv.boot <- c(bgv.boot, wrap.bgv(nx, w, national_est))  # calculate the BGV on the new data
    } 
    se.boot <- sd(bgv.boot)  # Estimate the standard error of BGV as the SD of all the bootstrap BGVs 
  }else{
    se.boot <- NA
  }
  
  # Formula-based SE: provided by Ahmad Hosseinpoor (WHO, Geneva)
  # This formula is not going to be corrected if the bgv estimate is based on
  # a national_est for the weighted.mean
    prop.pop <- w/sum(w)  # Each groups proportion of the population
    weighted.mean <- sum(prop.pop * x)
    p2__1_p2__se4 <- (prop.pop^2)*((1-prop.pop)^2)*(se^4)
    s4 <- (prop.pop^4)*(se^4)
    s2 <- (prop.pop^2)*(se^2)
    p2se2__y_mu__2 <- (prop.pop^2)*(se^2)*((x-weighted.mean)^2)  ################
    
    se.formula <- sqrt(4*(sum(p2se2__y_mu__2))+2*(((sum(s2))^2)-sum(s4)+sum(p2__1_p2__se4)))
  
  
  # Return the results as a list
  return(list(inequal.bgv=inequal.bgv, se.bgv.boot=se.boot,  se.bgv.formula=se.formula))  # return a list of the inequality measure and the standard error 
}



