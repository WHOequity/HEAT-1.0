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

######### Theil Index
# http://en.wikipedia.org/wiki/Theil_index
#########


wrap.theil <- function(y, w, national_est){
  # Theil Index wrapper function
  # Protect against log(0) by adding a tiny positive value to any 0
  #y[which(y==0)]  <- .000001
  
  prop.pop <- w/sum(w)
  
  if(is.null(national_est)){  # Calculate the population average from the data if a national average is unavailable
    
    mu <- weighted.mean(x, pop.prop)
  } else {
    #print(paste0("ti: ", national_est))
    mu <- national_est
  }
  
  #print(paste0("y is: ", paste(y, collapse=",")))
  #print(paste0("mu is: ", paste(mu, collapse=",")))
  
  rj <- y / mu
  #This next bit fails if we have an NA so I've commented it out
  # but probably don't want to comment out.
  #if(any(rj <=0)) return(NULL)
 
  inequal.ti <- sum(prop.pop * rj * log(rj))
  #inequal.ti <- 1000*inequal.ti # zev added to match provided data
  return(inequal.ti)
}


ti <- function(dat, bs=FALSE){ 
  y<-dat$r
  w<-dat$pop
  se<-dat$se
  national_est <-unique(dat$r_national)
  #maxopt <- unique(dat$maxoptimum)
  #rankorder <- dat$order
  # This function returns the Theil Index of inequality:
  # Usage
  # y -- a vector of numbers
  # w -- the population size.  The default is unit weights, which is suitable for quintiles
  # se -- the vector of standard errors for each estimated y
  #
  # Protect against log(0) by adding a tiny positive value to any 0
  #y[which(y==0)]  <- .000001

  if(any(is.na(c(y,w)))){
    return(list(inequal.ti=NA, se.ti.boot=NA,  se.ti.formula=NA))
  }
  
  
  if(any(is.na(w))){
    w <- -1
  }
  
  if(any(is.na(se))){
    se <- -1
  }

  
  if(!is.numeric(y) | !is.numeric(w) | !is.numeric(se)){
    stop('This function operates on vector of numbers')
  }
  
  if(length(w)==1 & w[1]==-1){  # i.e., if no population numbers are given assume each group has a weight of 1
    w <- rep(1, length(y))
  }
  
  if(length(se)==1 & se[1]==-1){  # i.e., if there are no standard errors provided, make the se's=0
    se <- rep(0, length(y))
  }
  
  if( !(length(y) == length(w)) | !(length(y)==length(se)) ){
    stop('the rates, population-size, and standard errors must all be of the same length')
  }
  
  if(all(w==0)){  # The calculation fails in midproppoint if their are no population numbers / this is a judgement call
    w <- rep(1, length(w))
  }
  
  inequal.ti <- wrap.theil(y, w, national_est)
  
  
  # Formula-based SE: provided by Ahmad Hosseinpoor (WHO, Geneva)
  prop.pop <- w/sum(w)
  mu <- sum(prop.pop * y)
  rj <- y / mu
  s_u <- prop.pop * rj * (1 + log(rj))
  ti.se.indiv <- ((1 + log(rj) - sum(s_u))^2)*((prop.pop^2)*(se^2)/(mu^2))
  se.formula <- sqrt(sum(ti.se.indiv))
  
  
  # Bootstrap SE
#   if(bs==T){
#     ti.boot <- c()  # Start with an empty vector of estimated BGVs
#     for(i in 1:200){  # Run 1000 bootstraps
#       ny <- abs(rnorm(length(y), y, se))  # create a new set of estimates (y) varying as rnorm(mean=y, sd=se)
#       ti.boot <- c(ti.boot, wrap.theil(ny, w, national_est))  # calculate the RCI on the new data
#     } 
#     se.boot <- sd(ti.boot)  # Estimate the standard error of RCI as the SD of all the bootstrap BGVs 
#   }else{
    se.boot <- NA
  #}
  return(list(inequal.ti=inequal.ti, se.ti.boot=se.boot, se.ti.formula=se.formula))
}
