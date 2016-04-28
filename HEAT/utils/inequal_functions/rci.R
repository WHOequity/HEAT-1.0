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

######### Relative Concentration Index (RCI)
# Is a measures of the covariance between social rank and health and measures the extent to which health/illness 
# is concentrated among groups on the absolute scale.  It may only be used with groups that have a natural ordering (p.9);
# e.g., income quintiles, levels of education, etc.
#
# Reference:
#########

wrap.rci <- function(y, w, se, rankorder, national_est){
  # Relative Concentration Index wrapper function
  prop.pop <- w/sum(w)  # Each groups proportion of the population
  
  if(is.null(national_est)){  # Us the weighted mean of the data if there is no national estimate
    
    w.mean <- weighted.mean(y, pop.prop)
  } else {
    #print(paste0("rci: ", national_est))
    w.mean <- national_est
  }

  dat <- data.frame(r=y, pop=w, se=se, order=rankorder)

  inequal.aci <- aci(dat, bs=F)$inequal.aci  # ACI
  inequal.rci <- inequal.aci / w.mean
  
  #inequal.rci <- 100 * inequal.rci #zev added to match provided data
  return (inequal.rci)  
}


#dat <- onestrata
rci <- function(dat, bs=FALSE){ 
  y<-dat$r
  w<-dat$pop
  se<-dat$se
  national_est <-unique(dat$r_national)
  #maxopt <- unique(dat$maxoptimum)
  rankorder <- dat$order
  # This function returns the Absolute Concentration Index of inequality:
  # Usage
  # y -- a vector of numbers
  # w -- the population size.  The default is unit weights, which is suitable for quintiles
  # se -- the vector of standard errors for each estimated y
  #
  
  
  if(any(is.na(c(y,w)))){
    return(list(inequal.rci=NA, se.rci.boot=NA,  se.rci.formula=NA))
  }
  
  if(!is.rank(rankorder)){  # If these are not rankordered, then RCI does not apply
    return(list(inequal.rci=NA, se.rci.boot=NA,  se.rci.formula=NA))
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
  
  if(length(se)==1 & se[1]==-1){
    se <- rep(0, length(y))
  }
  
  if( !(length(y) == length(w)) | !(length(y)==length(se)) ){
    stop('the rates, population-size, and standard errors must all be of the same length')
  }
  
  # Make sure the vectors are ordered from most to least disadvantaged
  y <- y[order(rankorder)]
  w <- w[order(rankorder)]
  se <- se[order(rankorder)]
  rankorder <- rankorder[order(rankorder)]
  
    
  if(all(w==0)){  # The calculation fails in midproppoint if their are no population numbers / this is a judgement call
    w <- rep(1, length(w))
  }
  
  
  inequal.rci <- wrap.rci(y, w, se, rankorder, national_est)
  
  
  # Formula-based SE: provided by Ahmad Hosseinpoor (WHO, Geneva)
  prop.pop <- w/sum(w)  # Each groups proportion of the population
  mid.point <- midPointProp(w)  # cumulative mid point proportions ... Rank in the parlance of Ahmad Hosseinpoor
  sumprodsqrd <- sum(prop.pop * y)^2
  
  s2_s6 <- (prop.pop^2)*((2*mid.point-1)-inequal.rci)^2*se^2
  se.formula <- sqrt( sum(s2_s6) / sumprodsqrd )
  
  # Bootstrap SE
  if(bs==T){
    rci.boot <- c()  # Start with an empty vector of estimated BGVs
    for(i in 1:200){  # Run 200 bootstraps
      ny <- rnorm(length(y), y, se)  # create a new set of estimates (y) varying as rnorm(mean=y, sd=se)
      rci.boot <- c(rci.boot, wrap.rci(ny, w, se, rankorder, national_est))  # calculate the RCI on the new data
    } 
    se.boot <- sd(rci.boot)  # Estimate the standard error of RCI as the SD of all the bootstrap BGVs 
  }else{
    se.boot <- NA
  }
  
  return(list(inequal.rci=inequal.rci, se.rci.boot=se.boot, se.rci.formula=se.formula))
}


