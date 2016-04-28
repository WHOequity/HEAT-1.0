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

######### Population Attributable Risk (PAR)
# Population Attributable Risk is a measure of absolute inequality, and is based on the
# premise that inequality could be eliminated by improving the level of a health indicator
# in a population to match the best-performing subgroup. Simply put, population
# attributable risk shows the improvement possible if all subgroups had the same
# rate as a reference subgroup.
# Handbook on Healt Inequality Monitoring (2013)
#########

wrap.par <- function(x, w, maxopt=F, rankorder, national_est=NULL){

  pop.prop <- w/sum(w)
  w.mean <- national_est
  

  if(all(rankorder>0) & any(rankorder==1)){  # The data are ranked by subgroup and there is a base group (==1)
    refx <- x[rankorder==max(rankorder)]  # The reference group is the highest ranked subgroup
  } else {
    if( maxopt==T ){  # The data are NOT ranked by subgroup and the maximum value is optimum
      refx <- max(x)
    } else {  # The data are NOT ranked by subgroup and the minimum value is optimum
      refx <- min(x)
    }
  }
  
  inequal.par <- refx - w.mean
  
 
  if( maxopt==T & inequal.par < 0){ 
    inequal.par <- 0
  }
  if( maxopt==F & inequal.par > 0){ 
    inequal.par <- 0
  }
  
  
  return(inequal.par)
}

#dat<-onestrata      
PAR <- function(dat, bs=FALSE){
  #print(count)
  x<-dat$r
  w<-dat$pop
  se<-dat$se
  national_est <-unique(dat$r_national)
  maxopt <- unique(dat$maxoptimum)
  rankorder <- dat$order
  # This function returns the difference between the population rate and the most desirable group rate
  # Usage
  # x -- a vector of numbers
  # w -- population weights
  # se -- standard error
  # bs -- whether to use the bootstrapo to calculate confidence intervals
  # refx -- the reference category
  # national_est -- the national average for the indicator of interest
  # returns the difference between the reference group and the mean
  #  
  
  na.result <- list(inequal.par=NA, se.par.boot=NA,  se.par.formula=NA)
  
  if(is.na(maxopt)) return(na.result)
  if(is.na(national_est)) return(na.result)
  # See github 106, 104
  
  # If rankable and we don't have the lowest level
  rankable <- is.rank(rankorder)
  tmpmin <- x[rankorder==1]
  tmpmax <- x[rankorder==max(rankorder)]
  
  if(rankable && (is.na(tmpmin) | length(tmpmin)==0 | is.na(tmpmax) | length(tmpmax) == 0)) return(na.result)
  
  
  # zev added based on github 106 if non-rankable and any are missing
  if(!rankable & any(is.na(x))) return(na.result)
  if(any(is.na(w))) w <- -1
  if(any(is.na(se))) se <- -1
  if(!is.numeric(x) | !is.numeric(w) | !is.numeric(se)) stop('This function operates on vector of numbers')
  if(length(w)==1 & w[1]==-1) w <- rep(1000, length(x))
  
  if(length(se)==1){
    if(se==-1){  # i.e., if there are no standard errors provided, make the se's=0
      se <- rep(0, length(x))
    }
  }
  
  if( !(length(x) == length(w)) | !(length(x)==length(se)) ){
    stop('the rates, population-size, and standard errors must all be of the same length')
  }
  
  inequal.par <- wrap.par(x, w, maxopt, rankorder, national_est)
  
  # Bootstrap SE
  se.boot <- NA
  if(bs==T){  ## Problem with the bootstrap SE
    par.boot <- c()  # Start with an empty vector of estimated PARs
    for(i in 1:200){  # Run 200 bootstraps 
      nx <- rnorm(length(x), x, se)  # create a new set of estimates (y) varying as rnorm(mean=x, sd=se)
      par.boot <- c(par.boot, wrap.par(nx, w, maxopt, rankorder, national_est))  # calculate the PAR on the new data
    } 
    se.boot <- sd(par.boot)  # Estimate the standard error of PAR as the SD of all the bootstrap PARs 
  }
  
  if(length(se)==length(x)){
    
    pop.prop <- w/sum(w)  
    w.mean <- national_est
    
    # ZEV: this is in the wrap.par function so not needed here I think
#     if(all(!rankorder==0)){  # The data are ranked by subgroup 
#       refx <- x[rankorder==1]
#     } else {
#       if( maxopt==T ){  # The data are NOT ranked by subgroup and the maximum value is optimum
#         refx <- max(x)
#       } else {  # The data are NOT ranked by subgroup and the minimum value is optimum
#         refx <- min(x)
#       }
#     }
#     
    
    
    mu<-weighted.mean(x, w/sum(w)) #af
    
    co6<-w[which.min(x)] - (min(x)/100)*w[which.min(x)]
    cp6<-(min(x)/100)*w[which.min(x)]
    cq6 <- (mu/100)*sum(w)-cp6
    cr6 <- sum(w)-cq6-cp6-co6
    cv6 <- mu-min(x)
    cx6 <- cv6/mu
    cs6<-sqrt((cr6+cx6*(cq6+co6))/((sum(w)*cp6)))
    
    ct6<-(log(1-cx6))-qnorm(0.975)*cs6
    cu6<-(log(1-cx6))+qnorm(0.975)*cs6
    cy6<-abs((1-exp(ct6))-(1-exp(cu6)))/(2*(qnorm(0.975)))
    
    #cw6<-abs(mu * ((cx6 + qnorm(0.975)*cy6) - (cx6-qnorm(0.975)*cy6)))/(2*qnorm(0.75))
    cw6<-abs(mu*((cx6+qnorm(0.975)*cy6)-(cx6-qnorm(0.975)*cy6))) / (2*qnorm(0.975))
    
    
    if(length(se)==length(x)){
      se.formula <- cw6
    }else{
      se.formula <- NA   
    }
    
    #     se.formula <- sqrt(sum(el1) +  el2)
  }
  
  # Return the results as a list
  #  print(list(inequal.par=inequal.par, se.par.boot=se.boot,  se.par.formula=se.formula))
  return(list(inequal.par=inequal.par, se.par.boot=se.boot,  se.par.formula=se.formula))  # return a list of the inequality measure and the standard error 
  
}
