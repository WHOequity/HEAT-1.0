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

######### Population Attributable Fraction (PAF)
# Population Attributable Risk is a measure of absolute inequality, and is based on the
# premise that inequality could be eliminated by improving the level of a health indicator
# in a population to match the best-performing subgroup. Simply put, population
# attributable risk shows the improvement possible if all subgroups had the same
# rate as a reference subgroup.
# Handbook on Healt Inequality Monitoring (2013)
#########

wrap.paf <- function(x, w, maxopt=F, rankorder, national_est=NULL){
  # The population attributable fraction
  pop.prop <- w/sum(w) 
  
  w.mean <- national_est
  inequal.par <- wrap.par(x, w, maxopt, rankorder, national_est)
  
  inequal.paf <- (inequal.par/w.mean) * 100
  return(inequal.paf)
}


paf <- function(dat, bs=FALSE){
  #dat<-strata
  #dat<-dat[order(dat$order),]
  x<-dat$r
  w<-dat$pop
  se<-dat$se
  national_est <-unique(dat$r_national)
  maxopt <- unique(dat$maxoptimum)
  rankorder <- dat$order
  # This function returns the percentage of the PAR over the population rate
  # Usage
  # x -- a vector of numbers
  # w -- population weights
  # se -- standard error
  # bs -- whether to use the bootstrap to calculate confidence intervals
  # maxopt -- the highest vakue is the optimum value
  # rankorder -- the rank order of the subgroups (0 mean no rank order)
  #
  # returns the percentage of the PAR over the population rate
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
  if(length(se)==1 && se==-1)se <- rep(0, length(x)) # i.e., if there are no standard errors provided, make the se's=0
  
  
  if( !(length(x) == length(w)) | !(length(x)==length(se)) ) 
    stop('the rates, population-size, and standard errors must all be of the same length')
  
  
  inequal.paf <- wrap.paf(x, w, maxopt, rankorder, national_est)
  
  # Bootstrap SE
  se.boot <- NA
  if(bs==T){  ## Problem with the bootstrap SE
    paf.boot <- c()  # Start with an empty vector of estimated PARs
    for(i in 1:200){  # Run 200 bootstraps 
      nx <- rnorm(length(x), x, se)  # create a new set of estimates (y) varying as rnorm(mean=x, sd=se)
      paf.boot <- c(paf.boot, wrap.paf(nx, w, maxopt, rankorder, national_est))  # calculate the PAR on the new data
    } 
    se.boot <- sd(paf.boot)  # Estimate the standard error of PAR as the SD of all the bootstrap PARs 
  }
  
  
  
  # this matches up with the Excel spreadsheet I was provided
  # inequality measures(Sam Harper) v1
  
  #x<-c(95.5, 92.1, 100, 98, 100)
  #w<-c(102, 72, 78, 87, 77)
  
  #x<-c(25.2, 20.5, 31.4, 29, 26.7)
  #w<-c(244, 229, 196, 222, 182)
  
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
  cy6<-100*abs((1-exp(ct6))-(1-exp(cu6)))/(2*(qnorm(0.975)))
  
  
  if(length(se)==length(x)){
    se.formula <- cy6
  }else{
    se.formula <- NA   
  }
  
  
  
  
  # Return the results as a list
  return(list(inequal.paf=inequal.paf, se.paf.boot=se.boot,  se.paf.formula=se.formula))  # return a list of the inequality measure and the standard error 
  
}




# dat<-filter(maindata, country=="Albania", indic=="anc1", year=="2005", dimension=="Economic status")
# natl<-data.frame(filter(nationaldata, country=="Albania", indic=="anc1", year=="2005"))
# 
# x<-dat$r
# w<-dat$pop
# se<-dat$se
# national_est <-unique(dat$r_national)
# maxopt <- unique(dat$maxoptimum)
# rankorder <- dat$order
# 
# g=r
# h=se
# i=pop
# 
# mu<-weighted.mean(x, w/sum(w)) #af
# 
# co6<-w[which.min(x)] - (min(x)/100)*w[which.min(x)]
# cp6<-(min(x)/100)*w[which.min(x)]
# cq6 <- (mu/100)*sum(w)-cp6
# cr6 <- sum(w)-cq6-cp6-co6
# cv6 <- mu-min(x)
# cx6 <- cv6/mu
# cs6<-sqrt((cr6+cx6*(cq6+co6))/((sum(w)*cp6)))
# 
# ct6<-(log(1-cx6))-qnorm(0.975)*cs6
# cu6<-(log(1-cx6))+qnorm(0.975)*cs6
# cy6<-100*abs((1-exp(ct6))-(1-exp(cu6)))/(2*(qnorm(0.975)))
# 
# 
# 
# 
# cp6<-(MIN(x)/100)*(OFFSET(x[1],MATCH(MIN(x),x,0)-1,2))
# cq6 <- (AF6/100)*SUM(I2:I6)-CP6
# cr6 <- SUM(I2:I6)-CQ6-CP6-CO6
# cs6<-sqrt((CR6+CX6*(CQ6+CO6))/((sum(w)*CP6)))
# cx6 <- CV6/AF6
# ct6<-(ln(1-CX6))-NORMSINV(0.975)*CS6
# cu6<-(ln(1-CX6))+NORMSINV(0.975)*CS6
# fin<-abs((1-exp(CT6))-(1-exp(CU6)))/(2*(NORMSINV(0.975)))










