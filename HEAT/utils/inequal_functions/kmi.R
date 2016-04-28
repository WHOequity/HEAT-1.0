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

######### Kunst Mackenbach (Relative) Index

# wrap.kmi <- function(y, w){
#   # Relative Index of Inequality wrapper  
#  p <- w/sum(w)
# #   x <- midPointProp(w)
# #   sii_ <- wrap.sii(y, w)
# #   denominator <- (sum(p * y)) - (sii_ * (sum(p*x) -1))
#   #inequal.kmi <- (sii_/denominator)
#   
#   #X <- round(cumsum(p) - 0.5*p,2)
#   
#   #siiNumerator<-round(sum(p*X*y)-(sum(p*X)*sum(p*y)),2)
#   #siiDenom <- round(sum(p*X^2)-(sum(p*X)^2),2)
#   
#   #sii<-round(siiNumerator/siiDenom, 2)
#  
#   X <- midPointProp(w)
#   sii_ <- wrap.sii(y, p)
# 
#   b0<-sum(p*y) - sii_*sum(p*X)
#   inequal.kmi<-abs((b0+sii_)/b0 )
#   
#   
#   return(inequal.kmi)
# }


kmi <- function(dat, bs=FALSE){
 # print(data.frame(dat))
  #dat<-onestrata
  y<-dat$r
  w<-dat$pop
  se<-dat$se
  national_est <-unique(dat$r_national)
  maxopt <- unique(dat$maxoptimum)
  rankorder <- dat$order
  indic_num <- dat$indic_category_number[1]
  indic <- dat$indic[1]
  # This function returns the Kunst Mackenbach index of inequality:
  # Usage
  # y -- a vector of numbers
  # w -- the population size.  The default is unit weights, which is suitable for quintiles
  # se -- the vector of standard errors for each estimated y
  # rankorder -- for sii, the subgroups must be oderable (e.g., quintiles of wealth)
  # maxopt -- if higher indicators are better, maxopt is 1, if lower is better, 0
  # returns: kmi and its standard erros.
  #
  
  na.result <- list(inequal.sii=NA, se.sii.boot=NA,  se.sii.formula=NA)
  
  if(any(is.na(c(y,w)))) return(na.result)
  if(is.na(maxopt)) return(na.result)
  if(!is.rank(rankorder)) return(na.result)
  
  
  if(any(is.na(w))) w <- -1
  if(any(is.na(se)))se <- -1
  

  
  if(!is.numeric(y) | !is.numeric(w) | !is.numeric(se)) stop('This function operates on vector of numbers')

  
  if(length(w)==1 & w[1]==-1) w <- rep(1, length(y))  # i.e., if no population numbers are given assume each group has a weight of 1

  
  if(length(se)==1 & se[1]==-1) se <- rep(0, length(y))

  
  if( !(length(y) == length(w)) | !(length(y)==length(se))) 
    stop('the rates, population-size, and standard errors must all be of the same length')

  
  if(all(w==0)) w <- rep(1, length(w))  # The calculation fails in midproppoint if their are no population numbers / this is a judgement call
    

  inequal.kmi <- wrap.sii(y, w, indic, indic_num, maxopt, "kmi")
  
  
  ##############  Disable the Bootstrap SE's for kmi  
  #   # Bootstrap SE
  #   if(bs==T){
  #     kmi.boot <- c()  # Start with an empty vector of estimated kmis
  #     for(i in 1:2000){  # Run 200 bootstraps 
  #       ny <- rnorm(length(y), y, se)  # create a new set of estimates (y) varying as rnorm(mean=y, sd=se)
  #       kmi.boot <- c(kmi.boot, wrap.kmi(ny, w))  # calculate the kmi on the new data
  #     } 
  #     se.boot <- sd(kmi.boot)  # Estimate the standard error of RII as the SD of all the bootstrap RIIs 
  #   }
  #   else{
  #     se.boot <- NA
  #   }
  se.boot <- NA
  
  se.formula <- inequal.kmi[["kmi.se"]]
  inequal.kmi <- inequal.kmi[["kmi"]]
  
 
  
  return(list(inequal.kmi=inequal.kmi, se.kmi.boot=se.boot,  se.kmi.formula=se.formula))  # return a list of the inequality measure and the standard error 
  
}

