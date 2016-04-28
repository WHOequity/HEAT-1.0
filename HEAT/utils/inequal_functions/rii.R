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

######### Relative Index of Inequality (RII)


wrap.rii <- function(y, w, national_est, indic, indic_num, maxopt){
  # Relative Index of Inequality wrapper  
# y <- ny
  
  prop.pop <- w/sum(w)  # Each groups proportion of the population
  
  if(is.null(national_est)){  # Us the weighted mean of the data if there is no national estimate
    
    w.mean <- weighted.mean(y, pop.prop)
  } else {
    #print(paste0("rii: ", national_est))
    w.mean <- national_est
  }
 
  inequal.sii <- wrap.sii(y, w, indic, indic_num, maxopt, "sii")
  inequal.rii <- inequal.sii[['sii']]/w.mean
  return (inequal.rii) 
  
  
  
}


rii <- function(dat, bs=FALSE){
  dat <- dplyr::arrange(dat, order)
  y<-dat$r
  w<-dat$pop
  se<-dat$se
  national_est <-unique(dat$r_national)
  maxopt <- unique(dat$maxoptimum)
  rankorder <- dat$order
  indic_num <- dat$indic_category_number[1]
  indic <- dat$indic[1]
  
  # This function returns the slope index of inequality and is calculated as the beta-coefficient in the regression
  # of the mean health variable on the mean relative rank variable.:
  # Usage
  # y -- a vector of numbers
  # w -- the population size.  The default is unit weights, which is suitable for quintiles
  # se -- the vector of standard errors for each estimated y
  # rankorder -- for sii, the subgroups must be oderable (e.g., quintiles of wealth)
  # maxopt -- if higher indicators are better, maxopt is 1, if lower is better, 0
  # returns the beta-coefficient and its standard error.
  #
  
  na.result <- list(inequal.rii=NA, se.rii.boot=NA,  se.rii.formula=NA)
  
  
  if(any(is.na(c(y,w)))){
    return(list(inequal.rii=NA, se.rii.boot=NA,  se.rii.formula=NA))
  }
  
  
  
  if(is.na(maxopt)){
    return(list(inequal.rii=NA, se.rii.boot=NA,  se.rii.formula=NA))
  }
  
  if(!is.rank(rankorder)){  # If these are not rankordered, then RII does not apply
    return(list(inequal.rii=NA, se.rii.boot=NA,  se.rii.formula=NA))
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
  
  if(all(w==0)){  # The calculation fails in midproppoint if their are no population numbers / this is a judgement call
    w <- rep(1, length(w))
  }
   
  if(length(se)==1 & se[1]==-1){  # i.e., if there are no standard errors provided, make the se's=0
    se <- rep(0, length(y))
  }
  
  if( !(length(y) == length(w)) | !(length(y)==length(se)) ){
    stop('the rates, population-size, and standard errors must all be of the same length')
  }


  
  inequal.rii <- wrap.rii(y, w, national_est, indic, indic_num, maxopt)
  if(is.na(inequal.rii)) return(na.result)
  
  # Bootstrap SE
#   if(bs==T){
#     rii.boot <- c()  # Start with an empty vector of estimated BGVs
#     for(i in 1:100){  # Run 200 bootstraps 
#       # se[se==0] <- mean(se)
#       ny <- rnorm(length(y), y, se)  # create a new set of estimates (y) varying as rnorm(mean=x, sd=se)
#       if(any(ny>100 | ny<0)) next
#       rii.boot <- c(rii.boot, wrap.rii(ny, w, national_est, indic,  indic_num, maxopt))  # calculate the RII on the new data
#       } 
#     se.boot <- sd(rii.boot)  # Estimate the standard error of RII as the SD of all the bootstrap RIIs 
#   }else{
#     se.boot <- NA
#   }
  
  se.boot <- NA

  # Formula-based SE: provided by Sam Harper
  prop.pop <- (w/sum(w))
  midpoint <- midPointProp(w)
  weighted_mu <- sum( prop.pop * y)
  pXy <- (y * prop.pop * midpoint)
  pX_mu <- prop.pop* midPointProp(w) * weighted_mu
  psum__pXy__ <-  prop.pop * sum( pXy )
  
  pX_mu__psum__pXy__ <- se^2 * (pX_mu - psum__pXy__)^2 
  pX2 <- prop.pop * midpoint^2
  pX <- prop.pop * midpoint
  
  se.formula <- (sqrt((sum(pX_mu__psum__pXy__)/(((weighted_mu^4)*((sum(pX2)-(sum(pX)^2))^2))))))
  
  # Return the results as a list
  return(list(inequal.rii=inequal.rii, se.rii.boot=se.boot,  se.rii.formula=se.formula))  # return a list of the inequality measure and the standard error 
}





