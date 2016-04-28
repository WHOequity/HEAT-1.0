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

######### Index of Disparity (IDis)
# The Index of Disparity summarizes the difference between several group rates
# and a reference rate and expresses the average differences as a proportion of 
# the reference rate (Keppel & Pearcy, 2002: http://www.ncbi.nlm.nih.gov/pubmed/12432138).
# 
# 
#
#
#########

wrap.idis <- function(x, national_est){  
  # The wrapper for the Index of Disparity

  
  sum.disparity <- sum(abs(x - national_est))  # Calculate the differences between each x and the reference rate (min(x))
  mean.disparity <- sum.disparity/(length(x))
  inequal.idis <- mean.disparity/national_est*100
  
  return(inequal.idis)
}


idis <- function(dat, bs=FALSE){
  #dat <- disagdata.split[[1]]$data
  x<-dat$r
  w<-dat$pop
  se<-dat$se
  national_est <-unique(dat$r_national)
  # This function returns the difference (i.e., the range) between the national values in a vector of rates
  #
  # Usage
  # x -- a vector of numbers
  # returns the difference between max(x) and min(x) ($rd) and the rate difference standard error (se.rd)
  #  
  
  # If the reference category for best performance iz zero, return NULL otherwise there is a div0 problem
  #print(x)
  #print(national_est)
  
  na.results <- list(inequal.idis=NA, se.idis.boot=NA,  se.idis.formula=NA)
  
  if(any(is.na(x))) return(na.results)
  if(any(is.na(w))) w <- -1
  if(any(is.na(se))) se <- -1
  if(!is.numeric(x) | !is.numeric(w) | !is.numeric(se)) stop('This function operates on vector of numbers')

  if(length(w)==1 & w[1]==-1)w <- rep(1000, length(x))

  
  if(length(se)==1){
    if(se==-1){  # i.e., if there are no standard errors provided, make the se's=0
      se <- rep(0, length(x))
    }
  }
  
  if( !(length(x) == length(w)) | !(length(x)==length(se)) ){
    stop('the rates, population-size, and standard errors must all be of the same length')
  }
  
  inequal.idis <- wrap.idis(x, national_est)
                            
  if(is.na(inequal.idis)) return(na.results)

    
  # Bootstrap SE
  se.boot <- NA
  if(bs==T){
    idis.boot <- c()  # Start with an empty vector of estimated BGVs
    for(i in 1:200){  # Run 200 bootstraps 
      nx <- rnorm(length(x), x, se)  # create a new set of estimates (y) varying as rnorm(mean=x, sd=se)
      idis.boot <- c(idis.boot, wrap.idis(nx, national_est))  # calculate the idis on the new data
    } 
    se.boot <- sd(idis.boot, na.rm=T)  # Estimate the standard error of idis as the SD of all the bootstrap idis's 
  }
  
  
  
  # The SE from formula for the Index of Disparity
  # var(idis) = 1/refx^2
#   if(is.na(national_est)){
#     se.formula <- NA
#   }else{
#     refx <- national_est
#     el1 <- refx^2  
#     el2 <- (length(x))^2 * sum(se^2)   
#     el3 <- ((inequal.idis + 1)^2) * (dat$se_national^2) #zev made this change for now
#     #el3 <- (inequal.idis + 1)^2 * se[which(x==refx)]^2
#     se.formula <- sqrt((el2 + el3 )/el1)  
#   #se.formula <- NA
#   }
  se.formula <- NA
  # Return the results as a list
  return(list(inequal.idis=inequal.idis, se.idis.boot=se.boot,  se.idis.formula=se.formula))  # return a list of the inequality measure and the standard error 
  
}
