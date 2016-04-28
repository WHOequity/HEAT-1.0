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

######### Mean Difference from the Best performing Subgroup (mdb)
## 
# 
#
#
#########

wrap.mdb <- function(x, w, rankorder, maxopt=0){
  # (Weighted) Mean difference from the best performing subgroup 
  #refx <- ifelse(maxopt==1, max(x), min(x))  # The reference is the most desirable outcome among the subgroups
  #print(refx)
  p <- w/sum(w)
  
  if(all(rankorder>0) & any(rankorder==1)){  # The data are ranked by subgroup and there is a base group (==1)
    refx <- x[rankorder==max(rankorder)]  # The reference group is the highest ranked subgroup
  } else {
    if( maxopt==T ){  # The data are NOT ranked by subgroup and the maximum value is optimum
      refx <- max(x)
    } else {  # The data are NOT ranked by subgroup and the minimum value is optimum
      refx <- min(x)
    }
  }
  
  
  inequal.mdb <- sum(p * abs(refx-x))
  return(inequal.mdb)
}


mdb <- function(dat, bs=FALSE){
  
  #dat<-disag1strata
  x<-dat$r
  w<-dat$pop
  se<-dat$se
  #print("mdb maxoptimum")
  #print(dat$maxoptimum)
  maxopt <- unique(dat$maxoptimum)
  rankorder <- dat$order
  # This function returns the mean difference between each groups rate and the best performing group
  #
  # Usage
  # x -- a vector of numbers (the rate)
  # returns the mean difference between and the se
  #  
  
  if(any(is.na(c(x,w)))){
    return(list(inequal.mdb=NA, se.mdb.boot=NA,  se.mdb.formula=NA))
  }
  
  
  if(any(is.na(w))) w <- -1

  
  if(any(is.na(se))) se <- -1
  
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
  
  
  inequal.mdb <- wrap.mdb(x, w,rankorder, maxopt)
  
  # Bootstrap SE
  se.boot <- NA
  if(bs==T){
    mdb.boot <- c()  # Start with an empty vector of estimated BGVs
    for(i in 1:200){  # Run 200 bootstraps 
      nx <- rnorm(length(x), x, se)  # create a new set of estimates (y) varying as rnorm(mean=x, sd=se)
      mdb.boot <- c(mdb.boot, wrap.mdb(nx, w, rankorder, maxopt))  # calculate the MDB on the new data
    } 
    se.boot <- sd(mdb.boot)  # Estimate the standard error of IDisp as the SD of all the bootstrap IDisp's 
  }
  
  se.formula <- NA  
  
  
  # Return the results as a list
  return(list(inequal.mdb=inequal.mdb, se.mdb.boot=se.boot,  se.mdb.formula=se.formula))  # return a list of the inequality measure and the standard error 
  
}
