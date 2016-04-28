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

######### Rate Difference (RD)
# The rate difference is used to measure the extent to which one group is better (or worse) off than another group.  It is 
# implemented here in a way that ignores an a priori ordering and selects the two groups (from 2 plus groups) that have
# the best and worst outcomes.
#
# Reference: Ontario Agency for Health Protection and Promotion (Public Health Ontario). Summary measures of
#            socioeconomic inequalities in health. Toronto, ON: Queen’s Printer for Ontario; 2013. (p.17)
#########


#dat <- disagdata.split[[3722]]$data
#dat<-onestrata
d <- function(dat, bs=FALSE){
  
  
  x<-dat$r 
  w<-dat$pop 
  se<-dat$se 
  #national_est <-unique(dat$r_national)
  maxopt <- unique(dat$maxoptimum)
  rankorder <- dat$order
  # This function returns the difference (i.e., the range) between the maximum and minimum values in a vector of rates
  #
  # Usage
  # x -- a vector of numbers
  # rankorder -- for rd, if subgroups are oderable compare highest and lowest subgroup, otherwise max against min
  # maxopt -- if higher indicators are better, maxopt is 1, if lower is better, 0
  # returns the difference between max(x) and min(x) ($rd) and the rate difference standard error (se.rd)
  #  
  
  rankable <- is.rank(rankorder)
  
  # if it's rankable and we're missing the r value for the max or min
  # then return
  
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
  
  
  if(rankable){  # Identify most and least advantaged group
    
    
    
    
    
    
    if(maxopt == 1){
      rmin <- x[rankorder==min(rankorder)]
      rmax <- x[rankorder==max(rankorder)]
      inequal.d <- rmax - rmin
      semin <- se[rankorder==min(rankorder)]
      semax <- se[rankorder==max(rankorder)]
    }
    
    if(maxopt == 0){
      # for negative indicators reverse what is min and what is max
      rmin <- x[rankorder==max(rankorder)]
      rmax <- x[rankorder==min(rankorder)]
      inequal.d <- rmax - rmin
      semin <- se[rankorder==max(rankorder)]
      semax <- se[rankorder==min(rankorder)]
    }
    
    
    
    
    
  } 
  
  
  if(!rankable & !all(dat$subgroup%in%c("Female", "Male")))  {  # Identify best off and worst off group on the health indicator
    
    if(any(is.na(x))) { 
      return(list(inequal.d=NA, se.d.boot=NA, se.d.formula=NA))
    }
    
    x <- x[!is.na(x)]
    
    rmin <- x[x==min(x)]
    rmax <- x[x==max(x)]
    
    inequal.d <-  rmax - rmin
    
    semin <- se[x==min(x)]
    semax <- se[x==max(x)]
    
  }
  
  # new 2/24/2016, special treatment of male, female
  if(!rankable & all(dat$subgroup%in%c("Female", "Male")))  {  # Identify best off and worst off group on the health indicator
    
    if(any(is.na(x))) { 
      return(list(inequal.d=NA, se.d.boot=NA, se.d.formula=NA))
    }
    
    if(maxopt == 1){
      
      rmin <- x[dat$subgroup=="Male"] 
      rmax <- x[dat$subgroup=="Female"]
      
      
      inequal.d <-rmax-rmin # ifelse(maxopt==1,  rmin-rmax, rmax-rmin)
      
      semin <- se[dat$subgroup=="Male"]
      semax <- se[dat$subgroup=="Female"]
      
      
    }
    
    if(maxopt == 0){
      rmin <- x[dat$subgroup=="Female"]
      rmax <- x[dat$subgroup=="Male"]
      
      inequal.d <- rmax-rmin
      
      semin <- se[dat$subgroup=="Female"]
      semax <- se[dat$subgroup=="Male"]
      
    }
    
    
    
  }
  
  #   For favourable health intervention indicators (maxoptimum=1), 
  #   D and R should be calculated as D=Girls-Boys R=Girls/Boys
  #   For adverse health outcome indicators (maxoptimum=0), 
  #   D and R should be calculated as D=Boys-Girls R=Boys/Girls
  
  if(any(is.na(c(rmin, rmax)))){ 
    return(list(inequal.d=NA, se.d.boot=NA, se.d.formula=NA))
  }
  
  
  
  se.formula <- sqrt( semax^2 + semin^2 )  # The SE of the difference is the squareroot of the sum of the squared SEs     
  
  # Boot strap SE
  #se.boot <- NA
  if(bs==T){
    se.boot <- c()  # Start with an empty vector of estimated RDs
    for(i in 1:200){  # Run 200 bootstraps 
      boot.d <- abs(rnorm(n=1, rmin, semax) - rnorm(n=1, rmax, semin))  # create a new set of estimates (y) varying as rnorm(mean=x, sd=se)
      se.boot <- c(se.boot, boot.d)  # calculate the MLD on the new data
    } 
    se.boot <- sd(se.boot)  # Estimate the standard error of MLD as the SD of all the bootstrap MLDs 
  }
  
  # Return the results as a list
  return(list(inequal.d=inequal.d, se.d.boot=se.boot, se.d.formula=se.formula))
}



