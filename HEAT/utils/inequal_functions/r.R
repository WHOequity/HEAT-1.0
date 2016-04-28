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

######### Rate Ratio (RR)
# The rate Ratio is used to measure the extent to which one group is relatively better (or worse) off than another group.  It is 
# implemented here in a way that ignores an a priori ordering and selects the two groups (from 2 plus groups) that have
# the best and worst outcomes.
#
# Reference: Ontario Agency for Health Protection and Promotion (Public Health Ontario). Summary measures of
#            socioeconomic inequalities in health. Toronto, ON: Queen’s Printer for Ontario; 2013. (p.17)
#########


# which.hilo <- function(x, rankorder=NULL, maxopt=NULL){
#   # Work out which of two values represents the 'High' and 'Low' rate; given ordered and unordered groups
#   if(is.rank(rankorder)){  # Identify most and least advantaged group
#     g1 <- which(rankorder==min(rankorder))
#     g2 <- which(rankorder==max(rankorder))
#     
#     if(maxopt[1]==0){
#       if(x[g2]==0){
#         return(NULL)
#       }
#       lo.rate <- g2
#       hi.rate <- g1
#     } else if(maxopt[1]==1){
#       lo.rate <- g1
#       hi.rate <- g2
#     }
#   } else {  # Identify best off and worst off group on the health indicator
#     # in github issue 106 we have for rr that RR, if not rankable
#     # only needs two valid level so I'm setting na.rm=T
#     g1 <- which(x==min(x, na.rm=T))#zev added na.rm=T
#     g2 <- which(x==max(x, na.rm=T))
#     if(x[g1]==0){
#       return(NULL)
#     }
#     lo.rate <- g1
#     hi.rate <- g2
#   }  
#   return(list(lo=lo.rate, hi=hi.rate))
# }


#dat<-strata
r <- function(dat, bs=FALSE){
  
  x<-dat$r
  #w<-dat$pop
  se<-dat$se
  #national_est <-unique(dat$r_national)
  maxopt <- unique(dat$maxoptimum)
  rankorder <- dat$order
    
  
  
  # TODO: we were having trouble with the bootstrap results so we're setting the bootstrap to
  # be false if we have non-rankable dimensions which are sex and subnational region
  
  bs <- FALSE
  
  
  rankable <- is.rank(rankorder)
  

  
  if(rankable){  # Identify most and least advantaged group
    


    
    if(maxopt == 1){
      rmin <- x[rankorder==min(rankorder)]
      rmax <- x[rankorder==max(rankorder)]
      inequal.r <- rmax/rmin
      semin <- se[rankorder==min(rankorder)]
      semax <- se[rankorder==max(rankorder)]
    }
    
    if(maxopt == 0){
      # for negative indicators reverse what is min and what is max
      rmin <- x[rankorder==max(rankorder)]
      rmax <- x[rankorder==min(rankorder)]
      inequal.r <- rmax/rmin
      semin <- se[rankorder==max(rankorder)]
      semax <- se[rankorder==min(rankorder)]
    }
    

  } 
  

    
  if(!rankable & !all(dat$subgroup%in%c("Female", "Male")))  {  # Identify best off and worst off group on the health indicator
    
    if(any(is.na(x))) { 
      return(list(inequal.r=NA, se.r.boot=NA, se.r.formula=NA))
    }
    
    x <- x[!is.na(x)]
    
    rmin <- x[x==min(x)]
    rmax <- x[x==max(x)]
    semin <- se[x==min(x)]
    semax <- se[x==max(x)]
    
    inequal.r <-  rmax/rmin
    

    
  }
  
  
  # new 2/24/2016, special treatment of male, female
  if(!rankable & all(dat$subgroup%in%c("Female", "Male")))  {  # Identify best off and worst off group on the health indicator
    
    if(any(is.na(x))) { 
      return(list(inequal.d=NA, se.d.boot=NA, se.d.formula=NA))
    }
    
    #x <- x[!is.na(x)]
    if(maxopt==1){
      rmin <- x[dat$subgroup=="Male"]
      rmax <- x[dat$subgroup=="Female"]
      semin <- se[dat$subgroup=="Male"]
      semax <- se[dat$subgroup=="Female"]
    }else{
      rmin <- x[dat$subgroup=="Female"]
      rmax <- x[dat$subgroup=="Male"]
      semin <- se[dat$subgroup=="Female"]
      semax <- se[dat$subgroup=="Male"]
    }
 
    
    # if maxopt == 1 then you will have rmin=male
    # rmax =female and female/male
    # if maxopy == 0 then you will have rmin=female
    # rmax=male and male/female
    inequal.r <- rmax/rmin
    

    
  }
  
  #   For favourable health intervention indicators (maxoptimum=1), 
  #   D and R should be calculated as D=Girls-Boys R=Girls/Boys
  #   For adverse health outcome indicators (maxoptimum=0), 
  #   D and R should be calculated as D=Boys-Girls R=Boys/Girls
  
  
  if(is.na(maxopt)) return(list(inequal.r=NA, se.r.boot=NA,  se.r.formula=NA))
  
  if(any(is.na(se)))  se <- -1
  
  if(!is.numeric(x) | !is.numeric(se)){
    stop('This function operates on vector of numbers')
  }
    
  if(length(se)==1 & se[1]==-1){  # i.e., if there are no standard errors provided, make the se's=0
    se <- rep(0, length(x))
  }
    
  #hilo <- which.hilo(x=x, rankorder=rankorder, maxopt=maxopt)
  
#   if(is.null(hilo)){
#     return(list(inequal.r=NA, se.r.boot=NA,  se.r.formula=NA))
#   }
  
      
    if(any(is.na(c(rmin, rmax)))){ 
      return(list(inequal.r=NA, se.r.boot=NA, se.r.formula=NA))
    }
  
  # Bootstrap SE
  se.boot <- NA
  if(bs==T){
    r.boot <- c()  # Start with an empty vector of estimated RRs
    for(i in 1:1000){  # Run 200 bootstraps 
      r.est <- rnorm(n=1, rmax, semax)/rnorm(n=1, rmin, semin)  # create a new set of estimates (y) varying as rnorm(mean=x, sd=se)
      r.boot <- c(r.boot, r.est)  # calculate the RII on the new data
    } 
    se.boot <- sd(r.boot)  # Estimate the standard error of RRR as the SD of all the bootstrap RRs 
  }
  
  
  
  # The SE of the Rate Ratio
  se.formula <- sqrt((1/(rmin^2))*((semax^2)+(inequal.r^2)*((semin^2))))
  
  
#  Return the results as a list
  return(list(inequal.r=inequal.r, se.r.boot=se.boot,  se.r.formula=se.formula))  # return a list of the inequality measure and the standard error 
  
}
