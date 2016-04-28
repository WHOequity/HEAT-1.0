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

######### Slope Index of Inequality (SII)

wrap.sii <- function(y, w, indic, indic_num, maxopt, inequaltype){
  #inequaltype <- "sii"
  #inequaltype <- "kmi"
  #indic_num <- 4
  #maxopt

  if(indic_num == 6) unit <- 1 # rate per person (fertility)
  if(any(indic_num %in%c(1:5, 8:10))) unit <- 2 # percentage
  if(indic_num  == 7 | indic == "asfr1") unit <- 3 # rate per 1000
  
  
  divisor <- switch(unit,
         "1" = 10,
         "2" = 100,
         "3" = 1000)
  
  
  y <- round(y/divisor*w)
  w <- round(w)
  
  # Compute the ranks
  rank <- midPointProp(w)
  
  # Set up new data with the y/pop as y
  newdat <- data.frame(rank=rank, y=y, pop = w)
  
  # Using quasibinomial to get around non-integer warning
  # https://stat.ethz.ch/pipermail/r-help/2006-April/103862.html
#   model2 <- try(rma.glmm(measure="PLO", mods = ~rank, xi=y, ni=pop, method="FE",
#                      data=newdat), silent = TRUE)
  
  
  model2 <- rma.glmm(measure="PLO", mods = ~rank, xi=y, ni=pop, method="FE",data=newdat)
#   if(class(model2) == "try-error" & inequaltype=="sii") return(c("sii" = NA, "sii.se" = NA))
#   if(class(model2) == "try-error" & inequaltype=="kmi") return(c("kmi" = NA, "kmi.se" = NA))
  
  
  summary.info <- coef(summary(model2))
  coefs <- summary.info[,"estimate"]

  alpha <- summary.info["intrcpt","estimate"]
  se.alpha <- summary.info["intrcpt","se"]
  
  beta <- summary.info["rank","estimate"]
  se.beta <- summary.info["rank","se"]
  
  vcov <- vcov.rma(model2)
  q.val <- qnorm(0.975)
  
  # predicted values at bottom and top of rank
  p1 <- exp(alpha + beta) / (1 + exp(alpha + beta))
  p0 <- exp(alpha) / (1 + exp(alpha))
  
  
  # TODO: this needs to be filled in
  if(unit ==1){
    SII <- NA
    SIIse <- NA
  }
  
  
  
  #Case 1: Favourable health intervention indicators (maxoptimum=1) that are reported as percentages
  if(unit==2 & maxopt==1){
    
    SII <- 100 * (p1 - p0)  
    
    SIIse <- 100 * deltamethod(~ (exp(x1+x2) / (1+exp(x1+x2)) - (exp(x1) / (1+exp(x1)))), 
                               coefs, vcov)
    
    KMI <- p1/p0 

    KMIse <-  deltamethod(~ (exp(x1+x2) / (1+exp(x1+x2)) / (exp(x1) / (1+exp(x1)))), 
                          coefs, vcov)
    
    
  }
  
  
  #Case 2: Adverse health outcome indicators (maxoptimum=0) that are reported as percentages 
  
  if(unit==2 & maxopt==0){
    SII <- 100*(p0 - p1) 
    
    # standard error of SII
    SIIse <- 100 * deltamethod(~ (exp(x1+x2) / (1+exp(x1+x2)) - (exp(x1) / (1+exp(x1)))), 
                               coefs, vcov)
    
    
    KMI <- p0/p1
    
    # standard error of the KMI
    KMIse <-  deltamethod(~ (exp(x1+x2) / (1+exp(x1+x2)) / (exp(x1) / (1+exp(x1)))), 
                          coefs, vcov)
    
    
  }
  
  
  # Case 3: Adverse health outcome indicators (maxoptimum=0) that are reported as rates per 1000 
  
  if(unit == 3 & maxopt==0){
    SII <- 1000 * (p0 - p1) 
    
    # standard error of SII
    SIIse <- 1000* deltamethod(~ (exp(x1+x2) / (1+exp(x1+x2)) - (exp(x1) / (1+exp(x1)))), 
                               coefs, vcov)
    
    
    KMI <- p0/p1
    
    # standard error of the KMI
    KMIse <-  deltamethod(~ (exp(x1+x2) / (1+exp(x1+x2)) / (exp(x1) / (1+exp(x1)))), 
                          coefs, vcov)
    
  }
  
  
  # CASE 4:  Adverse health outcome indicators (maxoptimum=0) that are reported as rate per person
  if(unit == 1 & maxopt==0){
    SII <- 10 * (p0 - p1) 
    
    # standard error of SII
    SIIse <- 10* deltamethod(~ (exp(x1+x2) / (1+exp(x1+x2)) - (exp(x1) / (1+exp(x1)))), 
                               coefs, vcov)
    
    KMI <- p0/p1
    
    # standard error of the KMI
    KMIse <-  deltamethod(~ (exp(x1+x2) / (1+exp(x1+x2)) / (exp(x1) / (1+exp(x1)))), 
                          coefs, vcov)
    
  }
  

  if(inequaltype=="sii") return(c("sii" = SII, "sii.se" = SIIse))
  if(inequaltype=="kmi") return(c("kmi" = KMI, "kmi.se" = KMIse))
}


sii <- function(dat, bs=FALSE){
  
  dat <- dplyr::arrange(dat, order)
  y<-dat$r
  w<-dat$pop
  se<-dat$se
  maxopt <- unique(dat$maxoptimum)
  rankorder <- dat$order
  indic_num <- dat$indic_category_number[1]
  indic <- dat$indic[1]

  na.result <- list(inequal.sii=NA, se.sii.boot=NA,  se.sii.formula=NA)
  
  if(any(is.na(c(y,w)))) return(na.result)
  if(is.na(maxopt)) return(na.result)
  if(!is.rank(rankorder)) return(na.result)

  # this is different based on stata file from original where we put population
  # as -1 if any are missing
  if(any(is.na(w))) return(na.result)

  if(any(is.na(se))) se <- -1
  
  if(!is.numeric(y) | !is.numeric(w) | !is.numeric(se))stop('This function operates on vector of numbers')

  
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
  
  

  
  inequal.sii <- wrap.sii(y, w, indic, indic_num, maxopt, "sii")
  if(is.na(inequal.sii[["sii"]])) return(na.result)
  
  
  # Bootstrap SE
#   if(bs==T){
#     sii.boot <- c()  # Start with an empty vector of estimated SIIs
#     sii.se.boot <- c()
#     for(i in 1:100){  # Run 200 bootstraps 
#       ny <- rnorm(length(y), y, se)  # create a new set of estimates (y) varying as rnorm(mean=x, sd=se)
#       if(any(is.na(ny))) stop
#       #if(any(ny>100)) next
#       boot.result <- try(wrap.sii(ny, w, indic, indic_num, maxopt, "sii"), silent=TRUE)
#       if(class(boot.result)=="try-error"){
#         cat(paste(i, dat$rec[1], sep="-"), file="d:/junk/abc.txt", append=TRUE, sep="\n")
#         next
#       }
#       sii.boot <- c(sii.boot, boot.result[["sii"]])  # calculate the SII on the new data
#       #sii.se.boot <- c(sii.se.boot, boot.result[["sii.se"]] )
#     } 
#     se.boot <- sd(sii.boot)  # Estimate the standard error of SII as the SD of all the bootstrap SIIs 
#   }else{
#     se.boot <- NA
#   }
#   
  se.boot <- NA
  
  
  se.formula <- inequal.sii[["sii.se"]]
  inequal.sii <- inequal.sii[["sii"]]
  
  # Return the results as a list
  return(list(inequal.sii=inequal.sii, se.sii.boot=se.boot,  se.sii.formula=se.formula))  # return a list of the inequality measure and the standard error 
}
