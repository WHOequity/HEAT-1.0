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

######### All the measures of inequality
# This manages the return of the inequalities measures inequality measures
#########



getInequalData <- function(indicator = NULL, stratifier = NULL, countries = NULL, years = NULL,
                       mostrecent=NULL, datasource=NULL,  inequal_types=NULL, multiplier1=TRUE,multiplier2=TRUE, 
                       elasticity=NULL, anchorCountry = NULL){


  if(!is.null(mostrecent) && mostrecent){
    
    # for selecting the year for benchmark countries we want to use the year
    # from the anchor country
    tmpCountry <- ifelse(!is.null(anchorCountry), anchorCountry, countries[1])
    years <- getFilteredYear(country=tmpCountry, datasource)[1]
  }
  
  if(!is.null(elasticity) && !is.null(years)){
    # if mostrecent is TRUE then we want the focus_year, otherwise
    # we can use the input year
    focus_year <- years
    years <- as.integer(years)
    years<-seq(years-elasticity, years+elasticity)
    #print(paste("inequal year", paste(years, collapse=",")))
    
  }
  
  #print(inequal_types)
  filt_country <- TRUE
  filt_year <- TRUE
  filt_indicator <- TRUE
  filt_dimension <- TRUE
  filt_inequaltype<- TRUE
  filt_datasource <- TRUE
  
  if(!is.null(countries)) filt_country <- quote(country %in% countries)
  if(!is.null(years)) filt_year <- quote(year %in% years)
  #if(!is.null(years) && is.null(mostrecent)) filt_year <- quote(year %in% years)
  if(!is.null(indicator)) filt_indicator <- quote(indic %in% indicator)
  if(!is.null(stratifier)) filt_dimension <- quote(dimension %in% stratifier)
  if(!is.null(inequal_types)) filt_inequaltype <- quote(measure %in% inequal_types)
  if(!is.null(datasource) && datasource == 'All') filt_datasource <- TRUE
  if(!is.null(datasource) && datasource != 'All') filt_datasource <- quote(source == datasource)
  
  #     print(paste0('filt_country:', deparse(filt_country)))
  #     print(paste0('filt_year:', deparse(filt_year)))
  #     print(paste0('filt_indicator:', deparse(filt_indicator)))
  #     print(paste0('filt_dimension:', deparse(filt_dimension)))
  #     
  #     print(paste0('country:', countries))
  #     print(paste0('years:', years))
  #     print(paste0('indicator:', indicator))
  #     print(paste0('dimension:', stratifier))
  
  ineqDF <- filter(.rdata[['inequals']], filt_country)
  
  ineqDF <- filter(ineqDF, filt_year, filt_indicator, 
                   filt_dimension, filt_inequaltype, filt_datasource) %>% 
    select(country, year, indic, dimension,source, measure, inequal, boot.se, se, ccode, indic_name)

  #   if(is.null(ineqDF)){
  #     return()
  #   }
  
  ineqDF$year <- as.integer(ineqDF$year)
  ineqDF$se <- as.numeric(ineqDF$se)
  ineqDF$se[ineqDF$se == 0] <- NA
  ineqDF$boot.se <- as.numeric(ineqDF$boot.se)
  ineqDF$boot.se[ineqDF$boot.se==0] <- NA

  #ineqDF$combo.se <- ineqDF$se
  #ineqDF$combo.se[is.na(ineqDF$se)] <- ineqDF$boot.se[is.na(ineqDF$se)]
  
  ineqDF$se.lowerci <- ineqDF$inequal - (1.96 * ineqDF$se) 
  ineqDF$se.upperci <- ineqDF$inequal + (1.96 * ineqDF$se) 
  ineqDF$boot.lowerci <- ineqDF$inequal - (1.96 * ineqDF$boot.se) 
  ineqDF$boot.upperci <- ineqDF$inequal + (1.96 * ineqDF$boot.se) 


  
  # special rule for ineqDF rr and riikm only
  special<-which(ineqDF$measure%in%c("rr", "riikm"))
  ineqDF$se.lowerci[special] <- exp (log(ineqDF$inequal[special]) - (1.96 * ineqDF$se[special]/ineqDF$inequal[special]) )
  ineqDF$se.upperci[special] <- exp (log(ineqDF$inequal[special]) + (1.96 * ineqDF$se[special]/ineqDF$inequal[special]) )
  ineqDF$boot.lowerci[special] <-exp (log(ineqDF$inequal[special]) - (1.96 * ineqDF$boot.se[special]/ineqDF$inequal[special]) )
  ineqDF$boot.upperci[special] <- exp (log(ineqDF$inequal[special]) + (1.96 * ineqDF$boot.se[special]/ineqDF$inequal[special]) )

  
  #ineqDF$combo.lowerci <- ineqDF$inequal - (1.96 * ineqDF$combo.se) 
  #ineqDF$combo.upperci <- ineqDF$inequal + (1.96 * ineqDF$combo.se) 
  
  
  
  #ineqDF$combo.se[is.na(ineqDF$combo.se)] <- ineqDF$boot.se[is.na(ineqDF$combo.se)]  #  Make an se that is analytic if it exists, otherwise a boostrap
  #print("In getInequal function b")

  
  if(!is.null(multiplier1) && multiplier1){
    #print(paste("first", Sys.time()))
    #print(head(ineqDF))

    ineqDF$inequal[ineqDF$measure=='ti'] <- ineqDF$inequal[ineqDF$measure=='ti'] *1000
    ineqDF$inequal[ineqDF$measure=='mld'] <- ineqDF$inequal[ineqDF$measure=='mld'] *1000
    ineqDF$se[ineqDF$measure=='ti'] <- ineqDF$se[ineqDF$measure=='ti'] *1000
    ineqDF$se[ineqDF$measure=='mld'] <- ineqDF$se[ineqDF$measure=='mld'] *1000
    ineqDF$se.lowerci[ineqDF$measure=='ti'] <- ineqDF$se.lowerci[ineqDF$measure=='ti'] *1000
    ineqDF$se.lowerci[ineqDF$measure=='mld'] <- ineqDF$se.lowerci[ineqDF$measure=='mld'] *1000
    ineqDF$se.upperci[ineqDF$measure=='ti'] <- ineqDF$se.upperci[ineqDF$measure=='ti'] *1000
    ineqDF$se.upperci[ineqDF$measure=='mld'] <- ineqDF$se.upperci[ineqDF$measure=='mld'] *1000
    ineqDF$boot.se[ineqDF$measure=='ti'] <- ineqDF$boot.se[ineqDF$measure=='ti'] *1000
    ineqDF$boot.se[ineqDF$measure=='mld'] <- ineqDF$boot.se[ineqDF$measure=='mld'] *1000
    ineqDF$boot.lowerci[ineqDF$measure=='ti'] <- ineqDF$boot.lowerci[ineqDF$measure=='ti'] *1000
    ineqDF$boot.lowerci[ineqDF$measure=='mld'] <- ineqDF$boot.lowerci[ineqDF$measure=='mld'] *1000
    ineqDF$boot.upperci[ineqDF$measure=='ti'] <- ineqDF$boot.upperci[ineqDF$measure=='ti'] *1000
    ineqDF$boot.upperci[ineqDF$measure=='mld'] <- ineqDF$boot.upperci[ineqDF$measure=='mld'] *1000
#     ineqDF$combo.se[ineqDF$measure=='ti'] <- ineqDF$combo.se[ineqDF$measure=='ti'] *1000
#     ineqDF$combo.se[ineqDF$measure=='mld'] <- ineqDF$combo.se[ineqDF$measure=='mld'] *1000
#     ineqDF$combo.lowerci[ineqDF$measure=='ti'] <- ineqDF$combo.lowerci[ineqDF$measure=='ti'] *1000
#     ineqDF$combo.upperci[ineqDF$measure=='mld'] <- ineqDF$combo.upperci[ineqDF$measure=='mld'] *1000
#     ineqDF$combo.lowerci[ineqDF$measure=='ti'] <- ineqDF$combo.lowerci[ineqDF$measure=='ti'] *1000
#     ineqDF$combo.upperci[ineqDF$measure=='mld'] <- ineqDF$combo.upperci[ineqDF$measure=='mld'] *1000
    #print(paste("second", Sys.time()))
    #print(head(ineqDF))
    
  }
  
  if(!is.null(multiplier2) && multiplier2){
    #print("In dataTableInequal b")
    ineqDF$inequal[ineqDF$measure=='rci'] <- ineqDF$inequal[ineqDF$measure=='rci'] *100
    ineqDF$se[ineqDF$measure=='rci'] <- ineqDF$se[ineqDF$measure=='rci'] *100
    ineqDF$se.lowerci[ineqDF$measure=='rci'] <- ineqDF$se.lowerci[ineqDF$measure=='rci'] *100
    ineqDF$se.upperci[ineqDF$measure=='rci'] <- ineqDF$se.upperci[ineqDF$measure=='rci'] *100
    ineqDF$boot.se[ineqDF$measure=='rci'] <- ineqDF$boot.se[ineqDF$measure=='rci'] *100
    ineqDF$boot.lowerci[ineqDF$measure=='rci'] <- ineqDF$boot.lowerci[ineqDF$measure=='rci'] *100
    ineqDF$boot.upperci[ineqDF$measure=='rci'] <- ineqDF$boot.upperci[ineqDF$measure=='rci'] *100
#     ineqDF$combo.se[ineqDF$measure=='rci'] <- ineqDF$combo.se[ineqDF$measure=='rci'] *100
#     ineqDF$combo.lowerci[ineqDF$measure=='rci'] <- ineqDF$combo.lowerci[ineqDF$measure=='rci'] *100
#     ineqDF$combo.upperci[ineqDF$measure=='rci'] <- ineqDF$combo.upperci[ineqDF$measure=='rci'] *100
  }
  

  
  if(!is.null(elasticity)){
    
#     maxyear <- group_by(ineqDF, country) %>% 
#       summarise(maxyr = max(year))
    
    closestyr <- group_by(ineqDF, country) %>% 
      summarise(closestyr = closest_year(focus_year, year))
    
    ineqDF  <- semi_join( ineqDF , closestyr, by=c("country", "year" = "closestyr"))
  
    #ineqDF <- semi_join(ineqDF, maxyear,  by=c("country", "year" = "maxyr"))
    
  }
  
  
#   natdata <- filter(.rdata[['nationaldata']], country == i, year == elastic_years, indic==indicator) %>% 
#     select(country, year, indic, r)
  
  nationaldata <- select(.rdata[['nationaldata']], country, year, source, indic, r)
  
  #!!!!! You need source!!!! but inequal does not have it yet
  ineqDF <- left_join(ineqDF, nationaldata, by=c('country', 'year', 'source', 'indic'))
  #ineqDF <- left_join(ineqDF, select(nationaldata, -source), by=c('country', 'year', 'indic'))
  
  
  ineqDF <- rename(ineqDF, estimate = r)
  

  ineqDF <- inner_join(ineqDF, .rdata[['summary_measures_table']], 
                        by=c("measure"="measure_abbr"))
  
  return(ineqDF)
}





getDisagData <- function(indicator = NULL, stratifier = NULL, countries = NULL, 
                         years = NULL, mostrecent=FALSE, datasource=NULL, 
                         elasticity=NULL, anchor_country = NULL){
  

  
if(!is.null(mostrecent) && mostrecent){
  
  tmpcountry <- countries[1]
  if(!is.null(anchor_country)) tmpcountry <- anchor_country
  years <- getFilteredYear(country=tmpcountry, datasource)[1]
  #print(paste("disagdata year", paste(years, collapse=",")))
}
  

  
  if(!is.null(elasticity) && !is.null(years)){
    # if mostrecent is TRUE then we want the focus_year, otherwise
    # we can use the input year
    focus_year <- years
    years <- as.integer(years)
    years<-seq(years-elasticity, years+elasticity)
    #print(paste("disagdata elasticity", paste(years, collapse=",")))

    
  }
  
  
  
  filt_country <- TRUE
  filt_year <- TRUE
  filt_indicator <- TRUE
  filt_dimension <- TRUE
  filt_datasource <- TRUE
  
  
  
    if(!is.null(countries)) filt_country <- quote(country %in% countries)
    if(!is.null(years)) filt_year <- quote(year %in% years)
    #if(!is.null(years) && is.null(mostrecent)) filt_year <- quote(year %in% years)
    if(!is.null(indicator)) filt_indicator <- quote(indic %in% indicator)
    if(!is.null(stratifier)) filt_dimension <- quote(dimension %in% stratifier)
    if(!is.null(datasource) && datasource == 'All') filt_datasource <- TRUE
    if(!is.null(datasource) && datasource != 'All') filt_datasource <- quote(source == datasource)
    #if(!is.null(mostrecent) && mostrecent) filt_year <- TRUE
  
  # if you use this you need to use filter_ below ALSO the quote in Mother's education causes issues
#   if(!is.null(countries)) filt_country <- paste0("country %in%c('", paste0(countries, collapse="','"), "')")
#   if(!is.null(years) && !is.null(mostrecent) && !mostrecent) filt_year <- paste0("year %in%c('", paste0(years, collapse="','"), "')")
#   if(!is.null(years) && is.null(mostrecent)) filt_year <- paste0("year %in%c('", paste0(years, collapse="','"), "')")
#   if(!is.null(indicator)) filt_indicator <- paste0("indic %in%c('", paste0(indicator, collapse="','"), "')")
#   if(!is.null(stratifier)) filt_dimension <- paste0("dimension %in%c('", paste0(stratifier, collapse="','"), "')")
#   if(!is.null(datasource) && datasource == 'All') filt_datasource <- TRUE
#   if(!is.null(datasource) && datasource != 'All') filt_datasource <- paste0("source %in%c('", paste0(datasource, collapse="','"), "')")
#   
#   
  #     print(paste0('filt_country:', deparse(filt_country)))
  #     print(paste0('filt_year:', deparse(filt_year)))
  #     print(paste0('filt_indicator:', deparse(filt_indicator)))
  #     print(paste0('filt_dimension:', deparse(filt_dimension)))
  #     
  #     print(paste0('country:', countries))
  #     print(paste0('years:', years))
  #     print(paste0('indicator:', indicator))
  #     print(paste0('dimension:', stratifier))

  hetk.data <- filter(.rdata[['maindata']], filt_country)
  
  
  hetk.data <- filter(hetk.data, filt_year, filt_indicator, filt_dimension, filt_datasource) %>% 
    select(country, year, source, indic, dimension, subgroup, r, r_lower, r_upper, se, pop, iso3, 
           rankable, maxoptimum, popshare, flag, order, indic_name)
  
  
  hetk.data <- arrange(hetk.data, dimension, order)
  hetk.data$subgroup <- factor(hetk.data$subgroup, levels=unique(hetk.data$subgroup))
  
#print(head(hetk.data))
  
  
  
  #national.data <- dbGetQuery(con, selectNationalStr)
  
  # in original function getHETK there was a query and filter of national data but 
  # I'm not sure why this would be necessary if we do an inner join.
  
  nationaldata <- select(.rdata[['nationaldata']], country, year, source, indic, r)
  
  
  hetk.data <- inner_join(hetk.data, nationaldata, by=c('country', 'year', 'source', 'indic')) %>% 
    rename(estimate = r.x, national=r.y, lower_95ci=r_lower, upper_95ci=r_upper)
  
  
  #hetk.data <- filter(hetk.data, !is.null(estimate), !is.na(estimate), estimate!="")
  
  
  hetk.data$year <- as.integer(hetk.data$year)
  hetk.data$estimate <- as.numeric(hetk.data$estimate)
  hetk.data$se <- as.numeric(hetk.data$se)
  hetk.data$pop <- as.integer(hetk.data$pop)
  hetk.data$lower_95ci <- as.numeric(hetk.data$lower_95ci)
  hetk.data$upper_95ci <- as.numeric(hetk.data$upper_95ci)
  hetk.data$rankable <- as.integer(hetk.data$rankable)
  #names(hetk.data)[which(names(hetk.data)=='r')] <- 'estimate'
  #names(hetk.data)[which(names(hetk.data)=='r_lower')] <- 'lower_95ci'
  #names(hetk.data)[which(names(hetk.data)=='r_upper')] <- 'upper_95ci'
  
  
#   if(!is.null(mostrecent) && mostrecent){
#     
#     maxyear <- group_by(hetk.data, country) %>% 
#       summarise(maxyr = max(year))
#     
#     hetk.data <- semi_join(hetk.data, maxyear, by=c("year" = "maxyr"))
#     
#   }
#   
  
  
  if(!is.null(elasticity)){
  

#     maxyear <- group_by(hetk.data, country) %>% 
#       summarise(maxyr = max(year))
    
    closestyr <- group_by(hetk.data, country) %>% 
            summarise(closestyr = closest_year(focus_year, year))
    
    hetk.data <- semi_join(hetk.data, closestyr, by=c("country", "year" = "closestyr"))
    
  }
  
  hetk.data <- arrange(hetk.data, year, subgroup)
  
  return(hetk.data)
}





closest_year <- function(focus, x){
  focus<-as.numeric(focus)
  x<-as.numeric(x)
  diff <- abs(x-focus)
  mins<-which(diff==min(diff))
  
  if(length(mins)==1) return(x[mins])
  
  if(all(diff[mins]==0)) return(x[mins[1]])
  
  return(max(x[mins]))
  
}







