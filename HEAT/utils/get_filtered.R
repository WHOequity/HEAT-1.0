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

getCountryWHOregionIncome<-function(countryname){

  res <- filter(.rdata[['countrynames']], country==countryname)
  
  return(list(region = res$whoreg6_name, income = res$wbincome))
}



getFilteredCountries <- function(WBgroup=NULL, WHOregion=NULL)
{

  filt_WBgroup   <- TRUE
  filt_WHOregion <- TRUE
  
  
  if(!is.null(WBgroup) && all(WBgroup != "")) 
    filt_WBgroup <- quote(wbincome %in% WBgroup)

  if(!is.null(WHOregion) && all(WHOregion != "")) 
    filt_WHOregion <- quote(whoreg6_name %in% WHOregion)
  
  countries <- filter(.rdata[['countrynames']], filt_WBgroup, filt_WHOregion) %>% 
    select(country) %>% .$country
  


  return(countries)
  
  
}



getFilteredYear <-  function(countryname, datasource="All"){
  # This function filters the Years of surveys based on earlier choices about the Country
  # the Datasource and the Database 
  #datasource <- .rdata[['focus_data_source']]
  #print(paste("in get filtered year", countryname, datasource))
  #if(is.null(datasource)) datasource <- 'All'
  
  filt_country   <- quote(country %in% countryname)
  #filt_source <- TRUE

  #if(datasource != 'All') filt_source <- quote(source%in%datasource)
 
    years <- filter(.rdata[['years']], filt_country) %>%
      arrange(desc(year)) %>% .$year
   
#   years <- filter(.rdata[['years']], filt_country, filt_source) %>%
#     arrange(desc(year)) %>% .$year
  return(years)
  
}


# get_disag <-  function(field, countries, years, source, indic, stratifier){
#   # This function filters the Years of surveys based on earlier choices about the Country
#   # the Datasource and the Database 
#   #datasource <- .rdata[['focus_data_source']]
#   
#   filt_country <- TRUE
#   filt_year <- TRUE
#   filt_indicator <- TRUE
#   filt_dimension <- TRUE
#   #filt_inequaltype<- TRUE
#   
#   if(!is.null(countries)) filt_country <- quote(country %in% countries)
#   if(!is.null(years) && !is.null(mostrecent) && !mostrecent) filt_year <- quote(year %in% years)
#   if(!is.null(years) && is.null(mostrecent)) filt_year <- quote(year %in% years)
#   if(!is.null(indicator)) filt_indicator <- quote(indic %in% indicator)
#   if(!is.null(stratifier)) filt_dimension <- quote(dimension %in% stratifier)
# 
#   if(field=="indic"){
#     res <- filter(.rdata[['maindata']], filt_country, filt_year, filt_dimension, filt_inequaltype) %>%.$indic 
#   }
#   
# 
#   return(res)
#   
# }





