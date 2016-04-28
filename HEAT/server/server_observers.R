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

#*****************************************************************************
#  Observe -- interactions between explore and compare ----
#*****************************************************************************


observe({
  if(is.null(input$focus_country_explore)) return()
  if(input$focus_country_explore == .rdata[['focus_country']]) return()
  
  #print(paste(Sys.time(), "in observer focus_country_explore"))
  
  
  .rdata[['focus_country']] <<- input$focus_country_explore
  
  
  
  country <- isolate(input$focus_country_compare)
  
  selectYears <- getFilteredYear(country=input$focus_country_explore, 
                                 isolate(input$focus_data_source_explore))
  
  .rdata[['all_years']]<<-as.character(selectYears)
  .rdata[['focus_year']]<<-as.character(selectYears)
  #print(paste(Sys.time(), "In the observer country", class(.rdata[['focus_year']] )))
  
  #   print("--------------------")
  #   print(input$focus_country_explore)
  #   print(paste("In explore country observer focus_year is ", .rdata[['focus_year']]))
  
  #if(!is.null(isolate(input$focus_country_explore))){
  updateSelectInput(session, 'focus_year_explore', choices = selectYears, selected = selectYears)
  #}
  
  
  if(!is.null(country) && country!=.rdata[['focus_country']]){
    updateSelectInput(session, 'focus_country_compare', selected = .rdata[['focus_country']])
    updateSelectInput(session, 'focus_year_compare', choices = selectYears, selected = selectYears[1])
  }
  
  
  
  focus_country <- .rdata[['focus_country']]
  country_info<-getCountryWHOregionIncome(focus_country)
  
  .rdata[['focus_income_group']] <<- country_info$income
  .rdata[['focus_who_regions']] <<- country_info$region
  
  updateSelectInput(session, "benchmarkWHOregion", selected = country_info$region)
  updateSelectInput(session, "benchmarkWBgroup", selected = country_info$income)
  
  
  country_choices<-getFilteredCountries(country_info$income, country_info$region)
  country_choices <- country_choices[!country_choices%in%focus_country]
  
  .rdata[['benchmark_countries']] <<- country_choices
  #updateSelectInput(session, "benchmark_countries", choices = country_choices, selected=country_choices)
  

  
  
})



observe({
  
  if(is.null(input$focus_country_compare)) return()
  if(input$focus_country_compare == .rdata[['focus_country']]) return()
  #print(paste(Sys.time(), input$focus_country_compare))
  .rdata[['focus_country']] <<- input$focus_country_compare
  
  selectYears <- getFilteredYear(country=input$focus_country_compare, 
                                 isolate(input$focus_data_source_compare))
  
  .rdata[['all_years']]<<-as.character(selectYears)
  .rdata[['focus_year']]<<-as.character(selectYears)
  
  #   print("--------------------")
  #   print(input$focus_country_compare)
  #   print(paste("In compare country observer focus_year is ", .rdata[['focus_year']], "and country is", input$focus_country_compare))
  #   
  
  #if(!is.null(isolate(input$focus_country_compare))){
  updateSelectInput(session, 'focus_year_compare', choices = selectYears, selected = selectYears[1])
  #}
  
  
  country <- isolate(input$focus_country_explore)
  
  if(!is.null(country) && country!=.rdata[['focus_country']]){
    updateSelectInput(session, 'focus_country_explore', selected = .rdata[['focus_country']])
    updateSelectInput(session, 'focus_year_explore', choices = selectYears, selected = selectYears[1])
  }
  
  # update the who region and income groups
  
  
  focus_country <- .rdata[['focus_country']]
  country_info<-getCountryWHOregionIncome(focus_country)
  
  
  .rdata[['focus_income_group']] <<- country_info$income
  .rdata[['focus_who_regions']] <<- country_info$region
  
  updateSelectInput(session, "benchmarkWHOregion", selected = country_info$region)
  updateSelectInput(session, "benchmarkWBgroup", selected = country_info$income)
  
  
  country_choices<-getFilteredCountries(country_info$income, country_info$region)
  
  country_choices <- country_choices[!country_choices%in%focus_country]
  
  .rdata[['benchmark_countries']] <<- country_choices
  
  updateSelectInput(session, "benchmark_countries", choices = country_choices, selected=country_choices)
  
  
})



#****************** Watch year_explore

observe({
  if(is.null(input$focus_year_explore)) return()
  if(identical(input$focus_year_explore, .rdata[['focus_year']])) return()
  
  
  .rdata[['focus_year']] <<- input$focus_year_explore
  
  #print(paste(Sys.time(), "In the observer inpu$", class(.rdata[['focus_year']] )))
  
  year <- isolate(input$focus_year_compare)
  
  #print("--------------------")
  #print(paste("In explore year observer focus_year is ", .rdata[['focus_year']], "and compare is", year))
  
  if(is.null(year)) return()
  if(year == .rdata[['focus_year']][1]) return()
  
  #print("In explore observer after equality test")
  updateSelectInput(session, 'focus_year_compare', selected = .rdata[['focus_year']])
  
  
  
}, priority = 1)






#****************** Watch year_compare

observe({
  if(is.null(input$focus_year_compare)) return()
  if(input$focus_year_compare == .rdata[['focus_year']][1]) return()
  
  .rdata[['focus_year']] <<- input$focus_year_compare
  
  
  year <- isolate(input$focus_year_explore)
  
  #print("--------------------")
  #print(paste("In compare year observer focus_year is ", .rdata[['focus_year']], "and explore is", year))
  
  if(is.null(year)) return()
  if(identical(year, .rdata[['focus_year']])) return()
  
  updateSelectInput(session, 'focus_year_explore', selected = .rdata[['focus_year']])
  
})


#****************** Watch mostrecent_explore

observe({
  if(is.null(input$mostrecent_explore)) return()
  .rdata[['mostrecent']] <<- input$mostrecent_explore
  
  #selectYears <- getFilteredYear(country=isolate(input$focus_country_explore))
  
  #.rdata[['focus_year']]<<-selectYears[1]
  
  # print("--------------------")
  #print(paste("mostrecent explore focus year", .rdata[['focus_year']]))
  
  mostrecent <- isolate(input$mostrecent_compare)
  
  
  if(!is.null(mostrecent) && mostrecent!=.rdata[['mostrecent']]){
    updateCheckboxInput(session,'mostrecent_compare', value=.rdata[['mostrecent']])
    #updateSelectInput(session, 'focus_year_explore', selected = .rdata[['focus_year']])
    #updateSelectInput(session, 'focus_year_compare', selected = .rdata[['focus_year']])
  }
  
  
  
  
})


#****************** Watch mostrecent_compare

observe({
  
  
  if(is.null(input$mostrecent_compare)) return()
  if(is.null(isolate(input$focus_country_compare))) return()
  
  
  .rdata[['mostrecent']] <<- input$mostrecent_compare
  
  
  # selectYears <- getFilteredYear(country=isolate(input$focus_country_compare))
  
  # if no years match the criteria return nothing and do nothing 
  #if(length(selectYears)==0) return()
  
  #.rdata[['focus_year']]<<-selectYears[1]
  
  #print("--------------------")
  #print(paste("mostrecent compare focus year", .rdata[['focus_year']]))
  
  
  mostrecent <- isolate(input$mostrecent_explore)
  
  
  if(!is.null(mostrecent) && mostrecent!=.rdata[['mostrecent']]){
    updateCheckboxInput(session,'mostrecent_explore', value=.rdata[['mostrecent']])
    #     updateSelectInput(session, 'focus_year_explore', selected = .rdata[['focus_year']])
    #     updateSelectInput(session, 'focus_year_compare', selected = .rdata[['focus_year']])
  }
  
  
  
  
})




observe({
  #   if(is.null(input$focus_indicator_explore)){
  #     #print("in here")
  #     .rdata[['focus_indicator']] <<- NULL
  #     updateSelectInput(session, 'focus_indicator_compare', selected = NULL)
  #     return()
  #   }
  .rdata[['focus_indicator']] <<- input$focus_indicator_explore
  
  
  indicator <- isolate(input$focus_indicator_compare)
  if(is.null(indicator))return()
  #if(!is.null(indicator) && indicator!=.rdata[['focus_indicator']]){
  
  updateSelectInput(session, 'focus_indicator_compare', selected = .rdata[['focus_indicator']][1])
  # }
  
})



observe({
  #   if(is.null(input$focus_indicator_compare)){
  #     #return()
  #     updateSelectInput(session, 'focus_indicator_explore', selected = NULL)
  #     return()
  #   }
  .rdata[['focus_indicator']] <<- input$focus_indicator_compare
  
  
  indicator <- isolate(input$focus_indicator_explore)
  
  if(!is.null(indicator)  && !identical(indicator, .rdata[['focus_indicator']])){
    
    updateSelectInput(session, 'focus_indicator_explore', selected = .rdata[['focus_indicator']])
  }
  
})




observe({
  if(is.null(input$focus_dimension_explore)) return()
  .rdata[['focus_dimension']] <<- input$focus_dimension_explore
  
  
  dimension <- isolate(input$focus_dimension_compare)
  
  if(!is.null(dimension) && dimension!=.rdata[['focus_dimension']]){
    
    updateSelectInput(session, 'focus_dimension_compare', selected = .rdata[['focus_dimension']])
  }
  
})



observe({
  if(is.null(input$focus_dimension_compare)) return()
  .rdata[['focus_dimension']] <<- input$focus_dimension_compare
  
  
  dimension <- isolate(input$focus_dimension_explore)
  
  if(!is.null(dimension) && dimension!=.rdata[['focus_dimension']]){
    
    updateSelectInput(session, 'focus_dimension_explore', selected = .rdata[['focus_dimension']])
  }
  
})



observe({
  if(is.null(input$benchmark_countries)) return()
  
  .rdata[['benchmark_countries']] <<- input$benchmark_countries
  

  
}, priority = 1)



# observe({
#   
#   #if(is.null(input$benchmarkWBgroup) || input$benchmarkWBgroup == "") return()
#   tmpCountries<-getFilteredCountries(input$benchmarkWBgroup, isolate(input$benchmarkWHOregion))
#   tmpCountries <- append(.rdata[['benchmark_countries']], tmpCountries)
#   #print("in observe")
#   #bench <- .rdata[['benchmark_countries']]
#   #.rdata[['benchmark_countries']] <<- bench[bench%in%tmpCountries]
#   
#   #updateSelectInput(session, "benchmark_countries", choices = tmpCountries, selected = .rdata[['benchmark_countries']])
#   updateSelectInput(session, "benchmark_countries", choices = tmpCountries, selected=.rdata[['benchmark_countries']])
#   
# })



observe({

  #if(is.null(input$focus_country_compare)) return()
  income_group <-  input$benchmarkWBgroup
  who_region <- input$benchmarkWHOregion
  

  tmpCountries<-getFilteredCountries(income_group, who_region)
  
  # this is a careful balancing act. I set the first_time variable
  # in initial setting to TRUE and I do NOT set it to false on this
  # run. Instead, I also put this in the server logic where it
  # is set to FALSE
  if(.rdata[['first_time']]){
    return()
  }

  
  .rdata[['focus_income_group']] <<- income_group
  .rdata[['focus_who_regions']] <<- who_region
  #print(paste(Sys.time(),"observer", .rdata[['focus_income_group']]))
  
  
  focus_country <- .rdata[['focus_country']]
  tmpCountries <- tmpCountries[!tmpCountries%in%focus_country]
  
  if(length(tmpCountries)!=0){
    
    tmpCountries <- append(.rdata[['benchmark_countries']], tmpCountries)
    choices <- tmpCountries
    selected <- .rdata[['benchmark_countries']]
    #.rdata[['country_blank']] <<- FALSE
  }
  
  
  if(length(tmpCountries)==0){
    
    
    choices <- " "
    selected <- " "
    #.rdata[['country_blank']] <<- TRUE
  }
  

  
  updateSelectInput(session, "benchmark_countries", choices = choices, selected= selected)
  
  #rm(choices, selected)
})




# observe({
# 
#   if(is.null(input$focus_country_compare)) return()
#   
#   .rdata[['benchmark_countries']]<<-input$benchmark_countries
#   #print(.rdata[['benchmark_countries']])
#   
# })
# 
# 




#****************** Watch focus_data_source_explore

observe({
  if(is.null(input$focus_data_source_explore)) return()
  .rdata[['focus_data_source']] <<- input$focus_data_source_explore
  
  focus_data_source <- isolate(input$focus_data_source_compare)
  
  
  if(!is.null(focus_data_source) && focus_data_source!=.rdata[['focus_data_source']]){
    updateCheckboxInput(session,'focus_data_source_compare', value=.rdata[['focus_data_source']])
  }
  
  
})







#****************** Watch focus_data_source_compare

observe({
  if(is.null(input$focus_data_source_compare)) return()
  .rdata[['focus_data_source']] <<- input$focus_data_source_compare
  
  focus_data_source <- isolate(input$focus_data_source_explore)
  
  
  if(!is.null(focus_data_source) && focus_data_source!=.rdata[['focus_data_source']]){
    updateCheckboxInput(session,'focus_data_source_explore', value=.rdata[['focus_data_source']])
  }
  
  
})


#****************** Watch focus_inequal_type_explore

observe({
  if(is.null(input$focus_inequal_type_explore_table)) return()
  .rdata[['focus_inequal_type']] <<- input$focus_inequal_type_explore_table
  
  focus_inequal_type <- isolate(input$focus_inequal_type_compare)
  
  
  if(!is.null(focus_inequal_type) && focus_inequal_type!=.rdata[['focus_inequal_type']]){
    updateCheckboxInput(session,'focus_inequal_type_compare', value=.rdata[['focus_inequal_type']][1])
  }
  
  focus_inequal_type2 <- isolate(input$focus_inequal_type_explore_plot)
  
  if(!is.null(focus_inequal_type2) && focus_inequal_type2!=.rdata[['focus_inequal_type']]){
    updateCheckboxInput(session,'focus_inequal_type_explore_plot', value=.rdata[['focus_inequal_type']][1])
  }
  
  
})


#****************** Watch focus_inequal_type_explore_plot

observe({
  if(is.null(input$focus_inequal_type_explore_plot)) return()
  .rdata[['focus_inequal_type']] <<- input$focus_inequal_type_explore_plot
  
  focus_inequal_type <- isolate(input$focus_inequal_type_compare)
  
  
  if(!is.null(focus_inequal_type) && focus_inequal_type!=.rdata[['focus_inequal_type']]){
    updateCheckboxInput(session,'focus_inequal_type_compare', value=.rdata[['focus_inequal_type']][1])
  }
  
  focus_inequal_type2 <- isolate(input$focus_inequal_type_explore_table)
  
  if(!is.null(focus_inequal_type2) && focus_inequal_type2!=.rdata[['focus_inequal_type']]){
    updateCheckboxInput(session,'focus_inequal_type_explore_table', value=.rdata[['focus_inequal_type']][1])
  }
  
  
})




#****************** Watch focus_inequal_type_compare

observe({
  if(is.null(input$focus_inequal_type_compare)) return()
  .rdata[['focus_inequal_type']] <<- input$focus_inequal_type_compare
  
  focus_inequal_type <- isolate(input$focus_inequal_type_explore)
  
  
  if(!is.null(focus_inequal_type) && focus_inequal_type!=.rdata[['focus_inequal_type']]){
    updateCheckboxInput(session,'focus_inequal_type_explore', value=.rdata[['focus_inequal_type']])
  }
  
  
})


observe({
  if(is.null(input$dataTableItems)) return()
  
  .rdata[['focus_table_variables']] <<- input$dataTableItems
  
  
  
})




#****************** Titles

observe({
  
  if(is.null(input$main_title1)) return()
  #print(paste(Sys.time(), "in observer for input$main_title1"))
  .rdata[["plotDisag_explore_title"]] <<- input$main_title1
  
}, priority = 1)



observe({
  #print(paste(Sys.time(), "in observer for input$main_title2"))
  if(is.null(input$main_title2)) return()
  .rdata[["plotSummary_explore_title"]] <<- input$main_title2
  
}, priority = 1)



observe({
  
  #print(paste(Sys.time(), "in observer for input$main_title3"))
  if(is.null(input$main_title3)) return()
  .rdata[["plotDisag_compare_title"]] <<- input$main_title3
  
}, priority = 1)

observeEvent(input$main_title4, {
  
  .rdata[["plotSummary_compare_title"]] <<- input$main_title4
  
}, priority = 1)




