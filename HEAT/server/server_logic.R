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

#******************************************************************************
#******************************************************************************
# FUNCTIONS TO CREATE SELECTORS
#******************************************************************************
#******************************************************************************


focusCountry_selector <- function(id, multiple=FALSE){
  
  if(is.null(.rdata[['all_countries']])) return()
  
  countries <- .rdata[['all_countries']]
  
  selectInput(id, 
              "Select country", 
              choices  = countries, 
              multiple = multiple, 
              selected = .rdata[['focus_country']])
}



focusIndicator_selector <- function(id, multiple = FALSE, core=FALSE){
  
  indic<-ifelse(is.null(.rdata[['focus_indicator']]), "sba", .rdata[['focus_indicator']])
  
  
  selectInput(id, 
              "Select health indicators", 
              choices  = .rdata[['full_indicators']], 
              multiple = multiple, 
              selected = indic)
  
  
}


focusInequalType_selector <- function(id, multiple=FALSE){
  
  
  
  selectInput(id, 
              "Select summary measure", 
              choices= .rdata[['summary_measures_all']], 
              selected=.rdata[["focus_inequal_type"]], 
              multiple=multiple)
  
  
}


focusDimension_selector <- function(id, multiple = FALSE){
  
  
  ifelse(multiple, focus_dimen <- .rdata[['focus_dimension']], focus_dimen <- .rdata[['focus_dimension']][1])
  
  selectInput(inputId = id,
              "Select inequality dimensions",
              choices = .rdata[['equity_dimensions']],
              selected = focus_dimen,
              multiple=multiple,
              selectize=TRUE)
  
  
}


#******************************************************************************
#******************************************************************************
# CREATE SELECTORS, SLIDERS AND BUTTONS ---- 
#******************************************************************************
#******************************************************************************


# ----- Country selector -----------------------------------------------

output$focus_country_explore <- renderUI({
  
  focusCountry_selector("focus_country_explore")
  
})

# ----- Year and source selector -----------------------------------------------


output$focus_source_year_explore <- renderUI({
  
  list(
    #conditionalPanel(condition = "input.assessment_panel == 'datatable' | input.assessment_panel == 'dataplot'",
    radioButtons("focus_data_source_explore", "Select data source",
                 choices = .rdata[['data_sources']], #c("All", "DHS", "MICS"),
                 inline=TRUE,
                 selected=.rdata[['focus_data_source']]),
    # ),
    tags$span(class="control-label", "Select years"),
    checkboxInput('mostrecent_explore', 'Most recent year', .rdata[['mostrecent']]),
    
    conditionalPanel( condition = "!input.mostrecent_explore",  
                      
                      selectInput(inputId="focus_year_explore", 
                                  label=NA, 
                                  choices=.rdata[['all_years']], 
                                  multiple=T, 
                                  selected=.rdata[['focus_year']])
    )
  )
})


# ----- Indicator selector -----------------------------------------------


output$focus_indicator_explore <- renderUI({
  
  focusIndicator_selector("focus_indicator_explore", multiple=TRUE, core=FALSE)
  
  
})


# ----- Dimension selector -----------------------------------------------

output$focus_dimension_explore <- renderUI({
  #print(paste0("inputdatatable:", input$assessment_panel))
  focusDimension_selector("focus_dimension_explore", multiple=TRUE)
  
  
})


# ----- Variable selector -----------------------------------------------



output$dataTableItems_explore <- renderUI({
  
  list(
    
    selectInput(inputId = "dataTableItems",
                "Select table content",
                choices = .rdata[['all_table_variables']]$table_vars,
                selected = .rdata[['focus_table_variables']],
                multiple=TRUE)
  )
  
})


# ----- Variable selector -----------------------------------------------


output$dataTableItemsSummary_explore <- renderUI({
  
  
  list(
    
    selectInput(inputId = "dataTableItemsSummary",
                "Select table content",
                choices = .rdata[['all_table_variables_summary']]$table_vars,
                selected = .rdata[['focus_table_variables_summary']],
                multiple=TRUE)
  )
  
})


# ----- Plot type -----------------------------------------------


output$disag_plot_type <- renderUI({
  
  radioButtons("ai_plot_type", "Select graph type",
               c("Bar graph" = "data_bar",
                 "Line graph" = "data_line"),
               inline=T,
               selected="data_line")
  
  
})


# ----- Disaggregated error bars -----------------------------------------------

output$disag_plot_error_bars <- renderUI({
  checkboxInput('disag_error_bars', 'Include 95% confidence interval', FALSE)
})


# ----- Plot dimensions -----------------------------------------------


output$disag_plot_dimensions_explore <- renderUI({
  
  list(
    sliderInput('plot_height1', 'Select graph height', min=200, max=1500, value=650, step = 100,
                round = T,
                ticks = TRUE, animate = FALSE),
    
    sliderInput('plot_width1', 'Select graph width', min=200, max=1500, value=650, step = 100,
                round = T,
                ticks = TRUE, animate = FALSE)
  )
  
})


# ----- Plot dimensions -----------------------------------------------

output$summary_plot_dimensions_explore <- renderUI({
  
  list(
    sliderInput('plot_height2', 'Select graph height', min=200, max=1500, value=650, step = 100,
                round = T,
                ticks = TRUE, animate = FALSE),
    
    sliderInput('plot_width2', 'Select graph width', min=200, max=1500, value=650, step = 100,
                round = T,
                ticks = TRUE, animate = FALSE)
  )
  
})


# ----- Plot dimensions -----------------------------------------------


# These sliders were not showing up in Firefox (speed issue?) so I moved directly
# to the compare_inequality.R script
# output$summary_plot_dimensions_compare <- renderUI({
#   
#   list(
#     sliderInput('plot_height3', 'Select graph height', min=200, max=1500, value=650, step = 100,
#                 round = T,
#                 ticks = TRUE, animate = FALSE),
#     
#     sliderInput('plot_width3', 'Select graph width', min=200, max=1500, value=650, step = 100,
#                 round = T,
#                 ticks = TRUE, animate = FALSE)
#   )
#   
# })





# ----- Plot dimensions -----------------------------------------------

output$summary_plot_dimensions_compare <- renderUI({
  
  list(
    sliderInput('plot_height4', 'Select graph height', min=200, max=1500, value=650, step = 100,
                round = T,
                ticks = TRUE, animate = FALSE),
    
    sliderInput('plot_width4', 'Select graph width', min=200, max=1500, value=650, step = 100,
                round = T,
                ticks = TRUE, animate = FALSE)
  )
  
})


# ----- Summary Measure -----------------------------------------------

output$focus_summeasure_explore_summary_table <- renderUI({
  selectInput("focus_inequal_type_explore_table", 
              "Select summary measures", 
              choices= .rdata[['summary_measures_all']], 
              selected=.rdata[["focus_inequal_type"]], 
              multiple=TRUE)
})

# ----- Summary Measure -----------------------------------------------

output$focus_summeasure_explore_summary_plot <- renderUI({
  focusInequalType_selector("focus_inequal_type_explore_plot", multiple=FALSE)
})



# ----- Multiplier -----------------------------------------------

output$summary_measures <- renderUI({
  list(
    tags$span(class="control-label", "Select estimate display"),
    checkboxInput('summultiplier1', 'MLD and TI multiplied by 1000', TRUE),
    checkboxInput('summultiplier2', 'RCI multiplied by 100', TRUE)#,
    
  )
})


# ----- Download data-----------------------------------------------

output$downloadSummtable <- renderUI({ 
  theData <- datasetInequal()
  
  if(is.null(theData)){
    return()
  }
  if(nrow(theData)==0){
    return()
  } else {
    list(br(),
         actionButton("downloadSummtable", "Download data", class = "btn-primary"))
  }  
})



# ----- Plot height and width (summary) ---------------------------------------------

# I believe this is not needed anymore
# output$summary_plot_dimensions <- renderUI({
#   list(
#     sliderInput('plot_height_sum', 'Height', min=200, max=1500, value=400, step = 50,
#                 round = T,
#                 ticks = TRUE, animate = FALSE),
#     
#     sliderInput('plot_width_sum', 'Width', min=200, max=1500, value=600, step = 50,
#                 round = T,
#                 ticks = TRUE, animate = FALSE)
#   )
# })


# ----- Summary plot type ---------------------------------------------

output$summary_plot_type <- renderUI({
  radioButtons("sumplot_type", "Select graph type",
               c("Bar graph" = "data_bar",
                 "Line graph" = "data_line"),
               inline=T,
               selected="data_bar")
})

# ----- Summary error bars ---------------------------------------------

output$summary_plot_error_bars <- renderUI({
  checkboxInput('summary_error_bars', 'Include 95% confidence interval', FALSE)
})

# ----- Summary error bars ---------------------------------------------




output$summary_plot_CI_type <- renderUI({
  radioButtons("summary_CI_type", NA,
               c("Analytic CI" = "analytic",
                 "Bootstrap CI" = "bootstrap"),
               inline=T,
               selected="analytic")
})



#******************************************************************************
# Compare inquality: sidepanel -----
#******************************************************************************

output$focus_country_compare <- renderUI({
  
  focusCountry_selector("focus_country_compare")
  
})

output$focus_indicator_compare <- renderUI({
  
  indic<-ifelse(is.null(.rdata[['focus_indicator']]), focus_indicator, .rdata[['focus_indicator']])
  
  
  selectInput("focus_indicator_compare", 
              "Select health indicator", 
              choices  = .rdata[['full_indicators']], 
              multiple = FALSE, 
              selected = indic)
  
})


output$focus_source_year_compare <- renderUI({
  
  
  
  list(
    radioButtons("focus_data_source_compare", "Select data source",
                 choices = .rdata[['data_sources']],#c("All", "DHS", "MICS"),
                 inline=TRUE,
                 selected=.rdata[["focus_data_source"]]),
    tags$span(class="control-label", "Select year"),
    checkboxInput('mostrecent_compare', 'Most recent year', .rdata[['mostrecent']]),
    
    conditionalPanel( condition = "!input.mostrecent_compare",  
                      
                      selectInput(inputId="focus_year_compare", 
                                  label=NA, 
                                  choices=c( .rdata[['all_years']]), 
                                  multiple=FALSE, 
                                  selected=.rdata[['focus_year']][1])
    )
  )
})



output$focus_summeasure_compare_summary <- renderUI({
  focusInequalType_selector("focus_inequal_type_compare", multiple = FALSE)
})


output$focus_dimension_compare <- renderUI({
  
  focus_dimen <- .rdata[['focus_dimension']][1]
  selectInput(inputId = "focus_dimension_compare",
              "Select inequality dimension",
              choices = .rdata[['equity_dimensions']],
              selected = focus_dimen,
              multiple=FALSE,
              selectize=TRUE)
  
})



output$benchmark_countries <- renderUI({
  
  countries <- .rdata[['benchmark_countries']]
  focus <-.rdata[['focus_country']]
  
  countries <- countries[!countries%in%focus]
  
  .rdata[['benchmark_countries']]<<-countries
  
  
  selectInput("benchmark_countries", 
              "Select benchmark countries", 
              choices=countries, 
              selected=countries,
              multiple=TRUE)
})



output$benchmarkWBgroup <- renderUI({
  
  selectInput("benchmarkWBgroup", label = "Filter benchmark countries by income group",
              choices = .rdata[['income_groups']],
              selected = .rdata[['focus_income_group']],
              multiple=T)
})




output$benchmarkWHOregion <- renderUI({
  
  
  selectInput("benchmarkWHOregion", label = "Filter benchmark countries by WHO Region",
              choices=.rdata[['who_regions']],
              selected = .rdata[['focus_who_regions']],
              multiple=T)
  
})


output$benchmarkYears <- renderUI({
  list(
    sliderInput('benchmarkYears', 'Select years', min=0, max=5, value=2, step = 1,
                round = T, ticks = TRUE, animate = FALSE),
    helpText(HTML("By how many years can the benchmark countries' data vary from the focus country's data?"),
             style="color:#666666; font-size: 85%")
  )
  
})




#******************************************************************************
#******************************************************************************
# GETTING DATA FOR DISAGGREGATED DATA TABLES
#******************************************************************************
#******************************************************************************

# Creating the data tables requires three pieces. There is a reactive that 
# will grab the data based on user selections. This is called datasetInput. 
# This is used in the dataTable renderUI. Originally this was not a renderUI
# but a renderDataTable but since we wanted the labels on top of the table
# I changed to a renderUI. As a result, the renderDataTable is in the renderUI.
# The dataTable_options is the third piece and this sets the datatable items and,
# importantly, checks for the health indicator full name and then makes that column
# wider.


#http://stackoverflow.com/questions/31813601/using-renderdatatable-within-renderui-in-shiny
# Generate a view of the Managed Data

# ----- Reactive to grab disaggregated data table -----------------------------

datasetInput <- reactive({
  
  theData<-getDisagData(indicator=input$focus_indicator_explore, 
                        stratifier=input$focus_dimension_explore,  # in hetkdb.R
                        countries=input$focus_country_explore, 
                        years=input$focus_year_explore, 
                        mostrecent=input$mostrecent_explore,
                        datasource=input$focus_data_source_explore)
  
  theData <- arrange(theData, country, year, source, indic, dimension, subgroup)
  
})






# ----- Disaggregated data table UI -----------------------------


output$dataTable <- renderUI({
  
  droptable <- FALSE
  dataMSG <- "If estimates are not shown for a selected combination of variables, then data are not available."
  
  if(is.null(input$focus_country_explore)) return()
  if(is.null(input$dataTableItems)) return()
  
  ident_country <- identical(input$focus_country_explore, .rdata[['focus_country']])
  ident_year <- identical(input$focus_year_explore, .rdata[['focus_year']])
  
  if(!all(ident_country, ident_year)) droptable <- TRUE
  
  if(!droptable){
    
    theData <- datasetInput()
    
    thetest <- !is.null(theData) && nrow(theData)>0 && sum(is.na(theData$estimate))!=nrow(theData)
    nodataMSG <-"There is no data for this combination of variables."
    
    if(!thetest) return(list(tags$div(class="datawarning", helpText(nodataMSG))))
    
    sigfig<-input$sumsigfig
    
    
    
#     theData <- theData %>% 
#       mutate(estimate = round(estimate, sigfig),
#              lower_95ci = round(lower_95ci,sigfig),
#              upper_95ci = round(upper_95ci,sigfig),
#              popshare = round(popshare, sigfig),
#              national = round(national, sigfig))
    
    theData<-theData %>% 
      rename(
        Country                = country,
        Year                   = year,
        `Data source`          = source,
        `Health indicator abbreviation`     = indic,
        `Inequality dimension` = dimension,
        Subgroup               = subgroup,
        Estimate               = estimate,
        `95%CI lower bound`          = lower_95ci,
        `95%CI upper bound`          = upper_95ci,
        `Population share %`   = popshare,
        `National estimate`    = national,
        Flag                   = flag,
        `Health indicator name` = indic_name
      ) 
    
    alltableitems <-  .rdata[['all_table_variables']]$table_vars
    
    selectedtableitems<-alltableitems[alltableitems%in%input$dataTableItems]
    
    theData <- theData[, selectedtableitems, drop=FALSE]
    
    
    #dataTable_options <- function(){
    
    vartypes <- .rdata[['all_table_variables']]$var_type
    alltableitems <- .rdata[['all_table_variables']]$table_vars
    selectedtableitems<-alltableitems[alltableitems%in%input$dataTableItems]
    
    
    
    
    
    
    
  }
  
  
  
  output$dataTable_inside <- DT::renderDataTable({
    if(droptable) return()
    
    cols <- 1:length(selectedtableitems)
    indx <- which("Health indicator name"==selectedtableitems)
    indxNarrow <- cols[!cols%in%indx]
    
    #indx <- ifelse(length(indx)!=0, indx, 'nothing')
    
    indxCenter1<- which(vartypes[alltableitems%in%selectedtableitems]=="numeric")
    indxCenter2 <- which(selectedtableitems%in%.rdata[["centered_table_variables"]])
    
    indxCenter <- c(indxCenter1, indxCenter2)
    
    
    # In case I want to edit the other columns
    indxOther <- cols[!cols%in%indxCenter]

  
  
    dataTable_options <- list(pageLength = 100,
                              #autoWidth = TRUE,
                              dom='frtp', 
                              scrollX = TRUE, 
                              scrollY = '600px',
                              processing = FALSE,
                              scrollCollapse = TRUE,
                              columnDefs = list(
                                list(className = "table-text-center", targets = indxCenter-1),
                                list(className = "table-text-nocenter", targets = indxOther-1)
                              ),
                              initComplete = JS(
                                "function(settings, json) {",
                                "$(this.api().table().header()).css({'background-color': '#E8E7E7', 'color': '#5A5959', 'padding-bottom': '3px dotted grey'});",
                                
                                "}")
    )
    
    
    if(length(indx)!=0){
    theData[["Health indicator name"]] <- forceNonBreak(theData[["Health indicator name"]], maxCharPerLine = 30)
    }
    
    

    # *************************************************
    # If the health indicator name or summary measure name exist
    # grab their index from above and use to tell datatables
    # which to be wider
    # *************************************************
      indxSigFig <- which(selectedtableitems%in%c("Estimate", "95%CI lower bound", "95%CI upper bound",
                                                       "Population share %", "National estimate"))
     datatable(theData, 
               options = dataTable_options,
               filter = "none", 
               rownames=FALSE,
               escape=FALSE) %>% formatRound(columns=indxSigFig, digits = sigfig)
  })
  
  
  
  
  
  return(
    list(
      tags$div(class="datawarning", helpText(dataMSG)),
      DT::dataTableOutput("dataTable_inside")
    )
  )
  
  
  
  
})



#******************************************************************************
#******************************************************************************
# GETTING DATA FOR SUMMARY DATA TABLES
#******************************************************************************
#******************************************************************************



# ----- Reactive to grab summary data-----------------------------

datasetInequal <- reactive({
  
  if(is.null(input$assessment_panel)) return()
  
  
  ineqDF <- getInequalData(indicator=input$focus_indicator_explore,  
                           stratifier=input$focus_dimension_explore, 
                           countries= input$focus_country_explore, 
                           years=input$focus_year_explore, 
                           mostrecent=input$mostrecent_explore,
                           datasource=input$focus_data_source_explore,  
                           inequal_types=input$focus_inequal_type_explore_table,
                           multiplier1 = input$summultiplier1,
                           multiplier2 = input$summultiplier2)
  
  ineqDF <- arrange(ineqDF, country, year, source, indic, dimension, measure)
  
  if(input$assessment_panel=='sumplot'){
    
    ineqDF$boot.se[ineqDF$boot.se == 0] <- NA
    ineqDF$se[ ineqDF$se == 0] <- NA
    
    return(ineqDF)
  }  
  
  return(ineqDF)
})





# ----- Disaggregated data table UI -----------------------------

output$dataTableInequal <- renderUI({
  
  droptable <- FALSE
  dataMSG <- "If summary measures are not shown for a selected combination of variables, then data are not available or summary measures cannot be calculated. Note that only relevant summary measures are calculated for each dimension."
  
  if(is.null(input$focus_country_explore)) return()
  if(is.null(input$dataTableItemsSummary)) return()
  
  ident_country <- identical(input$focus_country_explore, .rdata[['focus_country']])
  ident_year <- identical(input$focus_year_explore, .rdata[['focus_year']])
  
  if(!all(ident_country, ident_year)) droptable <- TRUE
  
  if(!droptable){
    theData <- datasetInequal()
    
    thetest <- !is.null(theData ) && nrow(theData )>0
    
    nodataMSG <-"If summary measures are not shown for a selected combination of variables, then data are not available or summary measures cannot be calculated. Note that only relevant summary measures are calculated for each dimension."
    
    if(!thetest) return(list(tags$div(class="datawarning", helpText(nodataMSG))))
    
    sigfig<-input$sumsigfig2
    
    
    
    
    theData <- select(theData, -ccode)

    
    
    theData <- theData %>% 
      rename(
        `Bootstrap 95%CI upper bound` = boot.upperci,
        `Bootstrap 95%CI lower bound` = boot.lowerci,
        `Analytic 95%CI upper bound`          = se.upperci,
        `Analytic 95%CI lower bound`          = se.lowerci,
        Country                = country,
        Year                   = year,
        `Health indicator abbreviation`     = indic,
        `Inequality dimension` = dimension,
        `Estimate`    = inequal,
        `Summary measure abbreviation` = measure,
        `Data source`          = source,
        `National estimate`               = estimate,
        `Health indicator name` = indic_name,
        `Summary measure name` = measure_name
      )
    
    
    
    alltableitems <- .rdata[['all_table_variables_summary']]$table_vars
    
    selectedtableitems<-alltableitems[alltableitems%in%input$dataTableItemsSummary]
    
    theData <- theData[, selectedtableitems, drop=FALSE]
    
    
    
    
    vartypes <- .rdata[['all_table_variables_summary']]$var_type
    alltableitems <- .rdata[['all_table_variables_summary']]$table_vars
    
    selectedtableitems<-alltableitems[alltableitems%in%input$dataTableItemsSummary]
    
    
    
    
    
    
  }
  
  output$dataTableInequal_inside <- DT::renderDataTable({
    if(droptable) return()
    
    indx1 <- which("Health indicator name"==selectedtableitems)
    indx2 <- which("Summary measure name"==selectedtableitems)
#     
#     indxCenter1 <- which(vartypes[alltableitems%in%selectedtableitems]=="numeric")
#     indxCenter2 <- which(selectedtableitems%in%.rdata[["centered_table_variables"]])
#     
#     indxCenter <- c(indxCenter1, indxCenter2)
    
    cols <- 1:length(selectedtableitems)
    #indx <- ifelse(length(indx)!=0, indx, 'nothing')
    
    indxCenter1<- which(vartypes[alltableitems%in%selectedtableitems]=="numeric")
    indxCenter2 <- which(selectedtableitems%in%.rdata[["centered_table_variables"]])
    
    indxCenter <- c(indxCenter1, indxCenter2)
    
    
    # In case I want to edit the other columns
    indxOther <- cols[!cols%in%indxCenter]
    
    
    dataTableInequal_options <- list(pageLength = 100, 
                              dom='frtp', 
                              scrollX = TRUE, 
                              scrollY = '600px',
                              processing = FALSE,
                              scrollCollapse = TRUE,
                                    columnDefs = list(
                                list(className = "table-text-center", targets = indxCenter-1),
                                list(className = "table-text-nocenter", targets = indxOther-1)
                                    ),
                                    initComplete = JS(
                                      "function(settings, json) {",
                                      "$(this.api().table().header()).css({'background-color': '#E8E7E7', 'color': '#5A5959', 'padding-bottom': '3px dotted grey'});",
                                      
                                      "}")
    )
    
    
    if(length(indx1)!=0){
              theData[["Health indicator name"]] <- forceNonBreak(theData[["Health indicator name"]], 
                                                        maxCharPerLine = 30)
    }

    if(length(indx2)!=0){
                  theData[["Summary measure name"]] <- forceNonBreak(theData[["Summary measure name"]], 
                                                        maxCharPerLine = 30)
    }
    

    # *************************************************
    # If the health indicator name or summary measure name exist
    # grab their index from above and use to tell datatables
    # which to be wider
    # *************************************************
    
#     if(length(indx1)!=0){
#       dataTableInequal_options$columnDefs <- c(dataTableInequal_options$columnDefs, 
#                                               list(list(width = '200px', targets = c(indx1))))
#     }
#     
#     if(length(indx2)!=0){
#       dataTableInequal_options$columnDefs <- c(dataTableInequal_options$columnDefs, 
#                                               list(list(width = '100px', targets = c(indx2))))
#     }
#     
    # *************************************************
    # Return the table
    # *************************************************
    
          indxSigFig <- which(selectedtableitems%in%c("Estimate", "Analytic 95%CI lower bound", 
                                                      "Analytic 95%CI upper bound",
                                                       "Bootstrap 95%CI lower bound", 
                                                      "Bootstrap 95%CI upper bound",
                                                      "National estimate"))
    
    
    datatable(theData, options = dataTableInequal_options, 
              filter="none", escape=FALSE, rownames = FALSE) %>% 
      formatRound(columns=indxSigFig, digits = sigfig)
  })
  
  
  return(
    list(
      tags$div(class="datawarning", helpText(dataMSG)),
      DT::dataTableOutput("dataTableInequal_inside")
    )
  )
  
})

#******************************************************************************
#******************************************************************************
# REACTIVES TO GET COMPARE PLOTTING DATA
#******************************************************************************
#******************************************************************************


getBenchmarkData <- reactive({
  
  

  if(is.null(input$benchmarkYears)) return()
  
  anchordata<-getDisagData(indicator=input$focus_indicator_compare, 
                           stratifier=input$focus_dimension_compare,  # in hetkdb.R
                           countries=input$focus_country_compare, 
                           years=input$focus_year_compare, 
                           mostrecent=input$mostrecent_compare,
                           datasource=input$focus_data_source_compare)
  
  
  if(is.null(anchordata) || nrow(anchordata)==0) return()
  anchordata$anchor <- 1
  
  benchmarkdata <- getDisagData(indicator = input$focus_indicator_compare, 
                                stratifier = input$focus_dimension_compare, 
                                countries = input$benchmark_countries, 
                                years =  input$focus_year_compare, 
                                mostrecent = input$mostrecent_compare,
                                datasource = input$focus_data_source_compare,
                                elasticity = input$benchmarkYears,
                                anchor_country = input$focus_country_compare)
  
  
  if(!is.null(benchmarkdata) && nrow(benchmarkdata)!=0){
    benchmarkdata$anchor <- 0
    theData <- rbind(anchordata, benchmarkdata) 
  }else{
    
    
    theData <- anchordata
  }

  
  return(theData)
})


# ----- SUMMARY DATA -----------------------------

getBenchmarkDataSum <- reactive({
  
  
  
  if(is.null(input$focus_inequal_type_compare)) return()
  
  anchordata <- getInequalData(indicator=input$focus_indicator_compare,  
                               stratifier=input$focus_dimension_compare, 
                               countries=input$focus_country_compare, 
                               years=input$focus_year_compare, 
                               mostrecent=input$mostrecent_compare,
                               datasource=input$focus_data_source_compare,  
                               inequal_types=input$focus_inequal_type_compare,
                               elasticity = input$benchmarkYears,
                               multiplier1 = input$summultiplier1,
                               multiplier2 = input$summultiplier2)
  
  if(is.null(anchordata) || nrow(anchordata)==0) return()
  anchordata$anchor <- 1
  
  
  benchmarkdata <- getInequalData(indicator=input$focus_indicator_compare,  
                                  stratifier=input$focus_dimension_compare, 
                                  countries=input$benchmark_countries, 
                                  years=input$focus_year_compare, 
                                  mostrecent=input$mostrecent_compare,
                                  datasource=input$focus_data_source_compare,  
                                  inequal_types=input$focus_inequal_type_compare,
                                  elasticity = input$benchmarkYears,
                                  multiplier1 = input$summultiplier1,
                                  multiplier2 = input$summultiplier2,
                                  anchorCountry = input$focus_country_compare)
  
  
  if(!is.null(benchmarkdata) && nrow(benchmarkdata)!=0){
    benchmarkdata$anchor <- 0
    theData <- rbind(anchordata, benchmarkdata) 
  }else{
    

    
    theData <- anchordata
  }
  
  
  
  
  
  
  
  
  return(theData)
})


#******************************************************************************
#******************************************************************************
# PLOTTING USER INTERFACES
#******************************************************************************
#******************************************************************************

# ----- DISAGGREGATED PLOT EXPLORE -----------------------------

output$disag_plot_explore <- renderUI({ 
  
  .rdata[["disag_plot_explore"]] <<- .rdata[["blank_plot"]]
  
  # This is the general message that is usually shown even if data is avail
  # to let users know that there are situations where no data is available
  dataMSG <- "If estimates are not shown for a selected combination of variables, then data are not available."
  
  
  if(is.null(input$plot_height1)) return()
  if(is.null(input$plot_width1)) return()
  if(is.null(input$axis_limitsmin1 )) return()
  if(is.null(input$axis_limitsmax1 )) return()
  
  h <- input$plot_height1
  w <- input$plot_width1
  
  # Convert the axis limits to numbers and supress warnings so there is nothing in console
  axismin <- suppressWarnings(as.numeric(input$axis_limitsmin1))
  axismax <- suppressWarnings(as.numeric(input$axis_limitsmax1))
  
  
  # Test whether the axis limit value is either "" or not an NA
  isValid1 <- input$axis_limitsmin1 == "" | !is.na(axismin)
  isValid2 <- input$axis_limitsmax1 == "" | !is.na(axismax)
  
  # If the min or the max axis is not a "" or valid number
  if(!isValid1 | !isValid2) return(list(tags$div(class="datawarning", helpText("Please supply a valid number"))))

  
  
  
  if(is.null(input$ai_plot_type)){
    ai_plot_type <- "data_line"
  }else{
    ai_plot_type <- input$ai_plot_type
  }
  
  
  # *********************************************************************
  # A block of code testing for whether the input values are equal to their 
  # equivalent global variables 
  # *********************************************************************
  
  # If we fail the test, then this will be changed to true and instead of a 
  # plot we will return nothing below
  dropplot <- FALSE
  
  ident_country <- identical(input$focus_country_explore, .rdata[['focus_country']])
  ident_year <- identical(input$focus_year_explore, .rdata[['focus_year']])
  
  if(!all(ident_country, ident_year)) dropplot <-TRUE
  
  # *********************************************************************
  # If all inputs are equal to global variables then we don't have
  # to drop the plot and we can go ahead and get data
  # *********************************************************************
  
  if(!dropplot){
    
    
    # *********************************************************************
    # Get data
    # *********************************************************************
    
    plotData <- datasetInput()
   
    # *********************************************************************
    # Tests to make sure we have data and it's what we need
    # *********************************************************************
    
    thetest <- !is.null(plotData) && nrow(plotData)>0 && sum(is.na(plotData$estimate))!=nrow(plotData)
    nodataMSG <-"There is no data for this combination of variables."
    if(!thetest) return(list(tags$div(class="datawarning", helpText(nodataMSG))))

    
    # *********************************************************************
    # I'm setting and updating the title here, perhaps it should be in
    # an observer but it's working
    # *********************************************************************
    
    yrs <- paste(sort(unique(plotData$year)), collapse=", ")
    sources <- paste(sort(sort(unique(plotData$source))), collapse=" & ")
    .rdata[["plotDisag_explore_title"]] <<- paste0(.rdata[['focus_country']], ", ",sources," ", yrs)
    
    updateTextInput(session, "main_title1", value = .rdata[["plotDisag_explore_title"]])
    
    
    
    
    tmpSubgroup <- plotData$subgroup
    plotData$subgroup<-as.character(plotData$subgroup)
    plotData <- left_join(plotData, .rdata[['dimension_details']], by=c("dimension", "subgroup", "order"))
    
    plotData$subgroup<-tmpSubgroup
    
    
  }
  
  
  output$disag_plot_explore_inside <- renderPlot({
    
    if(dropplot) return()
    
    
    if(isolate(input$assessment_panel) == 'dataplot' & ai_plot_type=='data_bar'){        
      p <- plotDisagBar_explore(plotData, session)
      
    }
    if(isolate(input$assessment_panel) == 'dataplot' & ai_plot_type=='data_line'){
      
      p <- plotDisagLine_explore(plotData, session)
    }
    .rdata[["disag_plot_explore"]] <<- p
    
    print(p)
  }, res=90, height=exprToFunction(ifelse(is.null(h), 600, h)), 
  width=exprToFunction(ifelse(is.null(w), 600, w))
  )
  
  
  return(
    list(
      tags$div(class="datawarning", helpText(dataMSG)),
      plotOutput("disag_plot_explore_inside")
    )
  )
  
  
})  

# ----- SUMMARY PLOT EXPLORE -----------------------------

output$summary_plot_explore <- renderUI({ 
  
  .rdata[["summary_plot_explore"]] <<- .rdata[["blank_plot"]]
  # This is the general message that is usually shown even if data is avail
  # to let users know that there are situations where no data is available
  dataMSG <- "If summary measures are not shown for a selected combination of variables, then data are not available or summary measures cannot be calculated. Note that only relevant summary measures are calculated for each dimension."
  
  
  
  if(is.null(input$sumplot_type)) return()
  if(is.null(input$plot_height2)) return()
  if(is.null(input$plot_width2)) return()
  if(is.null(input$axis_limitsmin2)) return()
  if(is.null(input$axis_limitsmax2)) return()
  
  
  h <- input$plot_height2
  w <- input$plot_width2
  
  
  # Convert the axis limits to numbers and supress warnings so there is nothing in console
  axismin <- suppressWarnings(as.numeric(input$axis_limitsmin2))
  axismax <- suppressWarnings(as.numeric(input$axis_limitsmax2))
  
  # Test whether the axis limit value is either "" or not an NA
  isValid1 <- input$axis_limitsmin2 == "" | !is.na(axismin)
  isValid2 <- input$axis_limitsmax2 == "" | !is.na(axismax)
  
  # If the min or the max axis is not a "" or valid number
  if(!isValid1 | !isValid2) return(list(tags$div(class="datawarning", helpText("Please supply a valid number"))))
  
  
  
  # *********************************************************************
  # A block of code testing for whether the input values are equal to their 
  # equivalent global variables 
  # *********************************************************************
  
  # If we fail the test, then this will be changed to true and instead of a 
  # plot we will return nothing below
  dropplot <- FALSE
  
  
  ident_country <- identical(input$focus_country_explore, .rdata[['focus_country']])
  ident_year <- identical(input$focus_year_explore, .rdata[['focus_year']])
  
  if(!all(ident_country, ident_year)) dropplot <- TRUE
  
  
  
  # *********************************************************************
  # If all inputs are equal to global variables then we don't have
  # to drop the plot and we can go ahead and get data
  # *********************************************************************
  
  if(!dropplot){ 
    
    # *********************************************************************
    # Get data
    # *********************************************************************
    
    plotData <- datasetInequal()
    plotData <- filter(plotData, measure==input$focus_inequal_type_explore_plot, !is.na(inequal))
    
    
    
    # *********************************************************************
    # Tests to see if data is (A) Not NULL; (B) there is some non-NA data
    # with the anchor field == 1
    # *********************************************************************
    
    thetest <- !is.null(plotData) && nrow(plotData)>0
    
    nodataMSG <-"If summary measures are not shown for a selected combination of variables, then data are not available or summary measures cannot be calculated. Note that only relevant summary measures are calculated for each dimension."
    
    if(!thetest) return(list(tags$div(class="datawarning", helpText(nodataMSG))))
    
    # *********************************************************************
    # We can't have negative or 0 values as the y-axis if the variable
    # is log scale
    # *********************************************************************
    
    isLogScale <- plotData$logscale[1] == 1
    logscaleMSG <- "This summary measure is shown on a logarithmic scale, so axis limits must take values greater than zero."
    
    
    if(isLogScale){
      
      # test for log scale and incorrect y params
      okLogMin <- input$axis_limitsmin2 == "" | axismin > 0
      okLogMax <- input$axis_limitsmax2 == "" | axismax > 0
      
      logtest <- okLogMin & okLogMax
      
      if(!logtest) return(list(tags$div(class="datawarning", helpText(logscaleMSG))))
      
    }
    
    
    
    # *********************************************************************
    # I'm setting and updating the title here, perhaps it should be in
    # an observer but it's working
    # *********************************************************************
    
    sumMeasure <- plotData$measure[1]
    measureName <- gsub(" \\(.*\\)", "", plotData$measure_name[1])
    yrs <- paste(sort(unique(plotData$year)), collapse=", ")
    sources <- paste(sort(sort(unique(plotData$source))), collapse=" & ")
    
    .rdata[["plotSummary_explore_title"]] <<- paste0(measureName, " in ",
                                                     .rdata[['focus_country']], ", " , 
                                                     sources, " ",
                                                     yrs)
    updateTextInput(session, "main_title2", value = .rdata[["plotSummary_explore_title"]])
    
    
  }
  
  # *********************************************************************
  # Output the results
  # *********************************************************************
  
  output$summary_plot_explore_inside <- renderPlot({
    
    if(dropplot) return()
    
    if(isolate(input$assessment_panel) == 'sumplot' & input$sumplot_type=='data_bar'){                   
      p <- plotSummaryBar_explore(plotData, session)
      
    }
    if(isolate(input$assessment_panel) == 'sumplot' & input$sumplot_type=='data_line'){          
      p <- plotSummaryLine_explore(plotData, session)
      
    }  
    
    .rdata[["summary_plot_explore"]] <<- p
    
    print(p)
  }, res=90, height=exprToFunction(ifelse(is.null(h), 600, h)), 
  width=exprToFunction(ifelse(is.null(w), 600, w))
  )
  
  
  return(
    list(
      tags$div(class="datawarning", helpText(dataMSG)),
      plotOutput("summary_plot_explore_inside")
    )
  )
  
  
  
  
  
  
})  

# ----- COMPARE DISAGGREGATED PLOT -----------------------------

output$disag_plot_compare <- renderUI({

  .rdata[["disag_plot_compare"]] <<- .rdata[["blank_plot"]]
  # This is the general message that is usually shown even if data is avail
  # to let users know that there are situations where no data is available
  dataMSG <- "If estimates are not shown for a selected combination of variables, then data are not available."
  
  
  input$xaxis_title3
  input$yaxis_title3
  
  if(is.null(input$focus_country_compare)) return()
  if(is.null(input$axis_limitsmin3 )) return()
  if(is.null(input$axis_limitsmax3 )) return()
  
  
  if(.rdata[['first_time']]){
    .rdata[['first_time']] <<-FALSE
  }
  
  h <- input$plot_height3
  w <- input$plot_width3
  
  
  # Convert the axis limits to numbers and supress warnings so there is nothing in console
  axismin <- suppressWarnings(as.numeric(input$axis_limitsmin3))
  axismax <- suppressWarnings(as.numeric(input$axis_limitsmax3))
  
  
  # Test whether the axis limit value is either "" or not an NA
  isValid1 <- input$axis_limitsmin3 == "" | !is.na(axismin)
  isValid2 <- input$axis_limitsmax3 == "" | !is.na(axismax)
  
  # If the min or the max axis is not a "" or valid number
  if(!isValid1 | !isValid2) return(list(tags$div(class="datawarning", helpText("Please supply a valid number"))))
  
  
  # *********************************************************************
  # A block of code testing for whether the input values are equal to their 
  # equivalent global variables 
  # *********************************************************************
  
  # If we fail the test, then this will be changed to true and instead of a 
  # plot we will return nothing below
  dropplot <- FALSE
  
  ident_country <- identical(input$focus_country_compare, .rdata[['focus_country']])
  ident_year <- identical(input$focus_year_compare, as.character(.rdata[['focus_year']][1]))
  ident_income <- identical(input$benchmarkWBgroup, .rdata[['focus_income_group']])
  ident_who <-  identical(input$benchmarkWHOregion, .rdata[["focus_who_regions"]] )
  ident_bench <-identical(input$benchmark_countries, .rdata[['benchmark_countries']])
  
  if(is.null(input$benchmark_countries) && 
     (!is.null(.rdata[['benchmark_countries']]) & 
      length(.rdata[['benchmark_countries']])==0)) dropplot <- TRUE
  
  
  #if(!ident_bench && !is.null(input$benchmark_countries) && ident_country && ident_year && !(input$benchmark_countries==" ")) ident_bench<- TRUE
  if((!ident_bench) && !is.null(input$benchmark_countries) && (input$benchmark_countries==" ")) ident_bench <- TRUE
  if(!all(ident_country, ident_year,  ident_bench)) dropplot <-TRUE
  
  # *********************************************************************
  # If all inputs are equal to global variables then we don't have
  # to drop the plot and we can go ahead and get data
  # *********************************************************************
  
  if(!dropplot){
    
    # *********************************************************************
    # Get data
    # *********************************************************************
    
    plotData <- getBenchmarkData()
    
    # *********************************************************************
    # Tests to make sure we have data and it's what we need
    # *********************************************************************
    
    nonNull_plotData <- !is.null(plotData)
    someAnchorData <- nrow(plotData[plotData$anchor==1,])>0
    # I suppress a warning here because if there is no data it will still
    # run and give a warning but this is fine because the next piece of
    # code is &&
    notAllNA_Anchor <- suppressWarnings(sum(is.na(plotData$estimate))!=nrow(plotData))
    
    thetest <- nonNull_plotData  && someAnchorData && notAllNA_Anchor
    nodataMSG <-"There is no data for this combination of variables."
    if(!thetest) return(list(tags$div(class="datawarning", helpText(nodataMSG))))
    
    # *********************************************************************
    # I'm setting and updating the title here, perhaps it should be in
    # an observer but it's working
    # *********************************************************************
    
    .rdata[["plotDisag_compare_title"]] <<-  paste(plotData$indic_name[1], "by", tolower(plotData$dimension[1]), "in",
                                                   length(unique(plotData$country)), "countries")
    
    
    updateTextInput(session, "main_title3", value = .rdata[["plotDisag_compare_title"]])
    
    
    tmpSubgroup <- plotData$subgroup
    plotData$subgroup<-as.character(plotData$subgroup)
    
    plotData <- left_join(plotData, .rdata[['dimension_details']], by=c("dimension", "subgroup", "order"))
    
    plotData$subgroup<-tmpSubgroup
    
  }
  
  output$disag_plot_compare_inside <- renderPlot({
    
    # if we failed the test above, return nothing
    if(dropplot) return()
    
    p <- plotDisagLine_compare(plotData, session)
    .rdata[["disag_plot_compare"]] <<- p
    
    print(p)
  }, res=90, height=exprToFunction(ifelse(is.null(h), 600, h)), 
  width=exprToFunction(ifelse(is.null(w), 600, w)))
  
  
  
  return(
    list(
      tags$div(class="datawarning", helpText(dataMSG)),
      plotOutput("disag_plot_compare_inside")
    )
  )
  
})  



# ----- COMPARE SUMMARY PLOT -----------------------------
output$summary_plot_compare <- renderUI({
  
  .rdata[["summary_plot_compare"]] <<- .rdata[["blank_plot"]]
  
  # This is the general message that is usually shown even if data is avail
  # to let users know that there are situations where no data is available
  dataMSG <- "If estimates are not shown for a selected combination of variables, then data are not available or summary measures cannot be calculated. Note that only relevant summary measures are calculated for each dimension."
  
  #3/16/2015 commented out, not sure it's needed
  #   if(.rdata[['first_time']]){
  #     .rdata[['first_time']] <<-FALSE
  #   }
  
  input$xaxis_title4
  input$yaxis_title4
  
  if(is.null(input$focus_country_compare)) return()
  if(is.null(input$yaxis_limitsmin4 )) return()
  if(is.null(input$yaxis_limitsmax4 )) return()
  if(is.null(input$xaxis_limitsmin4 )) return()
  if(is.null(input$xaxis_limitsmax4 )) return()
  
  h <- input$plot_height4
  w <- input$plot_width4
  
  # Convert the axis limits to numbers and supress warnings so there is nothing in console
  axismin <- suppressWarnings(as.numeric(input$yaxis_limitsmin4))
  axismax <- suppressWarnings(as.numeric(input$yaxis_limitsmax4))
  Xaxismin <- suppressWarnings(as.numeric(input$xaxis_limitsmin4))
  Xaxismax <- suppressWarnings(as.numeric(input$xaxis_limitsmax4))
  
  # Test whether the axis limit value is either "" or not an NA
  isValid1 <- input$yaxis_limitsmin4 == "" | !is.na(axismin)
  isValid2 <- input$yaxis_limitsmax4 == "" | !is.na(axismax)
  isValid3 <- input$xaxis_limitsmin4 == "" | !is.na(Xaxismin)
  isValid4 <- input$xaxis_limitsmax4 == "" | !is.na(Xaxismax)
  
  # If the min or the max axis is not a "" or valid number
  if(!isValid1 | !isValid2 | !isValid3 | !isValid4) return(list(tags$div(class="datawarning", helpText("Please supply a valid number"))))
  
  # *********************************************************************
  # A block of code testing for whether the input values are equal to their 
  # equivalent global variables 
  # *********************************************************************
  
  # If we fail the test, then this will be changed to true and instead of a 
  # plot we will return nothing below
  dropplot <- FALSE
  
  ident_country <- identical(input$focus_country_compare, .rdata[['focus_country']])
  ident_year <- identical(input$focus_year_compare, as.character(.rdata[['focus_year']][1]))
  ident_income <- identical(input$benchmarkWBgroup, .rdata[['focus_income_group']])
  ident_who <-  identical(input$benchmarkWHOregion, .rdata[["focus_who_regions"]] )
  ident_bench <-identical(input$benchmark_countries, .rdata[['benchmark_countries']])
  
  if(is.null(input$benchmark_countries) && 
     (!is.null(.rdata[['benchmark_countries']]) & 
      length(.rdata[['benchmark_countries']])==0)) dropplot <- TRUE
  
  
  #if(!ident_bench && !is.null(input$benchmark_countries) && ident_country && ident_year && !(input$benchmark_countries==" ")) ident_bench<- TRUE
  if((!ident_bench) && !is.null(input$benchmark_countries) && (input$benchmark_countries==" ")) ident_bench <- TRUE
  if(!all(ident_country, ident_year,  ident_bench)) dropplot <-TRUE
  
  
  # *********************************************************************
  # If all inputs are equal to global variables then we don't have
  # to drop the plot and we can go ahead and get data
  # *********************************************************************
  
  if(!dropplot){
    
    # *********************************************************************
    # Get data
    # *********************************************************************
    
    plotData<-getBenchmarkDataSum()
    
    
    # *********************************************************************
    # Tests to see if data is (A) Not NULL; (B) there is some non-NA data
    # with the anchor field == 1
    # *********************************************************************
    
    thetest <- !is.null(plotData) && nrow(plotData[!is.na(plotData$inequal) & plotData$anchor==1,])>0
    
    nodataMSG <-"If summary measures are not shown for a selected combination of variables, then data are not available or summary measures cannot be calculated. Note that only relevant summary measures are calculated for each dimension."
    if(!thetest) return(list(tags$div(class="datawarning", helpText(nodataMSG))))
    
    
    # *********************************************************************
    # We can't have negative or 0 values as the y-axis if the variable
    # is log scale
    # *********************************************************************
    
    isLogScale <- plotData$logscale[1] == 1
    logscaleMSG <- "This summary measure is shown on a logarithmic scale, so axis limits must take values greater than zero."
    
    
    if(isLogScale){
      
      # test for log scale and incorrect y params
      okLogMin <- input$yaxis_limitsmin4 == "" | axismin > 0
      okLogMax <- input$yaxis_limitsmax4 == "" | axismax > 0
      
      logtest <- okLogMin & okLogMax
      
      if(!logtest) return(list(tags$div(class="datawarning", helpText(logscaleMSG))))
      
    }
    
    
    # *********************************************************************
    # I'm setting and updating the title here, perhaps it should be in
    # an observer but it's working
    # *********************************************************************
    
    .rdata[["plotSummary_compare_title"]] <<-  paste0(plotData$indic_name[1], 
                                                      ": national average and within-country inequality (according to ", 
                                                      tolower(plotData$dimension[1]), ") in ",
                                                      length(unique(plotData$country)), " countries")
    
    
    updateTextInput(session, "main_title4", value = .rdata[["plotSummary_compare_title"]])
    
    # *********************************************************************
    # I'm setting and updating the title here, perhaps it should be in
    # an observer but it's working
    # *********************************************************************
    
    
  }
  
  output$summary_plot_compare_inside <- renderPlot({
    
    # if we fail the test above and the app is behind
    # then return nothing
    if(dropplot) return()
    
    p <- plotSummaryScatter_compare(plotData, session)
    .rdata[["summary_plot_compare"]] <<- p
    print(p)
    
  }, res=90, height=exprToFunction(ifelse(is.null(h), 600, h)), 
  width=exprToFunction(ifelse(is.null(w), 600, w))
  )
  
  
  return(
    list(
      tags$div(class="datawarning", helpText(dataMSG)),
      plotOutput("summary_plot_compare_inside")
    )
  )
  
})  

