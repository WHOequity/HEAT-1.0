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
# Downloading explore
#******************************************************************************


# ----------------------------------------
# --- Explore disaggregated data
# ----------------------------------------

output$btnDownloadDisagData_explore <- renderUI({
  
  
       tags$div(class="button-holder",
                actionButton("btnDownloadDisagData_explore",
                             "Download data", 
                             class = "btn-primary", 
                             icon=icon("download"),
                             `data-toggle`="modal",
                             `data-target`="#datatableModal_explore")
       )
  
})



output$btnStartDownloadDisagData_explore <- downloadHandler(
  filename = function() {
    iso3 <- getISO3(input$focus_country_explore)
    paste(iso3, "_disaggregated_data", '.', input$filetype1, sep='')
  },
  
  content = function(file) {
    dat <- datasetInput()
    

    
    dat <- select(dat, country, year, source, indic, indic_name, dimension,
                  subgroup, estimate, se, lower_95ci, upper_95ci,
                   popshare, flag, national,iso3, maxoptimum,
                  rankable, order) %>% 
      rename(indicator_abbr = indic,
             `95ci_lb` = lower_95ci,
             `95ci_ub` = upper_95ci,
            
             population_share = popshare,
             indicator_name = indic_name
             )
    
    dat$flag[is.na(dat$flag)]<-""
    
    
    
    
    sep <- switch(input$filetype1, "csv" = ",", "tsv" = "\t")
  
    write_table_wcitation(dat, file,  sep, .rdata[["table_footnote"]]) # see global.R
    
    
  }
)




output$btnStartDownloadDisagPlot_explore  <- downloadHandler(
  filename = function() { 
    iso3 <- getISO3(input$focus_country_explore)
    paste(iso3, "_disaggregated_data", '.', input$disagPlotType_explore, sep='')
  },
  content = function(file) {
    
    g <- grid.arrange(.rdata[["disag_plot_explore"]], .rdata[["plot_footnote"]], 
                 ncol=1, nrow=2, heights=c(0.90, 0.1))
     ggsave(file, g, width=24, height=24, units="cm")
  }
)   




output$btnStartDownloadDisagPlotData_explore <- downloadHandler(
  filename = function() {
    iso3 <- getISO3(input$focus_country_explore)
    paste(iso3, "_disaggregated_data", '.', input$filetype_explore_disag, sep='')
  },
  content = function(file) {
    dat <- datasetInput()
    
    
    dat <- select(dat, country, year, source, indic, indic_name, dimension,
                  subgroup, estimate, se, lower_95ci, upper_95ci,
                   popshare, flag, national,iso3, maxoptimum,
                  rankable, order) %>% 
      rename(indicator_abbr = indic,
             `95ci_lb` = lower_95ci,
             `95ci_ub` = upper_95ci,
             population_share = popshare,
             indicator_name = indic_name
      )
    
    dat$flag[is.na(dat$flag)]<-""
    
    
    
    
    sep <- switch(input$filetype_explore_disag, "csv" = ",", "tsv" = "\t")
    
    write_table_wcitation(dat, file,  sep, .rdata[["table_footnote"]]) # see global.R
  }
)



# ----------------------------------------
# --- Explore summary data
# ----------------------------------------


output$btnDownloadSummaryData_explore <- renderUI({
  
  tags$div(class="button-holder",
       actionButton("btnDownloadSummaryData_explore", 
                    "Download data", 
                    class = "btn-primary", 
                    icon=icon("download"),
                    `data-toggle`="modal",
                    `data-target`="#summtableModal_explore")
  )
  
})



output$btnStartDownloadSummaryData_explore <- downloadHandler(
  filename = function() {
    iso3 <- getISO3(input$focus_country_explore)
    paste(iso3, "_summary_measures", '.', input$filetype2, sep='')
  },
  content = function(file) {
    dat <- datasetInequal()
    

    
    dat <- select(dat, country, year, source, indic, indic_name, dimension,
                  measure, measure_name, inequal, se, se.lowerci, se.upperci,
                  boot.se, boot.lowerci, boot.upperci, 
                  ccode, estimate) %>% 
      rename(indicator_abbr = indic,
             `analytic_95ci_lb` = se.lowerci,
             `analytic_95ci_ub` = se.upperci,
             `bootstrap_95ci_lb` = boot.lowerci,
             `bootstrap_95ci_ub` = boot.upperci,
             national = estimate,
             estimate = inequal,
             analytic_se = se,
             bootstrap_se = boot.se,
             iso3 = ccode,
             indicator_name = indic_name,
             measure_abbr = measure
      )
    

    
    
    
    sep <- switch(input$filetype2, "csv" = ",", "tsv" = "\t")
    
    write_table_wcitation(dat, file,  sep, .rdata[["table_footnote"]]) # see global.R
  }
)






# ----------------------------------------
# --- Explore summary plot
# ----------------------------------------


# output$btnDownloadSummaryPlot_explore <- renderUI({
#   thePlot <- "xx" #theDataPlot()
#   if(is.null(thePlot)){
#     return()
#   } else {
#     list(br(),
#          actionButton("btnDownloadSummaryPlot_explore", "Download graph", class = "btn-primary"))
#   }
# })



output$btnStartDownloadSummaryPlot_explore  <- downloadHandler(
  filename = function() { 
    iso3 <- getISO3(input$focus_country_explore)
    paste(iso3, "_summary_measures", '.', input$summaryPlotType_explore, sep='')
  },
  content = function(file) {
    g <- grid.arrange(.rdata[["summary_plot_explore"]], .rdata[["plot_footnote"]], 
                      ncol=1, nrow=2, heights=c(0.90, 0.10))
    ggsave(file, g, width=24, height=24, units="cm")
  }
)   





output$btnStartDownloadSummaryPlotData_explore <- downloadHandler(
  filename = function() {
    iso3 <- getISO3(input$focus_country_explore)
    paste(iso3, "_summary_measures", '.', input$filetype_explore_summary, sep='')
  },
  content = function(file) {
    dat <- datasetInequal()
    
    dat <- select(dat, country, year, source, indic, indic_name, dimension,
                  measure,measure_name, inequal, se, se.lowerci, se.upperci,
                  boot.se, boot.lowerci, boot.upperci, 
                  ccode, estimate) %>% 
      rename(indicator_abbr = indic,
             `analytic_95ci_lb` = se.lowerci,
             `analytic_95ci_ub` = se.upperci,
             `bootstrap_95ci_lb` = boot.lowerci,
             `bootstrap_95ci_ub` = boot.upperci,
             national = estimate,
             estimate = inequal,
             analytic_se = se,
             bootstrap_se = boot.se,
             iso3 = ccode,
             indicator_name = indic_name,
             measure_abbr = measure
      )
    
    sep <- switch(input$filetype_explore_summary, "csv" = ",", "tsv" = "\t")
    write_table_wcitation(dat, file,  sep, .rdata[["table_footnote"]]) # see global.R
  }
)




#******************************************************************************
# Downloading compare
#******************************************************************************

# ----------------------------------------
# --- Compare summary data
# ----------------------------------------



output$btnDownloadDisagData_compare <- renderUI({
  
  tags$div(class="button-holder",
       actionButton("btnDownloadDisagData_compare", 
                    "Download data", 
                    class = "btn-primary",  
                    icon=icon("download"),
                    `data-toggle`="modal",
                    `data-target`="#compdataModal_compare")
)
  
})



output$btnStartDownloadDisagData_compare <- downloadHandler(
  filename = function() {

    paste("benchmark_disaggregated_data", '.', input$filetype_benchmark, sep='')
  },
  content = function(file) {
    dat <- getBenchmarkData()

    
    
    dat <- select(dat, country, year, source, indic, indic_name, dimension,
                  subgroup, estimate, se, lower_95ci, upper_95ci,
                  popshare, flag, national,iso3, maxoptimum,
                  rankable, order) %>% 
      rename(indicator_abbr = indic,
             `95ci_lb` = lower_95ci,
             `95ci_ub` = upper_95ci,
             population_share = popshare,
             indicator_name = indic_name
      )
    
    dat$flag[is.na(dat$flag)]<-""
    
    
    
    
    
    sep <- switch(input$filetype_benchmark, "csv" = ",", "tsv" = "\t")
    
    write_table_wcitation(dat, file,  sep, .rdata[["table_footnote"]]) # see global.R
  }
)




output$btnStartDownloadDisagPlot_compare  <- downloadHandler(
  filename = function() { 
    paste("benchmark_disaggregated_data", '.', input$disagPlotType_compare, sep='')
  },
  content = function(file) {

    
    p <- .rdata[["disag_plot_compare"]]
    
    # this adds a little more space to the right plot margin because it was coming
    # to the edge of the graph but only if the graph is not a rectGrob (blank plot)
    
    if(!any(class(p)=="rect")){
    p <- p + theme(plot.margin = unit(c(0.5, 2, 0.5, 0.5), "cm"))
    }
    
    g <- grid.arrange(p, .rdata[["plot_footnote"]], 
                      ncol=1, nrow=2, heights=c(0.90, 0.10))
    ggsave(file, g, width=24, height=24, units="cm")
  }
)   







output$btnStartDownloadDisagPlotData_compare <- downloadHandler(
  filename = function() {
    paste("benchmark_disaggregated_data", '.', input$filetype_benchmark_disag, sep='')
  },
  content = function(file) {
    dat <- getBenchmarkData()

        if(is.null(dat)){
      dat <-       data.frame(country = NA,
             `year` = NA,
             `source` = NA,
             `indicator_abbr` = NA,
             `indicator_name` = NA,
             dimension = NA,
             subgroup = NA,
             estimate = NA,
             se = NA,
             `95ci_lb` = NA,
             `95ci_ub` = NA,
             population_share = NA,
             flag = NA,
             national = NA,
             iso3 = NA,
             maxoptimum = NA,
             rankable = NA,
             order = NA
      )
      
      dat <- filter(dat, !is.na(indicator_abbr)) %>% 
        rename(`95ci_lb` = X95ci_lb, `95ci_ub` = X95ci_ub)
        
        
    } else {
 
    
    dat <- select(dat, country, year, source, indic, indic_name, dimension,
                  subgroup, estimate, se, lower_95ci, upper_95ci,
                  popshare, flag, national,iso3, maxoptimum,
                  rankable, order) %>% 
      rename(indicator_abbr = indic,
             `95ci_lb` = lower_95ci,
             `95ci_ub` = upper_95ci,
             population_share = popshare,
             indicator_name = indic_name
      )
    
    dat$flag[is.na(dat$flag)]<-""
    
    }
    
    
    
    sep <- switch(input$filetype_benchmark_disag, "csv" = ",", "tsv" = "\t")
    
    write_table_wcitation(dat, file,  sep, .rdata[["table_footnote"]]) # see global.R
  }
)






output$btnStartDownloadSummaryPlot_compare  <- downloadHandler(
  filename = function() { 
    paste("benchmark_summary_measures", '.', input$summaryPlotType_compare, sep='')

  },
  content = function(file) {
    
    p <- .rdata[["summary_plot_compare"]]
    
    if(!any(class(p)=="rect")){
    p <- p + theme(plot.margin = unit(c(0.5, 2, 0.5, 0.5), "cm"))
    }
    
    g <- grid.arrange(p,  .rdata[["plot_footnote"]], 
                      ncol=1, nrow=2, heights=c(0.90, 0.10))
    ggsave(file, g, width=24, height=24, units="cm")
  }
)   






output$btnStartDownloadSummaryPlotData_compare <- downloadHandler(
  filename = function() {
    paste("benchmark_summary_measures", '.', input$filetype_benchmark_summary, sep='')
  },
  content = function(file) {
    dat <- getBenchmarkDataSum()
    
    # This is added to return a blank plot
    if(is.null(dat)){
      dat <-       data.frame(country = NA,
             `year` = NA,
             `source` = NA,
             `indicator_abbr` = NA,
             `indicator_name` = NA,
             dimension = NA,
             measure_abbr = NA,
             measure_name = NA,
             estimate = NA,
             analytic_se = NA,
             analytic_95ci_lb = NA,
             analytic_95ci_ub = NA,
             boostrap_se = NA,
             bootstrap_95ci_lb = NA,
             bootstrap_95ci_ub = NA,
             iso3 = NA,
             national = NA
      )
      
      dat <- filter(dat, !is.na(indicator_abbr))
        
        
    } else {
    dat <- select(dat, country, year, source, indic, indic_name, dimension,
                  measure, measure_name, inequal, se, se.lowerci, se.upperci,
                  boot.se, boot.lowerci, boot.upperci, 
                  ccode, estimate) %>% 
      rename(indicator_abbr = indic,
             `analytic_95ci_lb` = se.lowerci,
             `analytic_95ci_ub` = se.upperci,
             `bootstrap_95ci_lb` = boot.lowerci,
             `bootstrap_95ci_ub` = boot.upperci,
             national = estimate,
             estimate = inequal,
             analytic_se = se,
             bootstrap_se = boot.se,
             iso3 = ccode,
             indicator_name = indic_name,
             measure_abbr = measure
      )
  }
    
    
    sep <- switch(input$filetype_benchmark_summary, "csv" = ",", "tsv" = "\t")
    
    write_table_wcitation(dat, file,  sep, .rdata[["table_footnote"]]) # see global.R
  }
)







