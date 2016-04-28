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

output$explore_inequality_ui <- renderUI({
  
  
  #includeScript(file %>% .path(r_path,"base/www/js/returnTextAreaBinding.js")),
  sidebarLayout(
    sidebarPanel(
      tags$div(class="sectionhead1", "Explore inequality"),
      uiOutput("focus_country_explore"), 
      uiOutput('focus_source_year_explore'),
      uiOutput("focus_indicator_explore"),
      uiOutput("focus_dimension_explore"),
      
      
      conditionalPanel(condition = "input.assessment_panel == 'datatable'",   #### output.owndata gopt from server.R
                       
                       
                       tags$div(class="sectionhead", "Table options"),
                       uiOutput('dataTableItems_explore'),
                       sliderInput('sumsigfig', 'Select estimate precision', min=0, max=5, value=1, round=T, width='100%')
                       
      ),
      conditionalPanel(condition = "input.assessment_panel == 'dataplot'",                        tags$div(class="sectionhead", "Graph options"),
                       uiOutput("disag_plot_type"),
                       conditionalPanel(condition="input.ai_plot_type == 'data_bar'", uiOutput("disag_plot_error_bars")),
                       uiOutput("disag_plot_dimensions_explore"),
                       
                       
                       
                       tags$span(class="control-label axis-range", "Select axis range"),
                       #                        conditionalPanel("input.ai_plot_type == 'data_line'",
                       #                                         textInputRow(inputId="axis_limitsmin1", label="Axis-min", value = NULL)
                       #                        ),
                       tags$div(class="axis-minmax",
                                textInputRow(inputId="axis_limitsmin1", label="Axis minimum ", value = NULL),
                                textInputRow(inputId="axis_limitsmax1", label="Axis maximum", value = NULL)
                       ),
                       tags$span(class="control-label", "Select graph names"),
                       checkboxInput(inputId='long_names1', label='Use long health indicator names', value = TRUE),
                       
                       tags$div(class="axis-title-label",
                                textInput(inputId = 'main_title1', label = 'Main title', value = .rdata[["plotDisag_explore_title"]]),
                                textInput(inputId = 'xaxis_title1', label = 'Horizontal axis title', value = ""),
                                textInput(inputId = 'yaxis_title1', label = 'Vertical axis title', value = "")
                       )
                       
      ),
      
      
      
      
      conditionalPanel(condition = "input.assessment_panel == 'sumtable'",
                       uiOutput("focus_summeasure_explore_summary_table"),
                       tags$div(class="sectionhead", "Summary measure options"),
                       uiOutput("summary_measures"),
                       tags$div(class="sectionhead", "Table options"),
                       uiOutput("dataTableItemsSummary_explore"),
                       sliderInput('sumsigfig2', 'Select estimate precision', min=0, max=5, value=1, round=T, width='100%')
                       
                       
      ),
      conditionalPanel(condition = "input.assessment_panel == 'sumplot'",
                       
                       uiOutput("focus_summeasure_explore_summary_plot"),
                       tags$div(class="sectionhead", "Graph options"),
                       uiOutput("summary_plot_type"),
                       uiOutput("summary_plot_error_bars"),
                       conditionalPanel(condition="input.summary_error_bars", uiOutput("summary_plot_CI_type")),
                       uiOutput("summary_plot_dimensions_explore"),
                       
                       
                       
                       #checkboxInput(inputId='long_names2', label='Long health indicator names', value = FALSE),
                       
                       tags$span(class="control-label", "Select axis range"),
                       #                        conditionalPanel("input.sumplot_type == 'data_line'",
                       #                        textInputRow(inputId="axis_limitsmin2", label="Axis-min", value = NULL)
                       #                        ),
                       tags$div(class="axis-minmax",
                                textInputRow(inputId="axis_limitsmin2", label="Axis minimum", value = NULL),
                                textInputRow(inputId="axis_limitsmax2", label="Axis maximum", value = NULL), 
                                tags$span(class="control-label", "Select graph names")
                       ),
                       
                       checkboxInput(inputId='long_names2', label='Use long health indicator names', value = TRUE),
                       tags$div(class="axis-title-label",                
                                textInput(inputId = 'main_title2', label = 'Main title', value = ""),
                                textInput(inputId = 'xaxis_title2', label = 'Horizontal axis title', value = ""),
                                textInput(inputId = 'yaxis_title2', label = 'Vertical axis title', value = "")
                       )   
                       
                       
      )
    ),# end sidebarpanel
    
    
    mainPanel(
      bsModal_alt(id="datatableModal_explore",
                  title = "Download data",
                  trigger = "btnDownloadDisagData_explore",
                  tags$p("The data in the table will be downloaded as a text file with the values separated
                       by a comma or a tab.  Select your preferred field separator and then download the data.
                         These can be opened in a text editor, or spreadsheet package."),
                  br(),
                  tags$p("Close the window once the download has commenced."),
                  br(),
                  radioButtons(inputId="filetype1", label='Field separator:',
                               choices=c("Comma separated valued" = "csv",
                                         "Tab separated values" = "tsv")),
                  downloadButton(outputId = 'btnStartDownloadDisagData_explore', label = "Start")),
      
    
      
      bsModal_alt(id = "dataplotModal_explore", title = "Download graph", trigger = "btnDownloadDisagPlot_explore", 
              #tags$p("Set the dimensions for the plot here and download it."),
              #br(),
              tags$p("Titles and axis labels are displayed according to your selections."),
              br(),
              tags$p("Close the window once the download has commenced."),
              br(),
              #textInput("disagPlotWitdth_explore", "Graph width (cm)", value="24" ),
              #textInput("disagPlotHeight_explore", "Graph height (cm)", value="24" ),
              selectInput(inputId="disagPlotType_explore", label='Output format:',
                          choices=c("PDF" = "PDF",
                                    "PNG" = "PNG",
                                    "JPG" = "JPG")),
              downloadButton(outputId = 'btnStartDownloadDisagPlot_explore', label = "Start")),
      
      bsModal_alt(id = "compdataDisagModal_explore", title = "Download data", trigger = "btnDownloadDisagPlotData_explore", 
              tags$p("The data in the table will be downloaded as a text file with the values separated
                     by a comma or a tab.  Select your preferred field separator and then download the data.
                     These can be opened in a text editor, or spreadsheet package."),
              br(),
              tags$p("Close the window once the download has commenced."),
              br(),
              radioButtons(inputId="filetype_explore_disag", label='Field separator:',
                           choices=c("Comma separated valued" = "csv",
                                     "Tab separated values" = "tsv")),
              downloadButton(outputId = 'btnStartDownloadDisagPlotData_explore', label = "Start")),
      
      
      
      bsModal_alt(id = "summtableModal_explore", title = "Download data", trigger = "btnDownloadSummaryData_explore", 
              tags$p("The summary measures in the table will be downloaded as a text file with the values
                                  separated by a comma or a tab.  Select your preferred field separator and then download
                                  the data.  These can be opened in a text editor, or spreadsheet package."),
              br(),
              tags$p("Close the window once the download has commenced."),
              br(),
              radioButtons(inputId="filetype2", label='Field separator:',
                           choices=c("Comma separated valued" = "csv",
                                     "Tab separated values" = "tsv")),
              downloadButton(outputId = 'btnStartDownloadSummaryData_explore', label = "Start")),
      
      bsModal_alt(id = "summplotModal_explore", title = "Download graph", trigger = "btnDownloadSummaryPlot_explore", 
              #tags$p("Set the dimensions for the plot here and download it."),
              # br(),
              tags$p("Titles and axis labels are displayed according to your selections."),
              br(),
              tags$p("Close the window once the download has commenced."),
              br(),
              #textInput("summaryPlotWitdth_explore", "Graph width (cm)", value="24" ),
              #textInput("summaryPlotHeight_explore", "Graph height (cm)", value="24" ),
              selectInput(inputId="summaryPlotType_explore", label='Output format:',
                          choices=c("PDF" = "PDF",
                                    "PNG" = "PNG",
                                    "JPG" = "JPG")),
              downloadButton(outputId = 'btnStartDownloadSummaryPlot_explore', label = "Start")),
      
      bsModal_alt(id = "compdataSummaryModal_explore", title = "Download data", trigger = "btnDownloadSummaryPlotData_explore", 
              tags$p("The data in the table will be downloaded as a text file with the values separated
                     by a comma or a tab.  Select your preferred field separator and then download the data.
                     These can be opened in a text editor, or spreadsheet package."),
              br(),
              tags$p("Close the window once the download has commenced."),
              br(),
              radioButtons(inputId="filetype_explore_summary", label='Field separator:',
                           choices=c("Comma separated valued" = "csv",
                                     "Tab separated values" = "tsv")),
              downloadButton(outputId = 'btnStartDownloadSummaryPlotData_explore', label = "Start")),
      
      
      
      tabsetPanel(id="assessment_panel",
                  tabPanel(HTML("<h6 style='text-align: center;'>Disaggregated data</br>(tables)<h6>"), value='datatable' , 
                           uiOutput('btnDownloadDisagData_explore'),
                           #uiOutput('dataWarning_disag_explore1'),
                           #                            tags$div(class="datawarning", 
                           #                                     helpText("If estimates are not shown for a selected combination of variables, then data are not available.")
                           #                                     ),
                           uiOutput('dataTable')
                           #dataTableOutput(outputId="dataTable")
                  ), 
                  tabPanel(HTML("<h6 style='text-align: center;'>Disaggregated data</br>(graphs)<h6>"), value='dataplot' ,
                           #uiOutput('btnDownloadDisagPlotData_explore'),
                           tags$div(class="button-holder",
                                    actionButton("btnDownloadDisagPlotData_explore", 
                                                 "Download data", 
                                                 class = "btn-primary", 
                                                 icon=icon("download"),
                                                 `data-toggle`= "modal",
                                                 `data-target`="#compdataDisagModal_explore" ),
                                    
                                    actionButton("btnDownloadDisagPlot_explore", 
                                                 "Download graph", 
                                                 class = "btn-primary", 
                                                 icon=icon("download"),
                                                 `data-toggle`= "modal",
                                                 `data-target`="#dataplotModal_explore")
                           ),
                           #uiOutput('dataWarning_disag_explore2'),
                           #                            tags$div(class="datawarning", 
                           #                                     helpText("If estimates are not shown for a selected combination of variables, then data are not available.")
                           #                            ),
                           #uiOutput('btnDownloadDisagPlot_explore'),
                           #uiOutput('testButton'),
                           uiOutput('disag_plot_explore')
                  ), 
                  tabPanel(HTML("<h6 style='text-align: center;'>Summary measures</br>(tables)<h6>"), value='sumtable' , 
                           uiOutput('btnDownloadSummaryData_explore'),
                           #                            tags$div(class="datawarning", 
                           #                                     helpText("If summary measures are not shown for a selected combination of variables, then data are not available or summary measures cannot be calculated. Note that only relevant summary measure are calculated for each dimension.")
                           #                            ),
                           uiOutput(outputId="dataTableInequal")
                  ),              
                  tabPanel(HTML("<h6 style='text-align: center;'>Summary measures</br>(graphs)<h6>"), value='sumplot' ,
                           #uiOutput('btnDownloadSummaryPlotData_explore'),
                           #uiOutput('btnDownloadSummaryPlot_explore'),
                           tags$div(class="button-holder",
                                    actionButton("btnDownloadSummaryPlotData_explore", "Download data", 
                                                 class = "btn-primary", 
                                                 icon=icon("download"),
                                                 `data-toggle` = "modal",
                                                 `data-target` = "#compdataSummaryModal_explore"),
                                    actionButton("btnDownloadSummaryPlot_explore", 
                                                 "Download graph",
                                                 class = "btn-primary", 
                                                 icon=icon("download"),
                                                 `data-toggle` = "modal",
                                                 `data-target` = "#summplotModal_explore")
                           ),
                           #                            tags$div(class="datawarning", 
                           #                                     helpText("If summary measures are not shown for a selected combination of variables, then data are not available or summary measures cannot be calculated. Note that only relevant summary measure are calculated for each dimension.")
                           #                            ),
                           uiOutput('summary_plot_explore')
                  )
      )#end tabsetPanel
      
    )#end mainPanel
  )
  
  
  
  
})