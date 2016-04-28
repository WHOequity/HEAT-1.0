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

output$compare_inequality_ui <- renderUI({
  
  
  
  sidebarLayout(
    sidebarPanel(
      tags$div(class="sectionhead1", "Compare inequality"),
      uiOutput("focus_country_compare"),
      uiOutput('focus_source_year_compare'),
      #uiOutput("focus_year_compare"),
      uiOutput("focus_indicator_compare"),
      uiOutput("focus_dimension_compare"),
      
      conditionalPanel(condition = "input.comparison_panel == 'inequalsum'",
                       uiOutput("focus_summeasure_compare_summary")),
      tags$div(class="sectionhead", "Benchmark options"),
      uiOutput("benchmarkWBgroup"),
      uiOutput("benchmarkWHOregion"),
      uiOutput("benchmark_countries"),
      uiOutput('benchmarkYears'),
      
      
      
      conditionalPanel(condition = "input.comparison_panel == 'inequaldisag'",
                       
                       tags$div(class="sectionhead", "Graph options"),
                       
                       # These sliders were originally in server_logic but were not getting
                       # loaded if the compare tab was clicked first. Seems that the split second
                       # that the condition was not TRUE caused the sliders not to show up
                       sliderInput('plot_height3', 'Select graph height', min=200, max=1500, value=650, step = 100,
                                   round = T,
                                   ticks = TRUE, animate = FALSE),
                       
                       sliderInput('plot_width3', 'Select graph width', min=200, max=1500, value=650, step = 100,
                                   round = T,
                                   ticks = TRUE, animate = FALSE),
                       tags$span(class="control-label axis-range", "Select axis range"),
                       tags$div(class="axis-minmax",
                                textInputRow(inputId="axis_limitsmin3", label="Axis minimum", value = NULL),
                                textInputRow(inputId="axis_limitsmax3", label="Axis maximum", value = NULL)
                       ),
                       tags$span(class="control-label", "Select graph names"),
                       
                       
                       #checkboxInput(inputId='long_names3', label='Use long health indicator names', value = TRUE),
                       tags$div(class="axis-title-label",
                                textInput(inputId = 'main_title3', label = 'Main title', value = .rdata[["plotDisag_compare_title"]]),
                                textInput(inputId = 'xaxis_title3', label = 'Horizontal axis title', value = ""),
                                textInput(inputId = 'yaxis_title3', label = 'Vertical axis title', value = "")
                       )
      ),
      conditionalPanel(condition = "input.comparison_panel == 'inequalsum'",
                       
                       
                       tags$div(class="sectionhead", "Graph options"),
                       uiOutput("summary_plot_dimensions_compare"),
                       tags$span(class="control-label", "Select axis range"),
                       
                       tags$div(class="axis-minmax-dtl",
                                textInputRow(inputId="xaxis_limitsmin4", label = "Horizontal axis minimum", value = NULL),
                                textInputRow(inputId="xaxis_limitsmax4", label = "Horizontal axis maximum", value = NULL),
                                textInputRow(inputId="yaxis_limitsmin4", label = "Vertical axis minimum", value = NULL),
                                textInputRow(inputId="yaxis_limitsmax4", label = "Vertical axis maximum", value = NULL)
                       ),
                       tags$span(class="control-label", "Select graph names"),
                       
                       
                       tags$div(class="axis-title-label",
                                textInput(inputId = 'main_title4', label = 'Main title', value = ""),
                                textInput(inputId = 'xaxis_title4', label = 'X-axis label', value = ""),
                                textInput(inputId = 'yaxis_title4', label = 'Y-axis label', value = "")
                       )
      )
      
      
    ),# end sidebarpanel
    
    
    mainPanel(
      bsModal_alt(id = "compdataModal_compare", title = "Download data", trigger = "btnDownloadDisagData_compare", 
                  tags$p("The data in the table will be downloaded as a text file with the values separated
                       by a comma or a tab.  Select your preferred field separator and then download the data.
               These can be opened in a text editor, or spreadsheet package."),
                  br(),
                  tags$p("Close the window once the download has commenced."),
                  br(),
                  radioButtons(inputId="filetype_benchmark", label='Field separator:',
                               choices=c("Comma separated valued" = "csv",
                                         "Tab separated values" = "tsv")),
                  downloadButton(outputId = 'btnStartDownloadDisagData_compare', label = "Start")),
      bsModal_alt(id = "compplot1Modal_compare", title = "Download graph", trigger = "btnDownloadDisagPlot_compare", 
                  #tags$p("Set the dimensions for the plot here and download it. "),
                  # br(),
                  tags$p("Titles and axis labels are displayed according to your selections."),
                  br(),
                  tags$p("Close the window once the download has commenced."),
                  br(),
                  #textInput("disagPlotWitdth_compare", "Graph width (cm)", value="24" ),
                  #textInput("disagPlotHeight_compare", "Graph height (cm)", value="24" ),
                  selectInput(inputId="disagPlotType_compare", label='Output format:',
                              choices=c("PDF" = "PDF",
                                        "PNG" = "PNG",
                                        "JPG" = "JPG")),
                  downloadButton(outputId = 'btnStartDownloadDisagPlot_compare', label = "Start", class = NULL)),
      bsModal_alt(id = "compdataDisagModal_compare", title = "Download data", trigger = "btnDownloadDisagPlotData_compare", 
                  tags$p("The data in the table will be downloaded as a text file with the values separated
                     by a comma or a tab.  Select your preferred field separator and then download the data.
                     These can be opened in a text editor, or spreadsheet package."),
                  br(),
                  tags$p("Close the window once the download has commenced."),
                  br(),
                  radioButtons(inputId="filetype_benchmark_disag", label='Field separator:',
                               choices=c("Comma separated valued" = "csv",
                                         "Tab separated values" = "tsv")),
                  downloadButton(outputId = 'btnStartDownloadDisagPlotData_compare', label = "Start")),
      bsModal_alt(id = "compplot2Modal_compare", title = "Download graph", trigger = "btnDownloadSummaryPlot_compare", 
                  #tags$p("Set the dimensions for the plot here and download it."),
                  #br(),
                  tags$p("Titles and axis labels are displayed according to your selections."),
                  br(),
                  tags$p("Close the window once the download has commenced."),
                  br(),
                  #textInput("summaryPlotWitdth_compare", "Graph width (cm)", value="24" ),
                  #textInput("summaryPlotHeight_compare", "Graph height (cm)", value="24" ),
                  selectInput(inputId="summaryPlotType_compare", label='Output format:',
                              choices=c("PDF" = "PDF",
                                        "PNG" = "PNG",
                                        "JPG" = "JPG")),
                  downloadButton(outputId = 'btnStartDownloadSummaryPlot_compare', label = "Start", class = NULL)),
      bsModal_alt(id = "compdataSummaryModal_compare", title = "Download data", trigger = "btnDownloadSummaryPlotData_compare", 
                  tags$p("The data in the table will be downloaded as a text file with the values separated
                     by a comma or a tab.  Select your preferred field separator and then download the data.
                     These can be opened in a text editor, or spreadsheet package."),
                  br(),
                  tags$p("Close the window once the download has commenced."),
                  br(),
                  radioButtons(inputId="filetype_benchmark_summary", label='Field separator:',
                               choices=c("Comma separated valued" = "csv",
                                         "Tab separated values" = "tsv")),
                  downloadButton(outputId = 'btnStartDownloadSummaryPlotData_compare', label = "Start")),
      tabsetPanel(id = "comparison_panel", 
                  
                  tabPanel(HTML("<h6 style='text-align: center;'>Disaggregated data</br>(graphs)<h6>"), value='inequaldisag', 
                           tags$div(class="button-holder",
                                    actionButton("btnDownloadDisagPlotData_compare", 
                                                 "Download data", 
                                                 class = "btn-primary", 
                                                 icon=icon("download"),
                                                 `data-toggle`="modal",
                                                 `data-target`="#compdataDisagModal_compare"),
                                    actionButton("btnDownloadDisagPlot_compare", 
                                                 "Download graph", 
                                                 class = "btn-primary", 
                                                 icon=icon("download"),
                                                 `data-toggle`="modal",
                                                 `data-target`="#compplot1Modal_compare")
                           ),
                           
                           uiOutput('disag_plot_compare')
                  ),
                  tabPanel(HTML("<h6 style='text-align: center;'>Summary measures</br>(graphs)<h6>"), value='inequalsum', 
                           # Plot points (default) or country codes on the Comparison Summary Plot
                           tags$div(class="button-holder",
                                    actionButton("btnDownloadSummaryPlotData_compare", 
                                                 "Download data", 
                                                 class = "btn-primary", 
                                                 icon=icon("download"),
                                                 `data-toggle`="modal",
                                                 `data-target`="#compdataSummaryModal_compare"),
                                    actionButton("btnDownloadSummaryPlot_compare", 
                                                 "Download graph", 
                                                 class = "btn-primary", 
                                                 icon=icon("download"),
                                                 `data-toggle`="modal",
                                                 `data-target`="#compplot2Modal_compare")
                           ),
                           checkboxInput(inputId='points_ccode', 'Show country codes', value=FALSE),
                           #                            uiOutput('btnDownloadSummaryPlotData_compare'),
                           #                            uiOutput('btnDownloadSummaryPlot_compare'),
                           uiOutput('summary_plot_compare')
                           #div(class="container-fluid", style="overflow:visible;height:800px;", plotOutput('summary_plot_compare'))
                  )
      )#endtabsetpanel
      
      
    )# end mainPanel
  )# end sidebarLayout
  
  
  
  
})