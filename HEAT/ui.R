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

shinyUI(
  
  
  
  tagList(
    tags$head(tags$link(rel="stylesheet", type="text/css",href="spacelab.min.css")),
    tags$head(tags$link(rel="stylesheet", type="text/css",href="style.css")),
    tags$head(tags$title("Health Equity Assessment Toolkit")),
    
    tags$head(HTML('<div id="myModal" class="modal fade">
    <div class="modal-dialog">
        <div class="modal-content">
            <div class="modal-header">
                <h4 class="modal-title">Terms of use and software license agreement</h4>
            </div>
            <div class="modal-body" >
            <div id="modal-license">
                
                </div>
                <button  class="btn btn-primary" data-dismiss="modal">I accept</button>
            </div>
        </div>
    </div>
</div>')),
    tags$head(HTML("<script type='text/javascript'>
                      $( document ).ready(function() {

      $('#myModal').modal('show');
      $('#modal-license').load('license_agreement.html');

                      });
                      </script>")),
    
    navbarPage(title =  HTML('<span class="navtitle"><a rel="home" href="#" title="World Health Organization"><img class="whoimg" src="who_logo_white40px.png"></a><span class="navtext">Health Equity Assessment Toolkit</span></span>'),
               id= "who_heat", 
               inverse=TRUE, 
               collapsible = TRUE,
               tabPanel("Home", includeHTML("www/landing_page.html")),
               tabPanel("Explore Inequality", #includeScript(file %>% .path(r_path,"base/www/js/returnTextAreaBinding.js")),
                        
                        # EXPLORE INEQUALITY UI, ORIGINALLY I USED RENDERUI BUT THIS WAS DEFINITELY SLOWER
                        
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
                                                      textInput(inputId = 'main_title1', label = 'Main title', value = "Health Equity Disaggregated"),
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
               ),
               tabPanel("Compare Inequality", 
                        
                        # COMPARE INEQUALITY UI, ORIGINALLY I USED RENDERUI BUT THIS WAS DEFINITELY SLOWER
                        
                        
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
                                                      textInput(inputId = 'main_title3', label = 'Main title', value = "Health Equity Summary"),
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
               ),
               navbarMenu("About",
                          
                          tabPanel(h6("User manual"), value='gloss_panel', includeHTML("www/manual.html")),
                          tabPanel(h6("Technical notes"), value='gloss_panel', includeHTML("www/technical.html")),
                          tabPanel(h6("Indicator compendium"), value='gloss_panel', includeHTML("www/compendium.html")),
                          tabPanel(h6("Software"), value='gloss_panel', includeHTML("www/software.html")),  
                          tabPanel(h6("License"), value='gloss_panel', includeHTML("www/license.html")),
                          tabPanel(h6("Feedback"), value='gloss_panel', includeHTML("www/feedback.html")),
                          tabPanel(h6("Acknowledgements"), value='gloss_panel', includeHTML("www/acknowledgement.html"))
                          
               )
               
               
               
               
               
    ) # end navbarpage
  )  # End shinyUi
)




