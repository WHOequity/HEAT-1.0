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

######### plotter.R                                                                         #
# This file manages the six different kinds of plot used in the equity analysis tool kit.   #
# Plot 1: Barchart for a single country (Disaggregation of data)                            #
# Plot 2: Horizontal line chart for a single country (Disaggregation of data)               #
# Plot 3: Barchart for a single country (Summary of data)                                   #
# Plot 4: Slopped line chart for a single country (Summary of data)                         #
# Plot 5: Horizontal line chart for multiple countries (Disaggregation of data)             #
# Plot 6: Scatter plot for multiple countries (Summary of data)                             #
#############################################################################################


library(ggplot2)
library(grid)
library(RColorBrewer)


heat_theme <- function(){
  
  
  theme(
    
    axis.title.y = element_text(vjust=1.5),
    strip.text = element_text(colour = "black", angle = 0, size = 13),
    strip.text.x = element_text(hjust = 0.5, vjust = 0.65),
    strip.text.y = element_text(hjust = 0.5, vjust = 0.5, angle=0),
    axis.text.x = element_text(size=12),
    axis.text.y = element_text(size=12),
    #axis.line = element_blank(),
    legend.margin = unit(0.2, "cm"),
    legend.text = element_text(size=10),
    panel.margin = unit(0.75, "cm"),
    legend.title=element_blank(),
    legend.key = element_rect(fill = "white"),
    legend.position="bottom",
    axis.ticks = element_line(color="grey70"),
    legend.key.size = unit(0.9, "lines"), # space between legend items
    #plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")
    plot.title = element_text(vjust=2)
    
  )
}





#######PLOT 1
plotDisagBar_explore <- function(plotData, session){
  # Plot 1: Barchart for a single country (Disaggregation of data)
  
  # this is here to trigger a change if the title changes
  if(is.null(input$disag_error_bars)) return()
  input$main_title1
  
  xtitle_val <- ifelse(!is.null(input$xaxis_title1) && input$xaxis_title1 !="", input$xaxis_title1, "")
  ytitle_val <- ifelse(!is.null(input$yaxis_title1) && input$xaxis_title1 !="", input$yaxis_title1, "")
  maintitle_val <- .rdata[["plotDisag_explore_title"]]
  
  axismin <- isolate(input$axis_limitsmin1)
  axismax <- isolate(input$axis_limitsmax1)
  
  
  maintitle_val <- formatLabels(maintitle_val, .rdata[["numTitleChars"]], fixed=FALSE)
  
  
  
  numyears<-length(unique(plotData$year))
  binwidth <- ifelse(numyears>2, 0.75, 0.35)
  textwidth <- ifelse(numyears>2, 0.9, 0.35)
  
  
  
  form <- ifelse(input$long_names1, "indic_name ~ dimension", "indic ~ dimension")
  
  
  dat1 <- filter(plotData, dimension!=.rdata[['geo_dimension']])
  dat2 <- filter(plotData, dimension==.rdata[['geo_dimension']])
  
  dat1info<-select(dat1, subgroup, colors, shapes) %>% distinct
  
  cols<-NULL
  shapes<-NULL
  breaks<-NULL
  labels<-NULL
  if(nrow(dat1)>0){
    cols <- c(cols, dat1info$colors)
    shapes <- c(shapes, dat1info$shapes)
    breaks <- c(breaks, unique(as.character(dat1$subgroup)))
    labels <- c(labels, unique(as.character(dat1$subgroup)))
  } 
  
  if(nrow(dat2)>0){
    cols <- c(cols, dat2$colors)
    shapes <- c(shapes, dat2$shapes)
    breaks <- c(breaks, as.character(dat2$subgroup[1]))
    labels <- c(labels, .rdata[['geo_dimension']])
  } 
  
  
  
  mydat<-rbind(dat1, dat2)
  mydat$subgroup<-factor(mydat$subgroup, levels=unique(mydat$subgroup))
  
  
  
  p<-ggplot() +
    geom_bar(data=mydat, aes(x = as.factor(year), weight = estimate, fill=subgroup, y=estimate), 
             position=position_dodge(), stat="identity", color='white', width=binwidth)+
    geom_text(data=plotData, aes(x = as.factor(year), y=estimate,fill=subgroup, 
                                 label=round(estimate),ymin=lower_95ci, ymax=upper_95ci), 
              position=position_dodge(width=binwidth), vjust=-0.75,
              size=3, color="grey50", alpha=!input$disag_error_bars)+
    geom_errorbar(data=plotData, aes(x = as.factor(year), y=estimate,fill=subgroup, 
                                     label=round(estimate),ymin=lower_95ci, ymax=upper_95ci), color="grey50",  
                  position=position_dodge(width=binwidth), width=0.25, alpha=input$disag_error_bars) + 
    labs(x = xtitle_val, y=ytitle_val, title=maintitle_val)+
    #ylim(as.integer(input$axis_limitsmin1),  as.integer(input$axis_limitsmax1))+
    heat_theme()+
    scale_fill_manual(breaks=breaks, 
                      values=cols, name="",
                      labels=labels)
  # guides(fill = guide_legend(nrow=2,byrow=TRUE, override.aes = list(linetype=0)))
  
  p <- p+
    facet_grid(form, scale="free_y",  labeller = splitLabels)+
    labs(x = xtitle_val, y=ytitle_val, title=maintitle_val)+
    heat_theme()+
    guides(fill = guide_legend(nrow=5,byrow=FALSE, override.aes = list(linetype=0)))
  
  
  
  
  if(axismin!="" | axismax != "") {
    
    pbuild<-ggplot_build(p)
    axisrange<-pbuild$panel$ranges[[1]]$y.range
    axismin<-ifelse(axismin=="", axisrange[1], as.numeric(axismin))
    axismax<-ifelse(axismax=="", axisrange[2], as.numeric(axismax))
    p <- p + coord_cartesian(ylim=c(axismin, axismax))
    
  }
  
  
  
  return(p)
}



#######PLOT 2
plotDisagLine_explore <- function(plotData, session){
  #  Plot 2: Horizontal line chart for a single country (Disaggregation of data)

  
  # this is here to trigger a change if the title changes
  input$main_title1
  
  
  
  
  xtitle_val <- ifelse(!is.null(input$xaxis_title1) && input$xaxis_title1 !="", input$xaxis_title1, "")
  ytitle_val <- ifelse(!is.null(input$yaxis_title1) && input$yaxis_title1 !="", input$yaxis_title1, "")
  maintitle_val <- .rdata[["plotDisag_explore_title"]]
  
  axismin <- isolate(input$axis_limitsmin1)
  axismax <- isolate(input$axis_limitsmax1)
  
  
  
  maintitle_val <- formatLabels(maintitle_val, .rdata[["numTitleChars"]])
  
  
  
  plotData <- arrange(plotData, dimension, order)
  
  dat1 <- filter(plotData, dimension!=.rdata[['geo_dimension']])
  dat2 <- filter(plotData, dimension==.rdata[['geo_dimension']])
  
  mydat<-rbind(dat1, dat2)
  mydat$subgroup<-factor(mydat$subgroup, levels=unique(mydat$subgroup))
  
  
  dat1info<-select(dat1, subgroup, colors, shapes) %>% distinct
  
  #browser()
  
  cols<-NULL
  shapes<-NULL
  breaks<-NULL
  labels<-NULL
  alpha <- NULL
  if(nrow(dat1)>0){
    cols <- c(cols, dat1info$colors)
    shapes <- c(shapes, dat1info$shapes)
    breaks <- c(breaks, unique(as.character(dat1$subgroup)))
    labels <- c(labels, unique(as.character(dat1$subgroup)))
    alpha <- c(alpha, rep(0.75, nrow(dat1info)))
  } 
  
  if(nrow(dat2)>0){
    cols <- c(cols, dat2$colors)
    shapes <- c(shapes, dat2$shapes)
    breaks <- c(breaks, as.character(dat2$subgroup[1]))
    labels <- c(labels, .rdata[['geo_dimension']])
    alpha <- c(alpha, rep(0.4, nrow(dat2)))
    
  } 
  
  
  p<-ggplot(data = mydat, aes(x=estimate, y=as.factor(year)))+
    geom_line() +
    geom_point(aes(fill=subgroup, shape=subgroup, alpha=subgroup), color=NA, size=4) +
    scale_shape_manual(values= shapes, 
                       breaks=breaks,  
                       name="", 
                       labels=labels)+
    scale_fill_manual(breaks=breaks, 
                      values = cols,
                      name="",
                      labels=labels) +
    scale_alpha_manual(values=alpha, guide = 'none')
  
  
  
  form <- ifelse(input$long_names1, "indic_name ~ dimension", "indic ~ dimension")
  
  
  p <- p+
    facet_grid(form, scale="free_y", space = "free_y", labeller = splitLabels)+
    labs(x = xtitle_val, y=ytitle_val, title=maintitle_val)+
    
    heat_theme()+
    guides(shape = guide_legend(nrow=5,byrow=FALSE))
  
  
  
  if(axismin!="" | axismax != "") {
    
    pbuild<-ggplot_build(p)
    axisrange<-pbuild$panel$ranges[[1]]$x.range
    axismin<-ifelse(axismin=="", axisrange[1], as.numeric(axismin))
    axismax<-ifelse(axismax=="", axisrange[2], as.numeric(axismax))
    p <- p + coord_cartesian(xlim=c(axismin, axismax))
    
  }
  
  
  
  return(p)
}


#### PLOT 3
plotSummaryBar_explore <- function(plotData, session){
  # Plot 3: Barchart for a single country (Summary data)
  
  input$main_title2
  
  sumMeasure <- plotData$measure[1]
  measureName <- gsub(" \\(.*\\)", "", plotData$measure_name[1])
  
  logscale <- plotData$logscale[1]
  
  yrs <-paste(sort(unique(plotData$year)), collapse=", ")
  sources <- paste(sort(unique(plotData$source)), collapse=" & ")
  
  xtitle_val <- ifelse(!is.null(input$xaxis_title2) && input$xaxis_title2 !="", input$xaxis_title2, "")
  ytitle_val <- ifelse(!is.null(input$yaxis_title2) && input$yaxis_title2 !="", 
                       input$yaxis_title2, paste0("Inequality (", measureName, ")"))
  
  
  maintitle_val <- .rdata[["plotSummary_explore_title"]]
  
  maintitle_val <- formatLabels(maintitle_val, .rdata[["numTitleChars"]])
  
  axismin <- isolate(input$axis_limitsmin2)
  axismax <- isolate(input$axis_limitsmax2)
  numyears <- length(unique(plotData$year))
  
  
  binwidth <- ifelse(numyears>2, 0.75, 0.35)
  errwidth <- ifelse(numyears>2, 0.5, 0.25)
  
  
  #print("plotFigure3() in plotter.R")
  # Make sure that the data frame includes all the possible bars by adding missing data across the factor levels
  plotData <- plotData[,c('year', 'indic_name','indic', "dimension",'inequal', 
                          'se.lowerci', 'se.upperci', "boot.lowerci", "boot.upperci")]
  
  
  #probably I need this but not clear why
  allposs <-  expand.grid (year = unique(plotData$year), 
                           indic = unique(plotData$indic),
                           indic_name = unique(plotData$indic_name),
                           dimension = unique(plotData$dimension), stringsAsFactors = FALSE)
  
  plotData <- left_join(allposs, plotData, by=c("year", "indic", "indic_name", "dimension"))
  plotData$year <- as.factor(plotData$year)
  
  
  indictype <- ifelse(input$long_names2, "indic_name", "indic")
  form <- ifelse(input$long_names2, "indic_name ~ dimension", "indic ~ dimension")
  
  ifelse(is.null(input$summary_CI_type) || input$summary_CI_type == "analytic", 
         ci_type <-  c("se.lowerci", "se.upperci"), 
         ci_type <-  c("boot.lowerci", "boot.upperci"))
  
  
  p <- ggplot(plotData, aes_string(x = "year", weight = "inequal", fill=indictype,ymin=ci_type[1], ymax=ci_type[2]))+ 
    geom_text(data=plotData, aes_string(x = "year", y="inequal",fill=indictype, 
                                        label="format(round(inequal,1),nsmall=1)",ymin=ci_type[1], ymax=ci_type[2]), 
              position=position_dodge(width=binwidth), vjust=-0.75,
              size=3, color="grey50", alpha=!input$summary_error_bars)+
    geom_bar(aes(y=inequal), position=position_dodge(), stat="identity", color='white', 
             size=1, width=binwidth)+
    geom_errorbar(color="grey50",  
                  position=position_dodge(0.9), width=0.25, alpha=input$summary_error_bars)+
    labs(x = xtitle_val, y=ytitle_val, title=maintitle_val)+
    guides(colour=FALSE, fill=FALSE)+ 
    heat_theme() + 
    facet_grid(form, labeller = splitLabels)
  
  
  
  
  if(axismin!="" | axismax != "") {
    
    pbuild<-ggplot_build(p)
    axisrange<-pbuild$panel$ranges[[1]]$y.range
    axismin<-ifelse(axismin=="", axisrange[1], as.numeric(axismin))
    axismax<-ifelse(axismax=="", axisrange[2], as.numeric(axismax))
    p <- p + coord_cartesian(ylim=c(axismin, axismax))
    
  }
  
  if(logscale==1){
    if(any(c(axismin, axismax)<=0)) p <- p + coord_cartesian()
    brk <- c(1,unique(round(pretty(c(plotData$se.lowerci, plotData$se.upperci), n=5))))
    p <- p + scale_y_log10(breaks=brk, labels=brk) +
      ylab(gsub(")", ", axis log-scaled)", ytitle_val))
  }
  
  
  return(p)
}



####PLOT 4
plotSummaryLine_explore <- function(plotData, session){
  #  Plot 4: Line chart for a single country (Disaggregation of data)
  

  
  input$main_title2
  sumMeasure <- plotData$measure[1]
  measureName <- gsub(" \\(.*\\)", "", plotData$measure_name[1])
  
  logscale <- plotData$logscale[1]
  
  yrs <-paste(sort(unique(plotData$year)), collapse=", ")
  sources <- paste(sort(unique(plotData$source)), collapse=" & ")
  
  
  xtitle_val <- ifelse(!is.null(input$xaxis_title2) && input$xaxis_title2 !="", input$xaxis_title2, "")
  ytitle_val <- ifelse(!is.null(input$yaxis_title2) && input$yaxis_title2 !="", 
                       input$yaxis_title2, paste0("Inequality (", measureName, ")"))
  
  maintitle_val <-  .rdata[["plotSummary_explore_title"]]
  
  
  maintitle_val <- formatLabels(maintitle_val, .rdata[["numTitleChars"]])
  
  
  
  
  axismin <- isolate(input$axis_limitsmin2)
  axismax <- isolate(input$axis_limitsmax2)
  
  
  plotData$year <- as.integer(plotData$year)
  
  
  
  numyears<-length(unique(plotData$year))
  errwidth <- ifelse(numyears>2, 0.5, 0.25)
  
  if(numyears==1){
    
    tmp1 <- plotData
    tmp1$year <- tmp1$year+1
    tmp1$inequal <- NA
    tmp1$se.lowerci<-NA
    tmp1$se.upperci<-NA
    tmp1$boot.lowerci<-NA
    tmp1$boot.upperci<-NA
    
    tmp2 <- plotData
    tmp2$year <- tmp1$year-2
    tmp2$inequal <- NA
    tmp2$se.lowerci<-NA
    tmp2$se.upperci<-NA
    tmp2$boot.lowerci<-NA
    tmp2$boot.upperci<-NA
    
    
    plotData<-rbind(plotData, tmp1, tmp2)
    
    
  }
  

  
  
  ifelse(is.null(input$summary_CI_type) || input$summary_CI_type == "analytic", 
         ci_type <-  c("se.lowerci", "se.upperci"), 
         ci_type <-  c("boot.lowerci", "boot.upperci"))
  
  
  indictype <- ifelse(input$long_names2, "indic_name", "indic")
  p<-ggplot(plotData, aes_string(x="year", y="inequal", group=indictype, color=indictype))+
    geom_line(size=1) +
    geom_point(size=4)+
    geom_errorbar(aes_string(ymin=ci_type[1], ymax=ci_type[2]),   width=errwidth, alpha=input$summary_error_bars)+
    labs(x=xtitle_val, y=ytitle_val, title=maintitle_val)+
    scale_x_continuous(breaks=sort(unique(plotData$year)))+
    guides(col=guide_legend(ncol=1)) +
    heat_theme() +  
    #coord_cartesian(xlim=c(as.numeric(input$axis_limitsmin2), as.numeric(input$axis_limitsmax2)))+
    #ylim(as.numeric(input$axis_limitsmin2),  as.numeric(input$axis_limitsmax2))+
    facet_grid(dimension~.)
  
  
  if(axismin!="" | axismax != "") {
    
    pbuild<-ggplot_build(p)
    axisrange<-pbuild$panel$ranges[[1]]$y.range
    axismin<-ifelse(axismin=="", axisrange[1], as.numeric(axismin))
    axismax<-ifelse(axismax=="", axisrange[2], as.numeric(axismax))
    p <- p + coord_cartesian(ylim=c(axismin, axismax))
    
  }

  if(logscale==1){
    
    if(any(c(axismin, axismax)<=0)) p <- p + coord_cartesian()
    brk <- unique(c(1,round(pretty(c(plotData$se.lowerci, plotData$se.upperci), n=5))))
    brk <- brk[brk>0]
    p <- p + geom_hline(yintercept=1, alpha=0)+scale_y_log10(breaks=brk, labels=brk) +
      ylab(gsub(")", ", axis log-scaled)", ytitle_val))
    
    
  }
  
  
  
  
  return(p)
}



plotDisagLine_compare <- function(plotData, session){
  #  Plot 5: Horizontal line chart for benchmark countries (Disaggregation of data)
  
  
  input$main_title3


  xtitle_val <- ifelse(!is.null(isolate(input$xaxis_title3)) && 
                         isolate(input$xaxis_title3) !="", isolate(input$xaxis_title3), "")
  ytitle_val <- ifelse(!is.null(isolate(input$yaxis_title3)) && 
                         isolate(input$yaxis_title3) !="", isolate(input$yaxis_title3), "")
  
  
  
  plotData$country <- paste0(plotData$country, " (", plotData$source, " ", plotData$year, ")")
  plotData <- dplyr::arrange(plotData, anchor, desc(country))
  plotData <- mutate(plotData,
                     country = factor(country, levels=unique(country)))
  
  
  maintitle_val <- .rdata[['plotDisag_compare_title']]
  
  
  
  maintitle_val <- formatLabels(maintitle_val, .rdata[["numTitleChars"]])
  
  axismin <- isolate(input$axis_limitsmin3)
  axismax <- isolate(input$axis_limitsmax3)
  
  #p <- ggplot(plotData, aes(estimate, country)) + geom_line()
  
  datainfo<-select(plotData, subgroup, colors, shapes) %>% distinct
  
  indictype <- "indic_name"
  p<-ggplot(plotData, aes(estimate, country)) + geom_line()
  
  
  if(plotData$dimension[1] != .rdata[['geo_dimension']]){
    p <- p + geom_point(aes(fill=subgroup, shape=subgroup), size=4, color=NA)
    p<- p+ scale_shape_manual(values= datainfo$shapes, 
                              breaks=datainfo$subgroup,  
                              name="", 
                              labels=datainfo$subgroup)+
      scale_fill_manual(breaks=datainfo$subgroup, 
                        values = datainfo$colors,
                        name="",
                        labels=datainfo$subgroup)+
      guides(shape = guide_legend(nrow=5,byrow=FALSE))
    
  }
  
  
  if(plotData$dimension[1] == .rdata[['geo_dimension']]){
    
    p <- p + geom_point(aes(fill=subgroup), size=4, alpha=0.5, color=datainfo$colors[1])+
      scale_fill_manual(breaks=datainfo$subgroup[1], 
                        values = datainfo$colors,
                        name="",
                        labels=.rdata[['geo_dimension']])
    
  }
  
  
  
  p<-p+
    labs(x=xtitle_val, y=ytitle_val, title=maintitle_val)+
    facet_grid(paste0(".~", indictype), labeller = splitLabelsWide)+
    heat_theme()+theme(legend.justification = 'right',
                       strip.text = element_blank(),
                       strip.background = element_rect(fill="white"))
  

  
  if(axismin!="" | axismax != "") {
    
    pbuild<-ggplot_build(p)
    axisrange<-pbuild$panel$ranges[[1]]$x.range
    axismin<-ifelse(axismin=="", axisrange[1], as.numeric(axismin))
    axismax<-ifelse(axismax=="", axisrange[2], as.numeric(axismax))
    p <- p + coord_cartesian(xlim=c(axismin, axismax))
    
  }
  
  
  
  
  
  
  return(p)
  
}




plotSummaryScatter_compare <- function(plotData, session){
  #  Plot 6: Scatter plot showing benchmark countries National Average against inequality
  
  input$main_title4
  
  sumMeasure <- plotData$measure[1]
  
  measureName <- gsub(" \\(.*\\)", "", plotData$measure_name[1])
  
  logscale <- plotData$logscale[1]
  
  xtitle_val <- ifelse(!is.null(isolate(input$xaxis_title4)) && isolate(input$xaxis_title4) !="", isolate(input$xaxis_title4), "National average")
  ytitle_val <- ifelse(!is.null(isolate(input$yaxis_title4)) && isolate(input$yaxis_title4) !="", 
                       input$yaxis_title4, paste0("Inequality (", measureName, ")"))
  
  Xaxismin <- isolate(input$xaxis_limitsmin4)
  Xaxismax <- isolate(input$xaxis_limitsmax4)
  Yaxismin <- isolate(input$yaxis_limitsmin4)
  Yaxismax <- isolate(input$yaxis_limitsmax4)
  
  
  if(sum(plotData$anchor==0) == 0){
       # this dummy is added with NA data except for the benchmark
    # so that the legend and color-coding works in the plot
    
    anchordummy <- plotData[1,]
    anchordummy[, names(anchordummy)!="anchor"]  <- NA
    anchordummy$anchor <- 0
    plotData <- rbind(plotData, anchordummy)
  }
  
  
  
  plotPch <- c(19, 15)
  plotPalette <- c('#00616B', '#6B0A00')
  
  plotData <- plotData[order(plotData$anchor), ]
  
  maintitle_val <- .rdata[['plotSummary_compare_title']]
  
  
  maintitle_val <- formatLabels(maintitle_val, .rdata[["numTitleChars"]])
  
  p <- ggplot(plotData, aes(x=estimate, y=inequal), color=plotPalette)+
    labs(x=xtitle_val, y=ytitle_val, title=maintitle_val)
  
  if(!input$points_ccode){  # Plot dots, or Country codes
    p <- p + geom_point(aes(color=as.factor(anchor), shape=as.factor(anchor)), size=4)
    
    
    
  } else {
    
    p <- p + geom_text(aes(label=ccode, color=as.factor(anchor)), 
                       hjust=0.5, vjust=0.5, size=3.5, show_guide=FALSE)+
      geom_line(aes(color=as.factor(anchor)), size=0, alpha=0)+
      guides(colour = guide_legend(override.aes = list(size=2, alpha=1)))+heat_theme()
    
    
    
  }
  
  p <- p + scale_shape_manual(name  ="",
                              breaks=c(0, 1),
                              labels=c("Benchmark countries", plotData$country[plotData$anchor==1][1]),
                              values=plotPch)
  
  p <- p + scale_colour_manual(name  ="",
                               breaks=c(0, 1),
                               labels=c("Benchmark countries", plotData$country[plotData$anchor==1][1]),
                               values=plotPalette)  
  
  
  
  # if the user makes changes to any of the axes

  if(any(c(Xaxismin, Xaxismax, Yaxismin, Yaxismax)!="")) {
    
    pbuild<-ggplot_build(p)
    xaxisrange<-pbuild$panel$ranges[[1]]$x.range
    Xaxismin<-ifelse(Xaxismin=="", xaxisrange[1], as.numeric(Xaxismin))
    Xaxismax<-ifelse(Xaxismax=="", xaxisrange[2], as.numeric(Xaxismax))
    yaxisrange<-pbuild$panel$ranges[[1]]$y.range
    Yaxismin<-ifelse(Yaxismin=="", yaxisrange[1], as.numeric(Yaxismin))
    Yaxismax<-ifelse(Yaxismax=="", yaxisrange[2], as.numeric(Yaxismax))
    
    
    p <- p + coord_cartesian(xlim=c(Xaxismin, Xaxismax), ylim=c(Yaxismin, Yaxismax))

  }

  
  p<-p+heat_theme()
  
  if(logscale==1){
    
    if(any(c(Yaxismin, Yaxismax)<=0)) p <- p + coord_cartesian()
    brk <- sort(unique(c(1,unique(round(pretty(c(plotData$inequal), n=5))))))
    brk <- brk[brk!=0]
    p <- p + geom_hline(yintercept=1, alpha=0) + scale_y_log10(breaks=brk, labels=brk) +
      ylab(gsub(")", ", axis log-scaled)", ytitle_val))
  }
  
  
  
  p
}




