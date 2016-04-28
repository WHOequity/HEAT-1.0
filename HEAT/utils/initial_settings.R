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

# *****************************************************************************
# EDIT AS NEEDED: Choose the initial settings here ----
# *****************************************************************************
focus_country <- "Indonesia"
focus_dimension <- c("Economic status")
focus_year <- c(2012, 2007, 2002, 1997)
focus_indicator <- c("sba")
focus_summary_measure <- "d"


# *****************************************************************************
# EDIT AS NEEDED: Choose items to drop ----
# *****************************************************************************

drop_country <- c()                # e.g., c("Armenia", "Afghanistan")
drop_indicator <- c()              # e.g., c("anc1", 'asfr1')
drop_summary_measure <- c() # e.g., c('riikm', 'mld')

# -----

drop_dimension <- c() # DON'T EDIT THIS ONE


# *****************************************************************************
# EDIT AS NEEDED: Restrict to these regions ----
# *****************************************************************************

# to keep all regions use: c()
keep_region <- c() # to limit to Americas use keep_region <- c("Americas")


# *****************************************************************************
# EDIT AS NEEDED: Number of characters in the title ----
# *****************************************************************************


numTitleChars <- 50



# *****************************************************************************
# EDIT AS NEEDED: Alter color palettes ----
# *****************************************************************************

# ----- these are the original settings
# sex_palette <- c('#ad3c3c', '#ce0c0c')
# econ_palette <- colorRampPalette(c("#D8BD35", '#04660a'))
# educ_palette <- colorRampPalette(c('#f7aa4c', '#630202'))
# place_palette <- colorRampPalette(c('#3CB7CC', '#39737C'))
# geo_palette <- '#00008B'


sex_palette <- c('#ad3c3c', '#ce0c0c')
econ_palette <- colorRampPalette(c("#D8BD35", '#04660a'))
educ_palette <- colorRampPalette(c('#f7aa4c', '#630202'))
place_palette <- colorRampPalette(c('#3CB7CC', '#39737C'))
geo_palette <- '#00008B'




# *****************************************************************************
# DO NOT EDIT BELOW
# *****************************************************************************



.rdata<<-list()
.rdata[['the_plot']] <<- 1


# *****************************************************************************
# Plot titles
# *****************************************************************************

.rdata[["plotDisag_explore_title"]] <<- "Health Equity Disaggregated"
.rdata[["plotSummary_explore_title"]] <<- "Health Equity Summary"
.rdata[["plotDisag_compare_title"]] <<- "Health Equity Disaggregated"
.rdata[["plotSummary_compare_title"]] <<-"Health Equity Summary"


# *****************************************************************************
# MIGHT BE ABLE TO DELETE THIS
# *****************************************************************************

.rdata[['data_warning']] <<-"If estimates are not shown for a selected combination of variables, then data are not available."
table_options <<- NULL
.rdata[['table_options']] <<-list(pageLength = 100, dom='frtp', scrollX = TRUE, columnDefs = list(list(width = '400px', targets = c(3)))) 
#.rdata[['table_options']][['columnDefs']] <<- NULL


# *****************************************************************************
# An indicator for whether it is the initial app load
# *****************************************************************************

.rdata[['first_time']] <<- TRUE
#.rdata[['plot_height']] <<- 600



# *****************************************************************************
# The focus indicator
# *****************************************************************************
.rdata[['focus_indicator']]<<-focus_indicator


# *****************************************************************************
# Read in summary measures and order
# *****************************************************************************
summeasure <- readRDS("../../Data/summeasures.RDS") 
summeasure <- arrange(summeasure, measure_name)

# ----- DROP SUMMARY MEASURES from above
summeasure <- filter(summeasure, !measure_abbr%in%drop_summary_measure)

summeasurevect <- summeasure$measure_abbr
names(summeasurevect) <- paste0(summeasure$measure_name, " (", summeasure$measure_abbr, ")")

.rdata[['summary_measures_all']]<<- summeasurevect
.rdata[['summary_measures_table']]<<-select(summeasure, measure_name, measure_abbr, logscale)


# *****************************************************************************
# Read in country information
# *****************************************************************************


.rdata[['countrynames']]<<-readRDS("../../Data/countrynames.RDS")

regions <- unique(.rdata[['countrynames']]$whoreg6_name)

# ----- DROP countrynames
if(is.null(keep_region) || keep_region == ""){
  drop_country <- drop_country
}else{
  
  drop_region<-regions[!regions%in%keep_region]
  drop_country_xtra <- filter(.rdata[['countrynames']], whoreg6_name%in%drop_region) %>% .$country
  drop_country <- c(drop_country, drop_country_xtra)
}


.rdata[['countrynames']] <<- filter(.rdata[['countrynames']], !country%in%drop_country)

#if(!is.null(keep_region) && keep_region != "") .rdata[['countrynames']] <<- filter(.rdata[['countrynames']], whoreg6_name%in%keep_region)

.rdata[['all_countries']]<<-sort(.rdata[['countrynames']]$country)


# *****************************************************************************
# Read in strata information
# *****************************************************************************


strata <- readRDS("../../Data/strata.RDS")
.rdata[['strata']]<<-filter(strata, !indic%in%drop_indicator, !dimension%in%drop_dimension,
                            !country%in%drop_country)

rm(strata)



if(!is.null(.rdata[['strata']])){
  tmpindic <- select(.rdata[['strata']], indic_name, indic) %>% distinct %>% 
    arrange(indic_name)
  
  #print(dim(tmpindic)) should be 37
   namestmpindic <- tmpindic$indic_name
tmpindic <- tmpindic$indic
names(tmpindic)<-namestmpindic
  
}
.rdata[['full_indicators']] <<- tmpindic


# *****************************************************************************
# Set focus country, dimension and year
# *****************************************************************************

.rdata[['focus_country']]<<-focus_country

.rdata[['focus_dimension']]<<-focus_dimension
.rdata[['focus_year']]<<-focus_year




# *****************************************************************************
# Read in dimensions
# *****************************************************************************

dimensions <- readRDS("../../Data/dimensions.RDS")
.rdata[['dimension_details']] <<- filter(dimensions, !dimension%in%drop_dimension)
rm(dimensions)



.rdata[['equity_dimensions']] <<- sort(unique(.rdata[['dimension_details']]$dimension))
.rdata[['geo_dimension']] <<- .rdata[['dimension_details']]$dimension[.rdata[['dimension_details']]$dimension_type=="region"][1]


# *****************************************************************************
# Read in maindata, inequality data and national data
# *****************************************************************************

.rdata[['maindata']]<<-readRDS("../../Data/maindata.RDS")
.rdata[['inequals']]<<-readRDS("../../Data/inequals.RDS")
.rdata[['nationaldata']]<<-readRDS("../../Data/nationaldata.RDS")

# ----- DROP indicators, dimensions and countries as specified at top

.rdata[['maindata']]<<-filter(.rdata[['maindata']], !indic%in%drop_indicator, !dimension%in%drop_dimension,
                            !country%in%drop_country)

.rdata[['inequals']]<<-filter(.rdata[['inequals']], !indic%in%drop_indicator, !dimension%in%drop_dimension,
                              !country%in%drop_country)


.rdata[['nationaldata']]<<-filter(.rdata[['nationaldata']], !indic%in%drop_indicator, !dimension%in%drop_dimension,
                              !country%in%drop_country)


# *****************************************************************************
# Read in years and set years
# *****************************************************************************

.rdata[['years']]<<-readRDS("../../Data/years.RDS")
#rev(sort(c(1994, 1997,2002, 2007, 2012)))#
.rdata[['all_years']]<<-getFilteredYear(focus_country)#c(1994, 1997,2002, 2007, 2012)



# *****************************************************************************
# Set the shapes and palettes
# *****************************************************************************


shapes <- rep(c(21, 22, 23, 24, 25),2)

sex <- filter(.rdata[['dimension_details']], dimension_type=="sex")
sex$colors <- sex_palette
sex$shapes <- shapes[1:2] 

economic <- filter(.rdata[['dimension_details']], dimension_type=="wealth")
economic$colors <- econ_palette(nrow(economic))
economic$shapes <- shapes[1:nrow(economic)]

education <- filter(.rdata[['dimension_details']], dimension_type=="educ")
education$colors <- educ_palette(nrow(education))
education$shapes <- shapes[1:nrow(education)]


residence <- filter(.rdata[['dimension_details']], dimension_type=="area")
residence$colors <- place_palette(nrow(residence))
residence$shapes <- shapes[1:nrow(residence)]


geo <- filter(.rdata[['dimension_details']], dimension_type=="region")
geo$colors <- geo_palette
geo$shapes <- 21

.rdata[['dimension_details']] <<- rbind(sex, economic, education, residence, geo)




# *****************************************************************************
# Set data sources and some other initial settings
# *****************************************************************************

.rdata[['data_sources']] <<- c("All", sort(unique(.rdata[["strata"]]$source)))
.rdata[['focus_data_source']]<<-"All"
.rdata[['mostrecent']]<<-FALSE


.rdata[['focus_inequal_type']]<<-focus_summary_measure

.rdata[['benchmark_countries']]<<-filter(.rdata[['countrynames']], 
                                         wbincome=="Middle-income", whoreg6_name=="South-East Asia")%>% .$country

#.rdata[['benchmark_country_list']]<<-.rdata[['all_countries']]
.rdata[['income_groups']] <<- sort(unique(.rdata[['countrynames']]$wbincome))
.rdata[['who_regions']] <<- sort(unique(.rdata[['countrynames']]$whoreg6_name))

.rdata[['focus_income_group']] <<- filter(.rdata[['countrynames']], country==.rdata[['focus_country']]) %>% .$wbincome 
.rdata[['focus_who_regions']] <<- filter(.rdata[['countrynames']], country==.rdata[['focus_country']]) %>% .$whoreg6_name 



# *****************************************************************************
# Set table variables
# *****************************************************************************


.rdata[['all_table_variables']] <<- data.frame(table_vars = c("Country", 
                                      "Year", 
                                      "Data source",
                                      "Health indicator abbreviation" ,
                                      "Health indicator name" ,
                                      "Inequality dimension" ,
                                      "Subgroup",
                                      "Estimate" ,
                                      "95%CI lower bound" ,
                                      "95%CI upper bound" ,
                                      "Population share %"   ,
                                      "Flag" ,
                                      "National estimate" 
                                      ), 
                                      var_type = c("text", "numeric", rep("text", 5),
                                                   rep("numeric", 4), "text", "numeric"),
                                      stringsAsFactors = FALSE)


.rdata[['focus_table_variables']]<-c("Country", 
                                     "Year",  
                                     "Health indicator name", 
                                     "Inequality dimension", 
                                     "Subgroup", 
                                     "Estimate", 
                                     "Population share %")


.rdata[['all_table_variables_summary']] <- data.frame(table_vars = c("Country", 
                                             "Year", 
                                             "Data source", 
                                             "Health indicator abbreviation",
                                             "Health indicator name", 
                                             "Inequality dimension", 
                                             "Summary measure abbreviation",
                                             "Summary measure name",
                                             "Estimate",
                                             "Analytic 95%CI lower bound",
                                             "Analytic 95%CI upper bound",
                                             "Bootstrap 95%CI lower bound",
                                             "Bootstrap 95%CI upper bound",
                                             "National estimate"),
                                             var_type = c("text", "numeric", rep("text", 6),
                                                          rep("numeric", 6)),
                                             stringsAsFactors = FALSE)




.rdata[['focus_table_variables_summary']]<-c("Country", 
                                             "Year",  
                                             "Health indicator name", 
                                             "Inequality dimension", 
                                             "Summary measure name",
                                             "Estimate")


.rdata[["centered_table_variables"]] <- c("Health indicator abbreviation", 
                                          "Summary measure abbreviation", 
                                          "Data source")


# *****************************************************************************
# Plot foot notes
# *****************************************************************************


txt1 <- "Source: Health Equity Assessment Toolkit (HEAT): Software for exploring and comparing health inequalities in countries. Built-in database edition.\nVersion 1.0. Geneva, World Health Organization, 2016."
txt2 <- "Data source: The disaggregated data used in this version were drawn from the WHO Health Equity Monitor database (2015 update),\nand subsequent updates are likely to have occurred."


.rdata[["plot_footnote"]] <- textGrob( paste(txt1, txt2, sep="\n"),
                                            x=0.05,
                                            y=0.75,
                                            hjust=0,
                                       gp = gpar(fontface = "italic", 
                                                 fontsize = 9, 
                                                 col="grey50",
                                                 lineheight = 0.9))

# *****************************************************************************
# Table footnotes
# *****************************************************************************



txt1 <- "Health Equity Assessment Toolkit (HEAT): Software for exploring and comparing health inequalities in countries. Built-in database edition. Version 1.0. Geneva; World Health Organization; 2016."
txt2 <- "Data source: The disaggregated data used in this version were drawn from the WHO Health Equity Monitor database (2015 update); subsequent updates are likely to have occurred."
txt3 <- "Disaggregated are available from: http://apps.who.int/gho/data/node.main.HE-1540?lang=en"

.rdata[["table_footnote"]] <- paste(txt1, txt2, txt3, sep="\n")


# *****************************************************************************
# Plot storage
# *****************************************************************************

.rdata[["disag_plot_explore"]] <<- NULL
.rdata[["summary_plot_explore"]] <<- NULL
.rdata[["disag_plot_compare"]] <<- NULL
.rdata[["summary_plot_compare"]] <<- NULL


# *****************************************************************************
# Number of characters in title
# *****************************************************************************


.rdata[["numTitleChars"]] <- numTitleChars

# *****************************************************************************
# Blank plot for missing data
# *****************************************************************************
.rdata[["blank_plot"]] <- rectGrob(gp = gpar(col=rgb(1,1,1,0)))


# *****************************************************************************
# Plot warnings
# *****************************************************************************

# .rdata[["explore_datamsg"]] <- "If estimates are not shown for a selected combination of variables, then data are not available."
# .rdata[["explore_nodatamsg"]] <- "There is no data for this combination of variables."
# 
# .rdata[["compare_datamsg"]] <-"If summary measures are not shown for a selected combination of variables, then data are not available or summary measures cannot be calculated. Note that only relevant summary measures are calculated for each dimension."
# 
# .rdata[["logscalemsg"]] <- "This summary measure is shown on a logarithmic scale, so axis limits must take values greater than zero."




