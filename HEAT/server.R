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

library(shiny)
library(ggplot2)
library(grid)
library(gridExtra)
library(RColorBrewer)
library(dplyr)
library(DT)


options(shiny.maxRequestSize = 0.100*1024^2)

shinyServer(function(input, output, session){ 
  
  source("utils/get_filtered.R", local=TRUE)
  source("utils/initial_settings.R", local=TRUE)
  #if(length(.rdata)!=27) print("Check rdata for length")

  source("utils/get_plots.R", local=TRUE)
  source("utils/get_data.R", local=TRUE)

  source("ui/data_management.R", local=TRUE)
 # source("ui/explore_inequality.R", local=TRUE)
  #source("ui/compare_inequality.R", local=TRUE)
  source("ui/information.R", local=TRUE)
  source("server/server_logic.R", local=TRUE)
  source("server/server_observers.R", local=TRUE)
  source("server/server_downloading.R", local=TRUE)
  
})


# 3/15/2016
# > sessionInfo()
# R version 3.2.2 (2015-08-14)
# Platform: x86_64-w64-mingw32/x64 (64-bit)
# Running under: Windows 7 x64 (build 7601) Service Pack 1
# 
# locale:
# [1] LC_COLLATE=English_United States.1252  LC_CTYPE=English_United States.1252   
# [3] LC_MONETARY=English_United States.1252 LC_NUMERIC=C                          
# [5] LC_TIME=English_United States.1252    
# 
# attached base packages:
# [1] grid      stats     graphics  grDevices utils     datasets  methods   base     
# 
# other attached packages:
# [1] dplyr_0.4.3        RColorBrewer_1.1-2 gridExtra_2.0.0    ggplot2_2.1.0     
# [5] shiny_0.13.0.9000 
# 
# loaded via a namespace (and not attached):
#  [1] Rcpp_0.12.3          assertthat_0.1       digest_0.6.9        
#  [4] mime_0.4             R6_2.1.2             plyr_1.8.3          
#  [7] DBI_0.3.1            xtable_1.8-0         jsonlite_0.9.19     
# [10] gtable_0.2.0         magrittr_1.5         scales_0.4.0        
# [13] lazyeval_0.1.10.9000 tools_3.2.2          munsell_0.4.3       
# [16] parallel_3.2.2       httpuv_1.3.3         colorspace_1.2-6    
# [19] htmltools_0.3  