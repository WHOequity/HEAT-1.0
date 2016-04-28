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




# We want the Health indicator name and Summary measure name variables to
# be wider columns. I could not find a way to do this with DT after 
# extensive attempts. Instead I forced them wide by adding non-breaking
# spaces. Originally I wrote the forceNonBreakFull function which basically
# would add a series of non-breaking spaces throughout the name with the
# occasional space that would allow the name to break. But this could result 
# in some odd situations where you either have very long or relatively short 
# breaks. So I re-wrote to add non-breaking spaces only in the first N chars.


forceNonBreak <- function (labels, maxCharPerLine = 14) 
{
  
  labels <- as.character(labels)
  n = length(labels)
  splitX = strsplit(labels, split=" ")
  #newLabels = rep("", n)
  newLabels = rep("", n)
  for (l in 1:n) {
    #l <- 1
    if(length(splitX[[l]]) == 1 | nchar(splitX[[l]][1])>maxCharPerLine){
      finline <- labels[l]
    }else{
      
      line <- splitX[[l]]
      charsums <- cumsum(nchar(line))
      
      if(all(charsums<=maxCharPerLine)){
        finline <- paste(line, collapse = "&nbsp;")
      }else{
        
        below <- charsums[charsums<=maxCharPerLine]
        belowMax <- max(below)
        indxMax <- which(charsums == belowMax)
        longline <- paste(line[1:indxMax], collapse = "&nbsp;")
        rest <- paste(line[(indxMax+1):length(line)], collapse=" ")
        finline <- paste(longline, rest, sep=" ")
        
      }
      
      
    }
    newLabels[l] = finline
  }
  newLabels
}


# Keep this function, see above for description
# forceNonBreakFull <- function (labels, maxCharPerLine = 14, split = " ", fixed = TRUE, 
#                           newsplit = split, keepSplitAtEOL = TRUE) 
# {
#   #labels <- rep("Births attended by skilled health personnel (in the two or three years preceding the survey) (%)", 3)
#   # split <- " "
#   # fixed = TRUE
#   # maxCharPerLine <- 30
#   # newsplit <- "&nbsp;"
#   labels <- as.character(labels)
#   n = length(labels)
#   splitX = strsplit(labels, split = split, fixed = fixed)
#   newLabels = rep("", n)
#   for (l in 1:n) {
#     #l <- 1
#     nl = ""
#     line = ""
#     if (nchar(labels[l]) > 0) 
#       for (s in 1:length(splitX[[l]])) {
#         #s <- 1
#         newLen = nchar(line) + nchar(splitX[[l]][s])
#         if (nchar(line) < 5 | newLen <= maxCharPerLine) {
#           nl = paste(nl, splitX[[l]][s], sep = newsplit)
#           line = paste(line, splitX[[l]][s], sep = newsplit)
#         }
#         else {
#           nl = paste(nl, splitX[[l]][s], sep = paste0(if (keepSplitAtEOL) 
#             newsplit
#             else "", " "))
#           line = splitX[[l]][s]
#         }
#       }
#     newLabels[l] = nl
#   }
#   substring(newLabels, nchar(newsplit) + 1)
# }
# 


# this is the function for writing the table as CSV or TSV
# with the header lines. I use file() first so that I can
# essentially write.csv and then read.csv to get the formatting
# right and then I add the footnote
# dat is the data, file is the path and sep is the separator

write_table_wcitation <- function(dat, file, sep, citation){
  
  tmp <- file(encoding = "cp1252")
  write.table(dat, tmp, sep=sep, fileEncoding = "cp1252", row.names = FALSE)
  dat <- readLines(tmp, encoding = "cp1252")
  tmp2 <- file(file, encoding = "cp1252")
  cat(c(citation, dat), file=tmp2, sep="\n")
  close(tmp)
  
  
  
}




getISO3 <- function(countryname){
  filter(.rdata[['countrynames']], country==countryname) %>% .$iso3
}



# originally using shinyBS but the modals were not working with
# the new version of Shiny (perhaps because of new bootstrap)
# This is my own code to create a function to create a modal 
# allowing for extra tags in the body after the paragraph

bsModal_alt <- function(id, title, trigger, ...){
  # id<-'a'
  # trigger <- 'b'
  # title <- 'c'
  # body.p <- 
  mo <- tags$div(class = "modal fade", id = id, 
           `data-trigger` = trigger, 
           tabindex = "-1",
           role = "dialog",
           tags$div(class="modal-dialog", role="document",
                    tags$div(class="modal-content",        
                             tags$div(class = "modal-header", 
                                      tags$button(Type = "button", class = "close", `data-dismiss` = "modal",  HTML("&times;")), 
                                      tags$h3(title)), 
                             
                             tags$div(class = "modal-body"),
                             tags$div(class = "modal-footer", 
                                      tags$a(href = "#", class = "btn btn-primary", 
                                             `data-dismiss` = "modal", "Close")
                             )# end modal footer
                    )#end modal content
           )#end modal dialog
  )# end modal fade
  mo$children[[1]]$children[[1]]$children[[2]] <- 
    tagAppendChildren(mo$children[[1]]$children[[1]]$children[[2]], list = list(...))
  return(mo)
}



# bsModal_alt("myid", "mytrigger", "mytitle",
#             tags$p("This is a p"), tags$h3("this is h3"))



#http://stackoverflow.com/questions/11335836/increase-number-of-axis-ticks-in-ggplot2
number_ticks <- function(n) {function(limits) pretty(limits, n)}

# from package (WGCNA)

splitLabels <- function(variable, value){
  
  return(formatLabels(value, maxCharPerLine = 15))
}




splitLabelsWide <- function(variable, value){
  
  return(formatLabels(value, maxCharPerLine = 25))
}



formatLabels <- function (labels, maxCharPerLine = 14, split = " ", fixed = TRUE, 
                          newsplit = split, keepSplitAtEOL = TRUE) 
{
  labels <- as.character(labels)
  n = length(labels)
  splitX = strsplit(labels, split = split, fixed = fixed)
  newLabels = rep("", n)
  for (l in 1:n) {
    nl = ""
    line = ""
    if (nchar(labels[l]) > 0) 
      for (s in 1:length(splitX[[l]])) {
        newLen = nchar(line) + nchar(splitX[[l]][s])
        if (nchar(line) < 5 | newLen <= maxCharPerLine) {
          nl = paste(nl, splitX[[l]][s], sep = newsplit)
          line = paste(line, splitX[[l]][s], sep = newsplit)
        }
        else {
          nl = paste(nl, splitX[[l]][s], sep = paste0(if (keepSplitAtEOL) 
            newsplit
            else "", "\n"))
          line = splitX[[l]][s]
        }
      }
    newLabels[l] = nl
  }
  substring(newLabels, nchar(newsplit) + 1)
}





# Many of the calculation rely on knowing the cumnulative proportion of the population in the mid-point of each 
# ordered group.  That is the "Mid-Point Proportion".  This function returns that value.

midPointProp <- function(w){
  # This function returns the cumulative mid point proportion of each group:
  # Usage
  # w -- a vector of numbers of the population in each group
  # returns a vector representing the cumulative mid-point proportion
  #
  if(!is.numeric(w)){
    stop('This function operates on vector of numbers')
  }
  if(all(w==0)){
    stop('The population is of size 0 in all cells')
  }
  p <- w/sum(w)  # Calculate the pop. proportion in each group 
  p.mid <- p/2   # Calculate the mid-point proportion in each group
  p.cumsum <- cumsum(p) # Calculate the cumulative proprtion
  p.mid.cumsum <- p.mid + c(0, p.cumsum)[1:length(w)]  # Calculate the cumulative mid point proportion of each group
  return(p.mid.cumsum)  # Return the answer
}



textInputRow <-function (inputId, label, value = ""){
  # A function to handle the creation of side-by-side numeric input boxes
  div(style="display: inline-block;",
      tags$label(label, `for` = inputId), 
      tags$input(id = inputId, type = "text", value = value, class="input-custom"))
}


textPassword <-function (inputId, label, value = ""){
  # A function to handle the creation of side-by-side numeric input boxes
  div(style="display: inline-block;",
      tags$label(label, `for` = inputId), 
      tags$input(id = inputId, type = "password", value = value, class="input-custom"))
}


myHiddenBoolean <-function (inputId, label, value = ""){
  # A function to handle the creation of side-by-side numeric input boxes
  div(style="display: none;",
      tags$label(label, `for` = inputId), 
      tags$input(id = inputId, type = "checkbox", value = value, class="input-boolean"))
}

myHiddenText <-function (inputId, label, value = ""){
  # A function to handle the creation of side-by-side numeric input boxes
  div(style="display: none;",
      tags$label(label, `for` = inputId), 
      tags$input(id = inputId, type = "text", value = value, class="input-text"))
}


####################
# copied from single files
####################


# *****************************
lappend <- function (lst, ...){
  # Append an item to a list; e.g.:
  # vara <- list("axislimit"=T)
  # vara <- lappend(vara, "print"=F)
  lst <- c(lst, list(...))
  return(lst)
}

# *****************************
is.rank <- function(x){
  ranked <- T
  if(any(x < 1)){
    # The data are not an ordered subgroup
    ranked <- F
  }
  if(all(!x==1)){
    # The data are ordered by subgroup, but the base subgroup is missing
    ranked <- F
  }
  return(ranked)
}

# *****************************
notin <- function (vector1, vector2){
  # Return the elements in vector1 that are not in vector2
  el <- which(!(vector1 %in% vector2))  # svae in 'el' the element-locations on v1 not in v2
  if(length(el)==0){  # If there aren't any, return NULL
    return(NULL)
  }
  else{  # Else return the elements
    return (vector1[el])
  }
} 



findCommon <- function(vectora, vectorb){
  # Find the health indicators that are common to every year.
  # vectora: the list of heath indicators
  # vectorb: the years in which the indicators occur
  if(length(vectora)!=length(vectorb)){
    stop("Vectors must be of the same length")
  }
  names(which(rowSums(table(vectora, vectorb))==length(unique(vectorb)))) 
}



