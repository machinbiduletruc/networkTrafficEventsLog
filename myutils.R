#Packages required for the project
required_packages <- c('rstudioapi', 'fs', 'stringr', 'dplyr')
#Installation of all required packages not already installed
install.packages(setdiff(required_packages, rownames(installed.packages())))
#Loading required packages
library('stringr')

displayson = FALSE

debugprint <- function(strings){
    str <- ''
    for(s in strings){
        str <- paste(str, s)
    }
    if(displayson){
        print(str)
    }
}

trimstr <- function(valuetotrim){
    stringr::str_trim(valuetotrim)
}

nbofmissingvals <- function(col){
    i <- 0    
    for(value in col){
        if (is.null(value) || trimstr(value) == '' || is.na(value) || is.nan(col)){
            i <- i + 1
        }
    }
    i
}
