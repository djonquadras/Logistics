libraries <- c("chron", "openxlsx", "dplyr", "lubridate", "ggplot2",
               "readxl", "scales", "tibble", "magrittr", "stringr", "plotly", "forecast",
               "colourpicker", "DT")
for(i in 1:length(libraries)){
  if(libraries[i] %in% rownames(installed.packages()) == FALSE){
    install.packages(libraries[i])
  }
}

sapply(X = libraries,
       FUN = require, character.only = TRUE)

library(chron)
library(openxlsx)
library(dplyr)
library(lubridate)
library(ggplot2)
library(readxl)
library(scales)
library(tibble)
library(magrittr)
library(stringr)
library(plotly)
library(forecast)
library(colourpicker)
library(DT)