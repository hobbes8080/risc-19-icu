## new excel import
if(!require(readxl)){
    install.packages("readxl")
    library(readxl)
}
## excel export: write.xlsx
if(!require(xlsx)){
    install.packages("xlsx")
    library(xlsx)
}
## date/time
if(!require(lubridate)){
    install.packages("lubridate")
    library(lubridate)
}
## extended csv import
if(!require(readr)){
  install.packages("readr")
  library(readr)
}
## dplyr
if(!require(dplyr)){
    install.packages("dplyr")
    library(dplyr)
}
## ggplot2
if(!require(ggplot2)){
    install.packages("ggplot2")
    library(ggplot2)
}
## arrange ggplot plots
if(!require(cowplot)){
    install.packages("cowplot")
    library(cowplot)
}
