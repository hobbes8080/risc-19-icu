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
