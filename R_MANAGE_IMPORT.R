## Import raw data from raw data file (csv)
## 2020-03 M. Hilty

##import data

## input file path
inp <- "./Input-data"
## file name stem
fl_st <- "MulticentralCOVID19R_DATA_"

## find latest export file
fl_csv <- list.files(inp, paste(fl_st, ".*csv", sep=""))                     
csvfile <- paste(inp, "/", fl_csv[length(fl_csv)], sep="")

## read the files that are present
data <- read_csv(csvfile, skip_empty_rows = TRUE)
    
## get the actual date of the export
snapshot_date <- file.info(csvfile)$ctime


