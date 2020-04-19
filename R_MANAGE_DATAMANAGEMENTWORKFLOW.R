## Data management workflow
## 2020-03 by M. Hilty

paste(R.Version()$version.string, R.Version()$nickname)

## simplified initialization procedure

## install / load packages
source("R_MANAGE_PACKAGES.R")

## Import raw Data
source("R_MANAGE_IMPORT.R")

## TRANSFORM
## get column ranges (procedure is agnostic to database version)
cl <- colnames(data)
col_adm <- which(cl %in% "local_nr")+1
col_icu <- which(cl %in% "patient_characteristics_complete")+1
col_dis <- which(cl %in% "icu_parameters_complete")+1
col_admin <- which(cl %in% "outcome_complete")+1
## transform and calculate derived variables
source("R_MANAGE_TRANSFORM.R")

## calculate summary
## --------------------------------
casenum <- nrow(patients_char)
centers <- unique(data$center_ID)
centers_incl <- length(centers)
snapdate <- snapshot_date
snapstr <- snapshot_date_str

## ------------------
## data available at this point:
## data (raw export data from RedCAP, with automatic selection of the latest file)
## casenum (number of included patients)
## centers (list of centers that have included patients)
## centers_incl (number of centers that have included patients)
## snapshot_date (date / time of snapshot from RedCAP DB, automatically detected)
## snapshot_date_str (timestamp to be used for output filenames)
## patients_char (patient characteristics and outcome)
## patients_icu (icu treatment data, long dataframe mit $time {0, 1, 2, 3, 5, 7})
## ------------------
## input data is located in ./Input-data
