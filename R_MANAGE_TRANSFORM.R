## Data transformation
## 2020-03 M. Hilty

## -----------------------
## initial data processing

## date/time of snapshot
snapshot_date
## same, without spaces etc for filename stamps
snapshot_date_str <- gsub("[[:blank:]]", "", snapshot_date)
snapshot_date_str <- gsub("-", "", snapshot_date_str)
snapshot_date_str <- gsub(":", "", snapshot_date_str)

## create factors
data$centername_redcap <- factor(data$redcap_data_access_group)
data$form <- factor(data$redcap_event_name)
data$patient_ID <- factor(data$record_id)
s <- strsplit(as.character(data$patient_ID), "-")
data$center_ID <- factor(sapply(s, function(m) m[1] ))

## correct variable types
data$lactate <- as.numeric(data$lactate)
data$pct <- as.numeric(data$pct)



## all forms in redcap have all columns
## prepare admission and discharge forms. later merge them to replace the NA columns in the respective ones.
patients_adm <- data[data$form=="icu_admission__day_arm_1", ]
patients_dis <- data[data$form=="icu_discharge__out_arm_1", ]

## prepare long icu form
patients_icu <- data[data$form=="icu_admission__day_arm_1", ]
patients_icu$time <- 0
for(i in c(1, 2, 3, 5, 7)){
    df.t <- data[data$form==paste("day_", i, "_arm_1", sep=""), ]
    df.t$time <- i
    patients_icu <- rbind(patients_icu, df.t)
}
## remove adm and dis columns from icu DF
df.t <- cbind(patients_icu$record_id, patients_icu[, c(col_icu:(col_dis-1))])
colnames(df.t)[1] <- "record_id"
df.t <- cbind(df.t, patients_icu$time)
colnames(df.t)[length(colnames(df.t))] <- "time"
## add this here since it is needed below, but remove thereafter
patients_icu <- cbind(df.t, patients_icu$patient_ID)
colnames(patients_icu)[length(colnames(patients_icu))] <- "patient_ID"

## ------------------------
## ICU calcualtions
## ------------------------
## convert Fio2 from liter o2 to fio2
for(i in 1:nrow(patients_icu)){
    if(!is.na(patients_icu$o2_liter[i]) & patients_icu$o2_liter[i] == 1)
        patients_icu$fio2[i] <- 24
    if(!is.na(patients_icu$o2_liter[i]) & patients_icu$o2_liter[i] == 2)
        patients_icu$fio2[i] <- 28
    if(!is.na(patients_icu$o2_liter[i]) & patients_icu$o2_liter[i] == 3)
        patients_icu$fio2[i] <- 32
    if(!is.na(patients_icu$o2_liter[i]) & patients_icu$o2_liter[i] == 4)
        patients_icu$fio2[i] <- 36
    if(!is.na(patients_icu$o2_liter[i]) & patients_icu$o2_liter[i] == 5)
        patients_icu$fio2[i] <- 40
    if(!is.na(patients_icu$o2_liter[i]) & patients_icu$o2_liter[i] == 6)
        patients_icu$fio2[i] <- 44
    if(!is.na(patients_icu$o2_liter[i]) & patients_icu$o2_liter[i] == 7)
        patients_icu$fio2[i] <- 70
    if(!is.na(patients_icu$o2_liter[i]) & patients_icu$o2_liter[i] == 8)
        patients_icu$fio2[i] <- 80
    if(!is.na(patients_icu$o2_liter[i]) & patients_icu$o2_liter[i] == 9)
        patients_icu$fio2[i] <- 90
    if(!is.na(patients_icu$o2_liter[i]) & patients_icu$o2_liter[i] == 10)
        patients_icu$fio2[i] <- 95
}
## P/F ratio
patients_icu$pf_ratio <- patients_icu$pao2*7.5/(patients_icu$fio2/100)
## ventilatory ratio
## VR=(VEmeasured*PaCO2measured)/(VEpredicted*PaCO2perdicted)=(RR*8*IBW*PaCO2measured)/(100*IBW*5)=RR*PaCO2/62.5
## IBW=22*height^2 (not needed)
patients_icu$vent_ratio <- patients_icu$rr*patients_icu$paco2*7/500
## correct ddimer units
## logical <- (patients_icu$ddimer-round(patients_icu$ddimer))>0
## logical[is.na(logical)] <- F
## patients_icu$ddimer[logical] <- patients_icu$ddimer[logical]*1000

## remove patient_ID column from patients_icu to make it possible to merge with patients_char below
patients_icu <- patients_icu[, -which(colnames(patients_icu) %in% "patient_ID")]
colnames(patients_icu)

## ---------------------
## create a patients_char variable. it combines the admission and outcome forms
patients_char_adm <- patients_adm[, -c(col_icu:(col_admin-1))]
patients_char_dis <- cbind(patients_dis$record_id, patients_dis[, c(col_dis:(col_admin-1))])
colnames(patients_char_dis)[1] <- "record_id"
patients_char <- merge(patients_char_adm, patients_char_dis, by=c("record_id"), all.x=T)
## ------------------------
## calculate CHAR
## ------------------------
## calculate time periods
patients_char$sympt_to_hosp <- as.numeric(patients_char$date_adm - patients_char$date_first_symptoms)
patients_char$sympt_to_dg <- as.numeric(patients_char$date_first_symptoms - patients_char$date_first_symptoms)
patients_char$hosp_to_icu <- as.numeric(patients_char$date_adm_icu - patients_char$date_adm)
patients_char$los_icu <- as.numeric(patients_char$date_discharge_icu - patients_char$date_adm_icu)
patients_char$los_hosp <- as.numeric(as_datetime(patients_char$date_discharge_hosp) - patients_char$date_adm)
## parameters
patients_char$bmi <- patients_char$weight / (patients_char$height/100)^2
## generate simple comorbidities aus dem extended dataset

for(i in 1:nrow(patients_char)){
    if(!is.na(patients_char$adm_comorbid___3[i]) &
       patients_char$adm_comorbid___3[i] == 1)
        patients_char$adm_comorbid_simple___0[i] <- 1
    if(!is.na(patients_char$adm_comorbid___0[i]) &
       patients_char$adm_comorbid___0[i] == 1)
        patients_char$adm_comorbid_simple___1[i] <- 1
    if(!is.na(patients_char$adm_comorbid___1[i]) &
       patients_char$adm_comorbid___1[i] == 1)
        patients_char$adm_comorbid_simple___2[i] <- 1
    if((!is.na(patients_char$adm_comorbid___4[i]) &
        patients_char$adm_comorbid___4[i] == 1)|
       (!is.na(patients_char$adm_comorbid___5[i]) &
        patients_char$adm_comorbid___5[i] == 1))
        patients_char$adm_comorbid_simple___3[i] <- 1
    if((!is.na(patients_char$adm_comorbid___9[i]) &
        patients_char$adm_comorbid___9[i] == 1))
        patients_char$adm_comorbid_simple___4[i] <- 1
    if((!is.na(patients_char$adm_comorbid___15[i]) &
        patients_char$adm_comorbid___15[i]==1)|
       (!is.na(patients_char$adm_comorbid___16[i]) &
        patients_char$adm_comorbid___16[i]==1)|
       (!is.na(patients_char$adm_comorbid___17[i]) &
        patients_char$adm_comorbid___17[i]==1)|
       (!is.na(patients_char$adm_comorbid___18[i]) &
        patients_char$adm_comorbid___18[i]==1)|
       (!is.na(patients_char$adm_comorbid___19[i]) &
        patients_char$adm_comorbid___19[i]==1)|
       (!is.na(patients_char$adm_comorbid___20[i]) &
        patients_char$adm_comorbid___20[i]==1)|
       (!is.na(patients_char$adm_comorbid___21[i]) &
        patients_char$adm_comorbid___21[i]==1)|
       (!is.na(patients_char$adm_comorbid___22[i]) &
        patients_char$adm_comorbid___22[i]==1)|
       (!is.na(patients_char$adm_comorbid___23[i]) &
        patients_char$adm_comorbid___23[i]==1))
        patients_char$adm_comorbid_simple___5[i] <- 1
}


## transform bool vars
patients_char$survivor_icu <- factor(patients_char$survivor_icu)
levels(patients_char$survivor_icu) <- c("ICU non survivor", "ICU survivor")
patients_char$survivor_hosp <- factor(patients_char$survivor_hosp)
levels(patients_char$survivor_hosp) <- c("Hospital non survivor", "Hospital survivor")

patients_char$adm_comorbid_simple___0 <-
    factor(patients_char$adm_comorbid_simple___0)
patients_char$adm_comorbid_simple___1 <-
    factor(patients_char$adm_comorbid_simple___1)
patients_char$adm_comorbid_simple___2 <-
    factor(patients_char$adm_comorbid_simple___2)
patients_char$adm_comorbid_simple___3 <-
    factor(patients_char$adm_comorbid_simple___3)
patients_char$adm_comorbid_simple___4 <-
    factor(patients_char$adm_comorbid_simple___4)
patients_char$adm_comorbid_simple___5 <-
    factor(patients_char$adm_comorbid_simple___5)
patients_char$health_work_yn <- factor(patients_char$health_work_yn)


## merge pat char with icu data
patients_icu <- merge(patients_icu, patients_char, by=c("record_id"), all.x=T)

## perform calculations that require merged data
## ------------------------
