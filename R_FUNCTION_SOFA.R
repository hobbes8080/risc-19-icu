## SOFA score calculator
## 2012-09 by Matthias P Hilty
## Written for R Version 2.15.0

## Vincent JL, Moreno R, Takala J, et al. The SOFA (Sepsis-related Organ Failure Assessment) score to describe organ dysfunction/failure: on behalf of the Working Group on Sepsis-Related Problems of the European Society of Intensive Care Medicine. Intensive Care Med. 1996;22(7):707-710

## Input values: value unit varname
## Weight kg weight
## PaO2 kPa pao2
## FiO2 1 fio2
## Mech.vent bool mv
## GCS 1 gcs
## mSAP mmHg msap
## Dobutamine ug/min dobutamine
## Dopamine ug/min dopamine
## Epinephrine ug/min epinephrine
## Norepinephrine ug/min norepinephrine
## Bilirubin umol/L bilirubin
## Thrombocytes G/L tc
## Creatinine umol/L creatinine
## Urine ml/24h urine

## output value: data.frame containing subscores and score

## Note: Vasopressor Vasopressin in E/min * 1000 is equaled to norepinephrine in ug/min

## Note: values not given are supposed to be normal

SOFA<-function(weight=70, pao2=12, fio2=0.21, mv=F, gcs=15, msap=70, dobutamine=0, dopamine=0, epinephrine=0, norepinephrine=0, vasopressin=0, bilirubin=0, tc=400, creatinine=60, urine=2000, NAtreatasnormal=F){
    ## if NAtreatasnormal is TRUE, then treat NA values as normal values,
    ## otherwise return NA
    if(!is.na(weight) & weight==0)weight <- NA
    if(NAtreatasnormal){
        if(is.na(weight))
            weight<-70
        if(is.na(pao2))
            pao2<-12
        if(is.na(fio2))
            fio2<-0.21
        if(is.na(mv))
            mv<-F
        if(is.na(gcs))
            gcs<-15
        if(is.na(msap))
            msap<-70
        if(is.na(dobutamine))
            dobutamine<-0
        if(is.na(dopamine))
            dopamine<-0
        if(is.na(epinephrine))
            epinephrine<-0
        if(is.na(norepinephrine))
            norepinephrine<-0
        if(is.na(vasopressin))
            vasopressin<-0
        if(is.na(bilirubin))
            bilirubin<-0
        if(is.na(tc))
            tc<-400
        if(is.na(creatinine))
            creatinine<-60
        if(is.na(urine))
            urine<-2000
    }else{
        if(is.na(weight) | is.na(pao2) | is.na(fio2) | is.na(mv) | is.na(gcs) | is.na(msap) | is.na(dobutamine) | is.na(epinephrine) | is.na(dopamine) | is.na(norepinephrine) | is.na(vasopressin) | is.na(bilirubin) | is.na(tc) | is.na(creatinine) | is.na(urine))
            ## TODO: treat subscores seperately here, to get independent NA results
            return(data.frame(SOFA=NA, respiratory=NA, coagulation=NA, liver=NA, cardiovascular=NA, cns=NA, renal=NA))
    }
    ## Respiratory system subscore
    ## ------------------------------
    pao2<-pao2/0.133322368421     #conversion of pao2 from kPa to mmHg
    oxyindex<-pao2/fio2      #calculation of oxygenation index in mmHg
    ## perform oxyindex score lookup
    match<-findInterval(oxyindex, c(0, 100, 200, 300, 400))
    index<-c(4, 3, 2, 1, 0)[match]
    if(index>2){
        if(mv){
            resp<-index
        }else{
            resp<-2
        }
    }else{
        resp<-index
    }
    ## Nervous system subscore
    ## ------------------------------
    ## perform gcs score lookup
    match<-findInterval(gcs, c(0, 6, 9, 12, 14))
    index<-c(4, 3, 2, 1, 0)[match]
    neuro<-index
    ## Cardiovascular system subscore
    ## ------------------------------
    ## conversion of dose rates to dose rates per kgKG, and vasopressin conversion
    dobutamine<-dobutamine/weight
    dopamine<-dopamine/weight
    norepinephrine<-norepinephrine/weight + vasopressin*1000/weight
    epinephrine<-epinephrine/weight
    if(dopamine>15 | norepinephrine>0.1 | epinephrine>0.1){
        cardio<-4
    }else{
        if(dopamine>5 | norepinephrine>0 | epinephrine>0){
            cardio<-3
        }else{
            if(dopamine>0 | dobutamine>0){
                cardio<-2
            }else{
                if(msap<70){
                    cardio<-1
                }else
                    cardio<-0
            }
        }
    }
    ## Liver subscore
    ## ------------------------------
    ## perform bilirubin lookup
    ## shortcut to "index" without "match" is possible, because low values give low scores, and scores are consecutive
    index<-findInterval(bilirubin, c(1.2, 1.9, 5.9, 11.9))
    liver<-index
    ## Coagulation subscore
    ## ------------------------------
    ## perform tc score lookup
    match<-findInterval(tc, c(0, 20, 50, 100, 150))
    index<-c(4, 3, 2, 1, 0)[match]
    coag<-index
    ## Renal subscore
    ## ------------------------------
    creatinine<-creatinine*0.011312     #conversion of creatinint from umol/L to mg/dl
    ## perform creatinine score lookup
    ## shortcut to "index" without "match" is possible, because low values give low scores, and scores are consecutive
    index<-findInterval(creatinine, c(1.2, 1.9, 3.4, 4.9))
    if(urine<200 | index==4){
        renal<-4
    }else{
        if(urine<500 | index==3){
            renal<-3
        }else{
            renal<-index
        }
    }
    ## return score and subscores as data.frame (cave: also return the same data structure containing NA in case of a NA result (see above) to maintain output handling compatibility)
    sofa<-resp+coag+liver+cardio+neuro+renal
    return(data.frame(SOFA=sofa, SOFArespiratory=resp, SOFAcoagulation=coag, SOFAliver=liver, SOFAcardiovascular=cardio, SOFAcns=neuro, SOFArenal=renal))
}


SAPS <- function(age=30, HR=90, ssap=120, temp=37, pao2=12, fio2=0.21, mv=F,
                 urine=2000, HST=0, leuco=5, kalium=4, natrium=140,
                 hco3=22, bilirubin=0, gcs=15, cancer=F, hematcancer=F,
                 AIDS=F, medical_adm=F, emergencysurgery=F,
                 msap=65, RR=20, paco2=5, pH=7.4, creatinine=50, hct=0.4, #add APACHE
                 akf=F, chronicorganfailure=F, immunocompr=F,
                 NAtreatasnormal=F){
    if(NAtreatasnormal){
        if(is.na(age)) age <- 30
        if(is.na(pao2)) pao2<-12
        if(is.na(fio2)) fio2<-0.21
        if(is.na(mv)) mv<-F
        if(is.na(gcs)) gcs<-15
        if(is.na(msap)) msap<-65
        if(is.na(ssap)) ssap<-120
        if(is.na(temp)) temp<-37
        if(is.na(HR)) HR<-90
        if(is.na(hco3)) hco3<-22
        if(is.na(HST)) HST<-0
        if(is.na(leuco)) leuco<-5
        if(is.na(bilirubin)) bilirubin<-0
        if(is.na(natrium)) natrium<-140
        if(is.na(creatinine)) creatinine<-60
        if(is.na(urine)) urine<-2000
        if(is.na(kalium)) kalium<-4
        if(is.na(cancer)) cancer<-F
        if(is.na(hematcancer)) hematcancer<-F
        if(is.na(medical_adm)) medical_adm<-F
        if(is.na(emergencysurgery)) emergencysurgery<-F
        if(is.na(RR)) RR<-20
        if(is.na(paco2)) paco2<-5
        if(is.na(pH)) pH<-7.4
        if(is.na(hct)) hct<-0.4
        if(is.na(akf)) akf<-F
        if(is.na(chronicorganfailure)) chronicorganfailure<-F
        if(is.na(immunocompr)) immunocompr<-F
        if(is.na(AIDS)) AIDS<-F
    }else{
        ## TODO: treat SAPS and APACHE seperately here, to get independent NA results
        if(is.na(age) | is.na(pao2) | is.na(fio2) | is.na(mv) | is.na(gcs) | is.na(msap) | is.na(ssap) | is.na(temp) | is.na(HR) | is.na(hco3) | is.na(HST) | is.na(leuco) | is.na(bilirubin) | is.na(natrium) | is.na(creatinine) | is.na(urine) | is.na(kalium) | is.na(cancer) | is.na(hematcancer) | is.na(medical_adm) | is.na(emergencysurgery) | is.na(RR) | is.na(paco2) | is.na(pH) | is.na(hct) | is.na(akf) | is.na(chronicorganfailure) | is.na(immunocompr) | is.na(AIDS))
             return(data.frame(SAPS=NA, APACHEphysio=NA, APACHE=NA))
    }
    pao2<-pao2/0.133322368421     #conversion of pao2 from kPa to mmHg
    oxyindex<-pao2/fio2      #calculation of oxygenation index in mmHg
    aAo2 <- ((fio2 / 100 * (760 - 47)) - (paco2 * 7.5 / 0.8)) - (pao2 + 7.5)
    ## -------------------
    ## SAPS
    saps <- 0
    ## age
    match<-findInterval(age, c(0, 50, 60, 70, 75, 80))
    index<-c(0, 7, 12, 15, 16, 18)[match]
    saps <- saps + index
    ## HR
    match<-findInterval(HR, c(0, 40, 70, 120 ,160))
    index<-c(11, 2, 0, 4, 7)[match]
    saps <- saps + index
    ## ssap
    match<-findInterval(ssap, c(0, 70, 100, 200))
    index<-c(13, 5, 0, 2)[match]
    saps <- saps + index
    ## temp
    match<-findInterval(temp, c(0, 39))
    index<-c(0, 3)[match]
    saps <- saps + index
    ## oi
    if(mv){
        match<-findInterval(oxyindex, c(0, 100, 200))
        index<-c(11, 9, 6)[match]
        saps <- saps + index
    }
    ## urine
    match<-findInterval(urine, c(0, 500, 1000))
    index<-c(11, 4, 0)[match]
    saps <- saps + index
    ## HST
    match<-findInterval(HST, c(0, 10, 30))
    index<-c(0, 6, 10)[match]
    saps <- saps + index
    ## leuco
    match<-findInterval(leuco, c(0, 1, 20))
    index<-c(12, 0, 3)[match]
    saps <- saps + index
    ## kalium
    match<-findInterval(kalium, c(0, 3, 5))
    index<-c(3, 0, 3)[match]
    saps <- saps + index
    ## natrium
    match<-findInterval(natrium, c(0, 125, 145))
    index<-c(5, 0, 1)[match]
    saps <- saps + index
    ## hco3
    match<-findInterval(hco3, c(0, 15, 20))
    index<-c(6, 3, 0)[match]
    saps <- saps + index
    ## bilirubin
    match<-findInterval(bilirubin, c(0, 68.4, 102.6))
    index<-c(0, 4, 9)[match]
    saps <- saps + index
    ## gcs
    match<-findInterval(gcs, c(0, 6, 9, 11, 14))
    index<-c(26, 13, 7, 5, 0)[match]
    saps <- saps + index
    ## leuco
    match<-findInterval(leuco, c(0, 1, 20))
    index<-c(12, 0, 3)[match]
    saps <- saps + index
    ## chronic
    index <- 0
    if(AIDS){index <- 7}else{if(hematcancer){index <- 10}else{if(cancer){index <- 9}}}
    saps <- saps + index
    ## admission
    index <- 0
    if(emergencysurgery){index <- 8}else{if(medical_adm){index <- 6}}
    saps <- saps + index
    ## ---------------------
    ## APACHE
    apache <- 0
    ## temp
    match<-findInterval(temp, c(0, 30, 32, 34, 36, 38.5, 39 ,41))
    index<-c(4, 3, 2, 1, 0, 1, 3, 4)[match]
    apache <- apache + index
    ## msap
    match<-findInterval(msap, c(0, 50, 70, 110, 130, 160))
    index<-c(4, 2, 0, 2, 3, 4)[match]
    apache <- apache + index
    ## HR
    match<-findInterval(HR, c(0, 40, 55, 70, 110, 140, 180))
    index<-c(4, 3, 2, 0, 2, 3, 4)[match]
    apache <- apache + index
    ## RR
    match<-findInterval(RR, c(0, 5, 10, 12, 25, 35, 50))
    index<-c(4, 2, 1, 0, 1, 3, 4)[match]
    apache <- apache + index
    ## oi
    if(fio2>0.5){
        match<-findInterval(aAo2, c(0, 200, 350, 500))
        index<-c(0, 2, 3, 4)[match]
    }else{
        match<-findInterval(pao2*7.5, c(0, 55, 61, 70))
        index<-c(4, 3, 1, 0)[match]
    }
    if(length(index)==0) index <- 0
    apache <- apache + index    
    ## pH
    match<-findInterval(pH, c(0, 7.15, 7.25, 7.33, 7.5, 7.6, 7.7))
    index<-c(4, 3, 2, 0, 1, 3, 4)[match]
    apache <- apache + index
    ## natrium
    match<-findInterval(natrium, c(0, 110, 120, 130, 150, 155, 160, 180))
    index<-c(4, 3, 2, 0, 1, 2, 3, 4)[match]
    apache <- apache + index
    ## kalium
    match<-findInterval(kalium, c(0, 2.5, 3, 3.5, 5.5, 6, 7))
    index<-c(4, 2, 1, 0, 1, 3, 4)[match]
    apache <- apache + index
    ## creatinine
    match<-findInterval(creatinine/88.42, c(0, 0.6, 1.5, 2, 3.5))
    index<-c(2, 0, 2, 3, 4)[match]
    if(akf) index <- index * 2
    apache <- apache + index
    ## hct
    match<-findInterval(hct*100, c(0, 20, 30, 46, 50, 60))
    index<-c(4, 2, 0, 1, 2, 4)[match]
    apache <- apache + index
    ## leuco
    match<-findInterval(leuco, c(0, 1, 3, 15, 20, 40))
    index<-c(4, 2, 0, 1, 2, 4)[match]
    apache <- apache + index
    ## gcs
    index <- 15-gcs
    apache <- apache + index
    ## physiology subscore
    apache_physio <- apache
    ## age
    match<-findInterval(age, c(0, 45, 55, 65, 75))
    index<-c(0, 2, 3, 5, 6)[match]
    apache <- apache + index
    ## chronic disease
    index <- 0
    if(AIDS|cancer|hematcancer|immunocompr|chronicorganfailure){
        if(medical_adm|emergencysurgery){
            index <- 5
        }else{
            index <- 2
        }
    }   
    apache <- apache + index
    ## ---------------------------
    return(data.frame(SAPS=saps, APACHEphysio=apache_physio, APACHE=apache))
}

## extended-


