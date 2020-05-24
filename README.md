Version 1.0, 2020-04 M. Hilty for the RISC-19-ICU registry board
* Dr. Matthias P. Hilty MD, Pedro D. Wendel Garcia MSc, Prof. Dr. Reto A. Schüpbach MD, Dr. Jonathan Montomoli MD PhD, Dr. Philippe Guerci MD, Prof. Dr. Thierry Fumeaux MD
# The RISC-19-ICU registry
The Risk stratification in COVID-19 patients in the ICU (RISC-19-ICU) registry was founded during the emerging SARS-CoV-2 pandemic. COVID-19 is a novel disease caused by infection with the SARS-CoV-2 virus that was first described in December 2019. The disease has spread exponentially in many countries and has reached global pandemic status within three months. According to first experience, hospitalization was required in approximately 20 % of cases and severe, life-threatening illness resulted in approximately 10 %. In some countries, health care systems were overwhelmed by the rapid increase in critically ill patients that far exceeded their capacity. It is thus of utmost importance to gain knowledge about the characteristics and course of critically ill patients with COVID-19 and to stratify these patients according to their risk for further deterioration. A key part of fighting this pandemic is to exchange scientific information and advance our understanding of the disease.
The Risk stratification in COVID-19 patients in the ICU (RISC-19-ICU) registry aims to collect an anonymized dataset to characterize patients that develop life-threatening critical illness due to COVID-19 and make it accessible to collaborative analysis.
The data collected may be composed of a core dataset and/or an extended dataset. The core dataset consists of a basic set of parameters, of which many are commonly generated during treatment of critically ill patients with COVID-19 in an intensive care unit (the individual parameters are marked yellow in the attached case report forms, and are clearly marked on the electronic case report forms during data entry). The extended dataset consists of parameters that may be measured during treatment of critically ill patients with COVID-19 in an intensive care unit, depending on clinical practice, indication and availability of the measurement method. The data accumulating in the registry as the pandemic or subsequent waves develop are made available to the collaborators to support an optimal response to the pandemic threat. The information gained on the initial characteristics and disease course via the RISC-19-ICU registry may contribute to a better understanding of the risk factors for developing critical illness due to COVID-19 and for an unfavorable disease course, and thus support informed patient triage and management decisions.
# How to participate in RISC-19-ICU
As an intensive care unit caring for critically ill patients suffering from COVID-19, or as a university-based data science institute:
See www.risc-19-icu.net and fill out the "intention to collaborate" form. After a check of your data you will receive a collaboration agreement to be signed by you and your institution.
# The risc-19-icu github repository
The code in this repository provides basic data transformation. It aims to collaboratively enable data analysis in the RISC-19-ICU registry. This file contains a brief description of the input and output data of the transformation process.
## Input
* database export files can be placed in the ./Input-data folder
* the code selects the most recent file, import it, and generate a date/time stamp representing the database export
## Output
* generally, all variables pertaining to the extended and the core dataset are processed
* in case that a database record was filled out with the eCRF set to the core dataset specifications, the variables pertaining the the extended dataset will contain missing values
* the data is transformed and the following variables are generated
  * data.frame patients_char: one row per patient, contains patient characteristics at the time of ICU admission and leading up to hospitalization and ICU admission, and outcome variables pertaining to the patient state after dismissal from the ICU.
  * data.frame patients_adm: one row per patient, contains patient characteristics at the time of ICU admission and leading up to hospitalization and ICU admission, useful for predictive model training.
  * data.frame patients_icu: long data.frame, one row per patient and timepoint (as of ver 1.0, the first 7 days represented in the core dataset are processed), contains all variables that are measured at all timepoints - meaning, during the ICU treatment.
  * timepoints in patients_icu (patients_icu$time): Day 0 is the timepoint of ICU admission, matching the time of day of ICU admission. Days n are the timepoints at day 0 + n, at 06:00 or at the time of first supine position if the patient was in prone position at 06:00.
  * snapshot_date: date of database export
  * snapshot_date_str: same, as a string suitable to append to output file names
  * casenum: number of included patients
  * centers_incl: number of centers that have included patients
  * centers: list of centers that have included patients
* The code transforms the variables contained in the database into the above mentioned framework, and generates some additional calculated variables in both data.frames
* Additions to this repository by collaborating centers are encouraged
