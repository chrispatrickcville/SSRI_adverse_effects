setwd("~/Documents/Classes/STAT 6021 - Linear Regression/Final project")

library(jsonlite)
library(gdata)
library(stringr)
library(lavaan)
library(mice)
library(lattice)
library(ggplot2)
library(reshape)
library(scales)
library(ddply)
library(glmnet)
library(ROCR)
library(stringr)

#####################################################################################
##### To use this first set of code, please download JSON data from            ######
##### https://open.fda.gov/drug/event/reference/ (click on the "Show all 300   ######
##### download files" button). This code is designed to feed in data from a    ######
##### particular quarter (based on how the files are organized), although      ######
##### this can be adapted to work on a larger set of files if the JSON files   ######
##### are renamed. Currently, this set of code is set to work with Q1 data     ######
##### for 2015.                                                                ######
#####################################################################################

# load ssri generic names and brand names
ssri <- read.xls("SSRI.xlsx")

# SET VARIABLES - REVIEW AND CHANGE THESE FOR EVERY SET OF JSON FILES AS NEEDED

file_num <- 13 # Quarter 1 of 2015 has 13 JSON files
year <- 2015 
quarter <- "Q1"
JSON_path <- "~/Documents/Classes/STAT 6021 - Linear Regression/Final project/JSON Files"
RData_path <- "~/Documents/Classes/STAT 6021 - Linear Regression/Final project/RData"

# Keep these variables in the current envionrment
keep <- c('file_num', 'year', 'quarter', 'JSON_path', 'RData_path', 'keep')

# LOAD JSON FILES
for (a in c(1:file_num)){
  fn = ""
  if (a < 10){
    num <- paste("0",as.character(a), sep = "")
  }else{
    num <- as.character(a)
  }
  fn <- paste(JSON_path, "/drug-event-00", num, "-of-", "00", as.character(file_num), ".json", sep = "")
  stream <- fromJSON(file(fn))
  # save as data frame
  result <- as.data.frame(stream$results)
  rm(stream)
  # data frame: patient 
  # each patient has a unique index number
  patients <- result$patient
  filename <- paste(as.character(year), quarter, num, sep = "") 
  patients$index <- paste(filename, c(1:nrow(patients)))
  add_col <- c("seriousnessother","serious","seriousnesshospitalization","seriousnessdisabling","seriousnesslifethreatening","seriousnessdeath")
  patients <- cbind(patients, result[, add_col])
  # data frame: drug
  # medicinalproduct, drugindication, generic_name, brand_name, drugadditional, actiondrug, 
  # drugintervaldosagedefinition, drugintervaldosageunitnumb, drugrecurreadministration, 
  # drugstructuredosageunit, drugcharacterization, 
  # indexed by patient number 
  drug <- data.frame(matrix(0, nrow = 0, ncol = 12))
  names(drug) <- c("index","medicinalproduct", "drugindication","generic_name","brand_name","drugadditional",
                   "actiondrug","drugintervaldosagedefinition","drugintervaldosageunitnumb", "drugrecurreadministration",
                   "drugstructuredosageunit", "drugcharacterization")
  for (i in 1:nrow(patients)){
    df <- as.data.frame(patients$drug[i])
    df$index <- paste(filename, as.character(i))
    if (!"medicinalproduct" %in% names(df)){
      df$medicinalproduct <- NA
    }
    if (!"drugindication" %in% names(df)){
      df$drugindication <- NA
    }
    if (!"drugadditional" %in% names(df)){
      df$drugadditional <- NA
    }
    if (!"actiondrug" %in% names(df)){
      df$actiondrug <- NA
    }
    if (!"drugintervaldosagedefinition" %in% names(df)){
      df$drugintervaldosagedefinition <- NA
    }
    if (!"drugintervaldosageunitnumb" %in% names(df)){
      df$drugintervaldosageunitnumb <- NA
    }
    if (!"drugrecurreadministration" %in% names(df)){
      df$drugrecurreadministration <- NA
    }
    if (!"drugstructuredosageunit" %in% names(df)){
      df$drugstructuredosageunit <- NA
    }
    if (!"drugcharacterization" %in% names(df)){
      df$drugcharacterization <- NA
    }
    if (!"openfda" %in% names(df)){
      df$generic_name <- NA
      df$brand_name <- NA
    }else{
      openfda <- as.data.frame(df$openfda)
      if (!"generic_name" %in% names(openfda)){
        df$generic_name <- NA
      }else
        df$generic_name <- openfda$generic_name
      
      if (!"brand_name" %in% names(openfda)){
        df$brand_name <- NA
      }else
        df$brand_name <- openfda$brand_name
    }
    
    drug <- rbind(drug, df[, names(drug)])
  }

  reaction <- data.frame(matrix(0, nrow = 0, ncol = 3))
  names(reaction) <- c("index","reactionoutcome","reactionmeddrapt")
  for (i in 1:nrow(patients)){
    df <- as.data.frame(patients$reaction[i])
    df$index <- paste(filename, as.character(i))
    if (!"reactionoutcome" %in% names(df)){
      df$reactionoutcome <- NA
    }
    if (!"reactionmeddrapt" %in% names(df)){
      df$reactionmeddrapt <- NA
    }
    reaction <- rbind(reaction, df[, names(reaction)])
  }
  rm(df, openfda, result)
  savefn <- paste(RData_path, "/", year, quarter, "_", num, ".RData", sep = "") 
  save.image(savefn)
  rm(list=setdiff(ls(), keep))
}

#############################################################################
##### This next set of code will convert the RData created above into  ######
##### usable dataframes.                                               ######
#############################################################################

names <- c("index","patientonsetage", "patientweight","patientsex","patientonsetageunit","patientagegroup",
           "seriousnessother","serious","seriousnesshospitalization","seriousnessdisabling","seriousnesslifethreatening",
           "seriousnessdeath","ESCITALOPRAM","CITALOPRAM","FLUOXETINE","PAXIL","SERTRALINE","otherDrugs")
ssri5 <- c("ESCITALOPRAM","CITALOPRAM","FLUOXETINE","PAXIL","SERTRALINE")

# Initialize patient and drugFinal data frames if they do not yet exist
if (!exists("patient")){
  patient <- data.frame() 
}

if (!exists("drugFinal")){
  drugFinal <- data.frame() 
}

for (a in 1:file_num){ 
  if (a < 10){
    num <- paste("0",as.character(a), sep = "")
  }else{
    num <- as.character(a)
  }
  Rdata <- paste(RData_path, "/", year, quarter, "_", num, ".Rdata", sep = "")
  load(Rdata)
  drug$generic_name <- sapply(drug$generic_name, function(x) paste(unlist(x), collapse = ';'))
  drug$brand_name <- sapply(drug$brand_name, function(x) paste(unlist(x), collapse = ';'))
  
  # Check to see if patientagegroup is a column in patients; if not, add it and add NA as values
  if(!"patientagegroup" %in% colnames(patients))
  {
    patients$patientagegroup <- NA
  }
  
  # CITALOPRAM, ESCITALOPRAM, FLUOXETINE, PAXIL, SERTRALINE
  patients$ESCITALOPRAM <- 0
  patients$CITALOPRAM <- 0
  patients$FLUOXETINE <- 0
  patients$PAXIL <- 0
  patients$SERTRALINE <- 0
  patients$otherDrugs <- NA
  for (i in 1:nrow(drug)){
    # Pass over any lines that have no information for medicinal product or generic name
    if (is.na(drug$medicinalproduct[i]) & is.null(drug$generic_name[[i]]) | drug$generic_name[[i]] == ""){
      next 
    }
    else if (str_detect(drug$medicinalproduct[i], "ESCITALOPRAM") == TRUE | str_detect(drug$generic_name[i], "ESCITALOPRAM") == TRUE){
      patients$ESCITALOPRAM[patients$index == drug$index[i]] <- 1
      drugFinal <- rbind(drugFinal, drug[i,])
    }else if (str_detect(drug$medicinalproduct[i], "CITALOPRAM") == TRUE | str_detect(drug$generic_name[i], "CITALOPRAM") == TRUE){
      patients$CITALOPRAM[patients$index == drug$index[i]] <- 1
      drugFinal <- rbind(drugFinal, drug[i,])
      
    }else if (str_detect(drug$medicinalproduct[i], "FLUOXETINE") == TRUE | str_detect(drug$generic_name[i], "FLUOXETINE") == TRUE){
      patients$FLUOXETINE[patients$index == drug$index[i]] <- 1
      drugFinal <- rbind(drugFinal, drug[i,])
      
    }else if (str_detect(drug$medicinalproduct[i], "PAXIL") == TRUE | str_detect(drug$generic_name[i], "PAXIL") == TRUE){
      patients$PAXIL[patients$index == drug$index[i]] <- 1
      drugFinal <- rbind(drugFinal, drug[i,])
      
    }else if (str_detect(drug$medicinalproduct[i], "SERTRALINE") == TRUE | str_detect(drug$generic_name[i], "SERTRALINE") == TRUE){
      patients$SERTRALINE[patients$index == drug$index[i]] <- 1
      drugFinal <- rbind(drugFinal, drug[i,])
      
    }else{
      if (drug$generic_name[i] != "NA" & !is.na(drug$generic_name[i])){
        patients$otherDrugs[patients$index == drug$index[i]] = paste(patients$otherDrugs[patients$index == drug$index[i]], drug$generic_name[i], sep = ';')
      }
    }
  }
  patient <- rbind(patient, patients[rowSums(patients[, ssri5]) > 0, names])
}

#### Run the code below after each a in 1:file_num complete loop, *IF* you are 
#### combining multiple patient and drugFinal data frames

if (!exists("patient_all")){
  patient_all <- data.frame() 
}

if (!exists("drugFinal_all")){
  drugFinal_all <- data.frame() 
}
patient_all <- rbind(patient_all, patient)
drugFinal_all <- rbind(drugFinal_all, drugFinal)

#####


#########################################
# Add columns for drug characterization #
#########################################

# Pull duplicate drug records (on index) - for exploration only
dupes <- drugFinal_all[duplicated(drugFinal_all$index) | duplicated(drugFinal_all$index, fromLast=TRUE),]

# Eliminate duplicates from primary data frame (match on index, medicinal product)
drugFinal_reduced <- drugFinal_all[!(duplicated(drugFinal_all[c("index","medicinalproduct")])), ]

patient <- patient_all

# Add column for drug characterization
patient$ESCITALOPRAM_char <- 0
patient$CITALOPRAM_char <- 0
patient$FLUOXETINE_char <- 0
patient$PAXIL_char <- 0
patient$SERTRALINE_char <- 0

drug <- drugFinal_all

for (i in 1:nrow(drug)){
  
  if (str_detect(drug$medicinalproduct[i], "ESCITALOPRAM") == TRUE | str_detect(drug$generic_name[i], "ESCITALOPRAM") == TRUE){
    patient$ESCITALOPRAM_char[patient$index == drug$index[i]] <- drug$drugcharacterization[i]
    
  } else if (str_detect(drug$medicinalproduct[i], "CITALOPRAM") == TRUE | str_detect(drug$generic_name[i], "CITALOPRAM") == TRUE){
    patient$CITALOPRAM_char[patient$index == drug$index[i]] <- drug$drugcharacterization[i]
    
  }else if (str_detect(drug$medicinalproduct[i], "FLUOXETINE") == TRUE | str_detect(drug$generic_name[i], "FLUOXETINE") == TRUE){
    patient$FLUOXETINE_char[patient$index == drug$index[i]] <- drug$drugcharacterization[i]
    
  }else if (str_detect(drug$medicinalproduct[i], "PAXIL") == TRUE | str_detect(drug$generic_name[i], "PAXIL") == TRUE){
    patient$PAXIL_char[patient$index == drug$index[i]] <- drug$drugcharacterization[i]
    
  }else if (str_detect(drug$medicinalproduct[i], "SERTRALINE") == TRUE | str_detect(drug$generic_name[i], "SERTRALINE") == TRUE){
    patient$SERTRALINE_char[patient$index == drug$index[i]] <- drug$drugcharacterization[i]
    
  }  
}

patient_all <- patient

#############################################################################
# Add columns for the 50 most commonly featured 'other' non-SSRI drugs,     #
# to indicate if patient was taking the drug as well                        #
#############################################################################

patient_all$otherDrugs

drugs <- unlist(strsplit(patient_all$otherDrugs, ";"))
drugs <- unlist(strsplit(drugs, ","))
remove <- c(NA, 'NA', '')
drugs <- drugs[! drugs %in% remove]
drugs <- unlist(strsplit(drugs, " AND "))
drugs <- trimws(drugs, which = "left")

drugs <- as.data.frame(drugs)

drugs <- as.data.frame(table(drugs))
drugs <- arrange(drugs, -Freq)

# Select the 50 most common 'other' drugs for use in our model
drugs_most_freq <- drugs[1:50,]

# Get columns for drug names for patient data frame
drug_names <- as.vector(drugs_most_freq$drugs)

# Create copy of patient data frame
patient <- patient_all

# Add columns of 50 most prevalent drugs to patient data frame
for (name in drug_names) {
  patient[[name]] <- 0
}

# Change drug names to character vectors
drugs$drugs <- as.character(drugs$drugs)
drugs_most_freq$drugs <- as.character(drugs_most_freq$drugs)

# Ensure that names of each of the drugs in most prevalent drugs
# only appear once in full list of drugs - get a count of the number
# of times each drug appears in other drug names
for (i in 1:nrow(drugs_most_freq)){
  count = 0
  string = ""
  for (j in 1:nrow(drugs)){
    if (str_detect(drugs$drugs[j], drugs_most_freq$drugs[i]) == TRUE & drugs_most_freq$drugs[i] != drugs$drugs[j]) {
      count = count + 1
      string <- paste(string, drugs$drugs[j], sep=",", collapse=NULL)
    }
  }
  drugs_most_freq$count[i] <- count 
  string <- substring(string, 2)
  drugs_most_freq$other_names[i] <- string
}

for (i in 1:nrow(drugs_most_freq)){
  count_freq = 0
  string = ""
  for (j in 1:nrow(drugs_most_freq)){
    if (str_detect(drugs_most_freq$drugs[j], drugs_most_freq$drugs[i]) == TRUE & drugs_most_freq$drugs[i] != drugs_most_freq$drugs[j]) {
      count_freq = count_freq + 1
      string <- paste(string, drugs_most_freq$drugs[j], sep=",", collapse=NULL)
    }
  }
  drugs_most_freq$count_freq[i] <- count_freq
  string <- substring(string, 2)
  drugs_most_freq$other_names_freq[i] <- string
}

# Add counts for each drug in otherDrugs that is in list of most frequent drugs
for (i in 1:nrow(patient)) {
  for (name in drug_names) {
    name_check <- paste0(",", name, ",")
    if (!is.na(patient$otherDrugs[i]) & str_detect(patient$otherDrugs[i], name_check) == TRUE) {
      patient[[name]][i] <- 1
    }
  }
}

patient_all <- patient

#####################################

# CALCULATED AGE (using age units) - new column, in years = 'age'
# REMOVED DUPLICATES - unique obs = 145,647
# REMOVED ALL AGES BELOW 12 - (2,338), total obs = 143,309

df <- patient_all

#################
# Clean Dataset #
#################
# DUPLICATE INDICES
sum(duplicated(df$index))
dup <- df[duplicated(df$index) | duplicated(df$index, fromLast=T),] # See all duplicated observations
# REMOVE DUPLICATES
df <- unique(df) # 145,647 unique observations

## Convert age and weight from chr to num
df$patientonsetage <- as.numeric(df$patientonsetage)
df$patientweight <- as.numeric(df$patientweight)

#################
# Calculate age #
#################
table(df$patientonsetageunit) # majority of ages are years

## Create new column to input recalculated ages
df$age <- NA

## Recalculate ages of unit 'decades' (800)
#decades <- df[(df$patientonsetageunit=="800" & !is.na(df$patientonsetageunit)),] # Take a look, 202 obs
df$age <- ifelse((df$patientonsetageunit=="800" & !is.na(df$patientonsetageunit)), df$patientonsetage*10, df$age)
sum(!is.na(df$age)) # Check, 202 not NA

## Recalculate ages of unit 'months' (802)
#months <- df[(df$patientonsetageunit=="802"& !is.na(df$patientonsetageunit)),] # Take a look, 641 obs
df$age <- ifelse((df$patientonsetageunit=="802"& !is.na(df$patientonsetageunit)), df$patientonsetage/12, df$age)
sum(!is.na(df$age)) # Check, 843 not NA

## Recalculate ages of unit 'weeks' (803), assuming 52 weeks in a year
#weeks <- df[(df$patientonsetageunit=="803" & !is.na(df$patientonsetageunit)),] # Take a look, 71 obs
df$age <- ifelse((df$patientonsetageunit=="803" & !is.na(df$patientonsetageunit)), df$patientonsetage/52, df$age)
sum(!is.na(df$age)) # Check, 914 not NA

## Recalculate ages of unit 'days' (804), assuming 365 days in a year
#days <- df[(df$patientonsetageunit=="804" & !is.na(df$patientonsetageunit)),] # Take a look, 1785 obs
df$age <- ifelse((df$patientonsetageunit=="804" & !is.na(df$patientonsetageunit)), df$patientonsetage/365, df$age)
sum(!is.na(df$age)) # Check, 2699 not NA

## Recalculate ages of unit 'hours' (805), assuming 8760 hours in a year
#hours <- df[(df$patientonsetageunit=="805"& !is.na(df$patientonsetageunit)),] # Take a look, 71 obs
df$age <- ifelse((df$patientonsetageunit=="805" & !is.na(df$patientonsetageunit)), df$patientonsetage/8760, df$age)
sum(!is.na(df$age)) # Check, 2770 not NA

## Ages of unit 'years' (801) stays the same
#years <- df[(df$patientonsetageunit=="801" & !is.na(df$patientonsetageunit)),] # Take a look, 97248 obs
df$age <- ifelse((df$patientonsetageunit=="801" & !is.na(df$patientonsetageunit)), df$patientonsetage, df$age)
sum(!is.na(df$age)) # Check, 100016 not NA (would be 100018, but 2 obs with unit=801 are missing age data)

###########################
# ONLY KEEP AGES ABOVE 12 #
###########################
del1 <- df[(df$age<12 & !is.na(df$age)),] # =2,308 observations
df <- df[(df$age>=12 | is.na(df$age)),] # =143,339 observations
# There are still 30 observations of onsetage=0 but no age unit - remove those as well.
zero <- df[df$patientonsetage==0 &!is.na(df$patientonsetage),] # see the 30
df <- df[df$patientonsetage!=0 | is.na(df$patientonsetage),] # remove them, now 143,309 observations

##### 
# Age Values without Age Units & Vice Versa
##### 
ageNA <- df[is.na(df$patientonsetage) & !is.na(df$patientonsetageunit),] #2 observations with a unit but no age
unitNA <- df[!is.na(df$patientonsetage) & is.na(df$patientonsetageunit),] #605 observations with an age but no unit

#################################
# Explore newly calculated ages #
#################################
summary(df$age)

hist(plot$age, breaks=150000)
hist(plot$age, breaks=200)
hist(plot$age, breaks=80)

#######################
# Look at missingness #
#######################
# Look at patterns of missingness
sapply(df, function(x) sum(is.na(x))/nrow(df))

patient_all <- df

  
  
####################################################
###         Standarizing dosage interval        ####
####################################################
  
drugFinal_all[,c(6:12)] <- sapply(drugFinal_all[,c(6:12)], as.numeric)
patient_all$patientweight <- as.numeric(patient_all$patientweight)
drugFinal_all[is.na(drugFinal_all)] <- -3

drugFinal_all$dosage <- ifelse(drugFinal_all$drugintervaldosagedefinition == 801, drugFinal_all$drugintervaldosageunitnumb*365, drugFinal_all$drugintervaldosageunitnumb)
drugFinal_all$dosage <- ifelse(drugFinal_all$drugintervaldosagedefinition == 802, drugFinal_all$drugintervaldosageunitnumb*30, drugFinal_all$dosage)
drugFinal_all$dosage <- ifelse(drugFinal_all$drugintervaldosagedefinition == 803, drugFinal_all$drugintervaldosageunitnumb*7, drugFinal_all$dosage)
drugFinal_all$dosage <- ifelse(drugFinal_all$drugintervaldosagedefinition == 805, drugFinal_all$drugintervaldosageunitnumb/24, drugFinal_all$dosage)
drugFinal_all$dosage <- ifelse(drugFinal_all$drugintervaldosagedefinition == 807, drugFinal_all$drugintervaldosageunitnumb*90, drugFinal_all$dosage)
drugFinal_all$dosage <- ifelse(drugFinal_all$drugintervaldosagedefinition == 811, drugFinal_all$drugintervaldosageunitnumb*90, drugFinal_all$dosage)
drugFinal_all$dosage <- ifelse(drugFinal_all$drugintervaldosagedefinition == 812, -1, drugFinal_all$dosage)
drugFinal_all$dosage <- ifelse(drugFinal_all$drugintervaldosagedefinition == 813, -2, drugFinal_all$dosage)

ssri5 <- c("ESCITALOPRAM_dosage","CITALOPRAM_dosage","FLUOXETINE_dosage","PAXIL_dosage","SERTRALINE_dosage")
patient_all[, ssri5] <- 0
for (i in 1:nrow(drugFinal_all)){
  if (str_detect(drugFinal_all$medicinalproduct[i], "ESCITALOPRAM") == TRUE | str_detect(drugFinal_all$generic_name[i], "ESCITALOPRAM") == TRUE){
    patient_all[patient_all$index == drugFinal_all$index[i], "ESCITALOPRAM_dosage"] <- drugFinal_all$dosage[i]
  }else if (str_detect(drugFinal_all$medicinalproduct[i], "CITALOPRAM") == TRUE | str_detect(drugFinal_all$generic_name[i], "CITALOPRAM") == TRUE){
    patient_all[patient_all$index == drugFinal_all$index[i], "CITALOPRAM_dosage"] <- drugFinal_all$dosage[i]
  }else if (str_detect(drugFinal_all$medicinalproduct[i], "FLUOXETINE") == TRUE | str_detect(drugFinal_all$generic_name[i], "FLUOXETINE") == TRUE){
    patient_all[patient_all$index == drugFinal_all$index[i], "FLUOXETINE_dosage"] <- drugFinal_all$dosage[i]
  }else if (str_detect(drugFinal_all$medicinalproduct[i], "PAXIL") == TRUE | str_detect(drugFinal_all$generic_name[i], "PAXIL") == TRUE){
    patient_all[patient_all$index == drugFinal_all$index[i], "PAXIL_dosage"] <- drugFinal_all$dosage[i]
  }else if (str_detect(drugFinal_all$medicinalproduct[i], "SERTRALINE") == TRUE | str_detect(drugFinal_all$generic_name[i], "SERTRALINE") == TRUE){
    patient_all[patient_all$index == drugFinal_all$index[i], "SERTRALINE_dosage"] <- drugFinal_all$dosage[i]
  }
}

patient_all <- patient_all[patient_all$patientweight <= 250 & patient_all$patientweight >= 25, ]


plot(patient_all$age, patient_all$patientweight)
abline(h = 25)

temp <- patient_all[patient_all$patientweight < 25 & !is.na(patient_all$patientweight), ]
plot(temp$age, temp$patientweight)

####################################################
###           Multiple Imputation               ####
####################################################

#Dropping irrelevant columns and giving them proper datatypes
patient_all2 <- patient_all
patient_all2$seriousnessother[is.na(patient_all2$seriousnessother)]<-0
patient_all2$seriousnesshospitalization[is.na(patient_all2$seriousnesshospitalization)]<-0
patient_all2$seriousnessdisabling[is.na(patient_all2$seriousnessdisabling)]<-0
patient_all2$seriousnesslifethreatening[is.na(patient_all2$seriousnesslifethreatening)]<-0
patient_all2$seriousnessdeath[is.na(patient_all2$seriousnessdeath)]<-0
patient_all2$serious[patient_all2$serious==2]<-0

patient_all2$seriousnessother <- as.numeric(patient_all2$seriousnessother)
patient_all2$serious <- as.numeric(patient_all2$serious)
patient_all2$seriousnesshospitalization <- as.numeric(patient_all2$seriousnesshospitalization)
patient_all2$seriousnessdisabling <- as.numeric(patient_all2$seriousnessdisabling)
patient_all2$seriousnesslifethreatening <- as.numeric(patient_all2$seriousnesslifethreatening)
patient_all2$seriousnessdeath <- as.numeric(patient_all2$seriousnessdeath)

num_col <- c("ESCITALOPRAM_char", "CITALOPRAM_char", "FLUOXETINE_char", "PAXIL_char","SERTRALINE_char")
patient_all2[,num_col]<-apply(patient_all2[, num_col], 2, function(x) as.numeric(as.character(x)))

patient_all2$patientagegroup <- NULL
patient_all2$patientonsetageunit <- NULL
patient_all2$patientonsetage <- NULL
patient_all2$index<- NULL
patient_all2$otherDrugs<- NULL
patient_all2$patientsex <- as.factor(patient_all2$patientsex)

imp.patient <- mice(data=patient_all2, m=10, method = "pmm")

imputed_patients<-complete(imp.patient,1)

#putting the index back into imputed_patients 
imputed_patients$index <- patient_all$index
imputed_patients2 <- imputed_patients[,c(75,1:74)]
imputed_patients<-imputed_patients2
rm(imputed_patients2)

########################################################################################
############################## DATAFRAMES FOR MODEL INPUT ##############################
# In this file, we subset our full dataframe to create dataframes for modeling.        #
# We have 2 different sets of models:                                                  #
#     (1) Binary Response - Serious or Not Serious                                     #  
#     (2) Multiclass - for Serious events that have a specific 'seriousness' category, #
#         predict the specific category.                                               #
# Within these 2 sets models, we subset by SSRI. If an SSRI is listed for a individual,#
# they will be included in that subsetted dataframe. Furthermore, the                  #
# 'drug characterization' for that SSRI will be included in that dataframe.            #
# NOTE: Individuals can have more than one SSRI listed. If so, they are included in    #
#       the dataframes for each corresponding SSRI                                     #
#                                                                                      #
# Other Actions:                                                                       #
#  - Remove 'unknown' genders (patientsex=0)                                           #
#  - Calculate total number of SSRI's taken by given individual                        #
#  - Create Seriousness Levels                                                         #     
########################################################################################

fileName <- "Imputed_Missing.rdata"
load(fileName)
df <-imputed_patients

#################################
### Remove Gender=0 (Unknown) ###
#################################
df <- df[df$patientsex!=0,]
## Drop the extraneous level
df <- droplevels(df)
## Convert to numeric (for Lasso Regression in later code)
df$patientsex<-as.numeric(df$patientsex)
df$patientsex <- df$patientsex-1

#######################################
### Define column names of interest ###
#######################################
drugCols <- names(df)[25:74] #### MAY NEED TO REDEFINE, DEPENDING ON COLUMN SETUP
ssri <- c("ESCITALOPRAM","CITALOPRAM","FLUOXETINE","PAXIL","SERTRALINE")
df$number_ssri <- rowSums(df[,ssri])

###########################################
### Create Seriousness Levels - 'level' ###
###########################################
df$level <- NA
df$level <- ifelse(df$seriousnesshospitalization == 1 & df$seriousnessdisabling == 1 & df$seriousnesslifethreatening == 1, "hdl", df$level)
df$level <- ifelse(df$seriousnesshospitalization == 1 & df$seriousnessdisabling == 1 & df$seriousnesslifethreatening == 0, "hd", df$level)
df$level <- ifelse(df$seriousnesshospitalization == 1 & df$seriousnessdisabling == 0 & df$seriousnesslifethreatening == 1, "hl", df$level)
df$level <- ifelse(df$seriousnesshospitalization == 0 & df$seriousnessdisabling == 1 & df$seriousnesslifethreatening == 1, "dl", df$level)
df$level <- ifelse(df$seriousnesshospitalization == 1 & df$seriousnessdisabling == 0 & df$seriousnesslifethreatening == 0, "h", df$level)
df$level <- ifelse(df$seriousnesshospitalization == 0 & df$seriousnessdisabling == 1 & df$seriousnesslifethreatening == 0, "d", df$level)
df$level <- ifelse(df$seriousnesshospitalization == 0 & df$seriousnessdisabling == 0 & df$seriousnesslifethreatening == 1, "l", df$level)
df$level <- ifelse(df$seriousnessdeath == 1, "death", df$level)
df$level <- ifelse(df$seriousnessother == 1 & is.na(df$level), "other", df$level)
table(df$level)

### Convert level to Factor
df$level <- as.factor(df$level)

### Relevel so that "death" (variable of primary interest) is baseline
df$level <- relevel(df$level, ref="death")

#######################################
### Split into Training and Testing ###
#######################################
set.seed(12345)
training.indices <- sample(1:nrow(df), as.integer(nrow(df) * 0.80))
train <- df[training.indices,]
test <- df[-training.indices,]
total <- df #Store total dataset for any checks we perform after

########################
### Binary Responses ###
########################
df<-train
for (i in ssri){
  specChar <- paste(i, "_char", sep="")
  vars <- c(specChar,"patientweight","patientsex","age",drugCols,"number_ssri","serious")
  dfName <- paste("binaryTrain", i, sep="_")
  assign(dfName, df[df[i]=="1",vars])
}

df<-test
for (i in ssri){
  specChar <- paste(i, "_char", sep="")
  vars <- c(specChar,"patientweight","patientsex","age",drugCols,"number_ssri","serious")
  dfName <- paste("binaryTest", i, sep="_")
  assign(dfName, df[df[i]=="1",vars])
}

######################################
### Seriousness Category Responses ###
######################################
df <- train[!is.na(train$level),] # Subset dataframe to only observations with a seriousness category
for (i in ssri){
  specChar <- paste(i, "_char", sep="")
  vars <- c(specChar,"patientweight","patientsex","age",drugCols,"number_ssri","level")
  dfName <- paste("multiTrain", i, sep="_")
  assign(dfName, df[df[i]=="1",vars])
}

df <- test[!is.na(test$level),] # Subset dataframe to only observations with a seriousness category
for (i in ssri){
  specChar <- paste(i, "_char", sep="")
  vars <- c(specChar,"patientweight","patientsex","age",drugCols,"number_ssri","level")
  dfName <- paste("multiTest", i, sep="_")
  assign(dfName, df[df[i]=="1",vars])
}

#######################
### Save Dataframes ###
#######################
rm(patient, patient_all, patient_all2, drugs, drugs_most_freq, drugFinal_all, df)
save.image(file="modelDataframes.RData")

###################################################################
###
### CHECKS
###
df <- total
sum(!is.na(df$level))

sum(nrow(multiTrain_CITALOPRAM),nrow(multiTrain_ESCITALOPRAM),nrow(multiTrain_FLUOXETINE),nrow(multiTrain_PAXIL),nrow(multiTrain_SERTRALINE))
sum(nrow(multiTest_CITALOPRAM),nrow(multiTest_ESCITALOPRAM),nrow(multiTest_FLUOXETINE),nrow(multiTest_PAXIL),nrow(multiTest_SERTRALINE))
83136 + 20850

sum(nrow(binaryTrain_CITALOPRAM),nrow(binaryTrain_ESCITALOPRAM),nrow(binaryTrain_FLUOXETINE),nrow(binaryTrain_PAXIL),nrow(binaryTrain_SERTRALINE))
sum(nrow(binaryTest_CITALOPRAM),nrow(binaryTest_ESCITALOPRAM),nrow(binaryTest_FLUOXETINE),nrow(binaryTest_PAXIL),nrow(binaryTest_SERTRALINE))
115947 + 28990



####################################################
###             Model Building                  ####
####################################################

#####################
### CILATROPRAM   ###
#####################

###Lasso using multiTrain_CITALOPRAM as an example

#Training the 'LASSO'
multiTrain_CITALOPRAM.m <- as.matrix(multiTrain_CITALOPRAM)
View(multiTrain_CITALOPRAM.m)
multiTrain_CITALOPRAM.lasso <- glmnet(multiTrain_CITALOPRAM.m [,-c(56)], multiTrain_CITALOPRAM.m [,c(56)], alpha=1, family="multinomial")
plot(multiTrain_CITALOPRAM.lasso ,xvar="lambda",label=TRUE)

#predicting
dummy <- multiTest_CITALOPRAM
dummy$level <- NULL
dummy.m <- data.matrix(dummy)

#NOTE: Make sure to explore appropriate log(lambda) value in the 's' parameter
multiTest_CITALOPRAM$predict <- predict(multiTrain_CITALOPRAM.lasso, newx=dummy.m, s=exp(-5.9), family="multinomial", type="class")

#checking accuracy of predictions
sum(multiTest_CITALOPRAM$level==multiTest_CITALOPRAM$predict)/nrow(multiTest_CITALOPRAM)
table(multiTest_CITALOPRAM$predict)

#Accuracy for s=exp(-7.7) is 0.4337885
#accuracy for s=exp(-7.3) is 0.4339473*
#Accuracy for s=exp(-5.9) is 0.432677


#checking coefficients 
#NOTE: make sure to match the 's' parameter to that in line 19
coef(multiTrain_CITALOPRAM.lasso, s=exp(-7.3))

#NAIVE model predicts the most frequent level 
table(multiTrain_CITALOPRAM$level)
sum(multiTest_CITALOPRAM$level=='h')/nrow(multiTest_CITALOPRAM)
#0.035757 accuracy

#Precision and Recall for multiTrain_CITALOPRAM
# Get confusion matrix for multiclass predictor
lvls <- c('death','d','dl','h','hd','hdl','hl','l','other')
CITALOPRAM_mat <- table(factor(multiTest_CITALOPRAM$predict, levels=lvls), multiTest_CITALOPRAM$level)

# Get precision for each class 
CITALOPRAM_precision <- (precision <- diag(CITALOPRAM_mat) / rowSums(CITALOPRAM_mat))
CITALOPRAM_precision
#Death            h         other
#0.0259542    0.4462520     0.39287642

# Get recall for each class
CITALOPRAM_recall <- (diag(CITALOPRAM_mat) / colSums(CITALOPRAM_mat))
CITALOPRAM_recall
#d              h           other
#0.1611374    0.6212256     0.3616493

#Lasso using binaryTrain_CITALOPRAM
binaryTrain_CITALOPRAM.m <- as.matrix(binaryTrain_CITALOPRAM)

binaryTrain_CITALOPRAM.lasso <- glmnet(binaryTrain_CITALOPRAM.m [,-c(56)], 
                                       binaryTrain_CITALOPRAM.m [,c(56)], alpha=1, family="binomial")
plot(binaryTrain_CITALOPRAM.lasso ,xvar="lambda",label=TRUE)

lamdas_to_try <- c(-6.5,-5.5,-4.25)

#predicting
dummy <- binaryTest_CITALOPRAM
dummy$serious <- NULL
dummy.m <- data.matrix(dummy)

#NOTE: Make sure to explore appropriate log(lambda) value in the 's' parameter
s <- exp(-6.5)
binaryTest_CITALOPRAM$predict <- predict(binaryTrain_CITALOPRAM.lasso, newx=dummy.m, 
                                         s=s, family="binomial", type="class")

#checking accuracy of predictions
sum(binaryTest_CITALOPRAM$serious==binaryTest_CITALOPRAM$predict)/nrow(binaryTest_CITALOPRAM)
table(binaryTest_CITALOPRAM$predict)
table(binaryTrain_CITALOPRAM$serious)

#accuracy for Lambda = exp(-6.5), 0.7694373
#accuracy for Lambda = exp(-5.5), 0.7676065
#accuracy for Lambda = exp(-4.25), 0.7676065

#Naive Model Predicts
table(binaryTrain_CITALOPRAM$serious)
sum(binaryTest_CITALOPRAM$serious=='1')/nrow(binaryTest_CITALOPRAM)
#0.773099

#checking coefficients 
#NOTE: make sure to match the 's' parameter to that in line 19
coef(binaryTrain_CITALOPRAM.lasso, s=s)

#ROC curve for binaryTrain_CITALOPRAM
probs <- predict(binaryTrain_CITALOPRAM.lasso, newx=dummy.m, s=s, family="binomial", type = "response")
length(binaryTest_CITALOPRAM$serious)
length(probs)

# ROCR prediction object and extract ROC curve and AUC
prediction <- prediction(probs, binaryTest_CITALOPRAM$serious)
roc.curve <- performance(prediction,"tpr","fpr")
auc = performance(prediction, "auc")@y.values[[1]]

# plot
plot(roc.curve, main = paste("ROC (AUC=", round(auc,2), ")", sep=""))
abline(0, 1, lty="dashed")

#######################
### ESCILATROPRAM   ###
#######################

###Lasso using multiTrain_ESCITALOPRAM as an example

#Training the 'LASSO'
multiTrain_ESCITALOPRAM.m <- as.matrix(multiTrain_ESCITALOPRAM)
View(multiTrain_ESCITALOPRAM.m)
multiTrain_ESCITALOPRAM.lasso <- glmnet(multiTrain_ESCITALOPRAM.m [,-c(56)], multiTrain_ESCITALOPRAM.m [,c(56)], alpha=1, family="multinomial")
plot(multiTrain_ESCITALOPRAM.lasso ,xvar="lambda",label=TRUE)

#predicting
dummy <- multiTest_ESCITALOPRAM
dummy$level <- NULL
dummy.m <- data.matrix(dummy)

#NOTE: Make sure to explore appropriate log(lambda) value in the 's' parameter
multiTest_ESCITALOPRAM$predict <- predict(multiTrain_ESCITALOPRAM.lasso, newx=dummy.m, s=exp(-5.5), family="multinomial", type="class")

#checking accuracy of predictions
sum(multiTest_ESCITALOPRAM$level==multiTest_ESCITALOPRAM$predict)/nrow(multiTest_ESCITALOPRAM)
table(multiTest_ESCITALOPRAM$predict)

#Accuracy for s=exp(-8.5) is 0.4553
#accuracy for s=exp(-7) is 0.4559
#Accuracy for s=exp(-5.5) is 0.4559


#checking coefficients 
#NOTE: make sure to match the 's' parameter to that in line 19
coef(multiTrain_ESCITALOPRAM.lasso, s=exp(-5.5))

#NAIVE model predicts the most frequent level 
table(multiTrain_ESCITALOPRAM$level)
sum(multiTest_ESCITALOPRAM$level=='h')/nrow(multiTest_ESCITALOPRAM)
#0.4062168 accuracy

#Precision and Recall for multiTrain_ESCITALOPRAM
# Get confusion matrix for multiclass predictor
lvls <- c('death','d','dl','h','hd','hdl','hl','l','other')
ESCITALOPRAM_mat <- table(factor(multiTest_ESCITALOPRAM$predict, levels=lvls), multiTest_ESCITALOPRAM$level)

# Get precision for each class 
ESCITALOPRAM_precision <- (precision <- diag(ESCITALOPRAM_mat) / rowSums(ESCITALOPRAM_mat))
ESCITALOPRAM_precision
#h              other
#0.4602740     0.4560616

# Get recall for each class
ESCITALOPRAM_recall <- (diag(ESCITALOPRAM_mat) / colSums(ESCITALOPRAM_mat))
ESCITALOPRAM_recall
#h            other
#0.6592544    0.5049716 

#Lasso using binaryTrain_ESCITALOPRAM
binaryTrain_ESCITALOPRAM.m <- as.matrix(binaryTrain_ESCITALOPRAM)

binaryTrain_ESCITALOPRAM.lasso <- glmnet(binaryTrain_ESCITALOPRAM.m [,-c(56)], 
                                         binaryTrain_ESCITALOPRAM.m [,c(56)], alpha=1, family="binomial")
plot(binaryTrain_ESCITALOPRAM.lasso ,xvar="lambda",label=TRUE)

lamdas_to_try <- c(-6.5,-5.5,-4.25)

#predicting
dummy <- binaryTest_ESCITALOPRAM
dummy$serious <- NULL
dummy.m <- data.matrix(dummy)

#NOTE: Make sure to explore appropriate log(lambda) value in the 's' parameter
s <- exp(-5.5)
binaryTest_ESCITALOPRAM$predict <- predict(binaryTrain_ESCITALOPRAM.lasso, newx=dummy.m, 
                                           s=s, family="binomial", type="class")

#checking accuracy of predictions
sum(binaryTest_ESCITALOPRAM$serious==binaryTest_ESCITALOPRAM$predict)/nrow(binaryTest_ESCITALOPRAM)
table(binaryTest_ESCITALOPRAM$predict)
table(binaryTrain_ESCITALOPRAM$serious)

#accuracy for Lambda = exp(-6.5), 0.6976744
#accuracy for Lambda = exp(-5.5), 0.6980407**
#accuracy for Lambda = exp(-4.25),0.6973082

#Naive Model Predicts
table(binaryTrain_ESCITALOPRAM$serious)
sum(binaryTest_ESCITALOPRAM$serious=='1')/nrow(binaryTest_ESCITALOPRAM)
#0.6947446

#checking coefficients 
#NOTE: make sure to match the 's' parameter to that in line 19
coef(binaryTrain_ESCITALOPRAM.lasso, s=s)

#ROC curve for binaryTrain_ESCITALOPRAM
probs <- predict(binaryTrain_ESCITALOPRAM.lasso, newx=dummy.m, s=s, family="binomial", type = "response")
length(binaryTest_ESCITALOPRAM$serious)
length(probs)

# ROCR prediction object and extract ROC curve and AUC
prediction <- prediction(probs, binaryTest_ESCITALOPRAM$serious)
roc.curve <- performance(prediction,"tpr","fpr")
auc = performance(prediction, "auc")@y.values[[1]]

# plot
plot(roc.curve, main = paste("ROC (AUC=", round(auc,2), ")", sep=""))
abline(0, 1, lty="dashed")

#######################
###   SERTRALINE    ###
#######################

# Lasso using binaryTrain_SERTRALINE
binaryTrain_SERTRALINE.m <- data.matrix(binaryTrain_SERTRALINE)

binaryTrain_SERTRALINE.lasso <- glmnet(binaryTrain_SERTRALINE.m [,-c(56)], 
                                       binaryTrain_SERTRALINE.m [,c(56)], alpha=1, family="binomial")
plot(binaryTrain_SERTRALINE.lasso ,xvar="lambda",label=TRUE)
title("LASSO Trace Plot: Sertraline", line=3)

lamdas_to_try <- c(-6.5,-5.5,-4.25)

#predicting
dummy <- binaryTest_SERTRALINE
dummy$serious <- NULL
dummy$predict <- NULL
dummy.m <- data.matrix(dummy)

#NOTE: Make sure to explore appropriate log(lambda) value in the 's' parameter
s <- exp(-4.25)
binaryTest_SERTRALINE$predict <- predict(binaryTrain_SERTRALINE.lasso, newx=dummy.m, 
                                         s=s, family="binomial", type="class")

#checking accuracy of predictions
sum(binaryTest_SERTRALINE$serious==binaryTest_SERTRALINE$predict)/nrow(binaryTest_SERTRALINE)
table(binaryTest_SERTRALINE$predict)
table(binaryTrain_SERTRALINE$serious)

#checking coefficients 
#NOTE: make sure to match the 's' parameter to that in line 19
coef(binaryTrain_SERTRALINE.lasso, s=s)

# Lambda = exp(-6.5), performance 0.6884905
# Lambda = exp(-5.5), performance 0.6885998 ** best performance
# Lambda = exp(-4.8), performance 0.6863045 

#NAIVE model predicts the most frequent level  = 0.6863045
table(binaryTrain_SERTRALINE$serious)
sum(binaryTest_SERTRALINE$serious=='1')/nrow(binaryTest_SERTRALINE)

# model with best lambda
s <- exp(lamdas_to_try[2])
probs <- predict(binaryTrain_SERTRALINE.lasso, newx=dummy.m, s=s, family="binomial", type = "response")
length(binaryTest_SERTRALINE$serious)
length(probs)

sort(as.numeric(probs))

# ROCR prediction object and extract ROC curve and AUC
prediction <- prediction(probs, binaryTest_SERTRALINE$serious)
roc.curve <- performance(prediction,"tpr","fpr")
auc = performance(prediction, "auc")@y.values[[1]]

# plot
plot(roc.curve, main = paste("ROC for 'Serious' Predictive Model: Sertraline \n(AUC=", round(auc,2), ")", sep=""))
abline(0, 1, lty="dashed")

######################################################################

###Lasso using multiTrain_SERTRALINE 
#Training the 'LASSO'
multiTrain_SERTRALINE.m <- as.matrix(multiTrain_SERTRALINE)

multiTrain_SERTRALINE.lasso <- glmnet(multiTrain_SERTRALINE.m [,-c(56)], 
                                      multiTrain_SERTRALINE.m [,c(56)], alpha=1, family="multinomial")
plot(multiTrain_SERTRALINE.lasso ,xvar="lambda",label=TRUE)

lamdas_to_try <- c(-10.5,-6.5,-4.8)

#predicting
dummy <- multiTest_SERTRALINE
dummy$predict <- NULL
dummy$level <- NULL
dummy.m <- data.matrix(dummy)

#NOTE: Make sure to explore appropriate log(lambda) value in the 's' parameter
s <- exp(-6.5)
multiTest_SERTRALINE$predict <- predict(multiTrain_SERTRALINE.lasso, newx=dummy.m, 
                                        s=s, family="multinomial", type="class")

#checking accuracy of predictions
sum(multiTest_SERTRALINE$level==multiTest_SERTRALINE$predict)/nrow(multiTest_SERTRALINE)
table(multiTest_SERTRALINE$predict)
table(multiTrain_SERTRALINE$level)

#checking coefficients 
#NOTE: make sure to match the 's' parameter to that in line 19
options(scipen=999)
coef(multiTrain_SERTRALINE.lasso, s=s)$death

# Lambda = exp(-10.5), performance 0.4664532
# Lambda = exp(-6.5), performance 0.4675741 ** best
# Lambda = exp(-4.8), performance 0.4630905

#NAIVE model predicts the most frequent level  = 0.4131305
table(multiTrain_SERTRALINE$level)
sum(multiTest_SERTRALINE$level=='other')/nrow(multiTest_SERTRALINE)

####### 

# Get confusion matrix for multiclass predictor
lvls <- c('death','d','dl','h','hd','hdl','hl','l','other')
SERTRALINE_mat <- table(factor(multiTest_SERTRALINE$predict, levels=lvls), multiTest_SERTRALINE$level)

# Get precision for each class 
SERTRALINE_precision <- (precision <- diag(SERTRALINE_mat) / rowSums(SERTRALINE_mat))
SERTRALINE_precision

# Get recall for each class
SERTRALINE_recall <- (diag(SERTRALINE_mat) / colSums(SERTRALINE_mat))
SERTRALINE_recall

#######

#######################
###      PAXIL      ###
###  (PAROXETINE)   ### 
#######################

# Lasso using binaryTrain_PAXIL
binaryTrain_PAXIL.m <- data.matrix(binaryTrain_PAXIL)

binaryTrain_PAXIL.lasso <- glmnet(binaryTrain_PAXIL.m [,-c(56)], 
                                  binaryTrain_PAXIL.m [,c(56)], alpha=1, family="binomial")
plot(binaryTrain_PAXIL.lasso ,xvar="lambda",label=TRUE)
title("LASSO Trace Plot: PAXIL", line=3)

lamdas_to_try <- c(-6.0, -6.5, -4.6, -3.6)

#predicting
dummy <- binaryTest_PAXIL
dummy$serious <- NULL
dummy$predict <- NULL
dummy.m <- data.matrix(dummy)

#NOTE: Make sure to explore appropriate log(lambda) value in the 's' parameter
s <- exp(lamdas_to_try[1])
binaryTest_PAXIL$predict <- predict(binaryTrain_PAXIL.lasso, newx=dummy.m, 
                                    s=s, family="binomial", type="class")

#checking accuracy of predictions
sum(binaryTest_PAXIL$serious==binaryTest_PAXIL$predict)/nrow(binaryTest_PAXIL)
table(binaryTest_PAXIL$predict)
table(binaryTrain_PAXIL$serious)

#checking coefficients 
#NOTE: make sure to match the 's' parameter to that in line 19
coef(binaryTrain_PAXIL.lasso, s=s)

# Lambda = exp(-6.0), performance 0.688417 ** best performance
# Lambda = exp(-6.5), performance 0.6872587
# Lambda = exp(-4.6), performance 0.6861004
# Lambda = exp(-3.6), performance 0.6837838

#NAIVE model predicts the most frequent level  = 0.6853282
table(binaryTrain_PAXIL$serious)
sum(binaryTest_PAXIL$serious=='1')/nrow(binaryTest_PAXIL)

# model with best lambda
s <- exp(lamdas_to_try[1])
probs <- predict(binaryTrain_PAXIL.lasso, newx=dummy.m, s=s, family="binomial", type = "response")
length(binaryTest_PAXIL$serious)
length(probs)

sort(as.numeric(probs))

# ROCR prediction object and extract ROC curve and AUC
prediction <- prediction(probs, binaryTest_PAXIL$serious)
roc.curve <- performance(prediction,"tpr","fpr")
auc = performance(prediction, "auc")@y.values[[1]]

# plot
plot(roc.curve, main = paste("ROC for 'Serious' Predictive Model: PAXIL \n(AUC=", round(auc,2), ")", sep=""))
abline(0, 1, lty="dashed")

######################################################################

###Lasso using multiTrain_PAXIL 
#Training the 'LASSO'
multiTrain_PAXIL.m <- as.matrix(multiTrain_PAXIL)

multiTrain_PAXIL.lasso <- glmnet(multiTrain_PAXIL.m [,-c(56)], 
                                 multiTrain_PAXIL.m [,c(56)], alpha=1, family="multinomial")
plot(multiTrain_PAXIL.lasso ,xvar="lambda",label=TRUE)

lamdas_to_try <- c(-9.75, -6.4, -6.2, -4.5)

#predicting
dummy <- multiTest_PAXIL
dummy$predict <- NULL
dummy$level <- NULL
dummy.m <- data.matrix(dummy)

#NOTE: Make sure to explore appropriate log(lambda) value in the 's' parameter
s <- exp(lamdas_to_try[2])
multiTest_PAXIL$predict <- predict(multiTrain_PAXIL.lasso, newx=dummy.m, 
                                   s=s, family="multinomial", type="class")

#checking accuracy of predictions
sum(multiTest_PAXIL$level==multiTest_PAXIL$predict)/nrow(multiTest_PAXIL)
table(multiTest_PAXIL$predict)
table(multiTrain_PAXIL$level)

#checking coefficients 
#NOTE: make sure to match the 's' parameter to that in line 19
options(scipen=999)
coef(multiTrain_PAXIL.lasso, s=s)$death


lamdas_to_try <- c(-9.75, -6.4, -6.2, -4.5)
# Lambda = exp(-9.75), performance 0.4410857
# Lambda = exp(-6.4), performance 0.4441703 
# Lambda = exp(-6.2), performance 0.4466379 ** best
# Lambda = exp(-4.5), performance 0.4441703

#NAIVE model predicts the most frequent level  = 0.407773
table(multiTrain_PAXIL$level)
sum(multiTest_PAXIL$level=='other')/nrow(multiTest_PAXIL)

####### 

# Get confusion matrix for multiclass predictor
lvls <- c('death','d','dl','h','hd','hdl','hl','l','other')
PAXIL_mat <- table(factor(multiTest_PAXIL$predict, levels=lvls), multiTest_PAXIL$level)

# Get precision for each class 
PAXIL_precision <- (precision <- diag(PAXIL_mat) / rowSums(PAXIL_mat))
PAXIL_precision

# Get recall for each class
PAXIL_recall <- (diag(PAXIL_mat) / colSums(PAXIL_mat))
PAXIL_recall

#######

#######################
###   FLUOXETINE    ### 
#######################

############################ Binary  ############################
### Train the 'LASSO'
binaryTrain_FLUOXETINE.m <- as.matrix(binaryTrain_FLUOXETINE)
binaryTrain_FLUOXETINE.lasso <- glmnet(binaryTrain_FLUOXETINE.m [,-c(56)], binaryTrain_FLUOXETINE.m [,c(56)], alpha=1, family="binomial")
plot(binaryTrain_FLUOXETINE.lasso, xvar="lambda", label=TRUE)

### Predict on Testing Set 
# NAIVE model predicts the most frequent level 
table(binaryTrain_FLUOXETINE$serious)
sum(binaryTest_FLUOXETINE$serious=='1')/nrow(binaryTest_FLUOXETINE) # = 0.8190158
dummy <- binaryTest_FLUOXETINE
dummy$serious <- NULL
dummy$predict <- NULL
dummy.m <- data.matrix(dummy)

## Pick a lambda to explore
x <- -5 # Also looked at -7.5, -4.5, -4
# Predict (with s=x)
binaryTest_FLUOXETINE$predict <- predict(binaryTrain_FLUOXETINE.lasso, newx=dummy.m, s=exp(x), family="binomial", type="class")

# Check accuracy of predictions
sum( binaryTest_FLUOXETINE$serious == binaryTest_FLUOXETINE$predict ) / nrow( binaryTest_FLUOXETINE )
table(binaryTest_FLUOXETINE$predict)
table(binaryTest_FLUOXETINE$serious)

### Check coefficients 
coef(binaryTrain_FLUOXETINE.lasso, s=exp(x))

#######

# Model with best lambda
binaryTrain_FLUOXETINE.m <- as.matrix(binaryTrain_FLUOXETINE)
probs <- predict(binaryTrain_FLUOXETINE.lasso, newx=dummy.m, s=exp(x), family="binomial", type = "response")
length(binaryTest_FLUOXETINE$serious)
length(probs)
# ROCR prediction object and extract ROC curve and AUC
prediction <- prediction(probs, binaryTest_FLUOXETINE$serious)
roc.curve <- performance(prediction,"tpr","fpr")
auc = performance(prediction, "auc")@y.values[[1]]
# plot
plot(roc.curve, main = paste("ROC (AUC=", round(auc,2), ")", sep=""))
abline(0, 1, lty="dashed")

#######

## Lambda = -7.5, accuracy = 0.8128996, AUC = 0.71
## Lambda = -5, accuracy = 0.8154017, AUC = 0.71 ***
## Lambda = -4.5, accuracy = 0.8190158, , AUC = 0.7
## Lambda = -4, accuracy = 0.8190158, AUC = 0.68


############################ Multi  ############################
# Train the 'LASSO'
multiTrain_FLUOXETINE.m <- as.matrix(multiTrain_FLUOXETINE)
multiTrain_FLUOXETINE.lasso <- glmnet(multiTrain_FLUOXETINE.m [,-c(56)], multiTrain_FLUOXETINE.m [,c(56)], alpha=1, family="multinomial")
plot(multiTrain_FLUOXETINE.lasso ,xvar="lambda",label=TRUE)

### Predict on Testing Set
# NAIVE model predicts the most frequent level  
table(multiTrain_FLUOXETINE$level)
sum(multiTest_FLUOXETINE$level=='other')/nrow(multiTest_FLUOXETINE) # = 0.3487

dummy <- multiTest_FLUOXETINE
dummy$level <- NULL
dummy$predict <- NULL
dummy.m <- data.matrix(dummy)

## Pick a lambda to explore
x <- -8 # Also looked at -8.5, -7.5, -6, -5
multiTest_FLUOXETINE$predict <- predict(multiTrain_FLUOXETINE.lasso, newx=dummy.m, s=exp(x), family="multinomial", type="class")

# Check accuracy of predictions
sum(multiTest_FLUOXETINE$level==multiTest_FLUOXETINE$predict)/nrow(multiTest_FLUOXETINE)
table(multiTest_FLUOXETINE$predict)

## Lambda = -8.5, accuracy = 0.434976
## Lambda = -8, accuracy = 0.4366872 ***
## Lambda = -7.5, accuracy = 0.436345
## Lambda = -6, accuracy = 0.4315537
## Lambda = -5, accuracy = 0.426078

# Check coefficients 
options(scipen=999)
coef(multiTrain_FLUOXETINE.lasso, s=exp(x))$death

####### 

# Confusion matrix for multiclass predictor
lvls <- c('death','d','dl','h','hd','hdl','hl','l','other')
FLUOXETINE_mat <- table(factor(multiTest_FLUOXETINE$predict, levels=lvls), multiTest_FLUOXETINE$level)
# Precision for each class 
FLUOXETINE_precision <- (precision <- diag(FLUOXETINE_mat) / rowSums(FLUOXETINE_mat))
FLUOXETINE_precision
# Recall for each class
FLUOXETINE_recall <- (diag(FLUOXETINE_mat) / colSums(FLUOXETINE_mat))
FLUOXETINE_recall

#######

####################################################
###   Additional Plots for Final Report         ####
####################################################

dat <- read.table(text = "    Sex Age Weight
Good   138489  97708 58514
                  Missing   6221  45601 84629
                  Problematic   937  2338 2504",sep = "",header = TRUE)
dat$Sex <- dat$Sex/145647
dat$Age <- dat$Age/145647
dat$Weight <- dat$Weight/145647
dat <- round(dat, 4)
datm <- melt(cbind(dat, Category = rownames(dat)), id.vars = c('Category'))
pos <- dat
pos$Sex <- c(pos$Sex[1]/2, pos$Sex[1]+pos$Sex[2]/2, 1-pos$Sex[3]/2)
pos$Age <- c(pos$Age[1]/2, pos$Age[1]+pos$Age[2]/2, 1-pos$Age[3]/2)
pos$Weight <- c(pos$Weight[1]/2, pos$Weight[1]+pos$Weight[2]/2, 1-pos$Weight[3]/2)
posm <- melt(cbind(pos, Category = rownames(pos)), id.vars = c('Category'))

cbind(dat, pos)

ggplot(datm,aes(x = variable, y = value,fill = Category)) + 
  geom_bar(position = "fill",stat = "identity") + 
  theme(text = element_text(size=20)) + 
  scale_y_continuous(labels = percent_format())+
  geom_text(data=posm, aes(x = variable, y = value, 
                           label = paste(datm$value*100,"%")), size=6)+ ggtitle("Variable Distribution(%)")



###########################################
### Age and Weight Visualizations ###
###########################################
filename <- "dataFull.RDS"
df<-readRDS(filename)

summary(df$age)
nrow(df[df$age<12 & !is.na(df$age),])/nrow(df) # 1.6% later removed for modeling

hist(df$age, breaks=80,
     main="Patient Onset Ages",
     xlab="Onset Age (Years)",
     col='blue',
     border='grey')

hist(df$patientweight, breaks=80,
     main="Patient Onset Weight",
     xlab="Onset Age (Years)",
     col='blue',
     border='grey')