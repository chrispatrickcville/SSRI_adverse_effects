setwd("~/Documents/Classes/STAT 6021 - Linear Regression/Final project")

library(jsonlite)
library(gdata)
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

library(stringr)
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

