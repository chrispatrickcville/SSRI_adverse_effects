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
