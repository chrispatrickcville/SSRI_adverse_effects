install.packages("lavaan")
install.packages("mice")
library(lavaan)
library(mice)
library(lattice)

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
