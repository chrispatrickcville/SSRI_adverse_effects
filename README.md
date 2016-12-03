# SSRI_adverse_effects
Using OpenFDA drug adverse event data to predict the seriousness of adverse events associated with taking SSRIs

Includes:
- converting a particular quarter's JSON files to RData files
- preparing a particular quarter's RData files for dataframes (patient, drugFinal)
- adding columns for drug characterization (suspect=1, concomitant=2, interacting=3)
- adding columns for 50 most commonly featured 'other' drugs
- age standardization
