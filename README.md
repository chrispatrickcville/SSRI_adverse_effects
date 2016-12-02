# SSRI_adverse_effects
Using OpenFDA drug adverse event data to predict the seriousness of adverse events associated with taking SSRIs

Includes:
- convert a particular quarter's JSON files to RData files
- prepare a particular quarter's RData files for dataframes (patient, drugFinal)
- add columns for drug characterization (suspect=1, concomitant=2, interacting=3)
- add columns for 50 most commonly featured 'other' drugs
- age standardization
