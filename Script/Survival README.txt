Survival README

This README file contains information on how to run and use the survival portion of this analysis 

01 Survival:

Contains 2 scripts, one analysis uses harvest type (survival_harvest.R) as a variable the other uses percent tree cover (survival_canopy.R). The purpose of the two similar but different analyses is to investigate how the harvest and gap dynamics interact. The idea behind this is due to the non-uniform distribution of trees in the higher retention blocks. These two analyses look at individual locations.

A 3rd script will be added to survival that looks at all the location together (has not been added as of Nov 9th 2023). 


01a Survival Functions:

This contains the custom functions required to run the analyses. "data_prep_function.R" and "lrtest_function" are universal for both analyses. "lrtest_function.R" does contain some functions that are only required by one analysis but having them loaded will not interfere with use. "harvest_model_function.R" loads the functions that build models required for using harvest type as a variable, similarly "canopy_model_function.R" loads the functions that build models for using percentage tree cover. 


How to use:

Sourcing functions:
- At the start of both analyses scripts the functions are sourced from their respective R scripts. They were sourced so that the body of the analysis code could be easier to follow. 

Notation:
- 








 