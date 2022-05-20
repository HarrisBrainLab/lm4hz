# lm4hz

This repository contains the R-script for the linear model procedure applied in the paper Bell, T.K., Godfrey, K.J, Ware, A.L., Yeates, K.O., Harris, A.D., "Harmonization of multi-site MRS data with ComBat".

## Prerequisites

This code was written in R version 4.1.2 using the packages 'lme4' (version 1.1.27.1) and 'readxl' (version 1.3.1). 

## Running the script

This code will read in an excel file (.xlsx) with the data headings of vendor, site, subj, Gender, Age, GM_fraction. Specify the name of your metabolite column on line 17.  

The original excel file was generated using data from the BigGABA repository which can be found at https://www.nitrc.org/projects/biggaba/
