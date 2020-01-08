# CHI20-Displayed-Emotions-Methods
This repository contains the R scripts to validate the Emotional Footprints of Email Interruptions.



## Getting Started

#### Prerequisites
- R and RStudio
- Required packages

#### Installing R Packages
Packages are available on CRAN and can be installed using a simple call to `install.packages()`:

    install.packages('PackageName')
	
## Data Set
Please download Batch_FACS_data.csv and Continuous_FACS_data.csv from https://osf.io/mhdgt/ and place it in data/final-data folder.
	
## Script Set

##### Please run the following scripts sequentially

**Validation Scripts (vs)**
- vs-batch-continual.R
- vs-subject-wise-plot.R
- vs-anova-analysis.R
	
	
	
#### Note: Please do not run any script after this.
---------------------------------------------------------------------------------------------------------

**Utility Scripts (us)**

- us-common-functions.R
	- Useful functions that are called from the other scripts.



