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
	- Produces co-occurrence matrices plot for the batch and continual email groups.
	
![alt text](https://github.com/UH-CPL/CHI20-Displayed-Emotions-Methods/blob/master/plots/all_session_plots/DT/DT_group_summative.pdf)


<img src="https://github.com/UH-CPL/CHI20-Displayed-Emotions-Methods/blob/master/plots/all_session_plots/DT/DT_group_summative.pdf" width="250">


- vs-subject-wise-plot.R
	- Produces pair plot of emotion stack and co-occurance matrix for each subject from batch and continual email groups.
	- Produces meta data for subjectwise matrices.
- vs-anova-analysis.R
	- Produces interaction plot for dominating emotions (i.e., diagonal elements of co-occurrence matrices)
	
	
	
#### Note: Please do not run any script after this.
---------------------------------------------------------------------------------------------------------

**Utility Scripts (us)**

- us-common-functions.R
	- Useful functions that are called from the other scripts.



