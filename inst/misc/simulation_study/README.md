# Description of programs for the simulation study

The current folder contains the code used for the simulation study. It contains files described below:

* `README.md`: this file.
* `_AnomCreationSetup.R`: script consisting of the functions used for generating datasets with cellwise outliers.
* `step1_anom_creation.R`: script used to contaminate the real anomaly-free data.
* `step2_detection.R`: script to conduct the cellwise outlier detection using the **FuzzyHRT** algorithm.
