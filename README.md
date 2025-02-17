# A discrete-time multistate model of cognitive functioning 

Christian Dudel, dudel@demogr.mpg.de

Overview: This repository contains code for an application of discrete-time
multistate models to cognitive functioning. 

Data: The data comes from the US Health and Retirement Study. It can be obtained from https://hrs.isr.umich.edu after a free registration. Specifically, this project
uses the RAND-HRS file for the years 1992 to 2020, version 2. The file has to be
saved in the folder "Data".

Code: The code to run the analysis can be found in the folder "Code" and consists
of three files. The first file ("1-data_preparation.R") covers data cleaning and editing. 
The second file ("2-analysis.R") includes model estimation, calculation of transition
probabilities, as well as state expectancies and lifetime risks. The third file 
("3-results.R") contains the presentation of the results. All three files contain
detailed comments. 

Packages: The code uses several R packages. This includes the package dtms, which
is not available on CRAN, but on GitHub: https://github.com/christiandudel/dtms