# BHDS2010-Spring-2026

Group members: Jessica Astorga, Jenny Yang, Angelica Patino

## Assignment 3: Collaborating in Github

### Assignment Description

Data were collected on the number of text messages sent by participants in 
Group 1 and Group 2 at baseline and six months later. The objective of this 
analysis is to investigate this dataset and compare the text message count 
between both groups at each time point. 

Summary statistics, including mean, median, variance, and standard deviation 
are computed. Figures including box plots and bar charts stratified by group 
and time are generated as well. 

These outputs are used to perform a preliminary assessment of the spread and 
distribution of data, normality, and overall variability. In addition, they are 
also used for an initial comparison of means between groups at each time point, 
as well as a comparison of change from baseline between groups.

### Analysis Workflow

The analysis code is in a private GitHub repository called `BHDS2010-Spring-2026` 
in the folder `Assignment-3`. This folder has the following contents:
- raw data file, `TextMessages.csv`
- R script, `assignment-3.R`
- R Markdown file, `assignment-3.Rmd`
- final report file, `assignment-3.pdf`

To run the analysis, follow the steps listed below:
- Request GitHub account access to private repository and accept invitation
- Clone the repository to local RStudio environment by 
File > New Project > Version Control > Git, using the repository URL 
`https://github.com/patinoam/BHDS2010-Spring-2026.git`
- Run the R script `assignment-3.R` in RStudio

The R script `assignment-3.R` performs the analysis steps listed below:
- Load packages for analysis, including `pastecs`, `reshape2`, and `tidyverse`
	- Note: if these packages are not yet installed, run lines 3-5 which 
	should be uncommented
- Load the raw data file, `TextMessages.csv`, that is stored in the repository
	- Note: the raw data file has a wide data format
- Perform checks on the data file, including number of rows and variable types
- Store a data object with the data in a long format
- Generate the outputs below, stratified by group and time:
	- Summary statistics
	- Stratified box plot of text message count 
	- Stratified bar chart of text message count

The R Markdown file is run in RStudio to generate the final report as a PDF.

### Team Contributions

The tasks, assignee, and associated pull requests are summarized in the table below.

| Task                | Assignee       | Pull Requests                                                                                                    |
| ------------------- | -------------- | ---------------------------------------------------------------------------------------------------------------- |
| Create repository   | Angelica       |                                                                                                                  |
| Data preparation    | Angelica       | https://github.com/patinoam/BHDS2010-Spring-2026/pull/2                                                          |
| Summary statistics  | Angelica       | https://github.com/patinoam/BHDS2010-Spring-2026/pull/4, https://github.com/patinoam/BHDS2010-Spring-2026/pull/6 |
| Boxplot             | Jess           | https://github.com/patinoam/BHDS2010-Spring-2026/pull/5                                                          |
| Bar chart           | Angelica       | https://github.com/patinoam/BHDS2010-Spring-2026/pull/3                                                          |
| Final report review | Jess, Angelica |                                                                                                                  |
| Update README       | Angelica       |                                                                                                                  |