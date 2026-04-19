# BHDS.2010 Assignment 4

## Project Purpose
The goal of this project is to combine good coding practices and communication
via GitHub to collaborate as a team to create a Shiny App. For this project,
we will employ RStudio to analyze and display our data via regressive coding
(More to be added later)

## Direct Objective of Data
The dataset measles_county_all_updates_detailed.csv records the number of 
measles cases in the United States from January 1, 2025 until April 9, 2026. The 
information specifies the state and county in which each case occurred, as well as 
subject's vaccination status.

## Team Members and Roles
Angelica: filters, map, and line plots
Jenny: data overview, treands, and comparison
Jess: README documentation and summary statistics

## Raw Data Description


## Instructions to Run the Code
1. Clone or download this GitHub repository onto your local computer
2. Open the project folder in RStudio (clone/download our GitHub repository onto
your local computer using File -> New Project -> Version Control -> Git, then paste
the repository URL git clone https://github.com/patinoam/BHDS2010-Spring-2026.git
4. Ensure that the file measles_county_all_updates_detailed.csv is located in
the main project directory
5. Install all required packages if not already installed via the command
install.packages(c("shiny", "tiddyverse"))
6. Load the libraries and their dependencies via library("shiny") and
library("tiddyverse")
7. Run each script in sequence **FINSIH THIS ONCE ORDER OF PLOTS IS DECIDED**
8. Each visualization will appear in the plots pane
9. Once all code runs successfully, knit the file Assignment-4.Rmd into a
PDF to create the final compiled report

## Version Control Workflow
1. The same repository for Assignment 3 on Github was used under Angelica’s account
2. A separate folder for Assignment 4 was created
3. Angelica populated the BHDS.2010/Assignment-4 folder with the base of the
Assingment-4.Rmd file, uploaded the measles_county_all_updates_detailed.csv file,
and populated the README.md file
4. Within her branch, Angelica separated the geographic data so that the county
and state information were stored in separate columns, ensured that all categorical data
was recognized as factors, and converted the date data to type Date. Angelica committed her
work and pushed her changes into the cloud and submitted a pull request for her group members



## Commit and Pull Request Protocol
Each group member would regularly commit their changes using a simple 
descriptor of the changes made (ex. "add measles data file," "create separate
columns for county and state", etc.). All commits were pushed to the remote
repository before initiating the pull request. When a pull request was 
initiated, a request was sent to all group members to approve the proposed
changes. Once all changes were approved, the original creator of the pull
request. 

## Summary of Results


## Concluding/Additional Remarks and Notes
