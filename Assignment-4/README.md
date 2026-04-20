# BHDS.2010 Assignment 4

## Project Purpose
The goal of this project is to combine good coding practices and communication via GitHub to collaborate as a team to create a Shiny App. For this project, we will employ RStudio to analyze and display our data via regressive coding (More to be added later)

## Direct Objective of Data
The dataset measles_county_all_updates_detailed.csv records the number of measles cases in the United States from January 1, 2025 until April 9, 2026. The information specifies the state and county in which each case occurred, as well as subject's vaccination status and whether the disease was contracted locally or was imported from somewhere out of state.

## Team Members and Roles
Angelica: filters, map, line plots, and summary statistics
Jenny:
Jess: README documentation 

## Raw Data Description


## Instructions to Run the Code
1. Clone or download this GitHub repository onto your local computer
2. Open the project folder in RStudio (clone/download our GitHub repository onto your local computer using File -> New Project -> Version Control -> Git, then paste the repository URL git clone https://github.com/patinoam/BHDS2010-Spring-2026.git
4. Ensure that the file measles_county_all_updates_detailed.csv is located in the main project directory
5. Install all required packages if not already installed via the command install.packages(c("shiny", "leaflet", "tigris", "sf", "scales", "lubridate", "tidyverse", "DT")
6. Load the libraries and their dependencies via library(shiny), library(leaflet), library(sf), library(scales), library(lubridate), library(tidyverse), and library(DT)
7. Run each script in sequence. First, the code for the table setup, then for the correlation plot and table. Next, run the code for the sidebar layout and the four main output components (total cases, date range, state count, and county count). Then the code for Overview tab which includes the interactive map, two case count figures, and top locations table. Next, the code for the state summary and county summary tabs. Finally, run the code for the last tab, correlation analysis. Once this has been completed, run the server data. This includes the helper reactives, filtered data reactives, geographical data reactives, state summary table reactive, county summary table reactive, and correlation data reactive, as well as the upper panel outputs (i.e. the summary boxes). Finally, run all code for the interactive map, weekly case count plot output, cumulative cases by year chart output, top locations table output, state summary table output, county summary table output, and correlation analysis table output.
8. Once all code runs successfully, knit the file Assignment-4.Rmd into a PDF to create the final compiled report

## Version Control Workflow
1. The same repository for Assignment 3 on Github was used under Angelica’s account
2. A separate folder for Assignment 4 was created
3. Angelica populated the BHDS.2010/Assignment-4 folder with the base of the Assingment-4.Rmd file, uploaded the measles_county_all_updates_detailed.csv file, and populated the README.md file
4. Within Angelica's branch amp/assignment-4-data-preparation, she installed and loaded the following packages: shiny, leaflet, tigris, sf, scales, lubridate, and tidyverse. She then entered developer_mode using the code developer_mode <- TRUE to allow for testing within the app. She then loaded the raw data file via the read.csv() command. State and county data was separated and treated as factors for proper analysis. Finally, an outline of the shiny app was created. For all of these changes, Angelica provided commentary to the codes being run. Angelica then committed her work, pushed her changes, and created a pull request.
5. Jess and Jenny approved the pull request, and the data was merged with the main branch without errors.
6. Within the branch amp/assignment-4-add-more-app-features, Angelica set developer_mode to FALSE, provided commentary, and committed her changes.
7. Next, Angelica added correlation plots and table outputs for the correlation analysis tab, added the correlation data reactive coding using Spearman correlation figures for the correlation analysis tab, provided commentary on the code, and used the tabPanel command to create the correlation analysis tab and defined all necessary outputs. For each of these, she provided commentary on her code and committed her changes.
10. Next, Angelica added helper functions for the correlation scatter plot and table to be seen in the correlation analysis tab. She provided commenatry on her code and committed her changes.
11. For the county summary tab, Angelica added the summary table output, the corresponding county summary table reactive coding, and the code to create the county summary tab so that the table output listed above would be visible to users. For each of these three steps, Angelica provided commentary and committed her changes.
12. Angelica next added a slider input to allow users to select the number of desired counties. She then provided commentary and committed her changes.
13. For the state summary tab, Angelica defined the state summary table output and added the state summary reactive coding. She then provided commentary on her code and committed her changes.
14. For the overview tab, Angelica used a slider input to allow users to select the top X number of locations. She also defined the helper functions, and removed redundant coding. Angelica then provided commentary on the slider input and helper functions and committed her changes.
15. Next, Angelica added filters for the data based on location, provided filtered data reactive coding, and restructured coding to increase readability. She then provided commentary for the data filters that were added and committed her changes.
16. Angelica then added helper functions for the data tables, as well as a summary tab sepcifically for the individual states. She then added commentary to her code and committed her changes.
17. Next, using TabsetPanel and TabsPanel, Angelica restructured her outputs for the overview tab to increase user experience. She also added aesthetic changes, selecting a color pallete for the app's design. Angelica then provided commentary and committed her changes.
18. Finally, Angelica changed the dashboard title from "Caseload" to "Surveillance," set the developer mode the TRUE so that changes in the app may be tested, and added the DT package for the summary tables. She then provided commentary of these changes and committed her work.
19. For steps 6-18, Angelica then pushed her code to the main branch and created a pull request.
20. Jess reviewed the code and presented commentary regarding the output for the cumulative cases by year in the overview tab, as well as the unvaccinated vs local transmission plot in the correlation analysis tab.
21. **Need to add final steps**

## Commit and Pull Request Protocol
Each group member would regularly commit their changes using a simple descriptor of the changes made (ex. "add measles data file," "create separate columns for county and state", etc.). All commits were pushed to the remote
repository before initiating the pull request. When a pull request was initiated, a request was sent to all group members to approve the proposed changes. Once all changes were approved, the original creator of the pull
request. 

## Summary of Results
Both the top locations listing and the interactive map illustrate the highest location count by state within the Untied States. The listing and amount of shading on the map both visualize that South Carolina holds the highest measels count, with Texas in a close second. Of these, Texas is ranked second in total state population while South Carolina is 23rd most populated. While California holds only 59 total measels cases, they have the highest count of imported cases at 26; however, South Dakota holds the highest proportion of imported cases at 0.792. In contrast, multiple states are tied for 0.00 imported cases, included Kansas and Utah, both of which are listed in the top 10 for counts of local transmission cases. Looking closer at South Carolina, we see that of the states the hightest measels case count, South Carolina also holds by far the highest SD at 702.86 with Arizona in second at 110.47. Of South Carolina's 1,000 cases, 995 were local transmission, all of which occurred in only 2 counties: Upstate and Sumter. Of the 1,000 cases that occurred in South Carolina, 54 individuals were vaccinated while 946 were note, meaning 94.6% of all individuals were contracted the measels in SC were not vaccinated for the disease.There were 4 states who had 0 cases of measles: New Hampshire, Delaware, West Virginia, and Mississippi. Of these 4, Delaware and New Hampshire are ranked 44th and 40th respectively in state population count. 

## Concluding/Additional Remarks and Notes
