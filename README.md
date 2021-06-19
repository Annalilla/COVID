# Real-time analysis of predictors of COVID-19 infection spread in the countries of the European Union

Anikó Balogh, Anna Harman
<br>
<br>
## 1. Structure of GitHub Repository
<br>

### 1.1 cluster
- Hierarchical_cluster.R
> - Assigning countries to clusters with hierarchical clustering algorithms. 
### 1.2.	data_collection
- Collect_data.R
> - Downloads data from all sources.
- Create_database.R
> - Creates the databases for the first time. Downloads data from all sources, creates and saves two databases.
> - Uses the following scripts:
>> - data_collection/Save_data.R
>> - data_collection/Collect_data.R
>> - data_collection/Merge_data.R
- Merge_data.R
> - Formats the data and merges it into two databases (one for the time-constant country characteristics and one for the time-varying data) 
- Revise_data.R
> - Recollects and merges tdata from all data sources. Saves the updated dataset.
> - Lists the differences between the old and the new time-varying data.
> - Uses the following scripts:
>> - data_collection/Save_data.R
>> - data_collection/Collect_data.R
>> - data_collection/Merge_data.R
>> - data_collection/Data_revision_functions.R
- Save_data.R
> - Contains two functions (save database to local or to online location) to save databases.
- Update_data.R
> - Updates tdata with new records since the last download and saves it.
> - Uses the following scripts:
>> - functions/Data_preparation_functions.R
>> - data_collection/Save_data.R

### 1.3.	functions
- Data_cleansing_functions.R
> - Contains functions for data cleansing.
- Data_preparation_functions.R
> - Contains functions used during the preparation and merge of the data.
- Data_revision_functions.R
> - Contains functions used during the data revision.
> - Updates database.
- Get_data.R
> - If the database should be loaded from Google Sheets, an authentication file is necessary to reach the appropriate Google Sheets account. This file has to be located under a directory called “.secrets” in the working directory.
- RF_cluster_functions.R
> - Contains functions to prepare data for RF modelling at cluster level
- RF_functions.R
> - Contains functions to prepare data for RF modelling for countries

### 1.4.	helpers
- Add_variable_labels.R
> - Adds labels to the variables.
- Change_variable_types.R
> - Change variable types (to numerical, factor or date).
- Data_cleansing.R
> - Explores the data to discover the necessary data cleansing steps.
- Get_and_prepare_data.R
> - Loads the two (tdata and country_char) databases from Google Sheets.
> - Uses the following scripts:
>> - functions/Get_data.R
>> - helpers/Change_variable_types.R
>> - helpers/Add_variable_labels.R
- Near_stations.R
> - Lists weather station IDs near to capitals. 
- Prepare_run.R
> - Prepares the data collection and analysis.
> - Must be run before any other codes. 
- Restriction_labels.R
> - Contains the restriction labels and their descriptions to be displayed in the dashboard instead of their abbreviation.
- Set_up_authentication.R
> - Generates the token to access the private Google Sheets where the data are stored.
> - Don’t run this without the authentication key. The purpose of this script here is only to demonstrate how the authentication file was created.

### 1.5.	random_forest
- Cluster_RF.R
> - Runs Random Forest models for each cluster.
> - Uses the following script:
>> - functions/RF_cluster_functions.R
- Random_forest.R
> - Runs Random Forest models for each country.
> - Uses the following script:
>> - functions/RF_functions.R
- rank_corr.R
> - Creates rank variables per clusters and per countries
> - Computes correlation between the repeated variable importance rank of the clusters and the repeated variable importance rank of the countries within clusters.

### 1.6.	shinydashboard
- app.R
> - Contains the content and functionality of the shiny dashboard application.
> - Uses the following scripts:
>> - shinydashboard/Shiny_prep_and_functions.R
>> - shinydashboard/Shiny_vis_functions.R
- Call_shiny.R
> - Loads the data from googlesheets, carries out RF and hierarchical clustering, prepares the data and the results for visualization.
> - Starts the application.
> - Uses the following scripts:
>> - helpers/Get_and_prepare_data.R
>> - random_forest/Random_forest.R
>> - cluster/Hierarchical_cluster.R
>> - random_forest/Cluster_RF.R
>> - random_forest/rank_corr.R
>> - shinydashboard/Shiny_data_prep.R
- Shiny_data_prep.R
> - Prepares the data for the dashboard to enable fast visualisation and to improve interactivity.
> - Uses the following script:
>> - helpers/Restriction_labels.R
- Shiny_prep_and_functions.R
> - Runs when starting the application
> - Loads the data prepared in the previous steps when starting the shiny application.
> - Contains functions to respond interactively to the queries in the shiny application.
- Shiny_vis_functions.R
> - Runs when starting the application
> - Contains functions for the interactive visualisation in the shiny application.

<br>

## 2.	Reproduction of the Analysis 
<br>

### 2.1.	Preparations
1)	Download the codes from our GitHub repository and unzip them
2)	Open the script: helpers/Prepare_run.R
3)	Set your working directory and run the whole code. Your working directory should be the directory where you put the codes from our GitHub repository like this: “path_where_you_saved_the_unzipped_repository/COVID-main”.
4)	Run the script: helpers/Prepare_run.R


### 2.2.	All preparations and starting the application in one step
1)	Prerequisites:
>> - helpers/Prepare_run.R
>> - having an authentication file in a directory called “.secrets” within the working directory
2)	Script: shinydashboard/Call_shiny.R
3)	Run the whole script. The application starts. You do not have to run the steps described below


## If you would like to run the analysis step by step, instead of step 2.2. perform the steps below (from 2.3. to 2.9.).

### 2.3.	Data Collection
In order to reproduce our analysis, you either have to download the data with the help of our automated data collection program or you can download the prepared databases from googlesheets.
## Important Update on 13.06.2021:
As one of our main data sources, the UMD/Facebook World Symptom Survey is discontinued due to wave 11 revisions as of June, 2021, our automatic data download, update and revision processes described in section 2.3.1., 2.3.3. and 2.3.4 is no longer available. This means you have to download the prepared databases from googlesheets if you wish to reproduce the analysis as described in step 2.3.2.

### 2.3.1. Automated data download (no longer available)
1)	Prerequisites:
>> - helpers/Prepare_run.R
2)	Script: data_collection/Create_database.R
3)	Run the first 3 rows of the script. Itt will collect data from different sources and creates two databases (tdata and country_char).
4)	If you want to save the data local, run the functions save_data().
5)	If you want to save data to Google Sheets, run the functions save_data_online(). In this case you need to have an authentication file in a directory called “.secrets” within the working directory.
6)	If you want to set the type of the variables and add variable labels, run the scripts “helpers/Change_variable_types.R” and  “helpers/Add_variable_labels.R”.

### 2.3.2. Read data from Google Sheets
1)	Prerequisites:
>> - helpers/Prepare_run.R
>> - having an authentication file in a directory called “.secrets” within the working directory
2)	Script: helpers/Get_and_prepare_data.R
3)	Run the whole script.

### 2.3.3. Update data (no longer available)
1)	Prerequisites:
>> - helpers/Prepare_run.R
>> - database to update is loaded (helpers/Get_and_prepare_data.R)
2)	Script: data_collection/Update_data.R
3)	Run the whole script except the last two rows.
4)	If you want to save the data local, run the functions save_data() at the end of the code.
5)	If you want to save the data to Google Sheets, run the functions save_data_online at the end of the code. In this case you need to have an authentication file in a directory called “.secrets” within the working directory.

### 2.3.4. Data revision (no longer available)
1)	Prerequisites:
>> - helpers/Prepare_run.R
>> - Authentication file in a directory called “.secrets” within the working directory is available
2)	Script: data_collection/Revise_data.R
3)	Run the part of the script that is not commented out. The number of differences, first and last differences per variables, new variables and variables no more available will be listed in the output. In the function show_all_differences_in_one_var() you can set a variable name to get all differences for.
4)	In the commented out section you can update variables with all new or with selected records and add new variables.
5)	If you want to save the updated data local, run the functions save_data() at the end of the code.
6)	If you want to save the updated data to Google Sheets, run the functions save_data_online at the end of the code. In this case you need to have an authentication file in a directory called “.secrets” within the working directory.

## 2.4.	Random Forest Modelling and Hierarchical Clustering

### 2.4.1. Random Forest modelling for countries
1)	Prerequisites:
>> - helpers/Prepare_run.R
>> - Time-varying database is loaded (tdata)
2)	Script: random_forest/Random_forest.R
3)	Run the whole script. Returns RF timeslice models by countries.

### 2.4.2. Hierarchical clustering
1)	Prerequisites:
>> - helpers/Prepare_run.R
>> - Time-constant database is loaded (country_char)
2)	Script: cluster/Hierarchical_cluster.R
3)	Run the whole script. Returns with clusters to which the countries are assigned.

### 2.4.3. Random Forest modelling for clusters
1)	Prerequisites:
>> - helpers/Prepare_run.R
>> - cluster/Hierarchical_cluster.R
>> - Time-varying database is loaded (tdata)
2)	Script: random_forest/Cluster_RF.R
3)	Run the whole script. Returns RF timeslice models by clusters.

### 2.4.4. Correlation between the repeated variable importance rank of clusters and countries
1)	Prerequisites:
>> - helpers/Prepare_run.R
>> - random_forest/Random_forest.R
>> - cluster/Hierarchical_cluster.R
>> - Time-varying database is loaded (tdata)
2)	Script: random_forest/rank_corr.R
3)	Run the whole script. Returns RF timeslice models by clusters.


## 2.5.	Shiny application

### 2.5.1. Preparing the data and results for the shiny application
1)	Prerequisites:
>> - helpers/Prepare_run.R
>> - random_forest/Random_forest.R
>> - cluster/Hierarchical_cluster.R
>> - random_forest/rank_corr.R
>> - Time-varying database is loaded (tdata)
2)	Script: shinydashboard/shiny_data_prep.R
3)	Run the whole script.


### 2.5.2. Shiny application
1)	Prerequisites:
>> - helpers/Prepare_run.R
>> - random_forest/Random_forest.R
>> - cluster/Hierarchical_cluster.R
>> - random_forest/rank_corr.R
>> - shinydashboard/shiny_data_prep.R
2)	Script: shinydashboard/app.R
3)	Start the application.

<br>

## R packages
<br>

- caret, Max Kuhn et al (2020).
 caret: Classification and Regression Training. R package version 6.0-86.
- compare, Paul Murrell (2015). compare: Comparing Objects for Differences. R package version 0.2-6. https://CRAN.R-project.org/package=compare
- coronavirus, Rami Krispin and Jarrett Byrnes (2021). coronavirus: The 2019 Novel Coronavirus COVID-19 (2019-nCoV) Dataset. R package version 0.3.1. https://CRAN.R-project.org/package=coronavirus
- Cowplot, Claus O. Wilke (2020). cowplot: Streamlined Plot Theme and Plot Annotations for 'ggplot2'. R package version 1.1.1.  https://CRAN.R-project.org/package=cowplot
- data.table, Matt Dowle and Arun Srinivasan (2020). data.table: Extension of `data.frame`. R package version 1.13.6. https://CRAN.R-project.org/package=data.table
- dplyr, Hadley Wickham, Romain François, Lionel Henry and Kirill Müller (2020). dplyr: A Grammar of Data Manipulation. R package version 1.0.2. https://CRAN.R-project.org/package=dplyr,
- eurostat, (C) Leo Lahti, Janne Huovari, Markus Kainu, Przemyslaw Biecek. Retrieval and analysis of Eurostat open data with the eurostat package. R Journal 9(1):385-392, 2017. Version 3.6.84 Package URL: http://ropengov.github.io/eurostat Manuscript URL: https://journal.r-project.org/archive/2017/RJ-2017-019/index.html
- factoextra, Alboukadel Kassambara and Fabian Mundt (2020). factoextra: Extract and Visualize the Results of Multivariate Data Analyses. R package version 1.0.7. https://CRAN.R-project.org/package=factoextra
- googlesheets4, Jennifer Bryan (2020). googlesheets4: Access Google Sheets using the Sheets API V4. R package version 0.2.0. https://CRAN.R-project.org/package=googlesheets4
- gridextra, Baptiste Auguie (2017). gridExtra: Miscellaneous Functions for "Grid" Graphics. R package version 2.3. https://CRAN.R-project.org/package=gridExtra
- Hmisc, Frank E Harrell Jr, with contributions from Charles Dupont and many others. (2020). Hmisc: Harrell Miscellaneous. R package version 4.4-2. https://CRAN.R-project.org/package=Hmisc
https://CRAN.R-project.org/package=caret 
- httr, Hadley Wickham (2020). httr: Tools for Working with URLs and HTTP. R package version 1.4.2. https://CRAN.R-project.org/package=httr
- iml, Molnar C, Bischl B, Casalicchio G (2018). “iml: An R package for Interpretable Machine Learning.” JOSS, 3(26), 786. https://joss.theoj.org/papers/10.21105/joss.00786. R package version 0.10.1 https://CRAN.R-project.org/package=iml
- jsonlite, Jeroen Ooms (2014). The jsonlite Package: A Practical and Consistent Mapping Between JSON Data and R Objects. arXiv:1403.2805 [stat.CO] URL https://arxiv.org/abs/1403.2805.
- lubridate, Garrett Grolemund, Hadley Wickham (2011). Dates and Times Made Easy with lubridate. Journal of Statistical Software, 40(3), 1-25. URL https://www.jstatsoft.org/v40/i03/.
- maps, Original S code by Richard A. Becker, Allan R. Wilks. R version by Ray Brownrigg. Enhancements by Thomas P Minka and Alex Deckmyn. (2018). maps: Draw Geographical Maps. R package version 3.3.0. https://CRAN.R-project.org/package=maps
- pdp, Brandon M. Greenwell (2017). pdp: An R Package for Constructing Partial Dependence Plots. The R Journal, 9(1), 421--436. URL https://journal.r-project.org/archive/2017/RJ-2017-016/index.html
- randomForest, Leo Breiman, Adele Cutler, Andy Liaw , Matthew Wiener (2018).
 randomForest: Breiman and Cutler's Random Forests for Classification and Regression. R package version 4.6-14. https://CRAN.R-project.org/package=randomForest 
- ranger, Marvin N. Wright, Andreas Ziegler (2017). ranger: A Fast Implementation of Random Forests for High Dimensional Data in C++ and R. Journal of Statistical Software, 77(1), 1-17. doi:10.18637/jss.v077.i01
- RColorBrewer, Erich Neuwirth (2014). RColorBrewer: ColorBrewer Palettes. R package version 1.1-2. https://CRAN.R-project.org/package=RColorBrewer
- readr, Hadley Wickham and Jim Hester (2020). readr: Read Rectangular Text Data. R package version 1.4.0. https://CRAN.R-project.org/package=readr
- reshape2, Hadley Wickham (2007). Reshaping Data with the reshape Package. Journal of Statistical Software, 21(12), 1-20. URL http://www.jstatsoft.org/v21/i12/
- rnoaa, Scott Chamberlain (2021). rnoaa: 'NOAA' Weather Data from R. R package version 1.3.0. https://CRAN.R-project.org/package=rnoaa
- rvest, Hadley Wickham (2020). rvest: Easily Harvest (Scrape) Web Pages. R package version 0.3.6. https://CRAN.R-project.org/package=rvest
- shiny, Winston Chang, Joe Cheng, JJ Allaire, Carson Sievert, Barret Schloerke, Yihui Xie, Jeff Allen, Jonathan McPherson, Alan Dipert and Barbara Borges (2021). shiny: Web Application Framework for R. R package version 1.6.0. https://CRAN.R-project.org/package=shiny
- shinyBS, Eric Bailey (2015). shinyBS: Twitter Bootstrap Components for Shiny. R package version 0.61. https://CRAN.R-project.org/package=shinyBS
- shinydashboard, Winston Chang and Barbara Borges Ribeiro (2018). shinydashboard: Create Dashboards with 'Shiny'. R package version 0.7.1. https://CRAN.R-project.org/package=shinydashboard
- sjlabelled, Lüdecke D (2020). _sjlabelled: Labelled Data Utility Functions (Version 1.1.7)_. doi: 10.5281/zenodo.1249215 (URL: https://doi.org/10.5281/zenodo.1249215), <URL: https://CRAN.R-project.org/package=sjlabelled>
- Stats, R Core Team (2020). R: A language and environment for statistical computing. R Foundation for Statistical Computing, Vienna, Austria. URL https://www.R-project.org/
- stringr, Hadley Wickham (2019). stringr: Simple, Consistent Wrappers for Common String Operations. R package version 1.4.0. https://CRAN.R-project.org/package=stringr
- tidyverse, Wickham et al., (2019). Welcome to the tidyverse. Journal of Open Source Software, 4(43), 1686, https://doi.org/10.21105/joss.01686
- zoo, Achim Zeileis and Gabor Grothendieck (2005). zoo: S3 Infrastructure for Regular and Irregular Time Series. Journal of  Statistical Software, 14(6), 1-27. doi:10.18637/jss.v014.i06

