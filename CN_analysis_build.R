##RScript which actually runs the main R script and then builds the infographic.


#Choose the range of *crop* years of interest here
#The CN_analysis script will convert these to sampyears.
cropyear_range <- (2019:2021)

title_var <- "Farm Business Survey 2021-22: farm level emissions and nitrogen usage"
source('CN_analysis.R')
source('Plots.R')
rmarkdown::render('CN_analysis_md.Rmd')
