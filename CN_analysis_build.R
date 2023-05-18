##RScript which actually runs the main R script and then builds the infographic.


#Choose the range of *crop* years of interest here
cropyear_range <- (2019:2021)

#These two variables automatically update based on crop_year. "year" is used for importing the census data.
#"datayear" is used for importing the FBS data.

#Variables for FBS thresholds. The exchange rate is from 2013, since that is when standard outputs were determined.

title_var <- paste0("Farm Business Survey environmental data - by farm type")
source('CN_analysis.R')
source('Plots.R')
rmarkdown::render('CN_analysis_md.Rmd')