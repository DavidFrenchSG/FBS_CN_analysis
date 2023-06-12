##RScript which actually runs the main R script and then builds the visual summary


#Choose the range of *crop* years of interest here
cropyear_range <- c(2019:2021)
#Specify the farmtypes to be included, and their order. 1: Cereal, 2: General cropping, 3: Dairy,
#4: LFA Sheep, 5: LFA Cattle, 6: LFA Cattle & Sheep, 7: Lowland livestock, 8: Mixed
#9: All farm types, 10: All LFA livestock types
Output_types = c(9,1,2,3,10,8)

#Use cropyears to determine sampyears
sampyear_range <- cropyear_range + 1
#Figure out the financial years associated with each sampyear. Eg., sampyear "2020" is financial year "2019/20"
financial_years_start <- sampyear_range - 1
financial_years_end <- sampyear_range - 2000
financial_years <- paste0(financial_years_start,"-",financial_years_end)

#Specify the document title
title_line1 <- paste0("Farm Business Survey ", max(financial_years),":")
title_line2 <- "Farm level emissions and nitrogen usage"
title_single_line <- paste0(title_line1,title_line2)

##Run the two R scripts and the Rmarkdown document
source('CN_analysis.R')
source('Plots.R')
rmarkdown::render('CN_analysis_md.Rmd')
# rmarkdown::render('CN_slides.Rmd')
# rmarkdown::render('CN_ppt.Rmd')

