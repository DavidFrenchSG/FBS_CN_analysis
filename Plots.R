#Separate script for creating charts
source('resas_theme.R')
#Read in csv files from Z drive
Input_directory <- '//s0177a/datashare/seerad/fas/raw_data/prod2022/Sefari_outputs'

Carbon_summary <- read.csv(paste0(Input_directory,"/Carbon_summary.csv"))
Nitrogen_summary <- read.csv(paste0(Input_directory,"/Nitrogen_summary.csv"))


#Create bar chart

g <- ggplot(transform(Carbon_summary, farmtype=factor(farmtype, levels=fbs_type_words)),
            aes(x=sampyear, 
                y=CO2e_per_ha_mean, 
                ymin=CO2e_per_ha_Q1, 
                ymax=CO2e_per_ha_Q3)) +
  geom_point()+
  geom_errorbar()
g + facet_wrap(farmtype~.)

