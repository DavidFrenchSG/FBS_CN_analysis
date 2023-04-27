#Separate script for creating charts
source('resas_theme.R')
#Read in csv files from Z drive
Input_directory <- '//s0177a/datashare/seerad/fas/raw_data/prod2022/Sefari_outputs'

Carbon_summary <- read.csv(paste0(Input_directory,"/Carbon_summary.csv"))
Nitrogen_summary <- read.csv(paste0(Input_directory,"/Nitrogen_summary.csv"))


#Transform
CO2e_per_ha_transform <- Carbon_summary %>%
  mutate(CO2e_per_ha_Q1 = CO2e_per_ha_Q1 - CO2e_per_ha_min,
         CO2e_per_ha_mean = CO2e_per_ha_mean - (CO2e_per_ha_Q1 + CO2e_per_ha_min),
         CO2e_per_ha_Q3 = CO2e_per_ha_Q3 - (CO2e_per_ha_Q1 + CO2e_per_ha_min + CO2e_per_ha_mean),
         CO2e_per_ha_max = CO2e_per_ha_max - (CO2e_per_ha_Q1 + CO2e_per_ha_min + CO2e_per_ha_mean + CO2e_per_ha_Q3)) %>% 
  # filter(type==9) %>% 
  select(sampyear, farmtype, starts_with("CO2e_per_ha"), -CO2e_per_ha_med) %>% 
  pivot_longer(starts_with("CO2e"), names_to = "Measure", values_to = "Value")

boxplot_test <- data.frame(
  sampyear=c(2020,2021),
  y0 = c(1,1),
  Q1 = c(2,2),
  mean = c(3,3.5),
  Q3 = c(4,4),
  y5 = c(5,6)
)

#Create bar chart

g <- ggplot(filter(CO2e_per_ha_transform), aes(x=sampyear, y = Value, fill = factor(Measure, levels=c("CO2e_per_ha_max", "CO2e_per_ha_Q3","CO2e_per_ha_mean","CO2e_per_ha_Q1","CO2e_per_ha_min")))) + 
  geom_bar(stat="identity")
g + facet_wrap(~farmtype) + 
  theme(legend.position='right')+
  theme(legend.title=element_blank())+
  ylab("tonnes CO2e per hectare")



h <- ggplot(boxplot_test, aes(group=sampyear, x=sampyear,ymin = y0, lower=Q1, middle=mean, upper = Q3, ymax = y5))
h + geom_boxplot(stat="identity")

for (filter_type in 1:10){
i <- ggplot(filter(Carbon_summary, type==filter_type), aes(group=sampyear, x=sampyear, 
                                                 ymin=CO2e_per_ha_min,
                                                 lower=CO2e_per_ha_Q1,
                                                 middle=CO2e_per_ha_mean,
                                                 upper=CO2e_per_ha_Q3,
                                                 ymax=CO2e_per_ha_max))+
  geom_boxplot(stat="identity") 
print(i)
}

