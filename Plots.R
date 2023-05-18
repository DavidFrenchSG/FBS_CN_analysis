#Separate script for creating charts
source('resas_theme.R')

library(gridExtra)
library(grid)
#Read in csv files from Z drive
####################
#Table 1
Create_plot_df <- function(Input_table, variable){
Output_table <- Input_table %>% 
  gather(all_of(financial_years), key = "Year", value = "Value") %>% 
  spread(key= "Measure", Value) %>% 
  mutate(Quantity = paste(variable))
colnames(Output_table) <- c("Farm type", "Year", "Median", "Q1", "Q3", "Quantity")
return(Output_table)
}
Table_1_plot <- Create_plot_df(Table_1, "t CO2-e per ha")

#Table 2
Table_2_plot <- Table_2 %>% 
  gather(all_of(financial_years), key = "Year", value = "Value") %>% 
  spread(key= "Measure", Value) %>% 
  mutate(Quantity = "kg CO2-e per kg output")
colnames(Table_2_plot) <- c("Farm type", "Year", "Median", "Q1", "Q3", "Quantity")
#Table 3
Table_3_plot <- Table_3 %>% 
  gather(all_of(financial_years), key = "Year", value = "Value") %>% 
  spread(key= "Measure", Value) %>% 
  mutate(Quantity = "Nitrogen surplus (kg)")
colnames(Table_3_plot) <- c("Farm type", "Year", "Median", "Q1", "Q3", "Quantity")
#Table 4
Table_4_plot <- Table_4 %>% 
  gather(all_of(financial_years), key = "Year", value = "Value") %>% 
  spread(key= "Measure", Value) %>% 
  mutate(Quantity = "Nitrogen use efficiency (%)")
colnames(Table_4_plot) <- c("Farm type", "Year", "Median", "Q1", "Q3", "Quantity")

Master_plots <- Table_1_plot %>% 
  rbind(Table_2_plot, Table_3_plot, Table_4_plot)
Master_plots$facet_order = factor(Master_plots$Quantity, 
                                  levels = c("t CO2-e per ha", "kg CO2-e per kg output",
                                             "Nitrogen surplus (kg)", "Nitrogen use efficiency (%)"))
for(i in Output_types){
  # print(i)
  # check <- filter(Master_plots, `Farm type`==fbs_type_words[i], Quantity == "t CO2-e per ha")
  
  g_co2perha <- ggplot(filter(Master_plots, `Farm type`==fbs_type_words[i], Quantity == "t CO2-e per ha")) +
    geom_point(aes(x=`Year`, y=Median)) +
    geom_errorbar(aes(x=`Year`, ymin=Q1, ymax = Q3, width=0.25)) +
    geom_hline(yintercept = 0,colour="grey90")+
    # facet_wrap(vars(facet_order), scales="free") +
    ylab("t CO2-e per ha")
  g_co2perkg <- ggplot(filter(Master_plots, `Farm type`==fbs_type_words[i], Quantity == "kg CO2-e per kg output")) +
    geom_point(aes(x=`Year`, y=Median)) +
    geom_errorbar(aes(x=`Year`, ymin=Q1, ymax = Q3, width=0.25)) +
    geom_hline(yintercept = 0,colour="grey90") +
    # facet_wrap(vars(facet_order), scales="free") +
    ylab("kg CO2-e per kg output")  
  g_Nsurplus <- ggplot(filter(Master_plots, `Farm type`==fbs_type_words[i], Quantity == "Nitrogen surplus (kg)")) +
    geom_point(aes(x=`Year`, y=Median)) +
    geom_errorbar(aes(x=`Year`, ymin=Q1, ymax = Q3, width=0.25)) +
    geom_hline(yintercept = 0,colour="grey90") +
    # facet_wrap(vars(facet_order), scales="free") +
    ylab("Nitrogen surplus (kg)")
  g_NUE <- ggplot(filter(Master_plots, `Farm type`==fbs_type_words[i], Quantity == "Nitrogen use efficiency (%)")) +
    geom_point(aes(x=`Year`, y=Median)) +
    geom_errorbar(aes(x=`Year`, ymin=Q1, ymax = Q3, width=0.25)) +
    geom_hline(yintercept = 0,colour="grey90") +
    # facet_wrap(vars(facet_order), scales="free") +
    ylab("Nitrogen use efficiency (%)")
  g_grid <- grid.arrange(g_co2perha, g_co2perkg, g_Nsurplus, g_NUE,
               top=textGrob(fbs_type_words[i],gp=gpar(fontsize=20,font=2),x=0,hjust=0))
  ggsave(file=paste0(Output_directory,"/Charts/Farm Business Survey ", max(sampyear_range)-1,"-",max(sampyear_range)-2000," - Carbon and Nitrogen charts - ",fbs_type_words[i],".png"), plot=g_grid, height=6,width=10)
}
