#Separate script for creating charts
source('resas_theme.R')

##Function for converting the tables created in CN_analysis.R into a format better for feeding into ggplot.
Create_plot_df <- function(Input_table, variable){
  Output_table <- Input_table %>% 
    gather(all_of(financial_years), key = "Year", value = "Value") %>% 
    spread(key= "Measure", Value) %>% 
    mutate(Quantity = paste(variable))
  colnames(Output_table) <- c("Farm type", "Year", "Median", "Q1", "Q3", "Quantity")
  return(Output_table)
}
#Use the function to create dataframes for each Table
Table_1_plot <- Create_plot_df(Table_1, "t CO2-e per ha")
Table_2_plot <- Create_plot_df(Table_2, "kg CO2-e per kg output")
Table_3_plot <- Create_plot_df(Table_3, "Nitrogen surplus (kg)")
Table_4_plot <- Create_plot_df(Table_4, "Nitrogen use efficiency (%)")

##Combine the four plot tables into one
Master_plots <- Table_1_plot %>% 
  rbind(Table_2_plot, Table_3_plot, Table_4_plot)
#Create a factor with the four different quantities, in the order that they should be plotted.
#Without this they are arranged alphabetically.
Master_plots$facet_order = factor(Master_plots$Quantity, 
                                  levels = c("t CO2-e per ha", "kg CO2-e per kg output",
                                             "Nitrogen surplus (kg)", "Nitrogen use efficiency (%)"))

# Function for creating the four plots for each type
# The input arguments are the type, quantity to be plotted, label for the y-axis and the rgb colour code.
# A default colour code (#23A845, one of the standard Resas greens) is included.
output_plot <- function(i, title_label, quantity_label, y_label, colour_code="#23A845"){
  output_plot <- ggplot(filter(Master_plots, `Farm type`==fbs_type_words[i], Quantity == quantity_label)) +
    geom_col(aes(x=`Year`, y=Median), fill=colour_code) +
    geom_point(aes(x=`Year`, y=Median)) +
    geom_errorbar(aes(x=`Year`, ymin=Q1, ymax = Q3, width=0.25)) +
    geom_hline(yintercept = 0,colour="grey90")+
    xlab(element_blank())+
    labs(title = title_label)+
    # facet_wrap(vars(facet_order), scales="free") +
    # scale_x_discrete(guide = guide_axis(n.dodge=3))+
    ylab(bquote(.(y_label)))
  return(output_plot)
}

# g_co2perha <- output_plot(9, "Absolute emissions", "t CO2-e per ha", "t CO"[2]~"-e / ha")
# plot(g_co2perha)
# g_co2perkg <- output_plot(i, "Emission intensity", "kg CO2-e per kg output", "kg CO"[2]~"-e / kg output")
# g_Nsurplus <- output_plot(i, "Nitrogen balance", "Nitrogen surplus (kg)", "kg N surplus / ha")
# g_NUE <- output_plot(i, "Nitrogen use efficiency", "Nitrogen use efficiency (%)", "NUE (%)")
# g_grid <- grid.arrange(g_co2perha, g_co2perkg, g_Nsurplus, g_NUE,
#                        top=textGrob(fbs_type_words[i],gp=gpar(fontsize=20,font=2),x=0,hjust=0))
# g_grid_no_typetitle <- grid.arrange(g_co2perha, g_co2perkg, g_Nsurplus, g_NUE)

