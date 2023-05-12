#R script for producing GHG analysis for experimental stats publication (Sefari project)
library(haven)
library(tidyverse)
library(spatstat)
library(data.table)
FBS_directory_path <- '//s0177a/sasdata1/ags/fas/'
Output_directory <- '//s0177a/datashare/seerad/fas/raw_data/prod2022/Sefari_outputs'

#Variables for farmtype names and numbering
fbs_type_numbers <- c(1:10)
fbs_type_words <- c("Cereal","General Cropping","Dairy","LFA Sheep","LFA Cattle","LFA Cattle and Sheep","Lowland Livestock","Mixed","All farm types [1]", "Less favoured area (LFA) livestock farms")
fbs_type_tab <- data.frame(fbs_type_numbers, fbs_type_words)
apply_type_formats <- function(table_name) {
  setkey(setDT(table_name),type)
  table_name[setDT(fbs_type_tab),farmtype:=i.fbs_type_words]
  return(table_name)
}

#Years of interest
sampyear_range <- 2020:2022

#Loop to read in data
for (sampyear in sampyear_range){
  if(sampyear==max(sampyear_range)){
    datayear=sampyear
  }  else {
    datayear=sampyear + 1
  }
  if(sampyear==min(sampyear_range)){
    #initialise variables?
    AllYears_fa <- NULL
    AllYears_carbon <- NULL
    AllYears_nue <- NULL
  }
  FBS_fa_data_file <- paste0("so_y", datayear, "_fa",".sas7bdat")
  FBS_carbon_file <- paste0("so_y", datayear, "_carbon",".sas7bdat")
  
  #sampyear rather than datayear for NUE filename
  FBS_nue_file <- paste0("so_y", sampyear, "_nue",".sas7bdat")

  #Single year's FA data
  FBS_fa_data <- tryCatch(
    {
      FBS_fa_data <- read_sas(FBS_fa_data_file)
    },
    error = function(e)
    {
      file.copy(paste0(FBS_directory_path, FBS_fa_data_file), getwd())
      return(read_sas(FBS_fa_data_file))
    }
  )
  names(FBS_fa_data) <- tolower(names(FBS_fa_data))
  for (x in colnames(FBS_fa_data)){
    attr(FBS_fa_data[[deparse(as.name(x))]],"format.sas")=NULL
  }
  #Process FA data
  FBS_fa_data_tidy <- FBS_fa_data %>% 
    filter(fa_id%%10000==sampyear) %>% 
    select(fa_id, type, fa_fbi, fa_outpt, fa_tarea, fa_aua) %>% 
    mutate(sampyear=fa_id%%10000)
  #Combine LFA livestock categories
  # FBS_fa_data_tidy$type[FBS_fa_data_tidy$type %in% 4:6] = 6
  #Single year's carbon data
  FBS_carbon_data <- tryCatch(
    {
      FBS_carbon_data <- read_sas(FBS_carbon_file)
    },
    error = function(e)
    {
      file.copy(paste0(FBS_directory_path, FBS_carbon_file), getwd())
      return(read_sas(FBS_carbon_file))
    }
  )
  names(FBS_carbon_data) <- tolower(names(FBS_carbon_data))
  for (x in colnames(FBS_carbon_data)){
    attr(FBS_carbon_data[[deparse(as.name(x))]],"format.sas")=NULL
  }
  #Process carbon data
  FBS_carbon_data_tidy <- FBS_carbon_data %>% 
    filter(fa_id%%10000==sampyear)
  #Single year's NUE data
  FBS_nue_data <- tryCatch(
    {
      FBS_nue_data <- read_sas(FBS_nue_file)
    },
    error = function(e)
    {
      file.copy(paste0(FBS_directory_path, FBS_nue_file), getwd())
      return(read_sas(FBS_nue_file))
    }
  )
  names(FBS_nue_data) <- tolower(names(FBS_nue_data))
  for (x in colnames(FBS_nue_data)){
    attr(FBS_nue_data[[deparse(as.name(x))]],"format.sas")=NULL
  }
  #Process NUE data
  FBS_nue_data_tidy <- FBS_nue_data %>% 
    filter(fa_id%%10000==sampyear,
           an_code =="NNKG")
    
  
  #Append each year's data to All Years dataset
  AllYears_fa <- AllYears_fa %>% 
    bind_rows(FBS_fa_data_tidy)
  AllYears_carbon <- AllYears_carbon %>% 
    bind_rows(FBS_carbon_data_tidy)
  AllYears_nue <- AllYears_nue %>% 
    bind_rows(FBS_nue_data_tidy)
}


FBS_weights_file <- paste0("new_weights.sas7bdat")
FBS_weights <- tryCatch(
  {
    FBS_weights <- read_sas(FBS_weights_file)
  },
  error = function(e)
  {
    file.copy(paste0(FBS_directory_path, FBS_weights_file), getwd())
    return(read_sas(FBS_weights_file))
  }
)
names(FBS_weights) <- tolower(names(FBS_weights))
for (x in colnames(FBS_weights)){
  attr(FBS_weights[[deparse(as.name(x))]],"format.sas")=NULL
}

#Join weights/fa to carbon and nue datasets
AllYears_carbon <- AllYears_carbon %>% 
  inner_join(FBS_weights, by="fa_id") %>% 
  inner_join(AllYears_fa, by="fa_id")
AllYears_nue <- AllYears_nue %>% 
  inner_join(FBS_weights, by="fa_id") %>% 
  inner_join(AllYears_fa, by="fa_id")

#Create carbon output table 
Carbon_summary <- AllYears_carbon %>% 
  group_by(sampyear,type) %>% 
  summarise(CO2e_per_ha_mean = weighted.mean(total_ha_co2,fbswt),
            CO2e_per_ha_Q1 = weighted.quantile(total_ha_co2, fbswt, 0.25),
            CO2e_per_ha_Q3 = weighted.quantile(total_ha_co2, fbswt, 0.75),
            CO2e_per_ha_min = min(total_ha_co2),
            CO2e_per_ha_med = weighted.median(total_ha_co2, fbswt),
            CO2e_per_ha_max = max(total_ha_co2),
            CO2e_per_kg_mean = weighted.mean(total_wf_co2, fbswt),
            CO2e_per_kg_Q1 = weighted.quantile(total_wf_co2, fbswt, 0.25),
            CO2e_per_kg_Q3 = weighted.quantile(total_wf_co2, fbswt, 0.75),
            CO2e_per_kg_min = min(total_wf_co2),
            CO2e_per_kg_med = weighted.median(total_wf_co2, fbswt),
            CO2e_per_kg_max = max(total_wf_co2),
            FBI_mean = weighted.mean(fa_fbi, fbswt),
            farm_output_mean = weighted.mean(farm_output_kg, fbswt),
            farm_output_Q1 = weighted.quantile(farm_output_kg, fbswt, 0.25),
            farm_output_Q3 = weighted.quantile(farm_output_kg, fbswt, 0.75),
            fbswt_sum = sum(fbswt),
            simple_count = n())
Carbon_summary_all <- AllYears_carbon %>% 
  group_by(sampyear) %>%
  summarise(CO2e_per_ha_mean = weighted.mean(total_ha_co2,fbswt),
            CO2e_per_ha_Q1 = weighted.quantile(total_ha_co2, fbswt, 0.25),
            CO2e_per_ha_Q3 = weighted.quantile(total_ha_co2, fbswt, 0.75),
            CO2e_per_ha_min = min(total_ha_co2),
            CO2e_per_ha_med = weighted.median(total_ha_co2, fbswt),
            CO2e_per_ha_max = max(total_ha_co2),
            CO2e_per_kg_mean = weighted.mean(total_wf_co2, fbswt),
            CO2e_per_kg_Q1 = weighted.quantile(total_wf_co2, fbswt, 0.25),
            CO2e_per_kg_Q3 = weighted.quantile(total_wf_co2, fbswt, 0.75),
            CO2e_per_kg_min = min(total_wf_co2),
            CO2e_per_kg_med = weighted.median(total_wf_co2, fbswt),
            CO2e_per_kg_max = max(total_wf_co2),
            FBI_mean = weighted.mean(fa_fbi, fbswt),
            farm_output_mean = weighted.mean(farm_output_kg, fbswt),
            farm_output_Q1 = weighted.quantile(farm_output_kg, fbswt, 0.25),
            farm_output_Q3 = weighted.quantile(farm_output_kg, fbswt, 0.75),
            fbswt_sum = sum(fbswt),
            simple_count = n()) %>% 
  mutate(type=9)
Carbon_summary_LFA <- AllYears_carbon %>%
  filter(type %in% 4:6) %>% 
  group_by(sampyear) %>%
  summarise(CO2e_per_ha_mean = weighted.mean(total_ha_co2,fbswt),
            CO2e_per_ha_Q1 = weighted.quantile(total_ha_co2, fbswt, 0.25),
            CO2e_per_ha_Q3 = weighted.quantile(total_ha_co2, fbswt, 0.75),
            CO2e_per_ha_min = min(total_ha_co2),
            CO2e_per_ha_med = weighted.median(total_ha_co2, fbswt),
            CO2e_per_ha_max = max(total_ha_co2),
            CO2e_per_kg_mean = weighted.mean(total_wf_co2, fbswt),
            CO2e_per_kg_Q1 = weighted.quantile(total_wf_co2, fbswt, 0.25),
            CO2e_per_kg_Q3 = weighted.quantile(total_wf_co2, fbswt, 0.75),
            CO2e_per_kg_min = min(total_wf_co2),
            CO2e_per_kg_med = weighted.median(total_wf_co2, fbswt),
            CO2e_per_kg_max = max(total_wf_co2),
            FBI_mean = weighted.mean(fa_fbi, fbswt),
            farm_output_mean = weighted.mean(farm_output_kg, fbswt),
            farm_output_Q1 = weighted.quantile(farm_output_kg, fbswt, 0.25),
            farm_output_Q3 = weighted.quantile(farm_output_kg, fbswt, 0.75),
            fbswt_sum = sum(fbswt),
            simple_count = n()) %>% 
  mutate(type=10)
Carbon_summary <- Carbon_summary %>% 
  bind_rows(Carbon_summary_all, Carbon_summary_LFA) %>% 
  #Convert kg to tonnes for per hectare calculations
  mutate_at(vars(starts_with("CO2e_per_ha")), function(x) x*0.001) 
Carbon_summary <- Carbon_summary[order(Carbon_summary$sampyear),]


Nitrogen_summary <- AllYears_nue %>% 
  group_by(sampyear, type) %>%
  summarise(N_surplus_mean = weighted.mean(farm_n_surplus, fbswt),
            N_surplus_Q1 = weighted.quantile(farm_n_surplus, fbswt, 0.25),
            N_surplus_Q3 = weighted.quantile(farm_n_surplus, fbswt, 0.75),
            N_surplus_min = min(farm_n_surplus),
            N_surplus_med = weighted.median(farm_n_surplus, fbswt),
            N_surplus_max = max(farm_n_surplus, fbswt),
            
            N_input_mean = weighted.mean(ninput_total, fbswt),
            N_input_Q1 = weighted.quantile(ninput_total, fbswt, 0.25),
            N_input_Q3 = weighted.quantile(ninput_total, fbswt, 0.75),
            N_input_min = min(ninput_total),
            N_input_med = weighted.median(ninput_total, fbswt),
            N_input_max = max(ninput_total, fbswt),
            
            N_output_mean = weighted.mean(noutput_total, fbswt),
            N_output_Q1 = weighted.quantile(noutput_total, fbswt, 0.25),
            N_output_Q3 = weighted.quantile(noutput_total, fbswt, 0.75),
            N_output_min = min(noutput_total),
            N_output_med = weighted.median(noutput_total, fbswt),
            N_output_max = max(noutput_total, fbswt),
            
            nue_mean = weighted.mean(nue, fbswt),
            nue_Q1 = weighted.quantile(nue, fbswt, 0.25),
            nue_Q3 = weighted.quantile(nue, fbswt, 0.75),
            nue_min = min(nue),
            nue_med = weighted.median(nue, fbswt),
            nue_max = max(nue),
            fbswt_sum = sum(fbswt),
            simple_count = n())
Nitrogen_summary_all <- AllYears_nue %>% 
  group_by(sampyear) %>%
  summarise(N_surplus_mean = weighted.mean(farm_n_surplus, fbswt),
            N_surplus_Q1 = weighted.quantile(farm_n_surplus, fbswt, 0.25),
            N_surplus_Q3 = weighted.quantile(farm_n_surplus, fbswt, 0.75),
            N_surplus_min = min(farm_n_surplus),
            N_surplus_med = weighted.median(farm_n_surplus, fbswt),
            N_surplus_max = max(farm_n_surplus, fbswt),
            
            N_input_mean = weighted.mean(ninput_total, fbswt),
            N_input_Q1 = weighted.quantile(ninput_total, fbswt, 0.25),
            N_input_Q3 = weighted.quantile(ninput_total, fbswt, 0.75),
            N_input_min = min(ninput_total),
            N_input_med = weighted.median(ninput_total, fbswt),
            N_input_max = max(ninput_total, fbswt),
            
            N_output_mean = weighted.mean(noutput_total, fbswt),
            N_output_Q1 = weighted.quantile(noutput_total, fbswt, 0.25),
            N_output_Q3 = weighted.quantile(noutput_total, fbswt, 0.75),
            N_output_min = min(noutput_total),
            N_output_med = weighted.median(noutput_total, fbswt),
            N_output_max = max(noutput_total, fbswt),
            
            nue_mean = weighted.mean(nue, fbswt),
            nue_Q1 = weighted.quantile(nue, fbswt, 0.25),
            nue_Q3 = weighted.quantile(nue, fbswt, 0.75),
            nue_min = min(nue),
            nue_med = weighted.median(nue, fbswt),
            nue_max = max(nue),
            fbswt_sum = sum(fbswt),
            simple_count = n()) %>% 
  mutate(type=9)
Nitrogen_summary_LFA <- AllYears_nue %>%
  filter(type %in% 4:6) %>% 
  group_by(sampyear) %>%
  summarise(N_surplus_mean = weighted.mean(farm_n_surplus, fbswt),
            N_surplus_Q1 = weighted.quantile(farm_n_surplus, fbswt, 0.25),
            N_surplus_Q3 = weighted.quantile(farm_n_surplus, fbswt, 0.75),
            N_surplus_min = min(farm_n_surplus),
            N_surplus_med = weighted.median(farm_n_surplus, fbswt),
            N_surplus_max = max(farm_n_surplus, fbswt),
            
            N_input_mean = weighted.mean(ninput_total, fbswt),
            N_input_Q1 = weighted.quantile(ninput_total, fbswt, 0.25),
            N_input_Q3 = weighted.quantile(ninput_total, fbswt, 0.75),
            N_input_min = min(ninput_total),
            N_input_med = weighted.median(ninput_total, fbswt),
            N_input_max = max(ninput_total, fbswt),
            
            N_output_mean = weighted.mean(noutput_total, fbswt),
            N_output_Q1 = weighted.quantile(noutput_total, fbswt, 0.25),
            N_output_Q3 = weighted.quantile(noutput_total, fbswt, 0.75),
            N_output_min = min(noutput_total),
            N_output_med = weighted.median(noutput_total, fbswt),
            N_output_max = max(noutput_total, fbswt),
            
            nue_mean = weighted.mean(nue, fbswt),
            nue_Q1 = weighted.quantile(nue, fbswt, 0.25),
            nue_Q3 = weighted.quantile(nue, fbswt, 0.75),
            nue_min = min(nue),
            nue_med = weighted.median(nue, fbswt),
            nue_max = max(nue),
            fbswt_sum = sum(fbswt),
            simple_count = n()) %>% 
  mutate(type=10)
Nitrogen_summary <- Nitrogen_summary %>% 
  bind_rows(Nitrogen_summary_all, Nitrogen_summary_LFA)
Nitrogen_summary <- Nitrogen_summary[order(Nitrogen_summary$sampyear), ]

#Apply wordy formats
Carbon_summary <- apply_type_formats(Carbon_summary) %>% 
  select(sampyear, farmtype, everything())
Nitrogen_summary <- apply_type_formats(Nitrogen_summary) %>% 
  select(sampyear, farmtype, everything())

#Write Carbon and Nitrogen summaries to a CSV in the Z drive
write.csv(Carbon_summary, file=paste0(Output_directory,"/Carbon_summary.csv"), row.names = FALSE)
write.csv(Nitrogen_summary, file=paste0(Output_directory,"/Nitrogen_summary.csv"), row.names = FALSE)  


for (i in 1:8){
  check <- AllYears_carbon %>% 
    filter(type==i, sampyear==2022) %>% 
    mutate(calc_area = wf_co2/total_ha_co2)
  
  Q1 <- weighted.quantile(check$total_ha_co2, check$fbswt,0.25)
  Q2 <- weighted.median(check$total_ha_co2, check$fbswt)
  Q3 <- weighted.quantile(check$total_ha_co2, check$fbswt,0.75)
  mean_ind <- weighted.mean(check$total_ha_co2, check$fbswt)
  mean_rat <- sum(check$wf_co2 * check$fbswt)/sum(check$fa_aua*check$fbswt)
  
  # check <- check %>%
  #   filter(total_wf_co2<1000)
  # # check<- check %>%
  #   filter(total_wf_co2<0.95*max(check$total_wf_co2),
  #          total_wf_co2 > 0)
  g=ggplot(check) +
    geom_histogram(aes(x=total_ha_co2, weight=fbswt))+
    geom_vline(xintercept = Q1,colour="red")+
    geom_vline(xintercept = Q2,colour="red")+
    geom_vline(xintercept = Q3,colour="red")+
    geom_vline(xintercept = mean_ind,colour="blue")+
    geom_vline(xintercept = mean_rat,colour="yellow")+
    annotate(x = Q1, y= 0, label = "Q1", geom = "label",colour="red")+
    annotate(x = Q2, y= 0, label = "Q2", geom = "label",colour="red")+
    annotate(x = Q3, y= 0, label = "Q3", geom = "label",colour="red")+
    annotate(x = mean_ind, y=0, label = "Mean_ind.", geom = "label",colour="blue")+
    annotate(x = mean_rat, y= 0, label = "Mean_ratio", geom = "label",colour="yellow")+
    labs(title=paste0(fbs_type_words[i]))
  print(g)
}
ggplot(filter(AllYears_carbon,type==1, abs(total_wf_co2)<10)) +
  geom_histogram(aes(total_wf_co2,weight=fbswt))
ggplot(filter(AllYears_carbon,type==5,total_wf_co2>-10,total_wf_co2<100)) +
  geom_histogram(aes(total_wf_co2,weight=fbswt))
ggplot(filter(AllYears_carbon, type==5))+
  geom_point(aes(x = fa_outpt, y= farm_output_kg))

# 
# write.csv(check, file=paste0(Output_directory,"/check.csv"), row.names = FALSE)
  
Output_types = c(9,1,2,3,10,8)
financial_years_start <- sampyear_range - 1
financial_years_end <- sampyear_range - 2000
financial_years <- paste0(financial_years_start,"-",financial_years_end)
Output_colnames = c("Farm type", "Measure", c(financial_years))

Create_output_table <- function(Input_table, variable, Output_table){
Table_name <- Input_table %>% 
  filter(type %in% Output_types) %>% 
  select("Average (median)"= paste0(variable,"_med"),
         "Lower quartile" = paste0(variable,"_Q1"), 
         "Upper quartile" = paste0(variable,"_Q3"),
         everything()) %>% 
  gather("Average (median)", "Lower quartile", "Upper quartile", key="Measure",value="Value") %>% 
  select("Farm type"=farmtype,type,sampyear,Measure,Value) %>% 
  spread(key=sampyear, Value)
Table_name$type <- factor(Table_name$type, levels = Output_types)
Table_name <- Table_name[order(Table_name$type),] %>% 
  select(-type)
colnames(Table_name) <- Output_colnames
write.csv(Table_name, file=paste0(Output_directory,"/",Output_table,"_output.csv"))
return(Table_name)}
Table_1 <- Create_output_table(Carbon_summary, "CO2e_per_ha", "Table_1")
Table_2 <- Create_output_table(Carbon_summary, "CO2e_per_kg", "Table_2")
Table_3 <- Create_output_table(Nitrogen_summary, "N_surplus", "Table_3")
Table_4 <- Create_output_table(Nitrogen_summary, "nue", "Table_4")
