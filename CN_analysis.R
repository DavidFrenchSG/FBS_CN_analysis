#R script for producing GHG analysis for experimental stats publication (Sefari project)
library(haven)
library(tidyverse)
library(spatstat)
FBS_directory_path <- '//s0177a/sasdata1/ags/fas/'
sampyear_range <- 2020:2022
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
    select(fa_id, type, fa_fbi) %>% 
    mutate(sampyear=fa_id%%10000)
  #Combine LFA livestock categories
  # FBS_fa_data_tidy$type[FBS_fa_data_tidy$type %in% 4:6] = 4.5
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
            # CO2e_per_ha_Q1_unwt = quantile(total_ha_co2, 0.25),
            CO2e_per_ha_Q1 = weighted.quantile(total_ha_co2, fbswt, 0.25),
            CO2e_per_ha_Q3 = weighted.quantile(total_ha_co2, fbswt, 0.75),
            CO2e_per_kg_mean = weighted.mean(total_wf_co2, fbswt),
            # CO2e_per_kg_Q1_unwt = quantile(total_wf_co2, 0.25),
            CO2e_per_kg_Q1 = weighted.quantile(total_wf_co2, fbswt, 0.25),
            CO2e_per_kg_Q3 = weighted.quantile(total_wf_co2, fbswt, 0.75))
Nitrogen_summary <- AllYears_nue %>% 
  group_by(sampyear) %>%
  summarise(nue_mean = weighted.mean(nue, fbswt))
  
  
