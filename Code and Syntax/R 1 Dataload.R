
# Packages 
rm(list = ls()) # clear environment

library(tidyverse)
library(tidyr)
library(dplyr)
library(writexl)
library(sf) 
library(readxl)

# data import csv, excel and shape files

age <- read.csv("C:/*YOUR FILE PATH*/census 2021 age.csv")
car <- read.csv("C:/*YOUR FILE PATH*/census 2021 car or van.csv")
ethnic <- read.csv("C:/*YOUR FILE PATH*/census 2021 ethnic group.csv")

hours <- read.csv("C:/*YOUR FILE PATH*/census 2021 hours worked.csv")
lang <- read.csv("C:/A*YOUR FILE PATH*/census 2021 main language.csv")
lone_parent <- read.csv("C:/*YOUR FILE PATH*/census 2021 household type.csv")
limited_act <- read.csv("C:/*YOUR FILE PATH*/census 2021 day-to-day activities are limited a lot.csv")
hh_comp <- read.csv("C:/*YOUR FILE PATH*/census 2021 household composition.csv")
carer <- read.csv("C:/*YOUR FILE PATH*/census 2021 unpaid carer.csv")
sr_health <- read.csv("C:/*YOUR FILE PATH*/census 2021 health sr.csv")

dep_edu <- read.csv("C:/*YOUR FILE PATH*/census 2021 deprived in education dimension.csv")
dep_hh <- read.csv("C:/*YOUR FILE PATH*/census 2021 deprived in hh dimension.csv")
dep_econ <- read.csv("C:/*YOUR FILE PATH*/census 2021 economic activity inc retired.csv")
dep_health <- read.csv("C:/*YOUR FILE PATH*/census 2021 dep heath dim.csv")

imd_all <- read.csv("C:/*YOUR FILE PATH*/File_7_-_All_IoD2019_Scores__Ranks__Deciles_and_Population_Denominators_3.csv")
lsoa_lookup <- read.csv("C:/*YOUR FILE PATH*/LSOA_(2011)_to_LSOA_(2021)_to_Local_Authority_District_(2022)_Exact_Fit_Lookup_for_EW_(V3).csv")

rurality <- read.csv("C:/*YOUR FILE PATH*/Rural_Urban_Classification_(2021)_of_LSOAs_in_EW.csv")
green <- read_excel("C:/*YOUR FILE PATH*/ons green space access reduced version.xlsx", sheet = "Eng_Wal") #Scotland removed
activity <- read_excel("C:/*YOUR FILE PATH*/summary_activity_levels.xlsx")
lsoa_shape <- read_sf(dsn = "04 shapefiles", layer = "LSOA_2021_EW_BSC_V4") 

#---------------------------------------------------------------
# reformat data files ready to combine in to one overall LSOA file
# 1 row per LSOA
#---------------------------------------------------------------
# Proportions af ages in LSOA
# Columns to sum for proportion once pivoted
age_cols <- c(
  "1_Aged 2 years and under",
  "2_Aged 3 to 15 years",
  "3_Aged 16 to 24 years",
  "4_Aged 25 to 34 years",
  "5_Aged 35 to 49 years",
  "6_Aged 50 to 64 years",
  "7_Aged 65 to 74 years",
  "8_Aged 75 years and over"
)

#Pivot wide then calculate %75+
age_wide <- age %>%
  # Pivot wider
  pivot_wider(
    id_cols = c(Lower.layer.Super.Output.Areas.Code, Lower.layer.Super.Output.Areas),
    names_from = c(Age..8.categories..Code, Age..8.categories.),
    values_from = Observation,
    names_sep = "_",
    values_fill = 0
  ) %>%
  
  mutate(
    total_age = rowSums(across(all_of(age_cols)), na.rm = TRUE),
    prop_3_15 = `2_Aged 3 to 15 years` / total_age,
    prop_3_24 = (`2_Aged 3 to 15 years` + `3_Aged 16 to 24 years`) / total_age,
    prop_65p = (`7_Aged 65 to 74 years` + `8_Aged 75 years and over`) / total_age,
    prop_75p = (`8_Aged 75 years and over`) / total_age,
    prop_50_74  = (`6_Aged 50 to 64 years`+`7_Aged 65 to 74 years`) / total_age #proxy for Sport Englands 55 to 74 age range
  )

#Access to cars or vans
# Columns to sum for proportion once pivoted
car_cols <- c(
  "0_No cars or vans in household",
  "1_1 or more cars or vans in household",
  "-8_Does not apply"
)

#Pivot wide then calculate % cars
car_wide <- car %>%
  # Pivot wider
  pivot_wider(
    id_cols = c(Lower.layer.Super.Output.Areas.Code, Lower.layer.Super.Output.Areas),
    names_from = c(Car.or.van.availability..3.categories..Code, Car.or.van.availability..3.categories.),
    values_from = Observation,
    names_sep = "_",
    values_fill = 0
  ) %>%
  
  mutate(
    total_cars = rowSums(across(all_of(car_cols)), na.rm = TRUE),
    prop_no_car = `0_No cars or vans in household` / total_cars
  ) %>% 
  mutate(
    log_prop_no_car = log((prop_no_car ) / (1 - prop_no_car )))

#Deprived in the economc dimension
# Columns to sum for proportion once pivoted
dep_econ_cols <- c(
  "-8_Does not apply",
  "1_Economically active (excluding full-time students): In employment",                                                                                                 
  "2_Economically active (excluding full-time students): Unemployed: Seeking work or waiting to start a job already obtained: Available to start working within 2 weeks",
  "3_Economically active and a full-time student: In employment"   ,                                                                                                     
  "4_Economically active and a full-time student: Unemployed: Seeking work or waiting to start a job already obtained: Available to start working within 2 weeks",       
  "5_Economically inactive: Retired" ,                                                                                                                                  
  "6_Economically inactive: Student" ,                                                                                                                                   
  "7_Economically inactive: Looking after home or family" ,                                                                                                              
  "8_Economically inactive: Long-term sick or disabled" ,                                                                                                              
  "9_Economically inactive: Other" 
)


#Pivot wide then calculate % inactive
dep_econ_wide <- dep_econ %>%
  # Pivot wider
  pivot_wider(
    id_cols = c(Lower.layer.Super.Output.Areas.Code, Lower.layer.Super.Output.Areas),
    names_from = c(Economic.activity.status..10.categories..Code, Economic.activity.status..10.categories.),
    values_from = Observation,
    names_sep = "_",
    values_fill = 0
  ) %>%
 
  mutate(
    total_econ = rowSums(across(all_of(dep_econ_cols)), na.rm = TRUE), #removes missing values
    
    prop_econ_inact_ret = `5_Economically inactive: Retired` / total_econ,
    
    prop_econ_inactive = (
      `5_Economically inactive: Retired` +
        `7_Economically inactive: Looking after home or family` +
        `8_Economically inactive: Long-term sick or disabled` +
        `9_Economically inactive: Other`
    ) / total_econ
  )


# Ethnicity
# Columns to sum for proportion non-white once pivoted
ethnic_cols <- c(
  "-8_Does not apply"  ,                                                      
  "1_Asian, Asian British or Asian Welsh: Bangladeshi"  ,                     
  "2_Asian, Asian British or Asian Welsh: Chinese" ,                          
  "3_Asian, Asian British or Asian Welsh: Indian" ,                           
  "4_Asian, Asian British or Asian Welsh: Pakistani"   ,                      
  "5_Asian, Asian British or Asian Welsh: Other Asian"  ,                     
  "6_Black, Black British, Black Welsh, Caribbean or African: African"  ,     
  "7_Black, Black British, Black Welsh, Caribbean or African: Caribbean"  ,   
  "8_Black, Black British, Black Welsh, Caribbean or African: Other Black" ,  
  "9_Mixed or Multiple ethnic groups: White and Asian",                       
  "10_Mixed or Multiple ethnic groups: White and Black African"  ,            
  "11_Mixed or Multiple ethnic groups: White and Black Caribbean" ,           
  "12_Mixed or Multiple ethnic groups: Other Mixed or Multiple ethnic groups",
  "13_White: English, Welsh, Scottish, Northern Irish or British"   ,         
  "14_White: Irish"   ,                                                       
  "15_White: Gypsy or Irish Traveller",                                       
  "16_White: Roma" ,                                                          
  "17_White: Other White" ,                                                   
  "18_Other ethnic group: Arab"   ,                                           
  "19_Other ethnic group: Any other ethnic group"
)


#Pivot wide then calculate % non-white
ethnic_wide <- ethnic %>%
  # Pivot wider
  pivot_wider(
    id_cols = c(Lower.layer.Super.Output.Areas.Code, Lower.layer.Super.Output.Areas),
    names_from = c(Ethnic.group..20.categories..Code, Ethnic.group..20.categories.),
    values_from = Observation,
    names_sep = "_",
    values_fill = 0
  ) %>%
  mutate(
    total_white = (`13_White: English, Welsh, Scottish, Northern Irish or British` + `14_White: Irish` + `17_White: Other White`),
    total_ethnic = rowSums(across(all_of(ethnic_cols)), na.rm = TRUE),
    prop_non_white = 1 - (total_white / total_ethnic),
    log_prop_non_white = log((prop_non_white + 0.001)/(1 - prop_non_white + 0.001)) # add 0.001 because there are some exact zeros
  ) 

# Language - non-english/uk language
# Columns to sum for proportion once pivoted
lang_cols <- c(
  "-8_Does not apply"  ,                                                      
  "1_English or Welsh" ,                     
  "2_Any other UK languages",            
  "3_European languages (EU)",           
  "4_Other European languages (non-EU)", 
  "5_Asian languages",                  
  "6_Oceanic or Australian languages",   
  "7_North or South American languages", 
  "8_African languages" ,                
  "9_Sign and supported languages" ,    
  "10_Any other languages"
)


#Pivot wide then calculate % non english
lang_wide <- lang %>%
  # Pivot wider
  pivot_wider(
    id_cols = c(Lower.layer.Super.Output.Areas.Code, Lower.layer.Super.Output.Areas),
    names_from = c(Main.language..11.categories..Code, Main.language..11.categories.),
    values_from = Observation,
    names_sep = "_",
    values_fill = 0
  ) %>%
  mutate(
    total_uk = (`1_English or Welsh` +  `2_Any other UK languages`),
    total_lang = rowSums(across(all_of(lang_cols)), na.rm = TRUE),
    prop_non_uk = 1 - (total_uk / total_lang),
    log_prop_non_uk = log((prop_non_uk ) / (1 - prop_non_uk ))) #right skewed and long right tail


# Day to day activities are limited
limited_act_cols <- c(
  "-8_Does not apply"  ,                                                      
  "0_No people disabled under the Equality Act whose day-to-day activities are limited a lot in household" ,                     
  "1_1 person disabled under the Equality Act whose day-to-day activities are limited a lot in household" ,            
  "2_2 or more people disabled under the Equality Act whose day-to-day activities are limited a lot in household"
)


#Pivot wide then calculate % limited activity
limited_act_wide <- limited_act %>%
  # Pivot wider
  pivot_wider(
    id_cols = c(Lower.layer.Super.Output.Areas.Code, Lower.layer.Super.Output.Areas),
    names_from = c(Number.of.disabled.people.in.household.whose.day.to.day.activities.are.limited.a.lot..4.categories..Code, 
                   Number.of.disabled.people.in.household.whose.day.to.day.activities.are.limited.a.lot..4.categories.),
    values_from = Observation,
    names_sep = "_",
    values_fill = 0
  ) %>%
mutate(
    total_limit_act = rowSums(across(all_of(limited_act_cols)), na.rm = TRUE),
    prop_limit_act = ((`1_1 person disabled under the Equality Act whose day-to-day activities are limited a lot in household` +
                            `2_2 or more people disabled under the Equality Act whose day-to-day activities are limited a lot in household`)
                          / total_limit_act)
  )


# Deprived in education dimension
# Columns to sum for proportion Deprived in education dimension
dep_edu_cols <- c(
  "-8_Does not apply"  ,                                                      
  "0_Household is not deprived in the education dimension" ,                     
  "1_Household is deprived in the education dimension" )

#Pivot wide then calculate % non english
dep_edu_wide <- dep_edu %>%
  # Pivot wider
  pivot_wider(
    id_cols = c(Lower.layer.Super.Output.Areas.Code, Lower.layer.Super.Output.Areas),
    names_from = c(Household.deprived.in.the.education.dimension..3.categories..Code, 
                   Household.deprived.in.the.education.dimension..3.categories.),
    values_from = Observation,
    names_sep = "_",
    values_fill = 0
  ) %>%
 mutate(
    total_dep_edu = rowSums(across(all_of(dep_edu_cols)), na.rm = TRUE),
    prop_dep_edu = (`1_Household is deprived in the education dimension`/ total_dep_edu)
  )



#Housing deprivation: higher values indicate overcrowding, poor housing, or poor access to services.
#Analysing links between education deprivation and health outcomes, LTC prevalence, or economic inactivity.

# Deprived in housing dimension
# Columns to sum for proportion deprived in household dimension
dep_hh_cols <- c(
  "-8_Does not apply"  ,                                                      
  "0_Household is not deprived in the housing dimension" ,                     
  "1_Household is deprived in the housing dimension" )

#Pivot wide then calculate % deprived
dep_hh_wide <- dep_hh %>%
  # Pivot wider
  pivot_wider(
    id_cols = c(Lower.layer.Super.Output.Areas.Code, Lower.layer.Super.Output.Areas),
    names_from = c(Household.deprived.in.the.housing.dimension..3.categories..Code, 
                   Household.deprived.in.the.housing.dimension..3.categories.),
    values_from = Observation,
    names_sep = "_",
    values_fill = 0
  ) %>%
  mutate(
    total_dep_hh = rowSums(across(all_of(dep_hh_cols)), na.rm = TRUE),
    prop_dep_hh = (`1_Household is deprived in the housing dimension`/ total_dep_hh),
    log_prop_dep_hh = log((prop_dep_hh + 0.001)/(1 - prop_dep_hh + 0.001)) # add 0.001 because there are some exact zeros
  )    

# Deprived in health dimension
# Health deprivation: higher values indicate poorer population health and higher disability prevalence.

# Columns to sum for proportion deprived in health dimension
dep_health_cols <- c(
  "-8_Does not apply"  ,                                                      
  "0_Household is not deprived in the health and disability dimension" ,                     
  "1_Household is deprived in the health and disability dimension")

#Pivot wide then calculate % non english
dep_health_wide <- dep_health %>%
  # Pivot wider
  pivot_wider(
    id_cols = c(Lower.layer.Super.Output.Areas.Code, Lower.layer.Super.Output.Areas),
    names_from = c(Household.deprived.in.the.health.and.disability.dimension..3.categories..Code, 
                   Household.deprived.in.the.health.and.disability.dimension..3.categories.),
    values_from = Observation,
    names_sep = "_",
    values_fill = 0
  ) %>%
  mutate(
    total_dep_health = rowSums(across(all_of(dep_health_cols)), na.rm = TRUE),
    prop_dep_health = (`1_Household is deprived in the health and disability dimension`/ total_dep_health), 
  )


# self reported health.
# Columns to sum for proportion once pivoted
sr_health_cols <- c(
  "-8_Does not apply"  ,                                                      
  "1_Very good or good health" ,                     
  "2_Fair health",
  "3_Bad or very bad health")

#Pivot wide then calculate % good/not good health
sr_health_wide <- sr_health %>%
  # Pivot wider
  pivot_wider(
    id_cols = c(Lower.layer.Super.Output.Areas.Code, Lower.layer.Super.Output.Areas),
    names_from = c(General.health..4.categories..Code, 
                   General.health..4.categories.),
    values_from = Observation,
    names_sep = "_",
    values_fill = 0
  ) %>%
  mutate(
    total_sr_health = rowSums(across(all_of(sr_health_cols)), na.rm = TRUE),
    prop_sr_health_not_good = 1 - (`1_Very good or good health`/ total_sr_health), #proportion not in good or very good
    prop_sr_health_good = (`1_Very good or good health`/ total_sr_health) #proportion not in good or very good
  )


#Household composition
# Columns to sum for proportion once pivoted
hh_comp_cols <- c(
  "-8_Does not apply"  ,                                                      
  "1_One-person household" ,                     
  "2_Single family household: All aged 66 years and over",
  "3_Single family household: Couple family household" ,
  "4_Single family household: Lone parent household",
  "5_Other household types"
  )

#Pivot wide then calculate % household composition
hh_comp_wide <- hh_comp %>%
  # Pivot wider
  pivot_wider(
    id_cols = c(Lower.layer.Super.Output.Areas.Code, Lower.layer.Super.Output.Areas),
    names_from = c(Household.composition..6.categories..Code, 
                   Household.composition..6.categories.),
    values_from = Observation,
    names_sep = "_",
    values_fill = 0
  ) %>%
  mutate(
    total_hh_comp = rowSums(across(all_of(hh_comp_cols)), na.rm = TRUE),
    prop_hh_comp_sing_66p = (`2_Single family household: All aged 66 years and over`/ total_hh_comp), #proportion 66+ single HH
    prop_hh_comp_single = ((`1_One-person household` + `2_Single family household: All aged 66 years and over`)/ total_hh_comp) #proportion single HH
  )


 
#Proportion of carers
# Columns to sum for proportion of households with carers
carer_cols <- c(
  "Does not apply"  ,                                                      
  "No unpaid carers in household"  ,                     
  "1 unpaid carer in household" ,
  "2 unpaid carers in household" ,
  "3 unpaid carers in household",
  "4 or more unpaid carers in household"
)

#Pivot wide then calculate % carers
carer_wide <- carer %>%
  # Pivot wider
  pivot_wider(
    id_cols = c(Lower.layer.Super.Output.Areas.Code, Lower.layer.Super.Output.Areas),
    names_from = c(Number.of.unpaid.carers.in.household..6.categories.),
    values_from = Observation,
    names_sep = "_",
    values_fill = 0
  ) %>%
  mutate(
    total_carer = rowSums(across(all_of(carer_cols)), na.rm = TRUE),
    prop_carer = ((`1 unpaid carer in household`+ `2 unpaid carers in household` + `3 unpaid carers in household` + `4 or more unpaid carers in household`)
                  / total_carer), #proportion 1+ carer
                 )

#Prepare Greenspace as LSOA2011 not 2021
#create weighted avg for LSOA's that have merged or split as current data is not available
#fewer 2011 lsoa's but converting to 21 lsoas fills the gaps'

green_21 <- green %>% 
  left_join(lsoa_lookup  %>% select(LSOA11CD, LSOA21CD),
            by = c("lsoa_code" = "LSOA11CD")) %>%
  group_by(LSOA21CD) %>%
  summarise(
    med_dist_green = sum(med_dist_green * population, na.rm = TRUE) / sum(population, na.rm = TRUE),
    med_size_green = sum(med_size_green * population, na.rm = TRUE) / sum(population, na.rm = TRUE),
    med_total_size_green = sum(med_total_size_green * population, na.rm = TRUE) / sum(population, na.rm = TRUE)
  )%>%                                                #population weighted averages calculated
  ungroup() %>%
  mutate(
    log_dist_green = log(med_dist_green),  # log(1 + x) avoids log(0)
    log_size_green = log(med_size_green),
    log_total_size_green = log(med_total_size_green)
  )                                                   #transform very skewed distributions with log

#Prepare activity table, remove blank row
activity <- activity[complete.cases(activity), ]

#Check categories in rurality
unique(rurality$RUC21NM)
#-------------------------------------
# check wide tables have same number of rows
#-------------------------------------
nrow(age_wide)
nrow(car_wide)
nrow(dep_econ_wide)
nrow(ethnic_wide)
nrow(lang_wide)
nrow(limited_act_wide)
nrow(dep_edu_wide)
nrow(dep_hh_wide)
nrow(dep_health_wide)
nrow(hh_comp_wide)
nrow(carer_wide)
nrow(rurality)
nrow(green)
nrow(green_21)

tables_list <- list(
  age_wide = age_wide,
  car_wide = car_wide,
  dep_econ_wide = dep_econ_wide,
  ethnic_wide = ethnic_wide,
  lang_wide = lang_wide,
  limited_act_wide = limited_act_wide,
  dep_edu_wide = dep_edu_wide,
  dep_hh_wide = dep_hh_wide,
  dep_health_wide = dep_health_wide,
  hh_comp_wide = hh_comp_wide,
  carer_wide = carer_wide,
  rurality = rurality,
  green = green_21
)

# Number of rows per table
sapply(tables_list, nrow)
sapply(tables_list, function(x) sum(is.na(x)))           # Check for missing values in table
#lapply(tables_list, function(x) colSums(is.na(x)))      # Check for missing values by columns


# Check all tables have the same lsoa's and that none are missing

                                                         #Create a list of the LSOA codes in each table
lsoa_lists <- lapply(tables_list, function(x) {
  if("Lower.layer.Super.Output.Areas.Code" %in% colnames(x)) {
    x$`Lower.layer.Super.Output.Areas.Code`
  } else if("LSOA21CD" %in% colnames(x)) {
    x$LSOA21CD
  } else {
    NULL
  }
})

sapply(lsoa_lists, function(x) length(unique(x)))       #Check LSOA counts per table
Reduce(intersect, lsoa_lists)                           #Check overlap across all tables
length(Reduce(intersect, lsoa_lists))                   #compare with the expected number of LSOAs
all_lsoas <- Reduce(union, lsoa_lists)                  #Check which LSOAs are missing in each table
missing_lsoas <- lapply(lsoa_lists, function(x) setdiff(all_lsoas, x))
missing_lsoas                                           #list of LSOAs missing in each table



#-------------------------------------
# Combine into one 'measures' table
#-------------------------------------

#Create table structure first with LSOA and name
starter_table <- age_wide %>%
  select(
    Lower.layer.Super.Output.Areas.Code,
    Lower.layer.Super.Output.Areas
  ) %>%
  distinct()

combined_lsoa_indicators <- starter_table %>%

  left_join(age_wide %>% select(Lower.layer.Super.Output.Areas.Code, prop_3_15, prop_3_24, prop_50_74, prop_65p, prop_75p), 
            by = "Lower.layer.Super.Output.Areas.Code") %>%
  left_join(car_wide %>% select(Lower.layer.Super.Output.Areas.Code, prop_no_car, log_prop_no_car), 
            by = "Lower.layer.Super.Output.Areas.Code") %>%
  left_join(dep_econ_wide %>% select(Lower.layer.Super.Output.Areas.Code, prop_econ_inact_ret, prop_econ_inactive), 
            by = "Lower.layer.Super.Output.Areas.Code") %>%
  left_join(ethnic_wide %>% select(Lower.layer.Super.Output.Areas.Code, prop_non_white, log_prop_non_white), 
          by = "Lower.layer.Super.Output.Areas.Code") %>%  
  left_join(lang_wide %>% select(Lower.layer.Super.Output.Areas.Code, prop_non_uk, log_prop_non_uk), 
            by = "Lower.layer.Super.Output.Areas.Code") %>%  
  left_join(limited_act_wide %>% select(Lower.layer.Super.Output.Areas.Code, prop_limit_act), 
            by = "Lower.layer.Super.Output.Areas.Code") %>% 
  left_join(dep_edu_wide %>% select(Lower.layer.Super.Output.Areas.Code, prop_dep_edu), 
            by = "Lower.layer.Super.Output.Areas.Code") %>%  
  left_join(dep_hh_wide %>% select(Lower.layer.Super.Output.Areas.Code, prop_dep_hh, log_prop_dep_hh), 
            by = "Lower.layer.Super.Output.Areas.Code") %>% 
  left_join(dep_health_wide %>% select(Lower.layer.Super.Output.Areas.Code, prop_dep_health), 
            by = "Lower.layer.Super.Output.Areas.Code") %>% 
  left_join(hh_comp_wide %>% select(Lower.layer.Super.Output.Areas.Code, prop_hh_comp_sing_66p, prop_hh_comp_single), 
          by = "Lower.layer.Super.Output.Areas.Code") %>%
  left_join(carer_wide %>% select(Lower.layer.Super.Output.Areas.Code, prop_carer), 
            by = "Lower.layer.Super.Output.Areas.Code") %>%
  left_join(rurality %>% select(LSOA21CD, RUC21NM, Urban_rural_flag),
            by = c("Lower.layer.Super.Output.Areas.Code" = "LSOA21CD")) %>%
  left_join(green_21  %>% select(LSOA21CD, med_dist_green,	med_size_green,	med_total_size_green, 
                              log_dist_green, log_size_green, log_total_size_green),
              by = c("Lower.layer.Super.Output.Areas.Code" = "LSOA21CD")) %>% 
    rename(lsoa_code = Lower.layer.Super.Output.Areas.Code,
           lsoa_name = Lower.layer.Super.Output.Areas)

nrow(combined_lsoa_indicators)

#Create final table for input to linear regression
activity_joined <- activity %>%
  left_join(combined_lsoa_indicators, by = "lsoa_code")

#Remove null rows
activity_joined <- activity_joined %>%
  filter(!if_all(everything(), is.na))

# Keep only England LSOAs from the shape file (codes starting with 'E') 
# only those in the training dataframe

lsoa_shape_england <- lsoa_shape %>%
  filter(startsWith(LSOA21CD, "E"))         # England only


#Save files for later use

#save(activity_joined, file=paste0('02 Data_Images/activity_joined','.RData'))
#save(activity, file=paste0('02 Data_Images/activity','.RData'))
#save(lsoa_lookup, file=paste0('02 Data_Images/lsoa_lookup','.RData'))
#save(green, file=paste0('02 Data_Images/green','.RData'))
#save(green_21, file=paste0('02 Data_Images/green_21','.RData'))
#save(lsoa_shape, file=paste0('02 Data_Images/lsoa_shape','.RData'))
#save(lsoa_shape_england, file=paste0('02 Data_Images/lsoa_shape_england','.RData'))
#save(combined_lsoa_indicators, file=paste0('02 Data_Images/combined_lsoa_indicators','.RData'))



# Load saved files for analysis
load(file.path('02 Data_Images','combined_lsoa_indicators.RData'))
load(file.path('02 Data_Images','activity_joined.RData'))
load(file.path('02 Data_Images','activity.RData'))
load(file.path('02 Data_Images','lsoa_lookup.RData'))
load(file.path('02 Data_Images','green.RData'))
load(file.path('02 Data_Images','green_21.RData'))
load(file.path('02 Data_Images','lsoa_shape.RData'))
load(file.path('02 Data_Images','lsoa_shape_england.RData'))
