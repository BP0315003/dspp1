library(ggplot2)
library(stringr)
library(tibble)
library(dplyr)
library(tidyr)
library(GGally)
library(scales)
library(corrplot)
library(car)

#use decimal formatting
options(scipen=999)

# Identify numeric columns
numeric_cols <- combined_lsoa_indicators %>% select(where(is.numeric))

# Identify non-numeric columns (like codes, names, flags)
non_numeric_cols <- combined_lsoa_indicators %>% select(where(~!is.numeric(.)))

summary(numeric_cols)
#summary(numeric_cols$prop_non_uk  )
#summary(numeric_cols$prop_dep_hh)
#sum(numeric_cols$prop_dep_hh == 0)
#sum(numeric_cols$prop_non_white == 0)
#sum(numeric_cols$prop_hh_comp_sing_66p == 0)
summary(green_21)


# Loop through numeric columns
for(col in names(numeric_cols)) {
  p <- ggplot(combined_lsoa_indicators, aes_string(x = col)) +
    geom_histogram(aes(y=..density..), bins=30, fill="#4682B4", color="black", alpha=0.7) +
    geom_density(color="#1C1F62", size=1) + 
    scale_x_continuous(breaks = pretty(combined_lsoa_indicators[[col]], n = 15)) +  # More x-axis ticks
    scale_y_continuous(breaks = scales::pretty_breaks(n = 10), labels = scales::number_format(accuracy = 1))+
    #geom_density(color="#1C1F62", size=1) +                                                     #for smooth estimate of distribution
    labs(title=paste("Histogram & density of", col), x=col, y="Number of LSOAs") +
    theme_minimal()
  
  print(p)
}





# Only numeric columns
#ggpairs(numeric_cols)


rurality_summary <- combined_lsoa_indicators %>%
  count(RUC21NM)%>%
  mutate(percent = n / sum(n))

# Plot
ruralality_num_2 <- ggplot(rurality_summary, aes(x = RUC21NM, y = n)) +
  geom_col(fill = "#4682B4", color = "white") +
  geom_text(aes(label = n),
            vjust = -0.5, size = 3.5) +
  labs(
    title = "Distribution of rurality group by LSOA",
    x = "Rurality group",
    y = "Number of LSOAs"
  ) +
  scale_y_continuous(labels = comma) + 
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5),  
    plot.title = element_text(hjust = 0.5)
  ) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 15))  

ruralality_num_2

max_rural_pct <- max(rurality_summary$percent)

ruralality_pct_2 <- ggplot(rurality_summary, aes(x = RUC21NM, y = percent)) +
  geom_col(fill = "#4682B4", color = "white") +
  geom_text(aes(label = percent(percent, accuracy = 1)),
            vjust = -0.5, size = 3.5) +
  labs(
    title = "Percentage of LSOAs by Rurality Group",
    x = "Rurality group",
    y = "Percentage of LSOAs"
  ) +
  scale_y_continuous(
    labels = percent_format(accuracy = 1),
    limits = c(0, max_rural_pct + 0.1)  
  ) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 15)) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    plot.title = element_text(hjust = 0.5)
  )

ruralality_pct_2

#Check activity distributions

# Identify numeric columns in activity
numeric_cols_act <- activity %>% select(where(is.numeric))
summary(numeric_cols_act)
#activity[!complete.cases(activity), ]

summary(numeric_cols_act$`active_aged 35-54`)
sum(numeric_cols_act$`active_aged 35-54` == 0) #check low summary number for zeros

# Loop through numeric columns
for(col in names(numeric_cols_act)) {
  p <- ggplot(numeric_cols_act, aes_string(x = paste0("`", col, "`"))) +
    geom_histogram(aes(y=..density..), bins=30, fill="#4682B4", color="black", alpha=0.7) +
    geom_density(color="#1C1F62", size=1) +
    labs(title=paste("Histogram & Density of", col), x=col, y="Density") +
    theme_minimal()
  
  print(p)
}

#-------------------------------------------------------------------------------
# Descriptive stats of variables of interest
#-------------------------------------------------------------------------------
#summary(numeric_cols_act$``active_aged 75+`)

desc_stat_vars <- activity_joined %>%
  select(
#activity and age
    `active_aged 75+`, 
    prop_75p, 
 #Household variables
    prop_dep_hh,                 # households in deprived dimension
    prop_hh_comp_sing_66p,       # households with single adult aged 66+
    prop_dep_edu,                # households deprived in education dimension
#Ethnicity and language    
    prop_non_white,
    prop_non_uk,                 # Not English, Welsh or Irish languages
#Green space and access 
    med_dist_green,	
    med_size_green,	
    med_total_size_green, 
    prop_no_car,   
#Social & Health related 
    prop_limit_act,               # persons reporting health limits daily life  
    prop_dep_health,          
    prop_hh_comp_single,          # households with 1 person or single family and 66 yrs plus
    prop_carer                    # households with carers
  )

descriptive_stats <- desc_stat_vars %>%
  summarise(across(
    everything(),
    list(
      min = ~min(.x, na.rm = TRUE),
      max = ~max(.x, na.rm = TRUE),
      mean = ~mean(.x, na.rm = TRUE),
      median = ~median(.x, na.rm = TRUE),
      sd = ~sd(.x, na.rm = TRUE)
    ),
    .names = "{.col}.{.fn}"   
  ))

descriptive_stats_long <- descriptive_stats %>%
  pivot_longer(
    everything(),
    names_to = c("variable", "stat"),
    names_sep = "\\."       # <<< SPLIT AT THE DOT
  ) %>%
  pivot_wider(
    names_from = stat,
    values_from = value
  )%>%

  mutate(across(where(is.numeric), ~round(.x, 4)))

#how many LSOAs have values of less than 5% - check MAPE reliability issues
sum(activity_joined$`active_aged 75+` < 0.05, na.rm = TRUE)
#=5
#-------------------------------------------------------------------------------
# Correlation checks
#-------------------------------------------------------------------------------
#colnames(activity_joined)
# Select only numeric predictor columns
#Comment out some variables after correlation checks, these will not be used

predictors <- activity_joined %>%
  select(
#activity and age
    `active_aged 75+`, 
    prop_75p, 
#Household variables
    log_prop_dep_hh,              # households in deprived dimension
    #prop_hh_comp_sing_66p,       # households with single adult aged 66+ 
    #prop_dep_edu,                # households deprived in education dimension
#Ethnicity and language    
    log_prop_non_white,
    #log_prop_non_uk,             # Not English, Welsh or Irish languages
#Green space and access 
    log_dist_green,  
  #log_size_green, 
    log_total_size_green, 
    #log_prop_no_car,   
#Social & Health related 
    #prop_limit_act,               # persons reporting health limits daily life  
    #prop_dep_health,          
    prop_hh_comp_single,          # households with 1 person or single family and 66 yrs plus
    prop_carer 
      )

#Urban_rural_flag ,RUC21NM, - not numerical omit from correlation


# Compute correlation matrix
cor_mat <- cor(predictors, use = "pairwise.complete.obs")

# Visualize
corrplot::corrplot(cor_mat, method = "color", type = "upper", tl.cex = 0.8)

# Show as table
cor_table <- cor_mat %>%
  as.data.frame() %>%
  rownames_to_column("Var1") %>%
  pivot_longer(
    cols = -Var1,
    names_to = "Var2",
    values_to = "Correlation"
  ) %>%
  filter(Var1 < Var2) 

# Find correlations above threshold
high_cor <- which(abs(cor_mat) > 0.7 & abs(cor_mat) < 1, arr.ind = TRUE)
cor_mat[high_cor]

data.frame(
  Var1 = rownames(cor_mat)[high_cor[,1]],
  Var2 = colnames(cor_mat)[high_cor[,2]],
  Correlation = cor_mat[high_cor]
)


#-------------------------------------------------------------------------------
# Correlation with numeric variable agains the rural urban flag
# biserial correlation - code urban rural as binary
#-------------------------------------------------------------------------------
predictorsUR <- activity_joined %>%
  select(
    `active_aged 75+`, 
    prop_75p, 
    log_prop_dep_hh,             
    prop_hh_comp_sing_66p,       
    prop_dep_edu,                
    log_prop_non_white,
    log_prop_non_uk,            
    log_dist_green,  
    log_size_green, 
    log_total_size_green, 
    log_prop_no_car,   
    prop_limit_act,                 
    prop_dep_health,          
    prop_hh_comp_single,         
    prop_carer,
    Urban_rural_flag
  )

# Make Urban_rural_flag binary (Urban = 1, Rural = 0)
predictorsUR <- predictorsUR %>%
  mutate(Urban_rural_bin = ifelse(Urban_rural_flag == "Urban", 1, 0))

# Take numeric variables only
numeric_vars <- predictorsUR %>%
  select(-Urban_rural_flag, -Urban_rural_bin)  # exclude the original flag and binary for now

# Correlation - numeric variables and Urban_rural_flag 
urban_cor <- sapply(numeric_vars, function(x) cor(x, predictorsUR$Urban_rural_bin, use = "pairwise.complete.obs"))

urban_cor_df <- data.frame(
  Variable = names(urban_cor),
  Urban_Rural_Correlation = round(urban_cor, 3)
)

ggplot(urban_cor_df, aes(x = reorder(Variable, Urban_Rural_Correlation), y = Urban_Rural_Correlation)) +
  geom_col(fill = "#4682B4") +
  coord_flip() +
  labs(
    title = "Variable correlation with Rural_Urban flag",
    x = "Variable",
    y = "Variable correlation with Urban (Positive) / Rural (Negative)"
  ) +
 theme_minimal()

#Results to note
#Distances to green space are further in rural areas - green space is considered public space, parks etc.  
#Rural areas have lots of green areas but are private land OR footpaths that are not covered by green space
#Greater proportion of non English language speaking or white ethnicity individuals in urban areas
#Higher proportion of older or single occupant households in rural areas



# Keep these variables:

#`active_aged 75+`     -  predictor variable
#prop_75p,             - proportion aged 75+, age structure variable
#log_prop_dep_hh       - deprivation and covers lack of vehicle access
#log_prop_non_white    - covers ethnicity and language
#log_dist_green        - distance to green space (access)
#log_total_size_green  - similar but different to total green space (provision)
#prop_hh_comp_single   - represents households with 1 person or single family and houses only 66 yrs plus 
#slightly higher correlation with log_prop_dep_hh (0.74)
#prop_carer            - represents households with carers, limiting factor for exercise.
#Urban_rural_flag      - used to understand level of correlation with other variables but RUC21NM encompasses 
                          # greater detail about distances to urban areas and remotness of areas.  This informs 
                          # on access to public services and public transport issues.  Use this in regression over
                          # basic flag

