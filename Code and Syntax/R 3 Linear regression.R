#-------------------------------------------------------------------------
# Load libraries needed
#-------------------------------------------------------------------------

library(ggplot2)
library(dplyr)
library(tidyr)
library(GGally)
library(scales)
library(corrplot)
library(car)
library(Metrics)
library(stringr)

#-------------------------------------------------------------------------
# Create dataset ready to feed into model
#-------------------------------------------------------------------------

model_data <- activity_joined %>%
  select(
    lsoa_code, 
    `active_aged 75+`,           # Outcome - target variable
    prop_75p,                    # Population proportion aged 75+
    log_prop_dep_hh,             # Household deprivation (logit)
    log_prop_non_white,          # Ethnicity (logit)
    log_dist_green,              # Median distance to nearest park (log)
    log_total_size_green,        # Combined park size within 1 km (log)
    prop_hh_comp_single,         # households with 1 person or single family and 66 yrs plus
    prop_carer,                  # Proportion hh with carer
    RUC21NM                      # Factor variable
  ) %>%
  # Ensure Urban_rural_flag is a factor
  mutate(RUC21NM = as.factor(RUC21NM))

#This is the reference category to assess the other rural urban categories against
 # (there will not be a coefficent for it)

model_data$RUC21NM <- relevel(model_data$RUC21NM, ref = "Urban: Nearer to a major town or city") 

#-------------------------------------------------------------------------
# Set seed for reproducibility and split into test and train datasets
#-------------------------------------------------------------------------

set.seed(123)  # for reproducibility

# 70% train, 30% test
#create sequence number & select 70% randomly, split the dataset based on the row (test or train)
train_index <- sample(seq_len(nrow(model_data)), size = 0.7 * nrow(model_data)) 
train_data <- model_data[train_index, ]                                         
test_data  <- model_data[-train_index, ]

# save(model_data, file=paste0('02 Data_Images/model_data','.RData'))
# save(train_data, file=paste0('02 Data_Images/train_data','.RData'))
# save(test_data, file=paste0('02 Data_Images/test_data','.RData'))

#-------------------------------------------------------------------------
# single model with individual variables first to check individual effects
#-------------------------------------------------------------------------

# Apply linear regression to train data
individual_effects_train <- lm(`active_aged 75+` ~ prop_75p + log_prop_dep_hh + log_prop_non_white +
                       log_dist_green + log_total_size_green +  prop_hh_comp_single + 
                       prop_carer + RUC21NM,
                     data = train_data)

summary(individual_effects_train)

#Test model on test data

test_predictions <- predict(individual_effects_train, newdata = test_data)
summary(test_predictions)

#-------------------------------------------------------------------------
# compare the train and test model outputs
#-------------------------------------------------------------------------
# Training:
# Get metric values for training data
training_predictions <- predict(individual_effects_train, newdata = train_data)

# RMSE
rmse_train <- rmse(train_data$`active_aged 75+`, training_predictions)
# MAE
mae_train <- mae(train_data$`active_aged 75+`, training_predictions)
# R²
r2_train <- cor(train_data$`active_aged 75+`, training_predictions)^2

adj_r2_train <- summary(individual_effects_train)$adj.r.squared

rmse_train; mae_train; r2_train; adj_r2_train

# Get metric values for test data

# Root Mean Squared Error (RMSE)
rmse_test <- rmse(test_data$`active_aged 75+`, test_predictions)
# Mean Absolute Error (MAE)
mae_test <- mae(test_data$`active_aged 75+`, test_predictions)
# R-squared on test set

rmse_test; mae_test; r2_test


#-------------------------------------------------------------------------
# Put results into a dataframe
#-------------------------------------------------------------------------

metrics <- data.frame(
  Dataset = c("Training", "Test"),
  RMSE = c(rmse(train_data$`active_aged 75+`, training_predictions),
           rmse(test_data$`active_aged 75+`, test_predictions)),
  MAE  = c(mae(train_data$`active_aged 75+`, training_predictions),
           mae(test_data$`active_aged 75+`, test_predictions)),
  R2   = c(cor(train_data$`active_aged 75+`, training_predictions)^2,
           cor(test_data$`active_aged 75+`, test_predictions)^2),
  Adjusted_R2 = c(adj_r2_train, NA)
)

metrics

# save(metrics, file=paste0('02 Data_Images/metrics_indiv','.RData'))
#-------------------------------------------------------------------------
# plot predicted and observed results on the test data
#-------------------------------------------------------------------------
test_plot_df <- data.frame(
  predicted = test_predictions,                  # vector of predictions
  observed  = test_data$`active_aged 75+`       # actual values
)

# Plot to visually see how test and train models perform

plot_df <- bind_rows(
  data.frame(
    Dataset = "Train",
    Predicted = values_train_data,
    Observed  = train_data$`active_aged 75+`
  ),
  data.frame(
    Dataset = "Test",
    Predicted = individual_effects_test,
    Observed  = test_data$`active_aged 75+`
  )
)

ggplot(plot_df, aes(x = Predicted, y = Observed, color = Dataset)) +
  geom_point(alpha = 0.2) +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  facet_wrap(~Dataset) +
  xlab("Predicted Activity 75+") +
  ylab("Observed Activity 75+") +
  ggtitle("Predicted results v Observed results: Comparing Train and Test models") +
  theme_minimal() +
  scale_color_manual(values = c("Train" = "#4682B4", "Test" = "#1C1F62"))+
  #convert to % for easier interpretation by users               
  scale_x_continuous(labels = percent_format(accuracy = 1)) +  
  scale_y_continuous(labels = percent_format(accuracy = 1))     

#-------------------------------------------------------------------------
# Look at interaction models - 2 variables
#-------------------------------------------------------------------------

# Create an interaction model on training data
interaction_train_data <- lm(`active_aged 75+` ~ 
                          prop_75p * log_dist_green +       # 1 does dist to green space affect activity levels
                          prop_75p * log_total_size_green + # 2 does total green space affect activity levels
                          prop_75p * RUC21NM +              # 3 does the effect of proportion of 75+ differ across rural/urban categories
                          log_prop_dep_hh * RUC21NM +       # 4 does deprivation affect activity levels differently across rural/urban categories
                          log_prop_non_white * RUC21NM +    # 5 does ethnicity affect activity levels differently across rural/urban categories
                          prop_hh_comp_single * RUC21NM +   # 6 do single households affect activity levels differently across rural/urban categories
                          prop_carer * RUC21NM +            # 7 do households with carers affect activity levels differently across rural/urban categories
                          RUC21NM,                          
                        data = train_data)

summary(interaction_train_data)

# Test interaction model on test data

# Predictions
train_predictions <- predict(interaction_train_data, newdata = train_data)
test_predictions  <- predict(interaction_train_data, newdata = test_data)


adj_r2_interaction <- summary(interaction_train_data)$adj.r.squared

# Training data metrics
train_metrics <- data.frame(
  Dataset = "Train",
  RMSE = rmse(train_data$`active_aged 75+`, train_predictions),
  MAE  = mae(train_data$`active_aged 75+`, train_predictions),
  R2   = cor(train_data$`active_aged 75+`, train_predictions)^2,
  Adjusted_R2 = adj_r2_interaction
)

# Test data metrics 
test_metrics <- data.frame(
  Dataset = "Test",
  RMSE = rmse(test_data$`active_aged 75+`, test_predictions),
  MAE  = mae(test_data$`active_aged 75+`, test_predictions)^2,
  R2   = cor(test_data$`active_aged 75+`, test_predictions)^2,
  Adjusted_R2 = NA
)

# Combine
metrics_interaction_model <- bind_rows(train_metrics, test_metrics)
#save(metrics_interaction_model, file=paste0('02 Data_Images/metrics_interaction_model','.RData'))

#-------------------------------------------------------------------
# Plots of interaction results
#-------------------------------------------------------------------

# Distance to green space  (1)
#------------------------------
# Group prop_75p values (low, medium, high) to see changes as proportions change
# (lower, average and higher than average proportions of older adults)
prop_75_vals <- quantile(model_data$prop_75p, probs = c(0.25, 0.5, 0.75)) 


pred_distance_df <- expand.grid(
  log_dist_green = seq(min(model_data$log_dist_green), max(model_data$log_dist_green), length.out = 100),
  prop_75p = prop_75_vals,
  log_total_size_green = mean(model_data$log_total_size_green),
  log_prop_dep_hh = mean(model_data$log_prop_dep_hh),
  log_prop_non_white = mean(model_data$log_prop_non_white),
  prop_hh_comp_single = mean(model_data$prop_hh_comp_single),
  prop_carer = mean(model_data$prop_carer),
  RUC21NM = "Urban: Nearer to a major town or city"
)

pred_distance_df$pred_active <- predict(interaction_train_data, newdata = pred_distance_df)
pred_distance_df$prop_75p_group <- factor(pred_distance_df$prop_75p, labels = c("Low", "Medium", "High"))

ggplot(pred_distance_df, aes(x = exp(log_dist_green), y = pred_active, color = prop_75p_group)) +
  geom_line(linewidth = 1.2) +
  scale_x_continuous(name = "Distance to nearest green space (m)", trans = "log10") +
  scale_y_continuous(name = "Predicted active - aged 75+") +
  labs(color = "Proportion of older adults ") +
  theme_minimal() +
  ggtitle("Proportion of older adults v Distance to green space")

# Size of green space (2)
#------------------------------

pred_size_df <- expand.grid(
  prop_75p = prop_75_vals,
  log_total_size_green = seq(min(model_data$log_total_size_green), max(model_data$log_total_size_green), length.out = 100),
  log_dist_green = mean(model_data$log_dist_green),
  log_prop_dep_hh = mean(model_data$log_prop_dep_hh),
  log_prop_non_white = mean(model_data$log_prop_non_white),
  prop_hh_comp_single = mean(model_data$prop_hh_comp_single),
  prop_carer = mean(model_data$prop_carer),
  RUC21NM = "Urban: Nearer to a major town or city"
)

pred_size_df$pred_active <- predict(interaction_train_data, newdata = pred_size_df)
pred_size_df$prop_75p_group <- factor(pred_size_df$prop_75p, labels = c("Low", "Medium", "High"))

ggplot(pred_size_df, aes(x = exp(log_total_size_green), y = pred_active, color = prop_75p_group)) + #convert back from log for size in m2
  geom_line(size = 1.2) +
  scale_x_continuous(
    name = "Total green space size (m²)",
    trans = "log10",  # keeps the logarithmic scale for spacing
    breaks = trans_breaks("log10", function(x) 10^x),  
    labels = comma  ) +
  scale_y_continuous(name = "Predicted active - aged 75+") +
  labs(color = "Older Adults Proportion") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)  
  ) +
  ggtitle("Proportion of older adults v Total green space")

# rural/Urban (3)
#------------------------------

pred_RUC21NM_df <- expand.grid(
  prop_75p = prop_75_vals,                          
  RUC21NM = unique(model_data$RUC21NM),            
  log_dist_green = mean(model_data$log_dist_green),
  log_total_size_green = mean(model_data$log_total_size_green),
  log_prop_dep_hh = mean(model_data$log_prop_dep_hh),
  log_prop_non_white = mean(model_data$log_prop_non_white),
  prop_hh_comp_single = mean(model_data$prop_hh_comp_single),
  prop_carer = mean(model_data$prop_carer)
)

# Predict
pred_RUC21NM_df$pred_active <- predict(interaction_train_data, newdata = pred_RUC21NM_df)

# Label prop_75p levels
pred_RUC21NM_df$prop_75p_group <- factor(pred_RUC21NM_df$prop_75p, labels = c("Low", "Medium", "High"))

# Plot
ggplot(pred_RUC21NM_df, aes(x = prop_75p, y = pred_active, color = RUC21NM)) +
  geom_line(size = 1.2) +
  scale_x_continuous(name = "Older Adults Proportion") +
  scale_y_continuous(name = "Predicted active - aged 75+") +
  labs(color = "Rural/Urban Category") +
  theme_minimal() +
  ggtitle("Proportion of older adults v Rural/Urban")


# Deprived households (4)
#------------------------------

# Representative values for deprivation (low, medium, high)
dep_vals <- quantile(model_data$log_prop_dep_hh, probs = c(0.25, 0.5, 0.75))

pred_dep_df <- expand.grid(
  log_prop_dep_hh = dep_vals,
  RUC21NM = unique(model_data$RUC21NM),
  prop_75p = mean(model_data$prop_75p),
  log_dist_green = mean(model_data$log_dist_green),
  log_total_size_green = mean(model_data$log_total_size_green),
  log_prop_non_white = mean(model_data$log_prop_non_white),
  prop_hh_comp_single = mean(model_data$prop_hh_comp_single),
  prop_carer = mean(model_data$prop_carer)
)

pred_dep_df$pred_active <- predict(interaction_train_data, newdata = pred_dep_df)
pred_dep_df$dep_group <- factor(pred_dep_df$log_prop_dep_hh, labels = c("Low", "Medium", "High"))

ggplot(pred_dep_df, aes(x = log_prop_dep_hh, y = pred_active, color = RUC21NM)) +
  geom_line(size = 1.2) +
  labs(x = "Deprivation (log roportion of households in deprived dimension)",
       y = "Predicted active - aged 75+",
       color = "Rural/Urban") +
  ggtitle("Proportion of households in deprived dimension v Rural/Urban") +
  theme_minimal()


# Ethnicity (5)
#------------------------------

eth_vals <- quantile(model_data$log_prop_non_white, probs = c(0.25, 0.5, 0.75))

pred_eth_df <- expand.grid(
  log_prop_non_white = eth_vals,
  RUC21NM = unique(model_data$RUC21NM),
  prop_75p = mean(model_data$prop_75p),
  log_dist_green = mean(model_data$log_dist_green),
  log_total_size_green = mean(model_data$log_total_size_green),
  log_prop_dep_hh = mean(model_data$log_prop_dep_hh),
  prop_hh_comp_single = mean(model_data$prop_hh_comp_single),
  prop_carer = mean(model_data$prop_carer)
)

pred_eth_df$pred_active <- predict(interaction_train_data, newdata = pred_eth_df)
pred_eth_df$eth_group <- factor(pred_eth_df$log_prop_non_white, labels = c("Low", "Medium", "High"))

ggplot(pred_eth_df, aes(x = log_prop_non_white, y = pred_active, color = RUC21NM)) +
  geom_line(size = 1.2) +
  labs(x = "log proportion non-white ethnicity)",
       y = "Predicted active - aged 75+",
       color = "Rural/Urban") +
  ggtitle("Proportion of non-white ethnicity v Rural/Urban") +
  theme_minimal()

# single households (6)
#------------------------------

# Representative values for single households
single_vals <- quantile(model_data$prop_hh_comp_single, probs = c(0.25, 0.5, 0.75))

pred_single_df <- expand.grid(
  prop_hh_comp_single = single_vals,
  RUC21NM = unique(model_data$RUC21NM),
  prop_75p = mean(model_data$prop_75p),
  log_dist_green = mean(model_data$log_dist_green),
  log_total_size_green = mean(model_data$log_total_size_green),
  log_prop_dep_hh = mean(model_data$log_prop_dep_hh),
  log_prop_non_white = mean(model_data$log_prop_non_white),
  prop_carer = mean(model_data$prop_carer)
)

pred_single_df$pred_active <- predict(interaction_train_data, newdata = pred_single_df)
pred_single_df$single_group <- factor(pred_single_df$prop_hh_comp_single, labels = c("Low", "Medium", "High"))

ggplot(pred_single_df, aes(x = prop_hh_comp_single, y = pred_active, color = RUC21NM)) +
  geom_line(size = 1.2) +
  labs(x = "Proportion of single households (aged 66+)",
       y = "Predicted active - aged 75+",
       color = "Rural/Urban") +
  ggtitle("Proportion of single households × Rural/Urban") +
  theme_minimal()



# Households with carers (7)
#------------------------------

# Representative values for prop_carer
prop_carer_vals <- quantile(model_data$prop_carer, probs = c(0.25, 0.5, 0.75))

pred_carer_df <- expand.grid(
  prop_carer = prop_carer_vals,
  RUC21NM = unique(model_data$RUC21NM),
  prop_75p = mean(model_data$prop_75p),
  log_dist_green = mean(model_data$log_dist_green),
  log_total_size_green = mean(model_data$log_total_size_green),
  log_prop_dep_hh = mean(model_data$log_prop_dep_hh),
  log_prop_non_white = mean(model_data$log_prop_non_white),
  prop_hh_comp_single = mean(model_data$prop_hh_comp_single)
)

pred_carer_df$pred_active <- predict(interaction_train_data, newdata = pred_carer_df)
pred_carer_df$prop_carer_group <- factor(pred_carer_df$prop_carer, labels = c("Low", "Medium", "High"))

ggplot(pred_carer_df, aes(x = prop_carer, y = pred_active, color = RUC21NM)) +
  geom_line(size = 1.2) +
  scale_x_continuous(name = "Proportion of households with carers") +
  scale_y_continuous(name = "Predicted active - aged 75+") +
  labs(color = "Rural/Urban Category") +
  theme_minimal() +
  ggtitle("Proportion of households with carers × Rural/Urban")


#----------------------------------------------------------------------------------
# extract coefficients and calculate contributions (not effect changes)to show 
# on a map - redo but with regression on full dataset?
#----------------------------------------------------------------------------------

#join model_data to shape file - has all model data and only england lsoas
model_data_shp <- lsoa_shape_england %>%
  left_join(model_data, by = c("LSOA21CD" = "lsoa_code"))

#extract coefficients from individual variable training model
ind_coefs <- coef(individual_effects_train)

# Multiply each variable by the coefficient for effect contribution
model_data_shp <- model_data_shp %>%
  mutate(
    prop_75p_effect           = ind_coefs["prop_75p"] * prop_75p,
    log_prop_dep_hh_effect    = ind_coefs["log_prop_dep_hh"] * log_prop_dep_hh,
    log_prop_non_white_effect = ind_coefs["log_prop_non_white"] * log_prop_non_white,
    log_dist_green_effect     = ind_coefs["log_dist_green"] * log_dist_green,
    log_total_size_green_effect = ind_coefs["log_total_size_green"] * log_total_size_green,
    prop_hh_comp_single_effect = ind_coefs["prop_hh_comp_single"] * prop_hh_comp_single,
    prop_carer_effect         = ind_coefs["prop_carer"] * prop_carer
  )

#filter to just BNSSG local authorities
model_data_shp_local <- model_data_shp %>%
  filter(str_detect(LSOA21NM, "^Bristol") |
           str_detect(LSOA21NM, "^North Somerset") |
           str_detect(LSOA21NM, "^South Gloucestershire"))

#proportion of 75+
ggplot(model_data_shp_local) +
  geom_sf(aes(fill = prop_75p_effect)) +
  scale_fill_viridis_c(option = "A") +
  labs(title = "Effect of the proportion of older adults on activity levels",
  subtitle = "Bristol, North Somerest and South Gloucestershire Local Authorities")+
  theme_void() +                        
  theme(legend.position = c(0.05, 0.95),  
        legend.justification = c("left", "top"))

#proportion of households in deprived dimension
ggplot(model_data_shp_local) +
  geom_sf(aes(fill = log_prop_dep_hh_effect)) +
  scale_fill_viridis_c(option = "A") +
  labs(title = "Effect of the proportion of households in the deprived dimension on activity levels",
  subtitle = "Bristol, North Somerest and South Gloucestershire Local Authorities")+
  theme_void() +                        
  theme(legend.position = c(0.05, 0.95),  
        legend.justification = c("left", "top"))

#proportion of non-white ethnicity
ggplot(model_data_shp_local) +
  geom_sf(aes(fill = log_prop_non_white_effect)) +
  scale_fill_viridis_c(option = "A") +
  labs(title = "Effect of the proportion of non-white ethnicity on activity levels in older adults",
       subtitle = "Bristol, North Somerest and South Gloucestershire Local Authorities") +
  theme_void() +                        
  theme(legend.position = c(0.05, 0.95),  
        legend.justification = c("left", "top"))

#Households with carers
ggplot(model_data_shp_local) +
  geom_sf(aes(fill = prop_carer_effect)) +
  scale_fill_viridis_c(option = "A") +
  labs(title = "Effect of proportionof households with carers on activity levels in older adults",
       subtitle = "Bristol, North Somerest and South Gloucestershire Local Authorities") +
  theme_void() +                        
  theme(legend.position = c(0.05, 0.95),  
        legend.justification = c("left", "top"))

#proportion of households with carers
ggplot(model_data_shp_local) +
  geom_sf(aes(fill = prop_carer_effect)) +
  scale_fill_viridis_c(option = "A") +
  labs(title = "Effect of proportionof households with carers on activity levels in older adults",
       subtitle = "Bristol, North Somerest and South Gloucestershire Local Authorities") +
  theme_void() +                        
  theme(legend.position = c(0.05, 0.95),  
        legend.justification = c("left", "top"))


#proportion of single occupant households aged 66+
ggplot(model_data_shp_local) +
  geom_sf(aes(fill = prop_hh_comp_single_effect)) +
  scale_fill_viridis_c(option = "A") +
  labs(title = "Effect of proportionof single occupant households (aged 66+) on activity levels in older adults",
       subtitle = "Bristol, North Somerest and South Gloucestershire Local Authorities") +
  theme_void() +                        
  theme(legend.position = c(0.05, 0.95),  
        legend.justification = c("left", "top"))

#log_dist_green_effect  
ggplot(model_data_shp_local) +
  geom_sf(aes(fill = log_dist_green_effect)) +
  scale_fill_viridis_c(option = "A") +
  labs(title = "Effect of distance to green space on activity levels in older adults",
       subtitle = "Bristol, North Somerest and South Gloucestershire Local Authorities") +
  theme_void() +                        
  theme(legend.position = c(0.05, 0.95),  
        legend.justification = c("left", "top"))

#log_total_size_green_effect 
ggplot(model_data_shp_local) +
  geom_sf(aes(fill = log_total_size_green_effect)) +
  scale_fill_viridis_c(option = "A") +
  labs(title = "Effect of total size of green space on activity levels in older adults",
       subtitle = "Bristol, North Somerest and South Gloucestershire Local Authorities") +
  theme_void() +                        
  theme(legend.position = c(0.05, 0.95),  
        legend.justification = c("left", "top"))































