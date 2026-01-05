library(ggplot2)
library(dplyr)
library(tidyr)
library(GGally)
library(scales)
library(corrplot)
library(car)

model_data <- activity_joined %>%
  select(
    `active_aged 75+`,           # Outcome
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

model_data$RUC21NM <- relevel(model_data$RUC21NM, ref = "Urban: Nearer to a major town or city")

#----------------------------------------------------------------
# single model with individual variables
#----------------------------------------------------------------

# Apply linear regression
lin_regression <- lm(`active_aged 75+` ~ prop_75p + log_prop_dep_hh + log_prop_non_white +
                 log_dist_green + log_total_size_green +  prop_hh_comp_single + 
                prop_carer + RUC21NM,
               data = model_data)

summary(lin_regression)

#----------------------------------------------------------------
# 2 way model with interaction terms to for age and distance
#----------------------------------------------------------------

lin_reg_inter <- lm(`active_aged 75+` ~ 
                      prop_75p * log_total_size_green + 
                      prop_75p * log_dist_green + 
                      log_prop_dep_hh + 
                      log_prop_non_white +
                      RUC21NM, data = model_data)


summary(lin_reg_inter)

# Create a sequence of distances (log scale)
dist_seq <- seq(min(model_data$log_dist_green, na.rm = TRUE),
                max(model_data$log_dist_green, na.rm = TRUE),
                length.out = 100)

# Choose representative values of prop_75p: low, medium, high
prop_75_vals <- quantile(model_data$prop_75p, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)

# Create a new data frame for predictions
pred_df <- expand.grid(
  log_dist_green = dist_seq,
  prop_75p = prop_75_vals,
  log_prop_dep_hh = mean(model_data$log_prop_dep_hh, na.rm = TRUE),
  log_prop_non_white = mean(model_data$log_prop_non_white, na.rm = TRUE),
  log_total_size_green = mean(model_data$log_total_size_green, na.rm = TRUE),
  RUC21NM = "Urban: Nearer to a major town or city"  # choose a reference category
)

# Add predicted values
pred_df$pred_active <- predict(lin_reg_inter, newdata = pred_df)

# Convert prop_75p to a factor for plotting
pred_df$prop_75p_group <- factor(pred_df$prop_75p, labels = c("Low 75+", "Medium 75+", "High 75+"))

# Plot
ggplot(pred_df, aes(x = exp(log_dist_green), y = pred_active, color = prop_75p_group)) +
  geom_line(size = 1.2) +
  scale_x_continuous(name = "Distance to Nearest Green Space (m)", trans = "log10") +
  scale_y_continuous(name = "Predicted Active Aged 75+") +
  labs(color = "Older Adults Proportion") +
  theme_minimal() +
  ggtitle("Interaction: Prop. of Older Adults x Distance to Green Space")


#----------------------------------------------------------------
# 2 way model with more interaction terms to include rurality
#----------------------------------------------------------------

lin_reg_inter_rural <- lm(`active_aged 75+` ~ 
                      prop_75p * log_dist_green +      # 75+ adults and distance to green space
                      prop_75p * RUC21NM +             # 75+ adults and rural/urban
                      log_prop_dep_hh + 
                      log_prop_non_white + 
                      log_total_size_green + 
                      RUC21NM, 
                    data = model_data)

summary(lin_reg_inter_rural)

ggplot(model_data, aes(x = log_dist_green, y = `active_aged 75+`, color = cut(prop_75p, breaks = 3))) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE) +
  labs(color = "Proportion 75+", x = "Log Distance to Green Space", y = "Activity 75+") +
  theme_minimal()

#----------------------------------------------------------------
# Check over fitting
#----------------------------------------------------------------

set.seed(123)  # for reproducibility

# 70% train, 30% test
#create sequence number & select 70% randomly, split the dataset based on the row (test or train)
train_index <- sample(seq_len(nrow(model_data)), size = 0.7 * nrow(model_data)) 
train_data <- model_data[train_index, ]                                         
test_data  <- model_data[-train_index, ]

#perform linear regression on train data
lin_reg_train <- lm(`active_aged 75+` ~ 
                prop_75p * log_dist_green + #does dist to green space depend on proportion of 75+
                prop_75p * RUC21NM +        #does the effect of proportion of 75+ differ across rural/urban categories
                log_prop_dep_hh + 
                log_prop_non_white + 
                log_total_size_green + 
                RUC21NM,
               data = train_data)

summary(lin_reg_train)


# run results on test data to get predictions
pred_test_op <- predict(lin_reg_train, newdata = test_data)

#-----------------------------------------------------------
# Calculate metrics
library(Metrics)
rmse_value <- rmse(test_data$`active_aged 75+`, pred_test_op)
mae_value  <- mae(test_data$`active_aged 75+`, pred_test_op)
r2_value   <- cor(test_data$`active_aged 75+`, pred_test_op)^2

rmse_value; mae_value; r2_value

ggplot(data.frame(actual = test_data$`active_aged 75+`,
                  predicted = pred_test_op),
       aes(x = actual, y = predicted)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Actual Active 75+", y = "Predicted Active 75+",
       title = "Test Data: Model Performance") +
  theme_minimal()

par(mfrow=c(2,2))
plot(lin_reg_inter_rural)
# Check for:
#   
#   Non-constant variance (funnel shape)
# 
# Non-normal residuals
# 
# Outliers (large Cook’s distance)
#-----------------------------------------------------------

# Check how good predictions were (Residuals  - observed minus predicted)
res_test_op <- test_data$`active_aged 75+` - pred_test_op

# Check Root Mean Squared Error (RMSE)
rmse <- sqrt(mean(res_test_op^2, na.rm = TRUE))




# Check variance explained in model with R-squared on test set
sum_sq_total <- sum((test_data$`active_aged 75+` - mean(test_data$`active_aged 75+`))^2) #total variance
sum_sq_res   <- sum(res_test_op^2)   #residual variance
r_sq  <- 1 - sum_sq_res/sum_sq_total #prop variance explained (nearer to 1 is better)

rmse  #0.05391933 or 5.4% - model is off by about 5.4% on predicted activity levels compared to actual activity levels
r_sq  #0.2769768 or 27.7%
summary(lin_reg_train) #Multiple R-squared:  0.2864,	Adjusted R-squared:  0.286 (so 27.7% test v 28.6% train)

#plot actual v predicted
pred_test <- as.numeric(pred_test)

#Use grey and black for colour blindness.
#Added + or - 1 RMSE to show how close prediction is
act_pred_plot <- ggplot(data = test_data, aes(x = `active_aged 75+`, y = pred_test)) +
  geom_point(alpha = 0.2, color = "#A9A9A9") +       # scatter points
  geom_abline(intercept = 0, slope = 1, color = "#000000", linetype = "solid", size  = 1.5) +  #straight 45 degree line
  geom_abline(intercept = 0.054, slope = 1, color = "#000000", linetype = "dashed", size  = 1.0) +  # +1 RMSE
  geom_abline(intercept = -0.054, slope = 1, color = "#000000", linetype = "dashed", size  = 1.0) + # -1 RMSE
  scale_x_continuous(name = "Observed Active Aged 75+", labels = scales::percent_format(accuracy = 1)) +
  scale_y_continuous(name = "Predicted Active Aged 75+", labels = scales::percent_format(accuracy = 1)) + #show as %
  labs(
    x = "Observed Active Aged 75+",
    y = "Predicted Active Aged 75+",
    title = "Predicted v Observed Activity Levels (ages 75+)",
    subtitle = "Small area estimates: November 2022-23 ",
    caption = "Data source: Sport England Active Lives Survey"
  ) +
  theme_minimal()+
  theme(
    plot.caption = element_text(hjust = 0, face = "italic", size = 9, color = "black"),  # left align, subtle style
    plot.title = element_text(hjust = 0,  size = 14, color = "black"),
    plot.subtitle = element_text(hjust = 0, size = 11, color = "black") 
  )




#Check spatial correlation

lsoa_shapes <- st_read("path_to_your_file/lsoa_boundaries.shp")
👉 Make sure LSOA codes in your boundary file match your data’s LSOA codes.

📌 Step 2: Join the Spatial Boundaries to Your Model Data
Assume your data has a field called LSOA_code (update if named differently):
  
  r
Copy code
model_spatial <- lsoa_shapes %>%
  left_join(model_data, by = c("LSOA_code" = "LSOA_code"))
📌 Step 3: Fit Your Regression Model and Extract Residuals
You’ve already fitted this one 👇 (example)

r
Copy code
lin_reg_inter_rural <- lm(`active_aged 75+` ~ 
                            prop_75p * log_dist_green +      
                            prop_75p * RUC21NM +             
                            log_prop_dep_hh + 
                            log_prop_non_white + 
                            log_total_size_green + 
                            RUC21NM, 
                          data = train_data)

Add residuals to spatial data:
  
  r
Copy code
model_spatial$residuals <- residuals(lin_reg_inter_rural)
📌 Step 4: Create a Spatial Weights Matrix
We’ll use neighbouring polygons to define spatial relationships.

r
Copy code
library(spdep)

# Create neighbours list using shared boundaries (queen adjacency)
nb <- poly2nb(model_spatial)

# Convert to spatial weights
lw <- nb2listw(nb, style = "W")
📌 Step 5: Moran’s I Test for Spatial Autocorrelation
r
Copy code
moran_test <- moran.test(model_spatial$residuals, lw)
moran_test
🔎 Interpretation
Moran’s I result	Interpretation
Positive and significant (p < 0.05)	Residuals are spatially clustered ❌ — model violated spatial independence
Near 0, not significant	No spatial autocorrelation ✔️ — model OK
Negative	Residuals dispersed (rare)

📊 Optional: Moran Scatterplot
r
Copy code
moran.plot(model_spatial$residuals, lw)
🗺️ Optional: Map the Residuals
r
Copy code
library(tmap)

tm_shape(model_spatial) +
  tm_fill("residuals", palette = "RdBu", style = "quantile") +
  tm_borders() +
  tm_layout(title = "Model Residuals (Active 75+)")




