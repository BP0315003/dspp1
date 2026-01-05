# BP0315003 Data Science
## Data Science Professional Practice 1
September 2025 term - Year 1 - Term 3

<!-- ## Education -->

<!-- ## Work Experience -->


## Public Project
### Active lives and falls in older adults
An anlysis of area level factors influencing activity levels in older adults with a view to implementing targeted interventions to prevent falls.

#### Summary:
Results show older adults’ lower activity is spatially clustered in deprived, urban LSOAs and in areas with higher proportions of carers and single-occupant households.
Rural areas have different challenges with limited access to public green space, abundant green space being mostly private.  Geographically and culturally targeted interventions are needed, support for carers and access to rural facilities

  - The full project report can be located in the Project report folder

#### Data sources:
  - Sport England - Active Lives, Adults 22/23
  - Census 2021 - Custom datasets
  - ONS - datasets and shape files

### Active lives
## Executive Summary 
Older people are the most sedentary segment of the population, just over half meet physical activity recommendations (Gawler et al., 2016).  Studies show inactive people fall more than moderately or very active people; rates increase for individuals aged 75+.  Injuries sustained “reduce mobility and independence and increase the risk of premature death” (Skelton and Todd, 2004).  Age UK (Fall prevention for the elderly, 2024) advise being active can maintain strength, balance and coordination, preventing falls.
Studies have examined intrinsic risk factors like medications and medical conditions.  This analysis examines factors influencing activity levels in older adults at a Lower Super Output Area (LSOA), focusing on demographics, deprivation, ethnicity, household composition, and green space access.  
Using linear regression on public LSOA data, the goal is to identify areas where targeted interventions can increase activity, improve health outcomes, and ensure equitable access for underserved populations.
# Findings:
Higher proportions of older adults slightly increase predicted activity, but this effect reduces with increased green space distance. 
Deprivation lowers activity, especially in rural areas, indicating need for targeted interventions.  Rural areas with higher non-white populations indicate lower activity; urban areas are less affected, possibly due to better access.
Higher proportions of single occupant households generally lower predicted activity.  Urban areas indicate lower activity, whereas some rural areas have higher levels, likely due to larger gardens or more distant facilities.
Longer distances to green spaces reduce activity; closer green spaces are important for older adults in urban areas.  Carers have the strongest negative impact on activity levels, reflecting constraints and health challenges.
The model performs and generalises well, with an adjusted R² of 42% - acceptable for social and demographic models.  Root mean squared error (RMSE) of 0.048 (train and test) indicates predictions are within 5% and mean absolute error (MAE) at 3.8 percentage points suggests no large outliers.  ‘Moran’s I’ confirms spatial autocorrelation warranting further development to include geographically weighted regression (GWR).
# Conclusion
Despite spatial autocorrelation limits, the model supports established ideas. It can inform tailored interventions addressing urban and rural uniqueness, influencing activity and fall risk in older adults. LSOA data reflects average characteristics, it is not appropriate to infer all individuals are the same.

## Data Infrastructure and Tools 
Public tools for standalone, reproducible pipelines were chosen.
R is preferred, using base regression functions, and libraries like ‘readcsv’ for data ingestion, ‘dplyr’ for data cleaning/manipulation, ‘ggplot2’ for exploratory data analysis (EDA) visualisation, and ‘Corrplot’ for correlations. These are easily installable and well-supported.  
Python offers equivalent libraries (pandas, numpy, seaborn, matplotlib, scikit-learn) for the same tasks. Both are free; Python performs better with ‘big data’ (BasuMallick, 2022), but R suffices for this smaller dataset.  Commercial visual pipeline and modelling tools like Alteryx, add unnecessary cost.
Power BI (PBI) displays LSOA activity data against England benchmarks.  Multiple years’ data were manually combined into a single excel file.  Copilot's accuracy in this task to save time could be explored to process futures published files.  
As Bristol, North Somerset and South Gloucestershire (BNSSG) ICB (Integrated Care Board) will use this data, it was loaded into SQL server for PBI scheduling, although direct import from Excel is possible.  Power query ‘unpivots’ wide datasets into a long format as shown in figure P1, simplifying visualisation with minimal slicers and measures.  

### Figure p1
