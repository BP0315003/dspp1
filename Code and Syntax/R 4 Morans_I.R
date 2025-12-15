library(sf)
library(spdep)


#----------------------------------------------------------------------------------
# Morans I - Are activity levels of older adults spatially correlated across LSOAs?
#----------------------------------------------------------------------------------

# Keep only England LSOAs from the shape file (codes starting with 'E') 
# only those in the training dataframe

lsoa_shape_filtered <- lsoa_shape_england %>%
  filter(LSOA21CD %in% train_data$lsoa_code)    # LSOAs in training set

# Create residuals dataframe from training data
ind_residuals_df <- data.frame(
  LSOA21CD = train_data$lsoa_code,            # LSOA codes from training data
  ind_residuals = residuals(individual_effects_train)
)

# Join residuals to shapefile - has only residuals
model_data_sf <- lsoa_shape_filtered %>%
  left_join(ind_residuals_df, by = "LSOA21CD")

# Create neighbours based on contiguity (queen) - 
# queen contiguity rule for sharing borders or corners

neighbours <- poly2nb(lsoa_shape_filtered, queen = TRUE, snap = 500) 
#(snap is changeable used 500m - approx 1/3 of a mile, see check section below)

# Create spatial weights matrix
lw <- nb2listw(neighbours, style = "W", zero.policy = TRUE) #Ignore LSOAs with no neighbours


# Moran's I
moran_test <- moran.test(model_data_sf$ind_residuals, lw, zero.policy = TRUE) #Ignore LSOAs with no neighbours

# View results
moran_test

sum(is.na(model_data_sf$ind_residuals))



#-------------------------------------------------------
# Check for LSOAs without neighbours - lots!
# Common sense suggests 500m (1/3 mile) is somewhere between
# LSOAs in urban areas being small and large in rural areas
# won't cover all of them but need to be pragmatic.
#-------------------------------------------------------


# Find LSOAs with no neighbours as per warning (island LSOAs) - 137 LSOA affected
# would snap distance help to connect isolated lsoas?

no_neighbors <- which(card(neighbours) == 0)  # card() gives number of neighbours
length(no_neighbors)  # How many LSOAs have zero neighbours
lsoa_shape_filtered$LSOA21CD[no_neighbors]  # IDs of those LSOAs

# Convert to sf if not already
lsoa_sf <- st_as_sf(lsoa_shape_filtered)

# Compute distance matrix (Euclidean distance between polygon centroids)
centroids <- st_centroid(lsoa_sf)
dist_matrix <- st_distance(centroids)

# Minimum distance to another polygon for each LSOA
min_dist <- apply(dist_matrix, 1, function(x) min(x[x > 0]))  # ignore self-distance
summary(min_dist)  # Gives min, median, max distances

library(ggplot2)

# Which LSOAs still have zero neighbours
# VERY difficult to see red LSOAs
no_neigh <- which(card(neighbours) == 0)

ggplot(lsoa_shape_filtered) +
  geom_sf(fill = "lightblue") +
  geom_sf(data = lsoa_shape_filtered[no_neigh, ], fill = "red") +
  ggtitle("LSOAs with zero neighbours (red)") +
  theme_minimal()



