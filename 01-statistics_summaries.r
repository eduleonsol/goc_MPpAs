library(tidyverse)
library(sf)
library(terra)
library(mapview)

# Load data -------------------------------
com <- st_read("data/selected_communities/proposes_communities.shp")

# List all raster files in data/raster/ and subfolders (including .tif and .tiff)
raster_files <- list.files("data/raster/", pattern = "\\.(tif|tiff)$", recursive = TRUE, full.names = TRUE)

# Load the reference raster
ref_raster <- rast("data/raster/socioeconomic/coastal_population_influence/coastal_population_adj.tif")

# Function to load, reproject, and resample rasters to match reference
prep_raster <- function(f, ref) {
  r <- rast(f)
  # Reproject if needed
  if (!compareGeom(r, ref, stopOnError = FALSE)) {
    r <- project(r, ref)
  }
  # Resample if needed
  if (!all(dim(r) == dim(ref))) {
    r <- resample(r, ref)
  }
  return(r)
}

# Prepare all rasters
rasters <- lapply(raster_files, prep_raster, ref = ref_raster)

# Stack all rasters
raster_stack <- rast(rasters)

# Optional: name the layers after their filenames
names(raster_stack) <- basename(raster_files)

# Check the stack
raster_stack

# Keep only the ID column from com
com_id <- com|>
        select(ID)

# Prepare a variable to indicate the source folder of each raster
folder_type <- ifelse(grepl("biophysical", raster_files), "biophysical", 
                      ifelse(grepl("socioeconomic", raster_files), "socioeconomic", NA))

# Extract raster values for each polygon (mean by default)
extracted_values <- terra::extract(raster_stack, com_id, fun = mean, na.rm = TRUE, ID = FALSE)

# Add the ID column
extracted_values$ID <- com_id$ID

# Convert to long format for easier handling of the qualitative variable

extracted_long <- pivot_longer(extracted_values, 
                               cols = -ID, 
                               names_to = "raster_name", 
                               values_to = "value")

# Add the qualitative variable
extracted_long$type <- ifelse(grepl("biophysical", extracted_long$raster_name), "biophysical", 
                              ifelse(grepl("socioeconomic", extracted_long$raster_name), "socioeconomic", NA))

# If you want to use the folder_type vector directly, you can also join it by raster_name
# extracted_long$type <- folder_type[match(extracted_long$raster_name, basename(raster_files))]
view(extracted_long)
# Preview result


mun_id <- com|>
        as.data.frame()|>
        select(ID,  comunidad )|>
        distinct()


extracted<- left_join(extracted_long, mun_id, by = c("ID"))

view(extracted)


#Critical habitats --------------

# List all shapefiles in the critical habitats folder
crit_hab_files <- list.files("data/shp/biophysical/critical_habitats/", pattern = "\\.shp$", full.names = TRUE)
# Load, tidy, and combine all critical habitat shapefiles

sf_use_s2(FALSE)  
critical_habitat_list <- lapply(crit_hab_files, function(shp) {
  hab_name <- tools::file_path_sans_ext(basename(shp))
  hab <- sf::st_read(shp, quiet = TRUE) |> st_make_valid()
  # Union all geometries into one
  hab_union <- hab %>%
    st_union() %>%
    st_sf() %>%
    mutate(habitat = hab_name)|>
    st_transform(crs=4326)
  # Calculate area in hectares
  hab_union$area_ha <- as.numeric(st_area(hab_union)) / 10000
  return(hab_union)
})
# 
# Bind all habitats into a single object
critical_habitat <- do.call(rbind, critical_habitat_list)

# Preview
critical_habitat



# Intersect critical habitats with communities
library(dplyr)

crit_by_comm <- st_intersection(com, critical_habitat)

# Calculate area of intersection (in hectares)
crit_by_comm <- crit_by_comm %>%
  mutate(area_ha = as.numeric(st_area(.)) / 10000)

# Group by comunidad and habitat, merge polygons, and sum area
comm_habitat <- crit_by_comm %>%
  group_by(comunidad, habitat) %>%
  summarise(geometry = st_union(geometry), area_ha = sum(area_ha, na.rm = TRUE), .groups = "drop")



# Join and calculate relative_value per comunidad and habitat
comm_habitat <- comm_habitat %>%
  left_join(critical_habitat |> as.data.frame() |> rename(total_area=area_ha)|> select(habitat, total_area), by = "habitat") %>%
  mutate(relative_value = (area_ha / total_area)*100 ) %>% 
        as.data.frame() %>% 
        select(-geometry)

writexl::write_xlsx(comm_habitat, "data/critical_habitats_summary_selected_communities.xlsx")



# Socioeconomic variables --------------------
## Gridded variables ------


soc.gridded <- st_read("../30x30_mx_eez/30x30_data/coastal_grid_matrix_V4_2025-01-31.gpkg") %>% 
        as.data.frame() %>% 
        select(ID,
                social_deficiencies=SD,
               coastal_pop=CP,
               diving_popularity=DT,
               fishing_hours=FH,
               marginalization= SM,
               coastal_infrastructure=CI,
               mean_GDP_PPP)


com.soc.grid <- left_join(com, soc.gridded) %>% 
        as.data.frame() %>% 
        select(-geometry)




#Summary statistics----------
matrix <- left_join(com, extracted_values) %>% 
        left_join(com.soc.grid) %>% 
        as.data.frame() %>% 
        select(-geometry)



summary.statistics <- matrix %>% 
        select(ID, comunidad, 8:22) %>% 
        pivot_longer(cols = (3:17), names_to = "variable",
                     values_to = "value") %>%
        group_by(comunidad,variable) %>%
        summarise(
              mean=mean(value, na.rm=T),
                     median=median(value, na.rm=T),
                     sd=sd(value, na.rm=T),
                     min=min(value,na.rm=T),
                     max=max(value, na.rm = T)
        ) %>% 
        mutate(variable=recode(variable,
                               "MHWs_frequency.tiff"="Marine Heatwaves Events",
                             "MHWs_slope.tiff"=   "Marine Heatwaves Slope",
                             "Mean carbon_stock.tif"=  "Mean Carbon Stock" ,
                              "coastal_infrastructure" = "Coastal Infrastructure",
                               "coastal_pop"="Coastal Population Influence",
                               "coastal_population_adj.tif"="Coastal Population Influence (Scaled 0-1)",
                             "diving_popularity"=  "Diving Popularity",
                              "fishing_hours_mean_2018_2021.tiff"=  "Average Fishing Hours (2018-2021)",
                             "fishing_hours_sum_2018_2021.tiff"=  "Fishing Hours Sum (2018-2021)",
                              "marginalization"= "Marginalization",
                               "mean_GDP_PPP"="Mean GDP (PPP)",
                               "rezago_educativo2020_adj.tif"="Educational Lag (2020)",
                               "social_deficiencies"="Social Deficiencies (2020)",
                               "sp_richness.tif"="Species Richness")) %>% 
        filter(!is.na(mean))


writexl::write_xlsx(summary.statistics, "data/summary_statistics_selected_communities.xlsx") 
