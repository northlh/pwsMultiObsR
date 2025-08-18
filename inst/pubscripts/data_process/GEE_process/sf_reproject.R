


# # Filename in
# hru_filename <- "model_nhru.shp"
hru_filename <- "/Users/lnorth/Desktop/pywatershed-SA-UA-main/example_nhm_DoloresRiv/GIS/model_nhru.shp"
#hru_filename <- "/Users/lnorth/Desktop/pywatershed-SA-UA-main/EastRiv_example_GIS/model_nhru.shp"

# # WGS84
target_crs_sf <- st_crs(4326)  # For sf objects
#target_crs_sf <- st_crs(32613)
#target_crs_sf <- st_crs(5070)
# target_crs_terra <- "EPSG:4326"  # For terra objects, specified as a string
# # Define the MODIS sinusoidal projection (SR-ORG:6974) using PROJ.4 string
#target_crs_sf <- "+proj=sinu +R=6371007.181 +nadgrids=@null +wktext"

# # Read the shapefile using sf (ensure it's in the same CRS)
#hru_sf <- st_read(file.path(gis_dir,hru_filename))
hru_sf <- st_read(hru_filename)


# Plot
plot(hru_sf)
plot(hru_sf["model_hru_"]) #or [[2]]
hru_sf[[1]] <- NULL

# Check for invalid geometries and make valid
invalid_geometries <- hru_sf[!st_is_valid(hru_sf),]
if (nrow(invalid_geometries) > 0) {
  hru_sf <- st_make_valid(hru_sf)
}

# # Special for dolores to reorder them?
n <- nrow(hru_sf)
hru_sf<- hru_sf[c((n-2):n, 1:(n-3)), ]
rownames(hru_sf) <-NULL
# hru_sf <- hru_sf[c(12,13),]
hru_sf <- hru_sf[12,]
plot(hru_sf)

# Apply a negative buffer (shrink polygons)
hru_sf <- st_buffer(hru_sf, dist = -100)


# # Find intersections
# intersections <- st_intersects(hru_sf)
# 
# # Check if any polygons intersect with others
# intersecting_polygons <- which(sapply(intersections, length) > 1)
# 
# if (length(intersecting_polygons) > 0) {
#   cat("Polygons with intersections:", intersecting_polygons, "\n")
# } else {
#   cat("No intersecting polygons found.\n")
# }


hru_sf <- st_simplify(hru_sf, dTolerance = 100) # Tolerance in CRS units (e.g., meters for WGS 84)
plot(hru_sf)


# Transform the CRS of the shapefile to match WGS
hru_target_sf <- st_transform(hru_sf, crs = target_crs_sf)

# Write new shapefile
output_path <- "/Users/lnorth/Desktop/RA/R/GEE_workflow/st_transform_sf/DoloresRiv/Dolores_hru_4326_6.shp"
st_write(hru_target_sf, output_path)
