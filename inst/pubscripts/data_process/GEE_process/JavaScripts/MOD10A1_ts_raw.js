// Used for extracting data, mapping in the web browser, and saving to google drive

// Load Dataset

var dataset = 'MODIS/061/MOD10A1'
var start_date = '2012-10-01'
var end_date = '2024-10-01'
var band_name = 'NDSI_Snow_Cover' //based on available data
var resolution = 500 //value in meters
var output_varname = "NDSI"
var output_filename = "MOD10A1061_TaylorRiv_NDSI_raw_20121001_20240930"

// Define the area of interest (AOI)
var aoi = ee.Geometry.Polygon(
        [[[-106.85, 38.8],
          [-106.85, 39.1],
          [-106.4, 39.1],
          [-106.4, 38.8]]]);

// Select the dataset (e.g., OpenET)
var rasterCollection = ee.ImageCollection(dataset)
  .filterDate(start_date, end_date)
  .filterBounds(aoi)  // Restrict to your area of interest
  .select(band_name);  // Select the specific band (e.g., ET)


// Example for checking pixel count over the AOI (for the first image in the set)
var pixelCount = rasterCollection.first().reduceRegion({
  reducer: ee.Reducer.count(),
  geometry: aoi,
  scale: resolution,  // Match the scale used in your export
  maxPixels: 1e13  // Set a high limit to avoid truncation
});

print('Pixel count:', pixelCount.get(band_name));


// Combine the collection into a single image (multi-band)
var rasterStack = rasterCollection.toBands();  // Each image in the collection becomes a band




// Export the multi-band image as a single GeoTIFF
Export.image.toDrive({
  image: rasterStack,
  description: output_filename,
  //folder: 'GEE_Exports',  // Optional folder name in Google Drive
  scale: resolution,  // Set the desired scale (e.g., 1000 meters per pixel)
  region: aoi,  // Define the region to export
  fileFormat: 'GeoTIFF',
  maxPixels: 1e8  // Set the maximum number of pixels
});
