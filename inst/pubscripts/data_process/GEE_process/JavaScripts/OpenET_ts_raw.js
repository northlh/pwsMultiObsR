// Used for extracting data, mapping in the web browser, and saving to google drive

// Load Dataset

var dataset = 'OpenET/ENSEMBLE/CONUS/GRIDMET/MONTHLY/v2_0'
var start_date = '2013-01-01'
var end_date = '2013-12-01'
var band_name = 'et_ensemble_mad' //based on available data
var resolution = 30 //value in meters
var output_varname = "actet_mm"
var output_filename = "OpenET_EastRiv_mad_raw_20130101_20131201"

// Define the area of interest (AOI)
var aoi = ee.Geometry.Polygon(
        [[[-107.2, 38.6],
          [-107.2, 39.1],
          [-106.7, 39.1],
          [-106.7, 38.6]]]);

// Select the dataset (e.g., OpenET)
var rasterCollection = ee.ImageCollection(dataset)
  .filterDate(start_date, end_date)
  .filterBounds(aoi)  // Restrict to your area of interest
  .select(band_name);  // Select the specific band (e.g., ET)


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
