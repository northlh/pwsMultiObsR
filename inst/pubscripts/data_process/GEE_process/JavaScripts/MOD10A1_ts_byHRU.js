// Used for extracting data, mapping in the web browser, and saving to google drive

// Establish HRUs shapefile (must have valid geometry on WGS 84)
// Add new asset
var hruCollection = ee.FeatureCollection("projects/ee-lhnorth01/assets/BlueRiv_hru_4326")

// Visualize geometry
Map.centerObject(hruCollection)
Map.addLayer(hruCollection, {color: 'blue'}, 'HRUs')

// Load Dataset

var dataset = 'MODIS/061/MOD10A1'
var start_date = '2000-02-24'
var end_date = '2024-11-05'
var band_name = 'NDSI_Snow_Cover' //based on available data
var resolution = 500 //value in meters
var output_varname = "NDSI"
var output_filename = "MOD10A1061_BlueRiv_NDSI_20000224_20241106"

var rasterCollection = ee.ImageCollection(dataset)
  .filterDate(start_date, end_date)
  .filterBounds(hruCollection) //neccessary, auto-transforms
  .select(band_name); //select band of interest

// Check the projection of the raster dataset
var rasterProjection = rasterCollection.first().projection();
print('Raster CRS:', rasterProjection);

// Reproject the shapefile to match the raster CRS
var hruCollection = hruCollection.map(function(feature) {
  return feature.transform(rasterProjection, resolution);  // raster resolution
});


// Define function to calculate mean for each polygon per image  
var calculateMeanForImage = function(image){
  var meanByPolygon = image.reduceRegions({
    collection: hruCollection, //length by geometry (hrus), cols by field (id's)
    reducer: ee.Reducer.mean().setOutputs([output_varname]),
    scale: resolution //in meters of dataset
  });
  // Add the image date as a property
  meanByPolygon = meanByPolygon.map(function(feature){
    return feature.set('Date', image.date().format('YYYY-MM-dd'));
  });
  
  return meanByPolygon
}; // close fn

// Map the function over the image collection
var timeseriesByPolygon = rasterCollection.map(calculateMeanForImage).flatten();

// Print a sample to check if the mean_value is present
print(timeseriesByPolygon.first());

Export.table.toDrive({
  collection: timeseriesByPolygon,
  description: output_filename,
  fileFormat: 'CSV'
});