// Used for extracting data, mapping in the web browser, and saving to google drive

// Establish HRUs shapefile (must have valid geometry on WGS 84)
// Add new asset
var hruCollection = ee.FeatureCollection("projects/ee-lhnorth01/assets/TaylorRiv_hru_4326")

// Visualize geometry
Map.centerObject(hruCollection)
Map.addLayer(hruCollection, {color: 'blue'}, 'HRUs')

// Load Dataset

var dataset = 'NASA/SMAP/SPL4SMGP/007'
var start_date = '2015-03-31'
var end_date = '2024-10-01'
var band_name = 'sm_surface_wetness' //based on available data
var resolution = 11000 //value in meters
//var output_varname = "ratio"
var output_filename = "SPL4SMGP_TaylorRiv_surfwt_20150331_20240930"

var rasterCollection = ee.ImageCollection(dataset)
  .filterDate(start_date, end_date)
  .filterBounds(hruCollection) //neccessary
  .select(band_name); //select band of interest

// Define function to calculate mean for each polygon per image  
var calculateMeanForImage = function(image){
  var meanByPolygon = image.reduceRegions({
    collection: hruCollection, //length by geometry (hrus), cols by field (id's)
    reducer: ee.Reducer.mean().setOutputs([band_name]),
    scale: resolution //in meters of dataset
  });
  // Add the image date as a property
  meanByPolygon = meanByPolygon.map(function(feature){
    return feature.set('Date', image.date().format('YYYY-MM-dd HH:mm:ss'));
  });
  
  return meanByPolygon
}; // close fn

// Map the function over the image collection
var timeseriesByPolygon = rasterCollection.map(calculateMeanForImage).flatten();

// Print a sample to check if the mean_value is present
print(timeseriesByPolygon.first());

// Allows removal of index and .geo columns, reduces file size
var colnames = ["Date","model_hru_", band_name]

Export.table.toDrive({
  collection: timeseriesByPolygon,
  description: output_filename,
  selectors: [colnames],
  fileFormat: 'CSV'
});