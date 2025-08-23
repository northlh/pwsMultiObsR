// Used for extracting data, mapping in the web browser, and saving to google drive

// Establish HRUs shapefile (must have valid geometry on WGS 84)
// Add new asset
var hruCollection = ee.FeatureCollection("projects/ee-lhnorth01/assets/TaylorRiv_hru_4326")

// Visualize geometry
Map.centerObject(hruCollection)
Map.addLayer(hruCollection, {color: 'blue'}, 'HRUs')

// Load Dataset

var start_date = '2013-01-01'
var end_date = '2023-12-01'
var band_name = 'et_ensemble_mad' //based on available data
var resolution = 30 //value in meters
var output_varname = "actET_monthly_mm"
var output_filename = "OpenET_TaylorRiv_mad_20130101_20231201"

var rasterCollection = ee.ImageCollection('OpenET/ENSEMBLE/CONUS/GRIDMET/MONTHLY/v2_0')
  .filterDate(start_date, end_date)
  .filterBounds(hruCollection) //neccessary
  .select(band_name); //select band of interest
  
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

//// Function to remove geometry by setting it to null
// This reduces the output file size by approximately 100 fold
//var removeGeometry = function(feature) {
//  return ee.Feature(null).copyProperties(feature);
//};
//// Apply the function to each feature in the collection
//var timeseriesByPolygon = timeseriesByPolygon.map(removeGeometry);


// Print a sample to check if the mean value is present
print(timeseriesByPolygon.first());

// Allows removal of index and .geo columns, reduces file size
var colnames = ["Date","model_hru_", output_varname]


Export.table.toDrive({
  collection: timeseriesByPolygon,
  description: output_filename,
  selectors: [colnames],
  fileFormat: 'CSV'
});
