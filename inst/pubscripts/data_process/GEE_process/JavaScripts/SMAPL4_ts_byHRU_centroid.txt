// Used for extracting data, mapping in the web browser, and saving to google drive

// Establish HRUs shapefile (must have valid geometry on WGS 84)
// Add new asset
var hruCollection = ee.FeatureCollection("projects/ee-lhnorth01/assets/DoloresRiv_hru_4326")

// Visualize geometry
Map.centerObject(hruCollection)
Map.addLayer(hruCollection, {color: 'blue'}, 'HRUs')

// Load Dataset

var dataset = 'NASA/SMAP/SPL4SMGP/007'
var start_date = '2015-03-31'
var end_date = '2024-10-01'
var band_name = 'sm_surface_wetness' //based on available data
var resolution = 11000 //value in meters
var output_varname = "ratio"
var output_filename = "SPL4SMGP_DoloresRiv_surfwt_20150331_20240930_centroid"

var rasterCollection = ee.ImageCollection(dataset)
  .filterDate(start_date, end_date)
  .filterBounds(hruCollection) //neccessary
  .select(band_name); //select band of interest



////// function to clip the entire ImageCollection to the field boundary //////
//function clipImgCollect(img) {
//  return img.clip(hruCollection);
//}

//// clip S2 ImageCollection to poly bounds
//var dataset_clip = rasterCollection.map(clipImgCollect);
  
  
//var smSurfaceVis = {
//  min: 0.0,
//  max: 0.9,
//  palette: ['0300ff', '418504', 'efff07', 'efff07', 'ff0303'],
//};
//Map.addLayer(dataset_clip, smSurfaceVis, 'SM Surface');  



// Define function to extract the pixel value at each HRU centroid (for HRU 2 and 4)

var extractPixelValue = function(image){
  var centroids = hruCollection.map(function(feature){
    return feature.setGeometry(feature.geometry().centroid());
  });
  
  // Sample pixel value at each centroid
  var pixelValues = image.sampleRegions({
    collection: centroids,
    scale: resolution,
    projection: image.projection()
  });
  
  // add date
  return pixelValues.map(function(feature){
    return feature.set({
      'Date': image.date().format('YYYY-MM-dd HH:mm:ss')
    });
  });
};


// Apply the function over the image collection and flatten the results
var timeseriesByPolygon = rasterCollection.map(extractPixelValue).flatten();


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
