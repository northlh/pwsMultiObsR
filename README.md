# pwsMultiObsR

An R package for sensitivity and uncertainty analysis of the USGS [pywatershed](https://github.com/EC-USGS/pywatershed) python hydrologic model.

## Getting Started

1. Install python and the pywatershed package. See the [pywatershed github page](https://github.com/EC-USGS/pywatershed) for detailed instructions on this step.

2. Install [RStudio](https://posit.co/downloads/)

3. In a R script or RStudio console execute: `devtools::install_github("northlh/pwsMultiObsR", build_vignettes = TRUE`)`

4. View the vignettes by executing: `utils::browseVignettes("pwsMultiObsR")`

5. Start by running or viewing the example workbooks (.Rmd). They may also be viewed in the [github]() web interface.

## About

The package was designed to carry out multi-observation model diagnostics of the USGS pywatershed model. The compatible observations with this package are:

- USGS daily mean [streamflow](https://cran.r-project.org/web/packages/dataRetrieval/index.html) (via dataRetrieval)
- SNOTEL [snow water equivalent](https://wcc.sc.egov.usda.gov/reportGenerator/) (via https requests)
- SNOTEL [soil moisture](https://wcc.sc.egov.usda.gov/reportGenerator/) (via https requests)
- Remotely sensed snow water equivalent from [ASO](https://www.airbornesnowobservatories.com/) (via manual downloads)
- Remotely sensed snow-covered area from [MODIS](https://developers.google.com/earth-engine/datasets/catalog/MODIS_061_MOD10A1) (via Google Earth Engine)
- Remotely sensed soil moisture from [SMAP](https://developers.google.com/earth-engine/datasets/catalog/NASA_SMAP_SPL4SMGP_008) (via Google Earth Engine)
- Remotely sensed evapotranspiration from [OpenET](https://developers.google.com/earth-engine/datasets/catalog/OpenET_ENSEMBLE_CONUS_GRIDMET_MONTHLY_v2_0) (via Google Earth Engine)

The data retrieval supported by this package are USGS streamflow and SNOTEL snow water equivalent and soil moisture. For data accessed via Google Earth Engine, example workflows in JavaScript are provided in the inst/data_process folder. Example input data such as default parameter files, control files, and GIS geometry is included in this package - these are neccessary to run pywatershed,

For the sensitivity analysis of pywatershed, the authors selected the Morris Elementary Effects Test, a computationally efficient screening method for large sets of potential parameters. Once the sensitive parameters have been identified, the user may create high density, space-filled parameter spaces via MaxiMin Latin Hypercube Sampling. From there, the user may apply their preferred filtering or uncertainty quantification methods for parameter estimation or model output.

## Citation

The journal article pertaining to these methods is in preparation as of August 2025.

## Disclaimer

This software is preliminary and subject to revision. The author makes no guarantee of backwards compatibility upon revisions. The author is not responsible for the usage or results generated from this software. 

