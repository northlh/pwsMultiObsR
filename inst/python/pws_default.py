# Author: Lucas North
# Last modified: April 10, 2024
# Description: # Test script, copying code from prms_legacy_eastriver02.ipynb

# Notes ---------------------------------------------------------------------
# the single # is indicative of a comment
### the triple pound sign is indicative of code in the example inpyb



# Cell 1 --------------------------------------------------------------------
# LNORTH - this sets up packages/dependencies
import pathlib as pl
from platform import processor
from pprint import pprint
from shutil import rmtree
from sys import platform
import warnings

import pydoc

import hvplot.pandas  # noqa
import numpy as np
import pywatershed as pws
import xarray as xr

# LNORTH - this calls the "helpers" folder that is in the examples folder
# LNORTH - in this instance "gis_files" is a .py file, which unlike the
###from helpers import gis_files
###gis_files.download()  # this downloads GIS files



# Cell 2 --------------------------------------------------------------------
# This calls the OG cbh and param/control files
#domain_dir = pws.constants.__pywatershed_root__ / "data/SA_test_input"
domain_dir = pl.Path("./default_input")

# LNORTH - this creates a new folder within the working directory of this notebook
###nb_output_dir = pl.Path("./02_prms_legacy_models")
#nb_output_dir = pl.Path("./SA_test_output")
nb_output_dir = pl.Path("./default_output")



# Cell 3 --------------------------------------------------------------------
# LNORTH - this creates a new folder within the output directory specified above
###cbh_nc_dir = nb_output_dir / "drb_2yr_cbh_files"

# LNORTH - Creates folder for climate .nc files
cbh_nc_dir = nb_output_dir / "cbh_files"
#cbh_nc_dir = nb_output_dir / "east_cbh_files"
if cbh_nc_dir.exists():
    rmtree(cbh_nc_dir)
cbh_nc_dir.mkdir(parents=True)

# LNORTH - the humidity file is not specified in the example, and the name for precip is different
# LNORTH - change the file suffix from .day to .cbh
# LNORTH - "precip.day" renamed to "prcp.cbh"

# LNORTH - commented out in parallel
cbh_files = [
    domain_dir / "prcp.cbh",
    domain_dir / "tmax.cbh",
    domain_dir / "tmin.cbh",
]

# LNORTH - this line specifies the parameter file
params = pws.parameters.PrmsParameters.load(domain_dir / "myparam.param")

# LNORTH - commented out in parallel
for cbh_file in cbh_files:
    out_file = cbh_nc_dir / cbh_file.with_suffix(".nc").name
    pws.utils.cbh_file_to_netcdf(cbh_file, params, out_file)



# Cell 4 --------------------------------------------------------------------
# LNORTH - Load processes
nhm_processes = [
    pws.PRMSSolarGeometry,
    pws.PRMSAtmosphere,
    pws.PRMSCanopy,
    pws.PRMSSnow,
    pws.PRMSRunoff,
    pws.PRMSSoilzone,
    pws.PRMSGroundwater,
    pws.PRMSChannel,
]

submodel_processes = [pws.PRMSSoilzone, pws.PRMSGroundwater, pws.PRMSChannel]



# Cell 5 --------------------------------------------------------------------
# LNORTH - load control file

###with warnings.catch_warnings():
###    warnings.simplefilter("ignore")
###    control = pws.Control.load_prms(domain_dir / "control_default.control")

# LNORTH - as of version 1.1
control = pws.Control.load_prms(domain_dir / "control_default.control", warn_unused_options=False)



# Cell 6 --------------------------------------------------------------------
# LNORTH - if the end time is modified and is before the start time in the control file, it causes an error

control.options["netcdf_output_var_names"] += ["infil_hru", "sroff_vol"]
###control.edit_end_time(np.datetime64("2010-09-30T00:00:00"))
run_dir = nb_output_dir / "nhm"
if run_dir.exists():
    rmtree(run_dir)
control.options = control.options | {
    "input_dir": cbh_nc_dir,
    "budget_type": "warn",
    "calc_method": "numba",
    "netcdf_output_dir": run_dir,
}



# Cell 7 --------------------------------------------------------------------
# LNORTH - this threw an error because the precip file is naned "precip.cbh" in the 4th chunk
# LNORTH - the control file is looking for a file named "prcp.cbh" - this was the name in the nhm control and data

nhm = pws.Model(
    nhm_processes,
    control=control,
    parameters=params,
)



# Cell 8 --------------------------------------------------------------------
###%%time
nhm.run(finalize=True)


# END --------------------------------------------------------------------
