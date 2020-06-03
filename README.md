# APWS Data Portal
[![DOI](https://zenodo.org/badge/210858212.svg)](https://zenodo.org/badge/latestdoi/210858212)

This repository contains the code for the [Asia Pacific Weather Statistics (APWS) application](https://hydra-water.shinyapps.io/APWS/).

APWS is a web-based Data as a Service (DaaS) application. The statistics data embedded within APWS is provided for use with SWAT (https://swat.tamu.edu) hydrologic models and / or the WXGN weather generator, for generating synthetic weather time-series for any location (or multiple locations) in the Asia Pacific region. The APWS application provides weather statistics at a 0.25 degree spatial resolution, and allows users to select and download weather statistics (ready for use with the SWAT model) for any area within the Asia Pacific region. The area is selected using either i) a map-based interactive polygon drawing tool or ii) user input of a shape file. 

## Technical Information
APWS is developed using the [R Shiny](https://shiny.rstudio.com/) web framework. Important information regarding the application code is as follows:

* *app.R* - Contains the R code for the entire application. One of the required packages, i.e., geoshaper, is not available on CRAN. Please refer to https://github.com/RedOakStrategic/geoshaper for installing geoshaper.
* *Final_WGN.csv* - Contains the dataset prepared for dissemination using this application (see Scientific Information for details).

APWS is currently deployed on [shinyapps.io](https://shinyapps.io/).

## Scientific Information
Scientific details regarding the underlying APWS dataset are available in our [discussion paper](https://www.earth-syst-sci-data-discuss.net/essd-2019-178/) published in Environmental System Science Data (ESSD).   

## Authors
Uttam Ghimire, Dr. Taimoor Akhtar, Dr. Narayan Shrestha and Dr. Prasad Daggupati, UNIVERSITY OF GUELPH.

The shiny application is developed by Dr. Taimoor Akhtar.

Please Contact Us at: pdaggupa@uoguelph.ca for any questions.

## Citing Us
If you use our data, application or codes, please cite this repository (zenodo DOI is provided above) and the following paper:
https://www.earth-syst-sci-data-discuss.net/essd-2019-178/

