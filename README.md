# The National Education Data Mapper
**Author: Joshua Eagan**<br/>
**Date: 10/20/2021**

## About This Project

This Shiny app allows users to easily make custom Leaflet style maps to explore education data. It is designed with users at state education agencies and academic institutions in mind. All code to make this app is free and open source, so anyone can fork the code to make changes, host this app locally, or run their own instances publicly.

The code for this app was written by Joshua Eagan as a part of the Missouri Data Fellows Program, a collaboration between University of Missouri's Economics Department and Missouri's Department of Elementary and Secondary Education (DESE). This Shiny app would not have been possible without the developers of the numerous packages this app depends on. In addition, thank you to [Zoe Rich](https://zoerichdesign.wixsite.com/home) for the free graphic design consolation.

## System Environment and Packages

The code for this app was written in **R version 3.6.1 (2019-07-05) -- "Action of the Toes"** and makes use of the following R packages:

- tidyverse 1.3.0 
- leaflet 2.0.4.1 
- leaflet.providers 1.9.0 
- sf 0.9.8 
- RColorBrewer 1.1.2 
- shiny 1.6.0 
- openxlsx 4.2.3 
- colourpicker 1.1.1 
- foreign 0.8.71 
- haven 2.3.1 
- shinythemes 1.2.0 
- shinycssloaders 1.0.0 
- shinyWidgets 0.6.2 
- htmlwidgets 1.5.4 
- mapview 2.10.0 
- shinyhelper 0.3.2 
- ggthemes 4.2.4

In order to make best use of the app, access it with a modern browser such as an up to date version of Google Chrome, Microsoft Edge, or Mozilla Firefox.

## Yearly Required Updates

In order for the District Mapper section of this website to remain up to date, The ".data\districts simple feature.R" file must be regenerated using the most up to date school district boundaries shape file [found here](https://nces.ed.gov/programs/edge/Geographic/DistrictBoundaries). To run this procedure, run the new shape file (instead of ".data\shapes\EDGE\_SCHOOLDISTRICT\_TL20\_SY1920\EDGE\_SCHOOLDISTRICT\_TL20\_SY1920.shp") through ".data\shapes\cleaning districts shape file.R".

For the School Mapper section to remain up to date, the ".data\schools\_pts.R" file must be regenerated each year using up to date data on school locations from the [NCES table generator](https://nces.ed.gov/ccd/elsi/tableGenerator.aspx). To run this procedure, run the new point data from the table generator (in place of the ".data\points\school points data.xlsx") through the ".data\points\cleaning schools\_pts.R" file. 

## Anticipated Software Updates
There are a few software updates planned for the future of this app:

- Implementation of a data editor module with point and click editing, filtering,  and variable selection capabilities
- Improvement of the Static School Mapper Output. Currently, static school maps are just screen shots of the leaflet map.
- Addition of user controls to customize the static map output by controlling size, adding a map legend, and giving the map a title.

##The Leaflet Map Maker Shiny Modules
If you would like to make a similar Dashboard with different data, the ".\R\districtMapperShinyModule.R" and ".\R\schoolMapperShinyModule.R" shiny modules (paired with the ".\R\appDistrictMapper.R" and ".\R\appSchoolMapper.R" respectively) can be augmented to work with different types of data. If you would like to use this procedure to make your own data dashboard and have questions, feel free to [reach out](mailto:jmemq7@mail.missorui.edu). 




