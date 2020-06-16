# EU Power Sector
RShiny App exploring the energy transition in the EU towards decarbonization

**Link to final RShiny app:** https://aquevedo.shinyapps.io/EU_Power_Sector/

## Key Takeaways

* Electricity production is becoming less carbon intensive.
* In 2015 and 2016, low-carbon energy sources (Renewables and Nuclear) dominated the electricity mix of the EU, together generating more power than fossil fuel sources.
* The number of coal power plants is decreasing in recent years.
* The transition from fossil fuels to renewable energy sources has led CO2 emissions in the power sector to fall 8.3% in 2018.
* More power plants does not equate to higher carbon emissions. 

## Executive Summary 

 This project aims to explore and understand the European Union’s (EU) power sector throughout the past two decades in a scrollytelling fashion; increasing the level of granularity and insight presented as the user scrolls through the page. While displaying key insights, the project also aims to be adaptable to the needs and curiosity of the user; this is achieved via drop down menus that allow the user to choose which aspects of the data being presented they would like to focus on, be it only some countries, dates, or fuel types. The exploration begins by looking at the EU power sector as a whole between 1990 and 2016 and examining whether electricity production has become more carbon intensive and whether the amount of electricity produced from each fuel type has changed over time. The graphs in this section show that electricity production has on average become less carbon intensive and that the EU is starting to depend more on renewables and less on fossil fuels (the amount of electricity produced by renewables has sharply increased by that of fossil fuels has decreased in the last two decades).
 
By zooming in to the last four years of data available (2015-2018) and increasing the level of granularity by looking at the power plant level, the shift from non-renewables to renewables becomes evident through a stacked bar chart that depicts a decrease in number of coal plants (with 11 plants closing in 3 years) and an increase in renewable plants (with 36 new plants in the same time period). This structural change was was likely driven by changes in regulation—both Germany and Spain announced in 2018 that coal phase-out plans were imminent, putting 75% of Europe’s hard coal generation under national phase-outs (the remaining 15% is predominantly in Poland). Additionally, Germany’s 2038 phase-out plan includes lignite coal generation, which covers 50% of generation in Europe; the remaining 50% resides in countries where that is not yet the case (Bulgaria, Czech Republic, Greece, Poland, Romania, and Slovenia).

When the users keep scrolling, they are presented with a map of Europe and two drop down menus, one for fuel type and one for country, which allow the user to explore the geographic distribution of power plants in Europe based on both criteria, with the size of each power plant being relative to its individual carbon emissions (in tonnes) in 2018 (the analysis being more granular than 2015-2018). Users are also presented with a stacked bar chart summarizing the number of power plants per country by fuel type.

The project ends by looking at carbon emissions, first from 2015-2018 and then zooming in to 2018 and exploring emissions by country, by sector through an interactive circle packing diagram.

## Data Sources and Transformations

**EAA** data was used to obtain emission intensity and gross electricity production by fuel data. Both datasets provide data at the power sector level and not the power plant level, and cover years ranging from 1990to2016(timeseriesdata). Theemissionintensitydataiscomposedof10,962rowsand4columns, including year, country, source, and co2 emissions. The data was manipulated by changing column names for ease of use and was then grouped by year in order to add frames to the plot obtained. The electricity production data contains 4,698 rows and 4 columns, including: year, country, fuel type, and gross electricity production. The data was manipulated by changing column names for ease of use and then grouped by year and fuel type to produce a stacked area chart and a line graph.

**JRC-PPDB-OPEN** was used to obtain data at the power plant level; namely, plant name, country, year, electricity generation, co2 emissions, capacity, primary fuel, commissioning year, geolocation, and unit count. The dataset is offered for free without licensing restrictions and is contained within a zip folder and four separate CSV files. The data contained in JRC-PPDB-OPEN is mainly based on information from the European Network of Transmission System Operators, which is composed of 43 electricity transmission operators from 36 European countries, open datasets, and analysis of historical hourly generation time series data (covers years ranging from 2015-2018). Like EAA data, it only includes data for the 28 European Union member countries.

I obtained the dataset by web scraping and extracting the two CSV files that contained relevant information for this study. I then proceeded to clean the data by first merging the two datasets, keeping only operating power plants, aggregating data at the plant level per year, removing unwanted columns, renaming columns to for ease of use, and converting generation from MWh to GWh using pint’s UnitRegistry for python. The resulting dataset has 3,883 rows and 12 columns.

For the visualizations presented in the R Shiny app, a number of transformations were required. For instance, I dropped plants missing geolocation, renamed primary fuel to be in line with fuels defined by the EAA, created a variable called fuel_category that grouped renewable sources (namely, Hydro, Wind, Biomass, and Geothermal) into a renewable category, and created two subsets of the data, one only for year 2018 and one groupingemissionsbyyear. The resulting main dataframe has 3,867 rows and 12 columns,while the 2018 subset has 992 rows and 12 columns and the co2 subset has 4 observations and two columns.

**EUROSTAT** was used to obtain the shapefile for the map of Europe.






