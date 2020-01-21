#Libraries
require(shiny)
require(shinythemes)
require(ggplot2)
require(rgeos)
require(maptools)
require(tidyverse)
require(plotly)
require(quantmod)
require(ggthemes)
require(scales)
require(rgdal)
require(sp)
require(sf)
require(circlepackeR)
require(data.tree)
require(htmlwidgets)
require(rcartocolor)
require(RColorBrewer)
theme_set(theme_bw())

### DEFINING COLOR BLIND FRIENDLY COLOR PALETTES ###

nb.cols <- 29
safe_pal<- colorRampPalette(carto_pal(12, "Safe"))(nb.cols)

cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", 
               "#F0E442", "#0072B2", "#D55E00", "#CC79A7","#000000")

cbPalette2<- c("#DDCC77","#999999","#6699CC","#CC6677", "#E69F00", "#56B4E9", "#009E73", 
               "#F0E442", "#D55E00", "#0072B2", "#CC79A7","#000000")


### LOADING DATA AND SHAPE FILES ###

##CO2 Emission Intensity 
EI<- read_csv('data/co2-emission-intensity-from-electricity-generation-2.csv')

##Gross Electricity Production by Fuel 
GEP<- read_csv('data/gross-electricity-production-by-fuel-4.csv')

##JRC
JRC <- read_csv('data/jrc_powerplants.csv')

##NUTS 
NUTS_shp <- readOGR("data/", "NUTS_RG_10M_2013")


## DATA WRANGLING ## 

##CO2 Emission Intensity 

#Renaming columns 
EI<- EI %>% 
    rename(
        year = 1,
        country = 2, 
        source= 3, 
        co2_emission_intensity=4
    )

accumulate_by <- function(dat, var) {
    var <- lazyeval::f_eval(var, dat)
    lvls <- plotly:::getLevels(var)
    dats <- lapply(seq_along(lvls), function(x) {
        cbind(dat[var %in% lvls[seq(1, x)], ], frame = lvls[[x]])
    })
    dplyr::bind_rows(dats)
}

EI <- EI %>%
    accumulate_by(~year)

# Define Country List
country_list <- distinct(EI,country)
country_list <- as.list(country_list$country)
#country_list <- c('All',country_list)


##Gross Electricity Production by Fuel

#Dropping columns with all observations being NA
GEP<- GEP %>% select_if(~sum(!is.na(.)) > 0)

#Renaming columns 
GEP<- GEP %>% 
    rename(
        year = 1,
        country = 2, 
        fuel_type= 3, 
        gross_electricity_production=4
    )

#Grouping by year and fuel type 
GEP_FY<- GEP %>%
    group_by(year,fuel_type) %>%
    summarize(gross_electricity_production= sum(gross_electricity_production))


##JRC

#Removing gppd_idnr column
JRC <-select (JRC ,-c(gppd_idnr))

#Dropping missing values if missing geolocation
JRC<-JRC[complete.cases(JRC[ , 9]),]

#Renaming some fuel types 
JRC$primary_fuel[JRC$primary_fuel == "Fossil Brown coal/Lignite"]<- "Coal and lignite"
JRC$primary_fuel[JRC$primary_fuel == "Fossil Coal-derived gas"]<- "Coal and lignite"
JRC$primary_fuel[JRC$primary_fuel == "Fossil Hard coal"]<- "Coal and lignite"
JRC$primary_fuel[JRC$primary_fuel == "Fossil Gas"]<- "Natural and derived gas"
JRC$primary_fuel[JRC$primary_fuel == "Fossil Oil"]<- "Oil"
JRC$primary_fuel[JRC$primary_fuel == "Fossil Hard shale"]<- "Oil"
JRC$primary_fuel[JRC$primary_fuel == "Fossil Oil shale"]<- "Oil"
JRC$primary_fuel[JRC$primary_fuel == "Fossil Peat"]<- "Oil"
JRC$primary_fuel[JRC$primary_fuel == "Wind Offshore"]<- "Wind"
JRC$primary_fuel[JRC$primary_fuel == "Wind Onshore"]<- "Wind"
JRC$primary_fuel[JRC$primary_fuel == "Hydro Pumped Storage"]<- "Hydro"
JRC$primary_fuel[JRC$primary_fuel == "Hydro Run-of-river and poundage"]<- "Hydro"
JRC$primary_fuel[JRC$primary_fuel == "Hydro Water Reservoir"]<- "Hydro"
JRC$primary_fuel[JRC$primary_fuel == "Other"]<- "Other fuels"

#Duplicating primary fuel
JRC$fuel_categories = JRC$primary_fuel

#Creating Renwables
JRC$fuel_categories[JRC$fuel_categories == "Biomass"]<- "Renewables"
JRC$fuel_categories[JRC$fuel_categories == "Wind"]<- "Renewables"
JRC$fuel_categories[JRC$fuel_categories == "Hydro"]<- "Renewables"
JRC$fuel_categories[JRC$fuel_categories == "Geothermal"]<- "Renewables"

#JRC subset including only plants in 2018
JRC_18<- filter(JRC, year == 2018)
JRC_18 <- JRC_18[order(JRC_18$country),]

#Grouping by year and adding emissions
JRC_FT<- JRC %>%
    group_by(year) %>%
    summarize(co2_emissions_tonne= sum(co2_emissions_tonne))

#Wrangling for Cicle Packing diagram 
Circ <- JRC_18 %>% group_by(country,primary_fuel)%>% 
    summarize(co2_emissions_tonne = mean(as.numeric(co2_emissions_tonne),na.rm=TRUE))

Circ<-na.omit(Circ)
Circ<- Circ %>% filter(co2_emissions_tonne > 0.000)

Circ$pathString <- paste("world", Circ$country, Circ$primary_fuel, sep = "/")
ems <- as.Node(Circ)

# Make the plot
cst <- circlepackeR(ems, size = "co2_emissions_tonne",
                    color_min = "#88CCEE", 
                    color_max = "#332288")
print(cst)

#f<-paste(getwd(),"/www/CP_co2_emissions.html", sep="")
#saveWidget(cst,file.path(normalizePath(dirname(f)),basename(f)))



##NUTS 

#Projecting to desired CRS 
NUTS_shp <- spTransform(NUTS_shp, CRS("+proj=longlat +datum=WGS84")) 

#Keeping only country borders
NUTS_shp@polygons <- NUTS_shp@polygons[NUTS_shp@data$STAT_LEVL_ == 0]
NUTS_shp@data <- NUTS_shp@data[NUTS_shp@data$STAT_LEVL_ == 0,]

#Converting shapefile to dataframe 
NUTS_shp_df <- fortify(NUTS_shp, region="NUTS_ID")
NUTS_shp_df <- NUTS_shp_df %>% filter(hole == FALSE) 
NUTS_shp_df <- NUTS_shp_df[order(NUTS_shp_df$order),]
summary(NUTS_shp_df)




ui <- fluidPage(
    
    #selecting theme
    theme = shinytheme("flatly"),
    
    #title panel
    titlePanel("Power Production in Europe"),
    
    #introduction text
    fluidRow(
        column(12,
               br(),
               p("In the last decade, power production policies across European countries have shifted, 
                 with fears emerging over the feasibility of the decarbonization targets set by the European Commission
                 and the nationally determined contributions (NDCs) settled during the Paris Agreement in 2016 
                 (in its NDC, the EU committed to emission reductions of at least 40%). In many instances, these 
                 shifts respond to national political preferences prioritizing long-term goals related to sustainability
                 (the European Parliament and a number of EU member states have in fact already called for increasing the
                 2030 target to a 55% reduction in carbon emissions, which would still not be enough the reach the boundary
                 of a 2 degree Celsius reduction in temperature), while in others, they respond to the fact that in the long run, 
                 renewable energy has been shown to be more cost effective when compared with non-renewable energy."), br(),
               p("While the EU is on-track to meeting its NDC, according to Climate Action Tracker, in 2018 “two thirds of emissions
                 in the EU’s power sector still came from coal-fired power plants”. However, the role of coal is projected to decrease 
                 within the next twelve or so years. During the second half of 2018, Finland and the Netherlands have accelerated their 
                 coal phase-out from 2030 to 2029, Austria has accelerated theirs from 2025 to 2020, and Germany has called for the closure 
                 of the last coal plant by 2038.Nevertheless, many other European countries are yet to draft such plans. In fact, in Poland, 
                 the government has announced plans to build new coal plants by next year."),br(),
               p("The data used for this analysis was retrieved from the European Environmental Agency (EEA) 
                 and the European Union’s Joint Research Centre’s (JRC) Open Power Plant Database (JRC-PPDB-OPEN), which includes time series
                 data on electricity generation, CO2 emissions, and unit count for the years 2015, 2016, 2017, and 2018 for all power plants
                 in the EU. 
"))
        
    ),
    
    #Key Takeaways
    fluidRow(
        column(12,
               h3("Key Takeaways"),
               br(),
               " 1. Electricity production is becoming less carbon intensive.", br(),
               " 2. In 2015 and 2016, low-carbon energy sources (Renewables and Nuclear) dominated the 
               electricity mix of the EU, together generating more power than fossil fuel sources.", br(),
               " 3. The number of coal power plants is decreasing in recent years.", br(),
               " 4. The transition from fossil fuels to renewable energy sources has led CO2 emissions in the power sector
               to fall 8.3% in 2018.", br(), 
               " 5. More power plants does not equate to higher carbon emissions.", br(),
               br(),
               p("The power sector is playing paramount role in the decarbonization of Europe; 
                 it is thus critical to track the electricity transition as accurately and timely
                 as possible to achieve effective and lasting change."),
               hr())
    ),
    
    #first graph interactive
    sidebarLayout(
        sidebarPanel(
            h4('Select a Country'),
            p('Select EU member states from the dropdown menu below'),
            selectInput(
                inputId = 'countries',
                label = 'Select',
                choices = EI$country,
                multiple= TRUE)
        ),
        
        mainPanel(
            h2("Carbon Intensity of Electricity Production"),
            p('CO2 emission intensity (g CO2/kWh) is calculated as the ratio of CO2 emissions from public electricity production 
              (as a share of CO2 emissions from public electricity and heat production related to electricity production), 
              and gross electricity production.'), br(),
            p('The graph below depicts CO2 emission intensity for all EU member states.
              Select a country or a number of countries from the dropdown menu to the left
              and press the play button below to see how the countries you selected have
              progressed over time. Overall, electricity production has become less carbon intensive during the past two decades;
              the carbon intensity of average EU electricity generation in 2016 was 44 % lower than in 1990 (decreasing from 524 g 
              CO2/kWh in 1990 to 296 g CO2/kWh in 2016). This represents an average annual decrease of 2 % per year.'),
            plotlyOutput("linegraph1")
        )
        
    ),
    
    #first graph static 
    fluidRow(
             column(12,
                    hr(),
                    h2("Gross Electricity Production by Fuel"),
                    p('One of the reasons why electricity production is becoming less carbon intensive
                    is the fact that the EU is depending more on renewable and nuclear energy (both low-carbon 
                    energy sources), and less on fossil fuels. The stacked area chart below indicates that, while
                    the overall amount of electricity generated has increased over time, the share of electricity produced
                    by renewables has significantly increased while the amount produced by fossil fuels, namely coal and natural and derived gas,
                      continues to decrease. In fact, the electricity produced from renewable sources more than doubled between 2005 and 2016
                      (from 921.7 TWh in 2005 to 1092.8 in 2016), while the electricity produced from natural and derived gas decreased by 9% 
                      in the same time period (from 1407.9 TWh in 2005 to 1284.6 TWh in 2016) and the amount generated from coal and lignite decreased by 27% over the period depicted
                      (1990-2016).'), br(),
                    p('Hover over the stacked area chart below to see the amount of electricity produced by fuel by year.
                      Feel free to click and drag on the plot to zoom-in and double-click to zoom-out completely if you wish to focus on a subset
                      of the data depicted.'),
                    plotlyOutput("stackarea"),
                    br(),
                    p('The line chart below allows us to see these trends more clearly by depicting the TWh of 
                      electricity produced by fuel type, and not their aggregation. Select one or multiple fuel types from the dropdown to the left in order to explore the
                      changes in electricity generation over time. As you will see, electricity generated from renewable
                      sources follows a positive trend while that generated from coal and lignite follows a negative trend.'))),

    #second graph interactive
    sidebarLayout(
        sidebarPanel(
            h4('Select a Fuel Type'),
            p('Select types of fuel from the dropdown menu below'),
            selectInput(
                inputId = 'fuels',
                label = 'Select',
                choices = GEP_FY$fuel_type,
                multiple= TRUE)
        ),
        
        mainPanel(
            plotlyOutput("linegraph2")
        )
        
    ),
    
    
    #second graph static 
    fluidRow(
        column(12,
               hr(),
               h2("Power Plants by Fuel Type: 2015-2018"),
               p("Zooming in to the 2015-2018 time period and looking at the power plant level (more granular than the power sector as a whole), we can observe the
                 structural shift from coal and lignite to reneables by the change in number of plants by fuel type depicted in
                 the stacked bar chart below. While the total number of plants per year is displayed at the top of each bar,
                 you can hover over each segment to see the number of plants per fuel type, per year. From 2015 to 2018, we can see that
                 the number of coal and lignite plants decreased from 210 to 199 (11 plants have closed in the span of three years),
                 and the number of renewable plants increased from 268 to 304 (a net increase of 36 plants)."),
               plotlyOutput("bar1"),
               p("Structural change was likely driven by changes in regulation—both Germany and Spain announced in 2018 that coal phase-out plans
                 were imminent, putting 75% of Europe’s hard coal generation under national phase-outs (the remaining 15% is predominantly in Poland). 
                 Additionally, Germany’s 2038 phase-out plan includes lignite coal generation, which covers 50% of generation in Europe; 
                 the remaining 50% resides in countries where that is not yet the case (Bulgaria, Czech Republic, Greece, Poland, Romania, and Slovenia)."),
               hr())),
    
    #third graph interactive
    sidebarLayout(
        sidebarPanel(
            h4('Select a Fuel Type'),
            p('Select a type of fuel from the dropdown menu below'),
            selectInput(
                inputId = 'fuels1',
                label = 'Select',
                choices = JRC_18$primary_fuel,
                multiple= TRUE),
            
            h4('Select a Country'),
            p('Select EU member states from the dropdown menu below'),
            selectInput(
                inputId = 'countries1',
                label = 'Select',
                choices = JRC_18$country,
                multiple= TRUE)
            
        ),
        
        mainPanel(
            h2("Geographic Distribution of Power Plants: 2018"),
            p('The most recent layout of power plants throughout the EU territory can be seen in the
              following map and stacked bar chart.'), br(),
            p('For the map, the two drop down menus on the left allow you to explore the distribution
              of power plants with respect to either fuel type or country; the size of each power plant 
              is relative to its individual carbon emissions (in tonnes) in 2018. Select one or more fuel types 
              to see where plants pertaining to the fuel types selected are located and to potentially identify geographical
              clusters (areas with higher concentration). For instance, if only selecting Hydro plants, you will be able to see that
              they are mostly concentrated in the United Kingdom. Conversely, select one or multiple countries from
              the second drop down menu in order to look at the energy portfolio of the selected countries along with their individual 
              carbon emissions. For instance, if only selecting France, you will be able to see that its energy sector is composed
              of Biomass, Coal and lignite, Hydro, Natural and derived gas, Nuclear, and Oil plants, with Coal and Gas plants
              emitting the most CO2. Selecting multiple countries allows you to compare their energy portfolio and CO2 emissions.'), 
            plotOutput("map1",
                       height= "800px")
        )
        
    ),
    
    
    #third graph static 
    fluidRow(
        column(12,
               p("Whilst the map allows you to see the geolocation and carbon emissions for every plant
                 in the EU, the stacked bar chart summarizes the total number of plants per country per fuel.
                 While the total number of plants per country is displayed at the top of each bar,
                 you can hover over each segment to see the number of plants per fuel type for each country. The stacked
                 bar chart is arranged in descending order."),
               plotlyOutput("bar2"),
               hr())),
    
    #fourth graph static 
    fluidRow(
        column(12,
               h2("CO2 Emissions of Power Sector: 2015-2018 "),
               p('Thus far, we have seen that the number of power plants has increased between 2015 and 2018,
                 with the number of coal plants decreasing and the number of renewable plants increasing. Has 
                 this necessarily led to a decrease in carbon emissions from the EU power sector as a whole?'), br(),
               p('As seen below, CO2 emissions increased between 2015 and 2017, and then dropped in 2018 (they dropped
                 72,217,261 tonnes, which represents an 8.3% drop from 2017). '),
               plotlyOutput("line3"),
               hr())),
    
    #circlepacking
    fluidRow(
        column(12,
        h2("CO2 Emissions of Power Sector by Country: 2018"),
        p('While Europe is making significant progress in transitioning from non-renewable to renewable energy sources,
          CO2 emissions are still largely driven by dated coal plants (mostly commissioned decades ago).
          The following circle packing diagram allows us to see CO2 emissions by country by fuel type in 2018.
          The larger the darker circle, the higher the carbon emissions for that country. For instance, the UK has the power sector
          emitting the most CO2. The smaller white circles represent fuel types; again, the larger the circle, the more
          carbon is emitted by the respective fuel type. In order to zoom into each country, click on the circle pertaining to the country
          you wish to see. This will allow you to see the decomposition of carbon emissions by fuel type for that country. 
          For instance, the Portuguese power sector emits most CO2 from its Coal and Gas plants. Once you are done looking at one country, 
          click anywhere in the diagram to zoom out. You can click on each country to see their CO2 emission distribution by fuel type. ' ), br(),
        p('We can quickly observe that the countries whose power sectors emit the most carbon are the United Kingdom, Poland, Portugal, and Germany.
          While we might have expected the United Kingdom and Germany to be amongst the top emitters given that they have more than 100 plants each, 
          the fact that Poland ranks third might be surprising at first given that they have less than 50 power plants.
          However, as explained above, Poland holds around 15% of Europe’s hard coal plants and in fact, relies mostly on coal sources for power production. 
          It might also seem surprising that, even though France ranks fourth in power plant count (with 108 plants within its borders), it ranks twelfth in CO2 emissions.
          This is the case given that, while still relying on some coal plants, France also counts with a vast amount of gas, and biomass plants.  
          This allows us to conclude that more power plants does not necessarily indicate more carbon emissions.'),
        uiOutput("inc"),
        hr())
    ),
    
    #Conclusion
    fluidRow(
        column(12,
               h2("Conclusion "),
               p('Renewable growth in the EU must step up substantially. Investing in renewable energy will help provide secure, affordable, and clean energy
                 to current and future citizens of European nations. According to Agora Enegiewende, in order to reach EU’s 2030 goal of covering 32% of energy demand with renewable energy, 
                 the share of renewables in the power sector must rise to 57% (it is currently at around 30%).'), br(),
               p('When addressing the need to rely more on renewable energy sources, it is essential to highlight the fact that even though each member and candidate state approaches 
                 the energy transition from a different starting point, meeting the 2030 targets requires all countries in Europe to develop their energy system in the same direction 
                 over the same decade: increasing the efficiency of energy use and replacing fossil energy sources with clean energy sources.')))
    
    
    
)


# Define server logic
server <- (function(input, output) {
    
    output$linegraph1<- renderPlotly({
        
        EI1<-EI %>%
            filter(country %in% input$countries)
        
        emission_intensity<- 
            ggplot(EI1, aes(x=year, y= co2_emission_intensity)) +
            geom_path(aes(group=country, 
                          color=country,
                          ids=country,
                          frame=frame),
                      size= 0.75)+
            geom_text(data = EI1 %>% filter(year == last(year)),
                      aes(label = country, 
                          x = year + 0.5, 
                          y = co2_emission_intensity, 
                          color = country),
                      size=3,
                      fontface = "bold") +
            scale_x_continuous(breaks = seq(1990, 2016, by = 1))+
            #scale_y_continuous(breaks = seq(0, 1500, by = 250))+
            labs(title = "CO2 Emission Intensity",
                 subtitle = "From Electricity Generation",
                 x= "Year",
                 y= "g CO2/kWh")+
            theme(plot.title = element_text(face="bold"),
                  axis.text.x = element_text(size=10.5, angle=45,  hjust = 1),
                  axis.title.x = element_text(face="bold"),
                  axis.title.y = element_text(face="bold"),
                  axis.text.y = element_text(size=10.5),
                  legend.position = "none")+
            scale_color_manual(values=safe_pal)
        
        emission_intensity <- ggplotly(emission_intensity)%>% 
            animation_opts(
                frame = 150, 
                transition = 0, 
                redraw = FALSE
            )
        emission_intensity
        
    })
    
    output$stackarea<- renderPlotly({
        
        electricity_by_fuel<-
            ggplot(GEP_FY, aes(x=year, y=gross_electricity_production, fill= fuel_type)) + 
            geom_area(position = 'stack')+
            scale_x_continuous(breaks = seq(1990, 2016, by = 1))+
            scale_y_continuous(breaks = seq(0, 7000, by = 1000))+
            labs(title = "Gross Electricity Production by Fuel",
                 subtitle = "European Union (EU) 1990-2016",
                 x= "Year",
                 y= "Terawatt-hours(TWh)",
                 fill = "Primary Fuel")+
            theme(plot.title = element_text(face="bold"),
                  axis.text.x = element_text(size=10.5, angle=45,  hjust = 1),
                  axis.title.x = element_text(face="bold"),
                  axis.title.y = element_text(face="bold"),
                  legend.title=element_text(face="bold"),
                  axis.text.y = element_text(size=10.5))+
            scale_fill_manual(values=cbPalette)
        
        electricity_by_fuel <- ggplotly(electricity_by_fuel)
        
        electricity_by_fuel
        
    })
    
    output$linegraph2<- renderPlotly({
        
        GEP_FY1<-GEP_FY %>%
            filter(fuel_type %in% input$fuels)
        
        ebf_line<-
            ggplot(GEP_FY1, aes(x=year, y=gross_electricity_production,
                               group=fuel_type, 
                               color=fuel_type)) + 
            geom_path(size = 1)+
            scale_x_continuous(breaks = seq(1990, 2016, by = 1))+
            #scale_y_continuous(breaks = seq(0, 2500, by = 250))+
            labs(title = "Gross Electricity Production by Fuel",
                 subtitle = "European Union (EU) 1990-2016",
                 x= "Year",
                 y= "Terawatt-hours(TWh)",
                 colour = "Primary Fuel")+
            theme(plot.title = element_text(face="bold"),
                  axis.text.x = element_text(size=10.5, angle=45,  hjust = 1),
                  axis.title.x = element_text(face="bold"),
                  axis.title.y = element_text(face="bold"),
                  legend.title=element_text(face="bold"),
                  axis.text.y = element_text(size=10.5))+
            scale_color_manual(values=cbPalette)
            
        ebf_line <- ggplotly(ebf_line)%>% 
            animation_opts(
                frame = 150, 
                transition = 0, 
                redraw = FALSE
            )
        ebf_line
        
    })
    
    output$bar1<- renderPlotly({
        
        PP_Years<- ggplot(JRC %>%
                              count(year, fuel_categories)%>% 
                              mutate(pct=n/sum(n)),
                          aes(year, n, fill=fuel_categories)) +
            geom_bar(stat="identity", position="stack")+
            geom_text(aes(label = stat(y), group = year), 
                      stat = 'summary', fun.y = sum, vjust = -0.5, size=3.5)+
            scale_y_continuous(breaks = seq(0, 1200, by = 250))+
            labs(title = "Power Plants by Fuel Type",
                 subtitle = "European Union (EU) 2015-2018",
                 x= "Year",
                 y= "Number of Plants",
                 fill = "Primary Fuel",
                 caption= "'Renewables' include: Biomass, Wind, Hydro, Geothermal.")+
            theme(plot.title = element_text(face="bold"),
                  axis.title.x = element_text(face="bold"),
                  axis.title.y = element_text(face="bold"),
                  legend.title=element_text(face="bold"),
                  axis.text.x = element_text(size=10.5),
                  axis.text.y = element_text(size=10.5))+
            scale_fill_manual(values= cbPalette)
        
        PP_Years <- ggplotly(PP_Years)%>% 
            style(textposition = "top")%>%
            layout(
                yaxis = list(
                    range = c(0,1100)
                )
            )
        PP_Years
        
    })
    
    output$map1<- renderPlot({
        
        map_europe <- ggplot() +
            geom_sf() +
            coord_sf(crs= "+proj=longlat +datum=WGS84", xlim = c(-25, 46), ylim = c(32,73), expand = FALSE)+
            geom_path(data = NUTS_shp_df, 
                      aes(x = long, y = lat, group = group),
                      color = 'black', size = .10)+
            labs(title = "Power Plants by Fuel Type in Europe",
                 subtitle = "European Union: 2018")+
            theme_map()
        
        JRC_18_1<-JRC_18 %>%
            filter(country %in% input$countries1| primary_fuel %in% input$fuels1)

        
        map_europe_pp<- 
            map_europe + 
            geom_point(data=JRC_18_1, aes(x=longitude, y=latitude,
                                        color=primary_fuel,
                                        size= co2_emissions_tonne), 
                       alpha=0.8,inherit.aes = FALSE) +
            labs(color="Primary Fuel",
                 shape="CO2 Emissions (tonne)")+
            theme(legend.position = "right",
                  legend.justification = c("right", "top"),
                  plot.title = element_text(face="bold", size=20),
                  plot.subtitle = element_text(size=15),
                  legend.title=element_text(face="bold", size=15),
                  legend.text = element_text(size=10))+
            guides(color = guide_legend(override.aes = list(size=11)),
                   shape = guide_legend(override.aes = list(size = 11)))+
            scale_color_manual(values = cbPalette2)
        
        map_europe_pp
        
    })
    
    output$bar2<- renderPlotly({
        
        stacked_all<- ggplot(JRC_18 %>%
                                 count(country, primary_fuel),
                             aes(reorder(country,-n,sum), n, fill=primary_fuel)) +
            geom_bar(stat="identity", position="stack")+
            geom_text(aes(label = stat(y), group = country), 
                      stat = 'summary', fun.y = sum, 
                      vjust = -0.5, size=3.5)+
            scale_y_continuous(breaks = seq(0, 150, by = 25))+
            scale_fill_manual(values = cbPalette2)+
            labs(title = "Power Plants by Fuel Type in Europe",
                 x= "Country",
                 y= "Number of Plants",
                 fill = "Primary Fuel")+
            theme(plot.title = element_text(face="bold"),
                  axis.text.x = element_text(size=10.5,angle=90),
                  axis.text.y = element_text(size=10.5),
                  axis.title.x = element_text(face="bold"),
                  axis.title.y = element_text(face="bold"),
                  legend.title=element_text(face="bold"))
        
        stacked_all <- ggplotly(stacked_all)%>% 
            style(textposition = "top")%>%
            layout(
                yaxis = list(
                    range = c(0,160)
                )
            )
        
        
        stacked_all
        
    })
    
    output$line3<- renderPlotly({
        
        co2_emissions<- ggplot(JRC_FT, aes(x=year, y=co2_emissions_tonne)) + 
            geom_path(color="#888888")+
            geom_point(color= "#CC6677", size=2)+
            scale_y_continuous(labels = comma)+
            labs(title = "The Power Sector's CO2 Emissions",
                 subtitle = "European Union (EU) 2015-2018",
                 x= "Year",
                 y= "CO2 Emissions (tonne)")+
            theme(plot.title = element_text(face="bold"),
                  axis.text.x = element_text(size=10.5, angle=45,  hjust = 1),
                  axis.title.x = element_text(face="bold"),
                  axis.title.y = element_text(face="bold"),
                  axis.text.y = element_text(size=10.5))
        
    
        co2_emissions <- ggplotly( co2_emissions)%>% 
            style(textposition = "top")
        
        co2_emissions
        
    })
    
    getPage<-function() {
        return(includeHTML("www/CP_co2_emissions.html"))
    }
    output$inc<-renderUI({getPage()})
    
    })

# Run the application 
shinyApp(ui = ui, server = server)


