# Load libraries
library(shiny)
library(shinyapps)
library(leaflet) # leaflet map viewer
library(sp) # spatial data package for over() function in observe
library(DT) # interactive data table package

# Load data obtained from Dr. Zeydy Ortiz's Data4Good United Way project
# Github account

load("E:/P_Consulting/Proposals-Dev/Todd/uwtracts.RData")
uwtracts2014 <- uwtracts
load("E:/P_Consulting/Proposals-Dev/Todd/uwtracts2009.RData")
uwtracts2009 <- uwtracts
load("E:/P_Consulting/Proposals-Dev/Todd/countyStats.RData")

# Create data frame for county ids
counties <- data.frame(Num = c("063","101","135", "183"), Name=c("Durham", "Johnston", "Orange", "Wake"))

#Shiny app's server code    
shinyServer(function(input, output, session){
    
    # Choose input data for the leaflet map viewers
    varInput <- reactive({
        switch(input$dataChoice,
               "Percent Families in Poverty" = "pctFamPov",
               "Percent People in Poverty" = "pctPeoplePov",
               "Number of Children" = "numChildren",
               "Number of Children in Poverty"= "numChildrenPov",
               "Percent Children in Poverty"= "pctChildrenPov")
    })
    
    # Define minimum color bound for leaflet maps
    minInput <- reactive({
        switch(input$dataChoice,
               "Percent Families in Poverty" = min(min(uwtracts2014$pctFamPov, na.rm=TRUE),min(uwtracts2009$pctFamPov, na.rm=TRUE)),
               "Percent People in Poverty" = min(min(uwtracts2014$pctPeoplePov, na.rm=TRUE),min(uwtracts2009$pctPeoplePov, na.rm=TRUE)),
               "Number of Children" = min(min(uwtracts2014$numChildren, na.rm=TRUE),min(uwtracts2009$numChildren, na.rm=TRUE)),
               "Number of Children in Poverty"= min(min(uwtracts2014$numChildrenPov, na.rm=TRUE),min(uwtracts2009$numChildrenPov, na.rm=TRUE)),
               "Percent Children in Poverty"= min(min(uwtracts2014$pctChildrenPov, na.rm=TRUE),min(uwtracts2009$pctChildrenPov, na.rm=TRUE)))
    })
    
    # Define minimum color bound for leaflet maps
    maxInput <- reactive({
        switch(input$dataChoice,
               "Percent Families in Poverty" = max(max(uwtracts2014$pctFamPov, na.rm=TRUE),max(uwtracts2009$pctFamPov, na.rm=TRUE)),
               "Percent People in Poverty" = max(max(uwtracts2014$pctPeoplePov, na.rm=TRUE),max(uwtracts2009$pctPeoplePov, na.rm=TRUE)),
               "Number of Children" = max(max(uwtracts2014$numChildren, na.rm=TRUE),max(uwtracts2009$numChildren, na.rm=TRUE)),
               "Number of Children in Poverty"= max(max(uwtracts2014$numChildrenPov, na.rm=TRUE),max(uwtracts2009$numChildrenPov, na.rm=TRUE)),
               "Percent Children in Poverty"=max(max(uwtracts2014$pctChildrenPov, na.rm=TRUE),max(uwtracts2009$pctChildrenPov, na.rm=TRUE)))
    })
    
    # Render the 2005-9 leaflet map with chosen data and color scale limits
    output$Map1 <- renderLeaflet({
        
        dpal <- colorQuantile("Blues", c(minInput(), maxInput()), n=9)
        opal <- colorQuantile("Oranges",c(minInput(), maxInput()), n=9)
        wpal <- colorQuantile("Reds", c(minInput(), maxInput()), n=9)
        jpal <- colorQuantile("Greens", c(minInput(), maxInput()), n=9)
        
        leaflet() %>%
            addProviderTiles("OpenStreetMap.BlackAndWhite",
                             options = providerTileOptions(noWrap = TRUE)
            ) %>%
            addPolygons(data=uwtracts2009[uwtracts2009$COUNTYFP00 == "063",], group="Durham", popup=~tract_popup,
                        fillColor=~dpal(eval(parse(text=varInput()))), fillOpacity = 0.5, color = "blue", weight = 1) %>%
            addPolygons(data=uwtracts2009[uwtracts2009$COUNTYFP00 == "101",], group="Johnston", popup=~tract_popup,
                        fillColor=~jpal(eval(parse(text=varInput()))), fillOpacity = 0.5, color = "green", weight = 1) %>%
            addPolygons(data=uwtracts2009[uwtracts2009$COUNTYFP00 == "135",], group="Orange", popup=~tract_popup,
                        fillColor=~opal(eval(parse(text=varInput()))), fillOpacity = 0.5, color = "orange", weight = 1) %>%
            addPolygons(data=uwtracts2009[uwtracts2009$COUNTYFP00 == "183",], group="Wake", popup=~tract_popup,
                        fillColor=~wpal(eval(parse(text=varInput()))), fillOpacity = 0.5, color = "red", weight = 1) %>%
            addLayersControl(overlayGroups = c("Wake","Durham","Orange","Johnston"),
                             options = layersControlOptions(collapsed = FALSE))
    })

    # Render the 2010-14 leaflet map with chosen data and color scale limits    
    output$Map2 <- renderLeaflet({
        
        dpal <- colorQuantile("Blues", c(minInput(), maxInput()), n=9)
        opal <- colorQuantile("Oranges",c(minInput(), maxInput()), n=9)
        wpal <- colorQuantile("Reds", c(minInput(), maxInput()), n=9)
        jpal <- colorQuantile("Greens", c(minInput(), maxInput()), n=9)
        
        leaflet() %>%
            addProviderTiles("OpenStreetMap.BlackAndWhite",
                             options = providerTileOptions(noWrap = TRUE)
            ) %>%
            addPolygons(data=uwtracts2014[uwtracts2014$COUNTYFP == "063",], group="Durham", popup=~tract_popup,
                        fillColor=~dpal(eval(parse(text=varInput()))), fillOpacity = 0.5, color = "blue", weight = 1) %>%
            addPolygons(data=uwtracts2014[uwtracts2014$COUNTYFP == "101",], group="Johnston", popup=~tract_popup,
                        fillColor=~jpal(eval(parse(text=varInput()))), fillOpacity = 0.5, color = "green", weight = 1) %>%
            addPolygons(data=uwtracts2014[uwtracts2014$COUNTYFP == "135",], group="Orange", popup=~tract_popup,
                        fillColor=~opal(eval(parse(text=varInput()))), fillOpacity = 0.5, color = "orange", weight = 1) %>%
            addPolygons(data=uwtracts2014[uwtracts2014$COUNTYFP == "183",], group="Wake", popup=~tract_popup,
                        fillColor=~wpal(eval(parse(text=varInput()))), fillOpacity = 0.5, color = "red", weight = 1) %>%
            addLayersControl(overlayGroups = c("Wake","Durham","Orange","Johnston"),
                             options = layersControlOptions(collapsed = FALSE))
    })
    
    # Identify polygon underlying point clicked on 2005-9 leaflet map
    # Output associated summary text
    observe({
        # Define x,y coordinates of a click event in leaflet map1
        event1 <- input$Map1_shape_click
        if(is.null(event1))
            return()
        x1 = event1$lat
        y1 = event1$lng
        yx1 = cbind(y1,x1)
        dimnames(yx1)[[1]] = "QueryPoint"
        pts1 = SpatialPoints(yx1)
        uwtractsCRS <- CRS("+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0")
        proj4string(pts1) <- uwtractsCRS
        # Find the values from the census polygons under that coordinate in both maps
        answer1map1 <- over(pts1, uwtracts2009, returnList=FALSE)
        answer1map2 <- over(pts1, uwtracts2014, returnList=FALSE)
        # Find the values associated with the choosen data layer
        index1 <- which(names(answer1map1)==varInput())
        value1map1 <- answer1map1[index1]
        value1map2 <- answer1map2[index1]
        # Define output summary text for matching and non-matching polygons
        text1a <- paste("You've selected ", answer1map1$NAMELSAD00, "in ", counties$Name[which(counties$Num==answer1map1$county)], "county.")
        text1b <- "Unfortunately this census tract was rezoned and the two time periods cannot be directly compared."
        text1c <- paste("This county has seen a ", round(((value1map2-value1map1)/value1map1)*100,2), "percent change in the ",
                        tolower(input$dataChoice), "from the 2009 to 2014 reporting periods.")
        
        if (answer1map1$tract!=answer1map2$tract){
            message1 <- paste(text1a, text1b)
        } else {
            message1 <- paste(text1a, text1c)
        }
        output$summary<-renderText({
            message1
        })
    })

    # Identify polygon underlying point clicked on 2010-14 leaflet map
    # Output associated summary text    
    observe({
        # Define x,y coordinates of a click event in leaflet map1
        event2 <- input$Map2_shape_click
        if(is.null(event2))
            return()
        x2 = event2$lat
        y2 = event2$lng
        yx2 = cbind(y2,x2)
        dimnames(yx2)[[1]] = "QueryPoint"
        pts2 = SpatialPoints(yx2)
        uwtractsCRS <- CRS("+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0")
        proj4string(pts2) <- uwtractsCRS
        # Find the values from the census polygons under that coordinate in both maps
        answer2map2 <- over(pts2, uwtracts2014, returnList=FALSE)
        answer2map1 <- over(pts2, uwtracts2009, returnList=FALSE)
        # Find the values associated with the choosen data layer
        index2 <- which(names(answer2map2)==varInput())
        value2map1 <- answer2map1[index2]
        value2map2 <- answer2map2[index2]
        # Define output summary text for matching and non-matching polygons
        text2a <- paste("You've selected ", answer2map2$NAMELSAD, "in ", counties$Name[which(counties$Num==answer2map2$county)], "county.")
        text2b <- "Unfortunately this census tract was rezoned and the two time periods cannot be directly compared."
        text2c <- paste("This county has seen a ", round(((value2map2-value2map1)/value2map1)*100,2), "percent change in the ",
                        tolower(input$dataChoice), "from the 2009 to 2014 reporting periods.")
        if (answer2map1$tract!=answer2map2$tract){
            message2 <- paste(text2a, text2b)
        } else {
            message2 <- paste(text2a, text2c)
        }
        output$summary<-renderText({
            message2
        })
    })
    
    sumTableInput <- reactive({
        switch(input$tableChoice,
               "2005 to 2009 Census Data" = "uwtracts2009",
               "2010 to 2014 Census Data" = "uwtracts2014",
               "Change from 2009 to 2014" = "countyStats")
    })
    
    library(DT)
    output$CensusTable <- DT::renderDataTable(DT::datatable({
        if(input$tableChoice=="Change from 2009 to 2014")
        {summData = countyStats
        summData[c(11,1:10)] # Reorder columns to put year first
        summData$county <- counties$Name[match(summData$county, counties$Num)] # Change county number to county name
        } 
        else {dfname <- eval(parse(text=sumTableInput()))
        summData <- data.frame("Census Tract"=dfname@data$tract, 
                              "County"=dfname@data$county, 
                              "Perc Families in Poverty"=dfname@data$pctFamPov,
                              "Perc People in Poverty"=dfname@data$pctPeoplePov,
                              "Num Children"=dfname@data$numChildren,
                              "Num Children in Poverty"=dfname@data$numChildrenPov,
                              "Perc Children in Poverty"=dfname@data$pctChildrenPov)
        summData$County <- counties$Name[match(summData$County, counties$Num)]} # Change county number to county name
        summData
    }))
})  

