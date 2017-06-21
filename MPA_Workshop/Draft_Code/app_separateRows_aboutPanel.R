### Load the Data:
  
  library( shiny )
  library( DT ) # for rendering datatables
  library( scales ) # for colors
  # Map libraries
  library( cartogram ) # for dorling cartogram
  library( eechidna ) # for dorling cartogram
  library( geojsonio ) # for manipulating GeoJSON files
  library( maps )
  library( RCurl ) # for importing files into R
  library( rgdal ) # for importing shapefiles into R
  library( sp ) # for workin with shapefiles

  # Read the .csv of the census tracts data file into your R environment
  tracts <- read.csv( "https://raw.githubusercontent.com/fjsantam/Maxwell-R-Labs/master/MPA_Workshop/CensusTracts.csv", header = T )
  
  # Read the .csv of the variables data file into your R environment
  variables <- read.csv( "https://raw.githubusercontent.com/fjsantam/Maxwell-R-Labs/master/MPA_Workshop/CensusVariables.csv", header = T )
  
  # Read the .csv of the ACS data file into your R environment
  dat <- read.csv( "https://raw.githubusercontent.com/fjsantam/Maxwell-R-Labs/master/MPA_Workshop/ACSData.csv", header = T )
  
  # Create the list of variables to be used in the selection pane of the shiny app
  var_names <- as.data.frame( unique( names(dat) ) )
  var_names <- var_names[ -c(1:4,93:94), ]
  names( var_names ) <- NULL #removes the column header from the dataframe
  years <- unique( dat$YEAR )
  
  
  
  # CLEANING THE TRACTS DATA
  # Return the structure of the vector "dat"
  #str( tracts )
  
  tracts_county <- tracts[ -13, ]
  tracts_county <- as.data.frame( tracts_county )
  #str( tracts_county )
  
  tracts_city <- tracts[14:18,]
  tracts_city <- as.data.frame( tracts_city )
  #str( tracts_city )
  
  rm( tracts )
  
  ##### Import GeoJSON map files
  
  # Code below is from: https://github.com/opetchey/RREEBES/wiki/Reading-data-and-code-from-an-online-github-repository
  # I used the first portion of the code from "Sourcing R code from github"
  # Download the code:
  script <- getURL("https://raw.githubusercontent.com/fjsantam/Maxwell-R-Labs/master/MPA_Workshop/cortland.geojson", 
                   ssl.verifypeer = FALSE)
  # Read the code as a shapefile:
  county <- readOGR( script )
  
  # Download the code and override the value in "script"
  script <- getURL("https://raw.githubusercontent.com/fjsantam/Maxwell-R-Labs/master/MPA_Workshop/cortland.city.geojson", 
                   ssl.verifypeer = FALSE)
  # Read the code as a shapefile:
  city <- readOGR( script )
  # Remove the holding variable "script":
  rm( script )



### Prep the User Interface:
ui <- shinyUI( fluidPage(
  
  titlePanel("Cortland County Census Data by Tract", windowTitle = "Cortland County Census Data" ),
  
  #Alternative layout options: https://shiny.rstudio.com/articles/layout-guide.html
  #title = "Cortland County Census Data", # Application title
  
  
  # Wrapping multiple outputs here: 
  ##### https://stackoverflow.com/questions/33204243/plotting-two-graphs-one-below-the-other-in-shiny-panel
  tabsetPanel(
    tabPanel( "Side-by-Side Maps for 2011 to 2015", 
            fluidRow(
              plotOutput( "maps.county", inline = T ),  # Show the first plot
              plotOutput( "maps.city", inline = T ) )
    ),
    #tabPanel( "About this Application", textOutput("text") ) # Show the second plot, a Dorling Cartogram
    tabPanel( "About this Application", includeMarkdown("include.md") ) # Show the About information
  ),
  
  hr(), # page break
  
  fluidRow(
    column( 3,
      h4( "Census Variable" ),
      selectInput( 
        inputId='variable', 
        label='Choose a Census Variable', 
        choices = var_names
        #width = "320px"
        #s  elected = " All Stations"
      )
    ),
    column( 4, #offset = 1,
      h4( "Census Data Dictionary" ),
      datatable( variables, options = list(
          columnDefs = list(list(className = 'dt-center', targets = 0)),
          pageLength = 2,
          lengthMenu = c(2, 3, 5, 10) )
      )
    )
  ),
  tags$head(
    tags$style(
      HTML("#dashboard{margin-bottom:0px;}")
    )
  )
))
  


# Define server logic
server <- shinyServer(function(input, output) {
  
  
  ####### Code below originally written by Christopher Davis for use in Maxwell Indepedent Study, Spring 2017. 
  ####### Adapted by Francisco Santamarina for use in Maxwell Capstone, Summer 2017.
  ####### margins information here: http://research.stowers.org/mcm/efg/R/Graphics/Basics/mar-oma/index.htm
  #create shiny plot
  
  output$maps.county <- renderPlot({
    
    #par(mfrow=c(2,3)) # Puts the 5 maps in the same panel
    par(mfrow=c(1,5)) # Puts the 5 maps in the same panel, next to each other (same row)
    par( 
      mar = c(0,1,3,1.5), # Strips inner margins
      #mar = 
      oma = c(0,0,3,0) # Sets outer margins
      #xpd=TRUE
    )
    #mar = c(1,1,1,1)
    #oma = c(0,0,0,0)
    # Other ideas: http://seananderson.ca/courses/11-multipanel/multipanel.pdf
    
    # To add a title at the top:
    #mtext("Cortland County Census Data", side=3, outer=TRUE, cex=1.5)
    #oma = c(0,0,2,0)
    
    
    #reference the inputs from the user
    #myName <- "F_TOT"   # For testing purposes
    myName <- input$variable   # USE THIS IN LIVE ENVIRONMENT
    #myYear <- input$yearInput
    #yr2011 <- dat$YEAR == 2011
    #yr2012 <- dat$YEAR == 2012
    #yr2013 <- dat$YEAR == 2013
    #yr2014 <- dat$YEAR == 2014
    #yr2015 <- dat$YEAR == 2015
    
    #subset by year
    #myData <- forDescriptives[fullFrame$YEAR==myYear, ]
    data2011 <- dat[dat$YEAR==2011, ] 
    data2012 <- dat[dat$YEAR==2012, ]
    data2013 <- dat[dat$YEAR==2013, ]
    data2014 <- dat[dat$YEAR==2014, ]
    data2015 <- dat[dat$YEAR==2015, ]
    
    
    #get variable for plot
    #myVar <- myData[, myName]
    myVar2011 <- data2011[, myName]
    myVar2012 <- data2012[, myName]
    myVar2013 <- data2013[, myName]
    myVar2014 <- data2014[, myName]
    myVar2015 <- data2015[, myName]
    
    #remove NAs
    #myVar[is.na(myVar)] <- 0
    myVar2011[ is.na( myVar2011 ) ] <- 0
    myVar2012[ is.na( myVar2012 ) ] <- 0
    myVar2013[ is.na( myVar2013 ) ] <- 0
    myVar2014[ is.na( myVar2014 ) ] <- 0
    myVar2015[ is.na( myVar2015 ) ] <- 0
    
    
    # Create color scheme for plot
    color.function <- colorRampPalette( c("#7D2526","#B4C7CB", "#172F34") ) 
    col.ramp <- color.function( 5 ) # number of groups you desire
    
    # Create color vectors by year
    ## For 2011 data:
    color.vector2011 <- cut( rank(myVar2011), breaks=5, labels=col.ramp )
    color.vector2011 <- as.character( color.vector2011 )
    ## For 2012 data:
    color.vector2012 <- cut( rank(myVar2012), breaks=5, labels=col.ramp )
    color.vector2012 <- as.character( color.vector2012 )
    ## For 2013 data:
    color.vector2013 <- cut( rank(myVar2013), breaks=5, labels=col.ramp )
    color.vector2013 <- as.character( color.vector2013 )
    ## For 2014 data:
    color.vector2014 <- cut( rank(myVar2014), breaks=5, labels=col.ramp )
    color.vector2014 <- as.character( color.vector2014 )
    ## For 2015 data:
    color.vector2015 <- cut( rank(myVar2015), breaks=5, labels=col.ramp )
    color.vector2015 <- as.character( color.vector2015 )
    
    # Create a vector for matching county tracts
    this.order <- match( county$TRACTCE10, dat$TRACT )
    ## Order color vectors for 2011 data:
    color.vec.ordered.2011 <- color.vector2011[ this.order ]
    ## Order color vectors for 2012 data:
    color.vec.ordered.2012 <- color.vector2012[ this.order ]
    ## Order color vectors for 2013 data:
    color.vec.ordered.2013 <- color.vector2013[ this.order ]
    ## Order color vectors for 2014 data:
    color.vec.ordered.2014 <- color.vector2014[ this.order ]
    ## Order color vectors for 2015 data:
    color.vec.ordered.2015 <- color.vector2015[ this.order ]
    
    
    # Create plots for each year
    ### BY COUNTY###
    
    ## 2011 Plot
    #plot( county, col=color.vec.ordered.2011, main = paste(myName, "in 2011", sep = " "), cex.main = 2)
    plot( county, col=color.vec.ordered.2011)
    #title( main = paste(myName, "in 2011", sep = " "), cex.main = 2.2)
    title("County Level for 2011", cex.main = 1.5)
    ### create scale
    #map.scale( metric=F, ratio=F, relwidth = 0.15, cex=2, font=2 )
    ### Create labels for legend
    first <- round(quantile(myVar2011, probs = seq(0, .8, .2)), digits = 2)
    last <- round(quantile(myVar2011, probs = seq(.2, 1, .2)), digits = 2)
    legend.text.2011 <- paste(first, last, sep = "-")
    ### Create legend
    op <- par(bg = "white")
    legend( "bottomright", bg=alpha("white", 0.9),
            pch=19, pt.cex=2, cex=1.5,
            legend=legend.text.2011, 
            col=col.ramp, 
            box.col=alpha("white", 0.9) 
            #title= paste(myName, "in 2011", sep = " ") 
    )
    
    ## 2012 Plot
    #plot( county, col=color.vec.ordered.2012, main = paste(myName, "in 2012", sep = " "), cex.main = 2)
    plot( county, col=color.vec.ordered.2012)
    title("County Level for 2012", cex.main = 1.5)
    ### create scale
    #map.scale( metric=F, ratio=F, relwidth = 0.15, cex=2, font=2 )
    ### Create labels for legend
    first <- round(quantile(myVar2012, probs = seq(0, .8, .2)), digits = 2)
    last <- round(quantile(myVar2012, probs = seq(.2, 1, .2)), digits = 2)
    legend.text.2012 <- paste(first, last, sep = "-")
    ### Create legend
    legend( "bottomright", bg=alpha("white", 0.9),
            pch=19, pt.cex=2, cex=1.5,
            legend=legend.text.2012, 
            col=col.ramp, 
            box.col=alpha("white", 0.9)
            #title= paste(myName, "in 2012", sep = " ") 
    )
    
    ## 2013 Plot
    #plot( county, col=color.vec.ordered.2013, main = paste(myName, "in 2013", sep = " "), cex.main = 2)
    plot( county, col=color.vec.ordered.2013)
    title("County Level for 2013", cex.main = 1.5)
    ### create scale
    #map.scale( metric=F, ratio=F, relwidth = 0.15, cex=2, font=2 )
    ### Create labels for legend
    first <- round(quantile(myVar2013, probs = seq(0, .8, .2)), digits = 2)
    last <- round(quantile(myVar2013, probs = seq(.2, 1, .2)), digits = 2)
    legend.text.2013 <- paste(first, last, sep = "-")
    ### Create legend
    legend( "bottomright", bg=alpha("white", 0.9),
            pch=19, pt.cex=2, cex=1.5,
            legend=legend.text.2013, 
            col=col.ramp, 
            box.col=alpha("white", 0.9)
            #title= paste(myName, "in 2013", sep = " ") 
    )
    
    ## 2014 Plot
    #plot( county, col=color.vec.ordered.2014, main = paste(myName, "in 2014", sep = " "), cex.main = 2)
    plot( county, col=color.vec.ordered.2014)
    title("County Level for 2014", cex.main = 1.5)
    ### create scale
    #map.scale( metric=F, ratio=F, relwidth = 0.15, cex=2, font=2 )
    ### Create labels for legend
    first <- round(quantile(myVar2014, probs = seq(0, .8, .2)), digits = 2)
    last <- round(quantile(myVar2014, probs = seq(.2, 1, .2)), digits = 2)
    legend.text.2014 <- paste(first, last, sep = "-")
    ### Create legend
    legend( "bottomright", bg=alpha("white", 0.9),
            pch=19, pt.cex=2, cex=1.5,
            legend=legend.text.2014, 
            col=col.ramp, 
            box.col=alpha("white", 0.9)
            #title= paste(myName, "in 2014", sep = " ") 
    )
    
    ## 2015 Plot
    #plot( county, col=color.vec.ordered.2015, main = paste(myName, "in 2015", sep = " "), cex.main = 2)
    plot( county, col=color.vec.ordered.2015)
    title("County Level for 2015", cex.main = 1.5)
    ### create scale
    #map.scale( metric=F, ratio=F, relwidth = 0.15, cex=2, font=2 )
    ### Create labels for legend
    first <- round(quantile(myVar2015, probs = seq(0, .8, .2)), digits = 2)
    last <- round(quantile(myVar2015, probs = seq(.2, 1, .2)), digits = 2)
    legend.text.2015 <- paste(first, last, sep = "-")
    ### Create legend
    legend( "bottomright", bg=alpha("white", 0.9),
            pch=19, pt.cex=2, cex=1.5,
            legend=legend.text.2015, 
            col=col.ramp, 
            box.col=alpha("white", 0.9)
            #title= paste(myName, "in 2015", sep = " ") 
    )
    
    mtext( paste( "    Census information for variable", myName, sep = " " ), outer = TRUE, cex = 1.7, adj = 0 )
    
  }, width = 1250, height= 225 )
  
  output$maps.city <- renderPlot({
    
    #par(mfrow=c(2,3)) # Puts the 5 maps in the same panel
    par(mfrow=c(1,5)) # Puts the 5 maps in the same panel, next to each other (same row)
    
    par( 
      mar = c(0,1,3.5,1.5), # Strips inner margins
      #mar = 
      oma = c(0.5,0.5,0.5,0.5) # Sets outer margins
      #xpd=TRUE
    )

    myName <- input$variable   # USE THIS IN LIVE ENVIRONMENT
    
    #subset by year
    #myData <- forDescriptives[fullFrame$YEAR==myYear, ]
    data2011 <- dat[dat$YEAR==2011, ] 
    data2012 <- dat[dat$YEAR==2012, ]
    data2013 <- dat[dat$YEAR==2013, ]
    data2014 <- dat[dat$YEAR==2014, ]
    data2015 <- dat[dat$YEAR==2015, ]
    
    
    #get variable for plot
    #myVar <- myData[, myName]
    myVar2011 <- data2011[, myName]
    myVar2012 <- data2012[, myName]
    myVar2013 <- data2013[, myName]
    myVar2014 <- data2014[, myName]
    myVar2015 <- data2015[, myName]
    
    #remove NAs
    #myVar[is.na(myVar)] <- 0
    myVar2011[ is.na( myVar2011 ) ] <- 0
    myVar2012[ is.na( myVar2012 ) ] <- 0
    myVar2013[ is.na( myVar2013 ) ] <- 0
    myVar2014[ is.na( myVar2014 ) ] <- 0
    myVar2015[ is.na( myVar2015 ) ] <- 0
    
    
    # Create color scheme for plot
    color.function <- colorRampPalette( c("#7D2526","#B4C7CB", "#172F34") ) 
    col.ramp <- color.function( 5 ) # number of groups you desire
    
    # Create color vectors by year
    ## For 2011 data:
    color.vector2011 <- cut( rank(myVar2011), breaks=5, labels=col.ramp )
    color.vector2011 <- as.character( color.vector2011 )
    ## For 2012 data:
    color.vector2012 <- cut( rank(myVar2012), breaks=5, labels=col.ramp )
    color.vector2012 <- as.character( color.vector2012 )
    ## For 2013 data:
    color.vector2013 <- cut( rank(myVar2013), breaks=5, labels=col.ramp )
    color.vector2013 <- as.character( color.vector2013 )
    ## For 2014 data:
    color.vector2014 <- cut( rank(myVar2014), breaks=5, labels=col.ramp )
    color.vector2014 <- as.character( color.vector2014 )
    ## For 2015 data:
    color.vector2015 <- cut( rank(myVar2015), breaks=5, labels=col.ramp )
    color.vector2015 <- as.character( color.vector2015 )
    
    # Create a vector for matching city tracts
    this.order.city <- match( city$TRACTCE10, dat$TRACT )
    ## Order color vectors for 2011 data:
    color.vec.ordered.2011.city <- color.vector2011[ this.order.city ]
    ## Order color vectors for 2012 data:
    color.vec.ordered.2012.city <- color.vector2012[ this.order.city ]
    ## Order color vectors for 2013 data:
    color.vec.ordered.2013.city <- color.vector2013[ this.order.city ]
    ## Order color vectors for 2014 data:
    color.vec.ordered.2014.city <- color.vector2014[ this.order.city ]
    ## Order color vectors for 2015 data:
    color.vec.ordered.2015.city <- color.vector2015[ this.order.city ]
    
        
    ### BY CITY###
    
    ## 2011 Plot
    plot( city, col=color.vec.ordered.2011.city)
    title( main = "City-Level Breakout for 2011", cex.main = 1.5)
    
    ## 2012 Plot
    plot( city, col=color.vec.ordered.2012.city)
    title( main = "City-Level Breakout for 2012", cex.main = 1.5)
    
    ## 2013 Plot
    plot( city, col=color.vec.ordered.2013.city)
    title( main = "City-Level Breakout for 2013", cex.main = 1.5)
    
    ## 2014 Plot
    plot( city, col=color.vec.ordered.2014.city)
    title( main = "City-Level Breakout for 2014", cex.main = 1.5)
    
    ## 2015 Plot
    plot( city, col=color.vec.ordered.2015.city)
    title( main = "City-Level Breakout for 2015", cex.main = 1.5)
  
  } , width = 1250, height= 175 )
  #output$text <- renderText({
  #  "This application was prepared by the Syracuse University MPA Capstone group for the Cortland Repertory Theatre, summer 2017.
  #  This application allows for real-time comparisons of U.S. census data related to Cortland County. 
  #  
  #  \nLast Update June 19, 2017."
  #} )
    
})

shinyApp( ui = ui, server = server)