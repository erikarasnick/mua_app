library(shiny)
library(dplyr)
library(leaflet)
library(sf)
library(tmap)
tmap_mode('view')

mua_shp <- readRDS("mua_shp_5072.rds")
rural_shp <- readRDS("rural_shp_5072.rds")

overlay_fn <- function(coords, shp){
    x <- st_join(coords, shp, left = FALSE, largest=TRUE)
    if (nrow(x) > 0) {return(x)}
    else return(NULL)
}

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Medically Underserved Area Locator"),


    # Sidebar with a text input
    sidebarLayout(
        sidebarPanel(
            h4("First geocode the address to obtain the latitude and longitude."),
          a("Click here to use the online geocoder provided by Texas A&M.", href="http://geoservices.tamu.edu/Services/Geocode/Interactive/", target = "_blank"),
            br(),
            h4("Then copy and paste the latitude and longitude below."),
            # textInput("street", label = h3("Street Address"), value = "Enter text..."),
            # textInput("city", label = h3("City"), value = "Enter text..."),
            # textInput("state", label = h3("State"), value = "Enter text..."),
            # textInput("zip", label = h3("Zip Code"), value = "Enter text..."),
            numericInput("lat", label = h3("Latitude"), value = "Enter text..."),
            numericInput("lon", label = h3("Longitude"), value = "Enter text..."),
            actionButton("button", "Search"),
            br(),

            conditionalPanel(condition = "output.rural",
                             br(),
                             h4(textOutput("mua", inline = TRUE)),
                              br(),
                              h4(textOutput("rural", inline = TRUE)))
        ),

        # Show mapview
        mainPanel(
           leaflet::leafletOutput("map",
                                  height = 800,
                                  width = 1100)
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    coords_pt <- eventReactive(input$button, {
        data.frame(lat = input$lat, lon = input$lon,
                   sf_lat = input$lat, sf_lon = input$lon) %>%
            st_as_sf(coords = c('sf_lon', 'sf_lat'), crs = 4326) %>%
            st_transform(5072) %>%
            mutate(label = 'my point')
    })
    
    crop_buffer <- reactive({ st_buffer(coords_pt(), dist = 241402, nQuadSegs = 1000) })
    mua_crop <- reactive({ suppressWarnings(st_intersection(mua_shp, crop_buffer())) })
    rural_crop <- reactive({ suppressWarnings(st_intersection(rural_shp, crop_buffer())) })
    overlay_mua <- reactive({ overlay_fn(coords_pt(), mua_crop()) })
    overlay_rural <- reactive({ overlay_fn(coords_pt(), rural_crop()) })
    
    output$mua <- renderText({
        if(is.null(overlay_mua())) {
            "Medically Underserved Area: No"
        } else "Medically Underserved Area: Yes"
    })
    
    output$rural <- renderText({
        if(is.null(overlay_rural())) {
            "Rural Health Area: No"
        } else "Rural Health Area: Yes"
    })

    output$map <- leaflet::renderLeaflet({
        tm <- tm_basemap("CartoDB.Positron") +
            tm_shape(mua_crop(), name = "Medically Underserved Areas") +
            tm_polygons(col = "#fdbb84",
                        alpha = 0.7) +
            tm_shape(rural_crop(), name = "Rural Health Areas") +
            tm_polygons(col = "#a1d99b",
                        alpha = 0.7) +
            tm_shape(coords_pt(),
                     is.master = TRUE, 
                     name = "Point") +
            tm_dots(col = "#000000",
                    id = 'label',
                    size = 0.1) +
            tm_view(set.view = 10)
        tmap_leaflet(tm)
    })

    outputOptions(output, "rural", suspendWhenHidden = FALSE)
}

# Run the application
shinyApp(ui = ui, server = server)
