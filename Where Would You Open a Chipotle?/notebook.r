
# Load tidyverse, leaflet, and leaflet.extras
library(tidyverse)
library(leaflet)
library(leaflet.extras)
library(sf)

# Read datasets/chipotle.csv into a tibble named chipotle using read_csv
chipotle <- read_csv("datasets/chipotle.csv")

# Print out the chipotle tibble using the head function
head(chipotle)

# These packages need to be loaded in the first @tests cell. 
library(testthat) 
library(IRkernel.testthat)

chipotle_test <- read_csv("datasets/chipotle.csv")

run_tests({
    test_that("packages are loaded", {
        expect_true("tidyverse" %in% .packages(), 
                    info = "Did you load the tidyverse package?")
        expect_true("leaflet" %in% .packages(), 
                    info = "Did you load the leaflet package?")
        expect_true("leaflet.extras" %in% .packages(),
                    info = "Did you load the leaflet.extras package?")
    })
    
    test_that("chipotle data loaded correctly", {
        expect_is(chipotle, "tbl_df",
                  info = "Did you read in chipotle.csv with read_csv?")
        expect_equal(chipotle, chipotle_test,
                     info = "chipotle contains the wrong values. Did you import the correct .csv file?")
    })
})


# Create a leaflet map of all closed Chipotle stores
closed_chipotles <- 
chipotle %>% 
  # Filter the chipotle tibble to stores with a value of t for closed
  filter(closed == 't') %>% 
  leaflet() %>% 
  # Use addTiles to plot the closed stores on the default Open Street Map tile
  addTiles() %>%
  # Plot the closed stores using addCircles
  addCircles() 

# Print map of closed chipotles
closed_chipotles

run_tests({
    test_that("map of closed chipotles is correct", {
    expect_s3_class(closed_chipotles, "leaflet") 
    expect_true(any(grepl("addTiles", closed_chipotles$x$calls)),
        info = "It looks like you forgot to call the addTiles function.")
    expect_true(any(grepl("addCircles", closed_chipotles$x$calls)),
        info = "It looks like you forgot to call the addCircles function.")   
    expect_equal(nrow(attr(closed_chipotles$x, "leafletData")), 15,
        info = "Did you filter by closed == 't'?")
    })
})

# Use count from dplyr to count the values for the closed variable
chipotle %>% 
  count(closed == 't')

# Create a new tibble named chipotle_open that contains only open chipotle 
chipotle_open <-
  chipotle %>% 
  filter(closed == "f") %>% 
  # Drop the closed column from chipotle_open
  select(-closed)

chipotle_open_test <-
  chipotle %>% 
  filter(closed == "f") %>% 
  select(-closed)

run_tests({
    test_that("the closed column was droped", {
    expect_false("closed" %in% colnames(chipotle_open), 
        info = "Did you remember to drop the closed column?")
    })
    
    test_that("the number of rows are correct",{
    expect_equal(nrow(chipotle_open), nrow(chipotle_open_test),
        info = "chipotle_open does not have the correct number of rows. Did you remember to filter the data using closed == 'f'")    
    })
})

# Pipe chipotle_open into a chain of leaflet functions
chipotle_heatmap <- 
chipotle_open %>% 
  leaflet() %>% 
  # Use addProviderTiles to add the CartoDB provider tile 
  addProviderTiles("CartoDB") %>%
  # Use addHeatmap with a radius of 8
  addHeatmap(radius = 8)

# Print heatmap
chipotle_heatmap 

run_tests({
    test_that("map of closed chipotles is correct", {
    expect_s3_class(chipotle_heatmap, "leaflet") 
    expect_true(any(grepl("addProviderTiles", chipotle_heatmap$x$calls)),
        info = "It looks like you forgot to call the addProviderTiles function.")
    expect_true(any(grepl("addHeatmap", chipotle_heatmap$x$calls)),
        info = "It looks like you forgot to call the addHeatmap function.") 
    expect_equal(chipotle_heatmap$x$calls[[1]]$args[[1]], "CartoDB",
        info = 'Remember to include "CartoDB" in your addProviderTiles function call.')
    expect_equal(attr(chipotle_heatmap$x, "leafletData"), chipotle_open,
        info = "Did you pipe chipotle_open into your chain of leaflet functions?")
    })
})

# Create a new tibble called chipotles_by_state to store the results
chipotles_by_state <- 
chipotle_open %>% 
  # Filter the data to only Chipotles in the United States
  filter(ctry == "United States") %>% 
  # Count the number of stores in chipotle_open by st
  count(st) %>% 
  # Arrange the number of stores by state in ascending order
  arrange(n)

# Print the state counts
chipotles_by_state

chipotles_by_state_test <- 
  chipotle_open_test %>% 
  filter(ctry == "United States") %>% 
  count(st) %>% 
  arrange(n)

run_tests({
    test_that("chipotle_open is filtered correctly", {
    expect_equal(nrow(chipotles_by_state), nrow(chipotles_by_state_test), 
        info = 'chipotles_by_state does not have the correct number of rows. Did you use filter(ctry == "United States")')
    })
  
    test_that("count function was used",{
    expect_equal(colnames(chipotles_by_state), colnames(chipotles_by_state_test),
        info = 'chipotles_by_state does not have the correct column names. Did you use count(st)?')      
    })
    
    test_that("data is arranged correctly",{
    expect_true(chipotles_by_state$n[1] == 1,
        info = 'chipotles_by_state is not sorted correctly. Did you use arrnage(n)?')
    })
})

# Print the state.abb vector
state.abb

# Use the %in% operator to determine which states are in chipotles_by_state
state.abb %in% chipotles_by_state$st

# Use the %in% and ! operators to determine which states are not in chipotles_by_state
!state.abb %in% chipotles_by_state$st

# Create a states_wo_chipotles vector
states_wo_chipotles <- state.abb[!state.abb %in% chipotles_by_state$st]

# Print states with no Chipotles
states_wo_chipotles

states_wo_chipotles_test <- state.abb[!state.abb %in% chipotles_by_state_test$st]

run_tests({
    test_that("states_wo_chipotles has the correct number of elements", {
    expect_true(length(states_wo_chipotles) == 3,  
        info = "The states_wo_chipotles vector does not have the correct number of elements. Did you remember to use the ! operator?")
    })
})

# Load south_dakota_pop.rds into an object called south_dakota_pop
south_dakota_pop <- readRDS("datasets/south_dakota_pop.rds")

# Create color palette to color map by county population estimate
pal <- colorNumeric(palette = "viridis", domain = south_dakota_pop$estimate)

sd_pop_map <-
  south_dakota_pop %>%
  leaflet() %>%
  addProviderTiles("CartoDB") %>%
  # Add county boundaries with addPolygons and color by population estimate
  addPolygons(stroke = FALSE, fillOpacity = 0.7, color = ~ pal(estimate)) %>%
  # Add a legend using addLegend 
  addLegend(pal = pal, values = ~estimate, title = "Population")

# Print map of South Dakota population by county
sd_pop_map

south_dakota_pop_test <- readRDS("datasets/south_dakota_pop.rds")
pal_test <- colorNumeric(palette = "viridis", domain = range(south_dakota_pop_test$estimate))

run_tests({
    test_that("South Dakota population data is loaded correctly", {
        expect_is(south_dakota_pop, "sf",
                info = "Did you read in south_dakota_pop.rds with readRDS?")
        expect_equal(south_dakota_pop, south_dakota_pop_test,
                info = "chipotle contains the wrong values. Did you import the correct .rds file?")
    })
    
    test_that("The color palette pal was created correctly.",{
        expect_equal(pal, pal_test,
                info = "The color palette is not correct. Did you use the estimate variable to create it?")
    })
    
    test_that("map of South Dakota population is correct", {
    expect_s3_class(sd_pop_map, "leaflet") 
    expect_true(any(grepl("addPolygons", sd_pop_map$x$calls)),
        info = "It looks like you forgot to call the addPolygons function.")
    expect_true(any(grepl("addLegend", sd_pop_map$x$calls)),
        info = "It looks like you forgot to call the addLegend function.") 
    expect_equal(attr(sd_pop_map$x, "leafletData"), south_dakota_pop,
        info = "Did you pipe south_dakota_pop into your chain of leaflet functions?")
    })
})

# Load chipotle_sd_locations.csv that contains proposed South Dakota locations  
chipotle_sd_locations <- read_csv("datasets/chipotle_sd_locations.csv")

# limit chipotle store data to locations in states boardering South Dakota
chipotle_market_research <- 
  chipotle_open %>% 
  filter(st %in% c("IA", "MN", "MT", "ND", "NE", "WY")) %>% 
  select(city, st, lat, lon) %>% 
  mutate(status = "open") %>% 
  # bind the data on proposed SD locations onto the open store data
  bind_rows(chipotle_sd_locations) 

# print the market research data
chipotle_market_research

chipotle_sd_locations_test <- read_csv("datasets/chipotle_sd_locations.csv")

run_tests({
    test_that("chipotle_sd_locations.csv data is loaded correctly", {
        expect_is(chipotle_sd_locations, "tbl_df",
                info = "Did you read in chipotle_sd_locations.csv with read_csv?")
        expect_equal(chipotle_sd_locations, chipotle_sd_locations_test,
                info = "chipotle_sd_locations contains the wrong values. Did you import the correct .csv file?")
    })
    
    test_that("the filtered data is correct",{
        expect_true(any(grepl("IA", chipotle_market_research$st)),
                info = "The number of states in the chipotle_market_research is incorrect. Did you include IA in your filter statement?")
        expect_true(any(grepl("SD", chipotle_market_research$st)),
                info = "The number of states in the chipotle_market_research is incorrect. Did you use bind_rows to append the proposed South Dakota locations?")
    })
})

# Create a blue and red color palette to distinguish between open and proposed stores
pal <- colorFactor(palette = c("Blue", "Red"), domain = c("open", "proposed"))

# Map the open and proposed locations
sd_proposed_map <-
  chipotle_market_research %>% 
  leaflet() %>% 
  # Add the Stamen Toner provider tile
  addProviderTiles("Stamen.Toner") %>%
  # Apply the pal color palette
  addCircles(color = ~pal(status)) %>%
  # Draw a circle with a 100 mi radius around the proposed locations
  addCircles(data = chipotle_sd_locations, radius = 100 * 1609.34, color = ~pal(status), fill = FALSE) 

# Print the map of proposed locations 
sd_proposed_map

pal_test <- colorFactor(palette = c("Blue", "Red"), domain = c("open", "proposed"))

sd_proposed_map_test <-
  chipotle_market_research %>% 
  leaflet() %>% 
  # Add the Stamen Toner provider tile
  addProviderTiles("Stamen.Toner") %>%
  # Apply the pal color palette
  addCircles(color = ~pal_test(status)) %>%
  # Draw a circle with a 100 mi radius around the proposed locations
  addCircles(data = chipotle_sd_locations, radius = 100 * 1609.34, color = ~pal(status), fill = FALSE)  

run_tests({
    test_that("the palette is correct", {
    expect_equal(pal, pal_test, 
        info = "pal is incorrect. Did you use the colorFactor function?")
    })
    
    test_that("map of propsed chipotles is correct", {
    expect_s3_class(sd_proposed_map, "leaflet") 
    expect_equal(sd_proposed_map$x$calls[[1]]$args[[1]], "Stamen.Toner",
        info = 'Remember to include "Stamen.Toner" in your addProviderTiles function call.')
    expect_equal(sd_proposed_map_test$x$calls[[2]]$args[[6]]$color, sd_proposed_map$x$calls[[2]]$args[[6]]$color,
        info = 'The color palette is not correct. Did you pass the status variable to ~pal() function?')
    expect_equal(sd_proposed_map$x$calls[[3]]$args[[3]], 160934,
        info = 'The radius of the large circles is not correct. Did you set the radius argument to 100 * 1609.34?')    
    expect_equal(attr(sd_proposed_map$x, "leafletData"), chipotle_market_research,
        info = "Did you pipe chipotle_market_research into your chain of leaflet functions?")
    })
})

# load the Voronoi polygon data 
polys <- readRDS("datasets/voronoi_polygons.rds")

voronoi_map <- 
  polys %>%
  leaflet() %>%
  # Use the CartoDB provider tile
  addProviderTiles(provider="CartoDB") %>%
  # Plot Voronoi polygons using addPolygons
  addPolygons(fillColor = ~pal(status), weight = 0.5, color = "black") %>%
  # Add proposed and open locations as another layer
  addCircleMarkers(data = chipotle_market_research, label = ~city, color = ~pal(status))

# Print the Voronoi map
voronoi_map

polys_test <- readRDS("datasets/voronoi_polygons.rds")

run_tests({
    test_that("voronoi_polygons.rds data is loaded correctly", {
        expect_is(polys, "sf",
                info = "Did you read in voronoi_polygons.rds with readRDS?")
        expect_equal(polys, polys_test,
                info = "polys contains the wrong values. Did you import the correct .rds file?")
        })
    
    test_that("map of voronoi polygons is correct", {
    expect_s3_class(voronoi_map, "leaflet") 
    expect_equal(voronoi_map$x$calls[[1]]$args[[1]], "CartoDB",
        info = 'Remember to include "CartoDB" in your addProviderTiles function call.')
    expect_true(any(grepl("addPolygons", voronoi_map$x$calls)),
        info = "It looks like you forgot to call the addPolygons function.") 
        expect_equal(voronoi_map$x$calls[[3]]$args[[1]], chipotle_market_research$lat,
        info = 'Did you pass chipotle_market_research to the data argument in the addCircleMarkers function?')    
    })
})

# Where should the next Chipotle store be? 
next_chipotle <- tibble(location = c("Rapid City, SD", "Sioux Falls, SD"),
                        open_new_store = c(TRUE, FALSE))

# One or more tests of the student's code. 
# The @solution should pass the tests.
# The purpose of the tests is to try to catch common errors and to 
# give the student a hint on how to resolve these errors.
run_tests({
    test_that("the next_chipotle tibble is correct", {
    expect_true(any(next_chipotle$open_new_store), 
        info = "Please set one location to TRUE.")
    expect_true(any(!next_chipotle$open_new_store), 
        info = "Please set one location to FALSE")
    })
    # You can have more than one test
})
