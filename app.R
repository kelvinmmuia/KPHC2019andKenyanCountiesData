library(shiny)
library(leaflet)
library(sf)
library(dplyr)
library(htmltools)
library(bslib)
# Load shapefiles
county_shape <- st_read("ken_admbnda_adm1_iebc_20191031.shp")
country_shape <- st_read("ken_admbnda_adm0_iebc_20191031.shp")

# Load merged data
merged_data <- read.csv("gdp_data_with_population.csv")

# Prepare county and country shapes
county_shape_selected <- county_shape %>%
  dplyr::select(ADM1_EN, ADM1_PCODE, geometry)

country_shape_selected <- country_shape %>%
  dplyr::select(geometry)  

# Combine county and country shapes
combined_shape <- bind_rows(county_shape_selected,
                            country_shape_selected)

# Merge data with shapefiles
combined_shape_selected <- combined_shape %>%
  dplyr::select(ADM1_EN, ADM1_PCODE, geometry) %>%
  mutate(County = ADM1_EN) %>%
  left_join(merged_data, by = c("County" = "County"))

# Compute national totals
national_totals <- combined_shape_selected %>%
  summarize(
    Total_GDP_KSh_millions = sum(GDP_KSh_millions, na.rm = TRUE),
    Total_GDP_USD_millions = sum(GDP_USD_millions, na.rm = TRUE),
    Total_Population = sum(Total, na.rm = TRUE),
    Total_Male_Population = sum(Male, na.rm = TRUE),
    Total_Female_Population = sum(Female, na.rm = TRUE),
    Total_Area_Km2 = sum(Area_Km2, na.rm = TRUE)
  )

# Compute national averages
national_averages <- combined_shape_selected %>%
  summarize(
    Average_GDP_per_capita_KSh = mean(GDP_per_capita_KSh, na.rm = TRUE),
    Average_GDP_per_capita_USD = mean(GDP_per_capita_USD, na.rm = TRUE),
    Average_Persons_Per_Sq_Km = mean(Persons_Per_Sq_Km, na.rm = TRUE)
  )

# Create a new row for national data
national_data <- data.frame(
  ADM1_EN = "Kenya",
  ADM1_PCODE = NA,
  County = "Kenya",
  GDP_KSh_millions = national_totals$Total_GDP_KSh_millions,
  GDP_USD_millions = national_totals$Total_GDP_USD_millions,
  GDP_per_capita_KSh = national_averages$Average_GDP_per_capita_KSh,
  GDP_per_capita_USD = national_averages$Average_GDP_per_capita_USD,
  Total = national_totals$Total_Population,
  Male = national_totals$Total_Male_Population,
  Female = national_totals$Total_Female_Population,
  Area_Km2 = national_totals$Total_Area_Km2,
  Persons_Per_Sq_Km = national_averages$Average_Persons_Per_Sq_Km,
  geometry = st_union(st_geometry(combined_shape_selected))
)

# Combine the national data with the existing shapefile
combined_shape_updated <- bind_rows(
  combined_shape_selected %>%
    filter(ADM1_EN != "Kenya"),
  national_data
)

# Round off all numeric columns to two decimal places
combined_shape_updated <- combined_shape_updated %>%
  mutate_if(is.numeric, round, 2)

# Define UI
ui <- fluidPage(
  theme = bs_theme(bootswatch = "morph"),  # Default theme
  titlePanel("KPHC 2019 and County GDP Data Visualization"),
  sidebarLayout(
    sidebarPanel(
      selectInput("theme_choice", "Choose a Theme:",
                  choices = c("morph", "quartz", "cerulean", "cyborg", "cosmo", "flatly",
                              "journal", "litera", "lumen", "lux", "materia",
                              "sandstone", "minty", "pulse",  "simplex",
                              "sketchy" , "slate", "solar",  "spacelab" , 
                              "superhero", "united" , "vapor", "yeti", "zephyr")),
      selectInput("map_level", "Select Map Level:",
                  choices = c("National" = "national", "County" = "county")),
      conditionalPanel(
        condition = "input.map_level == 'county'",
        selectInput("county_focus", "Focus on County:", 
                    choices = c("None", unique(combined_shape_selected$County))),
        radioButtons("focus_on", "Focus on Selected County?", 
                     choices = c("No", "Yes"), selected = "No")
      ),
      selectInput("variable", "Select Variable:",
                  choices = c("GDP (KSh millions)" = "GDP_KSh_millions",
                              "GDP (USD millions)" = "GDP_USD_millions",
                              "GDP per Capita (KSh)" = "GDP_per_capita_KSh",
                              "GDP per Capita (USD)" = "GDP_per_capita_USD",
                              "Total Population" = "Total",
                              "Male Population" = "Male",
                              "Female Population" = "Female",
                              "Area (Km2)" = "Area_Km2",
                              "Population Density" = "Persons_Per_Sq_Km")),
      selectInput("color_palette", "Select Color Palette:", 
                  choices = c("Viridis" = "viridis",
                              "Blues" = "Blues",
                              "Greens" = "Greens", 
                              "Reds" = "Reds"))
    ),
    mainPanel(
      leafletOutput("kenyaMap")
    )
  )
)


# Define Server
server <- function(input, output, session) {
  
  observe({
    # Update the theme based on the user's selection
    session$setCurrentTheme(
      bs_theme(bootswatch = input$theme_choice)
    )
  })
  
  selected_shape <- reactive({
    if (input$map_level == "national") {
      combined_shape_updated %>% filter(County == "Kenya")
    } else {
      if (input$focus_on == "Yes" && input$county_focus != "None") {
        combined_shape_updated %>% filter(County == input$county_focus)
      } else {
        combined_shape_updated %>% filter(County != "Kenya")
      }
    }
  })
  
  color_pal <- reactive({
    colorNumeric(palette = input$color_palette, 
                 domain = selected_shape()[[input$variable]],
                 na.color = "transparent")
  })
  
  output$kenyaMap <- renderLeaflet({
    data <- selected_shape()
    
    leaflet(data) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(
        fillColor = ~color_pal()(get(input$variable)),
        color = "#444444", weight = 1, opacity = 1, fillOpacity = 0.7,
        highlightOptions = highlightOptions(color = "grey",
                                            weight = 2, bringToFront = TRUE),
        label = ~paste(ADM1_EN, paste(input$variable, ":"), get(input$variable))
      ) %>%
      addLegend(
        pal = color_pal(),
        values = data[[input$variable]],
        title = input$variable,
        position = "bottomright"
      )
  })
}

# Run the application
shinyApp(ui, server)