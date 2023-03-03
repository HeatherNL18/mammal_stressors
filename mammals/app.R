#ADD LIBRARIES HERE
library(shiny)
library(tidyverse)
library(bslib) #themes for shinyapp
library(here)

#for maps
library(tmap)
library(tmaptools)
tmap_mode("plot")
library(sf)
library(raster)
library(tiff)
library(leaflet)

# SETUP DATASETS HERE
mammals_info <- read_csv(here("data_mammals", "mammals_info.csv"))

#stressor map files
filename = here("data_mammals", "stressor_maps", "sst_extremes_2020.tif" )
sst <- raster(filename)
filename2 = here("data_mammals", "stressor_maps", "ocean_acidification_2020.tif")
oa <- raster(filename2)


#for first graph -- individual species' vulnerabilities to different stressors
top10_species <- mammals_info %>%
  filter(species %in% c("arctocephalus australis", "balaena mysticetus", "balaenoptera physalus"))  #change these later depending on what species we want



 


#SETUP THE THEME - copied from lab last week we can change
#my_theme <- bs_theme(darkly)
#bg = "rgba(170, 208, 243)", #copy and pasted from the theme preview, background
#fg = "blue",
#primary = "black",
#base_font = font_google("Times")
#)
thematic::thematic_shiny()

#bs_theme_preview() lets you use a style sheet to make it pretty
#another way to do this during lab week 3's video, making a css file



######USER INTERFACE########
ui <- fluidPage(theme = bs_theme(bootswatch = "lux"),
                navbarPage(
                  "Mammal Vulnerability to Stressors",
                  
                  
                  
                  
                  
                  
                  #MAP ONE - ELERI
                  #   tabPanel("Thing 1",  #tabs up at the top we can select between
                  #           sidebarLayout( #creates a page that has a sidebar on one side that we can put widgets/explanations on one side, and then a larger panel on the right for graph/map
                  #            sidebarPanel("Widgets",
                  #                        checkboxGroupInput(
                  #                         inputId = "pick_species", label = "Choose Species:",
                  #                     choices = unique(dataset$columnn_name)
                  #                  )
                  #    ), #end sidebarPanel
                  #   mainPanel("Output",
                  #            plotOutput("plot_1")) #call your graph or thing from below here, this line of code comes from what you called your plot in output$plot below in the server
                  # ) #end sidebar layout
                  #     ), #end tabPanel("Thing 1")
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  #GRAPHS - HEATHER
                  tabPanel("Species Vulnerability Graphs",  #tabs up at the top we can select between
                           sidebarLayout( #creates a page that has a sidebar on one side that we can put widgets/explanations on one side, and then a larger panel on the right for graph/map
                             sidebarPanel("",
                                          selectInput(
                                            inputId = "pick_species", label = "Choose Mammal Species:",  #what goes in the input id?
                                            choices = unique(top10_species$species) #gives the options for the checkboxes
                                          )
                             ), #end sidebarPanel
                             mainPanel("Individual Species Vulnerability to All Stressors",
                                       plotOutput("species_graph", width = "600px")) #call your graph or thing from below here, this line of code comes from what you called your plot in output$plot below in the server
                           ) #end sidebar layout
                  ), #end tabPanel("Thing 2")
                  
                  
                  tabPanel("Stressor Graph",
                           sidebarLayout(
                             sidebarPanel("",
                                          selectInput(
                                            inputId = "pick_stressor", label = "Choose Stressor:",
                                            choices = unique(top10_species$stressor)
                                          )
                             ), #end sidebar panel
                             mainPanel("Species Vulnerability to Selected Stressor",
                                       plotOutput("stressor_graph")) #call your graph or thing from below here, this line of code comes from what you called your plot in output$plot below in the server
                           ) #end sidebar layout
                  ), #end tabPanel
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  #MAP TWO - MELISSA
                  tabPanel("Map of Environmental Stressors",  #tabs up at the top we can select between
                           #sidebarLayout( #creates a page that has a sidebar on one side that we can put widgets/explanations on one side, and then a larger panel on the right for graph/map
                           sidebarPanel("Stressor Options",
                                        checkboxGroupInput(
                                          inputId = "pick_stressor", label = "Choose Stressor:",
                                          choices = unique(mammals_info$stressor, #returns the stressors as options to check off
                                                           #"sst" = "sst_extremes_2020.tif", #still un able to make the stressor correspond to the tif value
                                                           #"oa" = "ocean_acidification_2020.tif"
                                                           #"uv" = "uv_radiation_2020.tif" #a map of the specified stressor will appear in this tab
                                          )
                                          , #end sidebarPanel
                                          mainPanel(
                                            tmapOutput(outputId = "stressor_Tmap")) #call map here, this line of code comes from what you called your plot in output$plot below in the server
                                        ) #end sidebar layout
                           )), #end tabPanel("Thing 4")
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  #BACKGROUND INFO - HEATHER
                  tabPanel("App Information",
                           mainPanel(h2("Background Information"),
                                     p("This app provides information about ten mammal species and their vulnerabilites to climate and other stressors across the globe.
                                       Coral species were selected based on (their endangered status/being the most common/etc.) The ten species included in this study are:"),
                                     br(),
                                     
                                     p("The stressors included in this study are:"),
                                     p("1) biomass removal,"),
                                     p("2) bycatch,"),
                                     p("3) entanglement in macroplastic,"),
                                     p("4) eutrophication and nutrient pollution,"),
                                     p("5) habitat loss and degradation,"),
                                     p("6) inorganic pollution,"),
                                     p("7) light pollution,"),
                                     p("8) marine heat waves,"),
                                     p("9) ocean acidification,"),
                                     p("10) oceanographic,"),
                                     p("11) organic pollution,"),
                                     p("12) microplastic pollution,"),
                                     p("13) poisons and toxins,"),
                                     p("14) salinity changes,"),
                                     p("15) sedimentation,"),
                                     p("16) sea level rise,"),
                                     p("17) sea surface temperature rise,"),
                                     p("18) storm disturbance,"),
                                     p("19) UV radiation, and"),
                                     p("20) wildlife strikes."),
                                     p("Each coral is given a vulnerability ranking between 0 and 1 for each of these stressors."),
                                     
                                     br(),
                                     h2("Methodology"),
                                     p("The initial dataset contained information on over X number of mammal species,"),
                                     br(),
                                     h2("Data Sources"),
                                     p("Data were collected courtesy of . . . ")
                                     
                                     
                           ) #close MainPanel
                  )  #Close tabPanel
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  #LEAVE THIS HERE TO END USER INTERFACE
                ) #end navbarPage()
) #end fluidPage(theme = my_theme)


########SERVER########
#input from user interface, output is getting passed back to user interface to run and show users
server <- function(input, output) {
  
  
  # MAP ONE REACTIVE - ELERI
  #sw_reactive <- reactive((
  #data_mammals %>%
  #filter(data_column %>% input$pick_species)  #from above
  #return(newdataframe)
  #))
  
  #output$plot_name <- #graph or map function like in R markdown here
  
  #now we need to tell user interface where to put the plot we created. go back up to UI and show where you want it to go
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  # GRAPHS REACTIVE - HEATHER
  #graph one
  graph_byspecies <- reactive((
    top10_species %>%
      filter(species == input$pick_species)
    #%>%
    #  filter(stressor %in% c("inorganic_pollution", "light_pollution"))
  ))
  
  output$species_graph <- renderPlot(
    ggplot(data = graph_byspecies(), aes(x = stressor, y = vuln)) +
      geom_col(aes(color = stressor, fill = stressor)) +
      scale_x_discrete(labels = function(x) str_wrap(x, width = 20)) +
      coord_flip() +
      ylim(0,1) +
      labs(x = "Stressor", y = "Vulnerability") + #reactive title
      theme_minimal() +
      theme(legend.position = "none"))
  
  #graph two
  graph_bystressor <- reactive((
    top10_species %>%
      filter(stressor %in% input$pick_stressor)
  ))
  
  output$stressor_graph <- renderPlot(
    ggplot(data = graph_bystressor(), aes(x = species, y = vuln)) +
      geom_col(aes(color = species, fill = species)) +
      scale_x_discrete(labels = function(x)
        stringr::str_wrap(x, width = 10)) +
      ylim(0,1) +
      labs(x = "Species", y = "Vulnerability", title = top10_species$species) + #reactive title?
      theme_minimal() +
      theme(legend.position = "none"))
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  # MAP TWO REACTIVE - MELISSA
  #map_bystressor <- reactive((filename = here("data_mammals", "stressor_maps", input$pickstressor )))
  #filename_r <- raster(map_bystressor) #get error message: "unable to find an inherited method for function ‘raster’ for signature ‘"reactiveExpr"’
  
  stressor_Tmap <- tm_shape() + tm_raster(palette = "Oranges") + tm_layout(legend.outside = TRUE)
  #output$stressor_map <- tmap_leaflet(stressor_Tmap) #tried leaflet
  #now we need to tell user interface where to put the plot we created. go back up to UI and show where you want it to go
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  # LEAVE THIS HERE TO CLOSE SERVER PANEL
}

# LEAVE THIS HERE TO RUN THE APPLICATION
shinyApp(ui = ui, server = server)


