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
  filter(species %in% c("balaenoptera physalus", "balaenoptera musculus", "physeter macrocephalus", "eubalaena glacialis", "eschrichtius robustus", "delphinapterus leucas", "megaptera novaeangliae", "orcinus orca", "balaenoptera acutorostrata", "globicephala macrorhynchus")) %>% 
#fin whale (vu), blue whale (en), sperm whale (vu), north atlantic right whale (cr), gray whale (lc), beluga whale (lc), humpback whale (lc), killer whale (unknown), common minke whale (lc), short-finned pilot whale (lc) 
  mutate(common_name = ifelse(species == "balaenoptera physalus", "fin whale", ifelse(species == "balaenoptera musculus", "blue whale", ifelse(species == "physeter macrocephalus", "sperm whale", ifelse(species == "eubalaena glacialis", "north atlantic right whale", ifelse(species == "eschrichtius robustus", "gray whale", 
                      ifelse(species == "delphinapterus leucas", "beluga whale", ifelse(species == "megaptera novaeangliae", "humpback whale", ifelse(species == "orcinus orca", "killer whale", ifelse(species == "balaenoptera acutorostrata", "common minke whale", "short-finned pilot whale"))))))))))




#SETUP THE THEME - copied from lab last week we can change
#my_theme <- bs_theme(darkly)
#bg = "rgba(170, 208, 243)", #copy and pasted from the theme preview, background
#fg = "blue",
#primary = "black",
#base_font = font_google("Times")
#)
#thematic::thematic_shiny()

#bs_theme_preview() lets you use a style sheet to make it pretty
#another way to do this during lab week 3's video, making a css file



######USER INTERFACE########
ui <- fluidPage(theme = bs_theme(bootswatch = "darkly"),
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
                  tabPanel("Species Vulnerability Graphs",  
                           sidebarLayout( 
                             sidebarPanel("",
                                          selectInput(
                                            inputId = "pick_species", label = "Choose Mammal Species:",  
                                            choices = unique(top10_species$common_name) #gives the options for the checkboxes
                                          )
                             ), #end sidebarPanel
                             mainPanel(
                               # textInput("title", "Enter title:", value = "Initial Title"),
                                       plotOutput("species_graph", width = "600px")) 
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
                                       plotOutput("stressor_graph")) 
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
                                     p("This app provides information about ten commonly known whale species and their vulnerabilites to climate and other stressors across the globe."), 
                                    p("These ten species, in order of increasing vulnerability, include:"), 
                                    p("1) the killer whale (unknown),"),
                                    p("2) the beluga whale (least concern),"), 
                                    p("3) the humpback whale (least concern),"), 
                                    p("4) the common minke whale (least concern), "), 
                                    p("5) the short-finned pilot whale (least concern),"),
                                    p("6) the gray whale (least concern),"), 
                                    p("7) the fin whale (vulnerable),"), 
                                    p("8) the sperm whale (vulerable),"), 
                                    p("9) the blue whale (endangered), and"), 
                                    p("10) the north atlantic right whale (critically endangered)."), 
                                  
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
                                     p("Each coral is given a vulnerability ranking between 0 and 1 for each of these stressors, with numbers closer to one indicating a higher vulnerability to that stressor. "),
                                     
                                     br(),
                                     h2("Methodology"),
                                     p("The initial dataset contained information on many marine mammal species. 
                                       However, this study specifically focused on commonly-known whale species, as they are charasmatic animals that many people are willing to protect. 
                                       Highlighting differences in species' ranges and their primary vulnerabilities to climate change and human stressors can help direct future efforts for conservation efforts.
                                       Graphs detail how vulnerable individual species are to every stressor, and provide comparisons of vulnerabilty across all species for each stressor."), 
                                      br(), 
                                      p("Maps x x x."),
                                     br(),
                                     h2("Data Sources"),
                                     p("Data were collected courtesy of Casey O'Hara. Information individual whale species classifications was sourced from https://www.iucnredlist.org.")
                                     
                                     
                           ) #close MainPanel
                  )  #Close tabPanel
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  #LEAVE THIS HERE TO END USER INTERFACE
                ) #end navbarPage()
) #end fluidPage(theme = my_theme)


########SERVER########
server <- function(input, output) {
  
  
  # MAP ONE REACTIVE - ELERI
  #sw_reactive <- reactive((
  #data_mammals %>%
  #filter(data_column %>% input$pick_species)  #from above
  #return(newdataframe)
  #))
  
  #output$plot_name <- #graph or map function like in R markdown here
  

  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  # GRAPHS REACTIVE - HEATHER
  #graph one
  graph_byspecies <- reactive((
    top10_species %>%
      filter(common_name == input$pick_species)
    #%>%
    #  filter(stressor %in% c("inorganic_pollution", "light_pollution"))
  ))
  
  output$species_graph <- renderPlot(
    ggplot(data = graph_byspecies(), aes(x = stressor, y = vuln)) +
      geom_col(aes(color = stressor, fill = stressor)) +
      #ggtitle(input$title) + 
      scale_x_discrete(labels = function(x) str_wrap(x, width = 20)) +
      coord_flip() +
      ylim(0,1) +
      labs(x = "Stressor", y = "Vulnerability") + #reactive title
      theme_minimal() +
      theme(legend.position = "none"))
  
  ###Casey, I want to add a reactive title to the ggplot that changes with the selected species but can't figure it out. It's the code below, the ggtitle comment right above, and then the commented out title piece in the UI above. I think I would also need to add "session" server <- function(input, output) above, but don't want to add that in now and mess everything up
 # observeEvent(input$pick_species, {
   # updateTextInput(session, "title", value = paste("Risk of", input$pick_species))
 # })
#}
  
  #graph two
  graph_bystressor <- reactive((
    top10_species %>%
      filter(stressor %in% input$pick_stressor)
  ))
  
  output$stressor_graph <- renderPlot(
    ggplot(data = graph_bystressor(), aes(x = common_name, y = vuln)) +
      geom_col(aes(color = common_name, fill = common_name)) +
      scale_x_discrete(labels = function(x)
        stringr::str_wrap(x, width = 10)) +
      ylim(0,1) +
      labs(x = "Species", y = "Vulnerability") +
           #title = top10_species$species) #reactive title?
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


