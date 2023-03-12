#ADD LIBRARIES HERE
library(shiny)
library(tidyverse)
library(bslib) #themes for shinyapp
library(here)
library(dplyr)

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
#default map when user opens tab of stressors:
main_map <- here("data_mammals", "spatial", "ocean_area_mol.tif")


#for ELERI'S graph 
# Whale Species
# fin_range <- read_csv(here("iucn_spp_mol_2478.csv")
# blue_range <- read_csv("iucn_spp_mol_2477.csv")
# sperm_range <- read_csv("iucn_spp_mol_41755.csv")
# naright_range <- read_csv("iucn_spp_mol_41712.csv")
# gray_range <- read_csv("iucn_spp_mol_8097.csv")
# beluga_range <- read_csv("iucn_spp_mol_6335.csv")
# humpback_range <- read_csv("iucn_spp_mol_13006.csv")
# orca_range <- read_csv("iucn_spp_mol_15421.csv")
# minke_range <- read_csv("iucn_spp_mol_2474.csv")
# pilot_range <- read_csv("iucn_spp_mol_9249.csv")
# list_whales = list(fin_range, blue_range, sperm_range, naright_range, gray_range, beluga_range, humpback_range, orca_range, minke_range, pilot_range)
# whales_joined <- list_whales %>% reduce(inner_join, by='cell_id')
# ocean_tif <- here("whale_ranges_data/ocean_area_mol.tif")
# ocean_raster <- terra:rast(ocean_tif)
# ocean_raster
### Combining the ocean-whales data into map
### rasterize the whales_joined data?
### stack with ocean_raster?
### how do I get this to load into Shiny?
### how do I get my Git reconnected with my R?


#for first graph -- individual species' vulnerabilities to different stressors
top10_species <- mammals_info %>%
  filter(species %in% c("balaenoptera physalus", "balaenoptera musculus", "physeter macrocephalus", "eubalaena glacialis", "eschrichtius robustus", "delphinapterus leucas", "megaptera novaeangliae", "orcinus orca", "balaenoptera acutorostrata", "globicephala macrorhynchus")) %>% 
#fin whale (vu), blue whale (en), sperm whale (vu), north atlantic right whale (cr), gray whale (lc), beluga whale (lc), humpback whale (lc), killer whale (unknown), common minke whale (lc), short-finned pilot whale (lc) 
  mutate(common_name = ifelse(species == "balaenoptera physalus", "fin whale", ifelse(species == "balaenoptera musculus", "blue whale", ifelse(species == "physeter macrocephalus", "sperm whale", ifelse(species == "eubalaena glacialis", "north atlantic right whale", ifelse(species == "eschrichtius robustus", "gray whale", 
                      ifelse(species == "delphinapterus leucas", "beluga whale", ifelse(species == "megaptera novaeangliae", "humpback whale", ifelse(species == "orcinus orca", "killer whale", ifelse(species == "balaenoptera acutorostrata", "common minke whale", "short-finned pilot whale")))))))))) %>% 
  filter(stressor != "air_temp") %>% 
  filter(stressor != "invasive_species") %>% 
  filter(stressor != "sea_level_rise") %>% 
  mutate(stressor = case_when(
    stressor == "wildlife_strike" ~ "wildlife strike",
    stressor == "storm_disturbance" ~ "storm disturbance",
    stressor == "sst_rise" ~ "sea surface temperature rise",
    stressor == "poisons_toxins" ~ "poisons and toxins", 
    stressor == "plastic_pollution_microplastic" ~ "microplastic pollution", 
    stressor == "organic_pollution" ~ "organic pollution", 
    stressor == "noise_pollution" ~ "noise pollution", 
    stressor == "marine_heat_waves" ~ "marine heat waves", 
    stressor == "light_pollution" ~ "light pollution", 
    stressor == "inorganic_pollution" ~ "inorganic pollution", 
    stressor == "habitat_loss_degradation" ~ "habitat loss and degradation", 
    stressor == "eutrophication_nutrient_pollution" ~ "nutrient pollution", 
    stressor == "entanglement_macroplastic" ~ "macroplastic entanglement", 
    stressor == "biomass_removal" ~ "biomass removal", 
    TRUE ~ stressor
)) # what is oceanographic? 




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
                  "Whale Vulnerability to Climate and Human Stressors",
                  
                  #BACKGROUND INFO - HEATHER
                  tabPanel("Info",
                           mainPanel(h2("Background Information"),
                                     p("This app focuses on 10 common whale species and some of the major threats to their populations. Specifically, it  
                                       highlights differences in species' ranges and their primary vulnerabilities to climate change and human stressors in an effort to help direct future conservation initiatives."), 
                             tabsetPanel(
                             tabPanel(
                                     h5("Whale Species"),
                                     br(), 
                                     p("This app provides information about ten commonly known whale species and their vulnerabilites to climate and other stressors across the globe."), 
                                     p("These ten species, in order of increasing vulnerability, include:"), 
                                     p("1) the killer whale (unknown),"),
                                     p("2) the beluga whale (least concern),"), 
                                     p("3) the humpback whale (least concern),"), 
                                     p("4) the common minke whale (least concern), "), 
                                     p("5) the short-finned pilot whale (least concern),"),
                                     p("6) the gray whale (least concern),"), 
                                     p("7) the fin whale (vulnerable),"), 
                                     p("8) the sperm whale (vulnerable),"), 
                                     p("9) the blue whale (endangered), and"), 
                                     p("10) the north atlantic right whale (critically endangered)."), 
                               ), # end tabset panel  
                                tabPanel(
                                     h5("Stressors"),
                                     br(), 
                                     p("The nineteen stressors included in this study and that are affecting the ten whale species include:"),
                                     strong("1) biomass removal,"),
                                     p("which is based on total catch from nonindustrial fisheries and standardized to regional productivity for 2015-2017"), 
                                     br(), 
                                     strong("2) bycatch,"),
                                     p("which is based on total discards from fishing and standardized to local productivity for 2015-2017"), 
                                       br(),
                                     strong("3) entanglement in macroplastic,"),
                                     br(),
                                     strong("4) eutrophication and nutrient pollution,"),
                                     p("which is based on nitrgoen effluent from human-caused runoff from 2017"), 
                                     br(), 
                                     strong("5) habitat loss and degradation,"),
                                     br(),
                                     strong("6) inorganic pollution,"),
                                     br(),
                                     strong("7) light pollution,"),
                                     p("which is based on harmonized nighttime light data from 2017-2018"),
                                     br(),
                                     strong("8) marine heat waves,"),
                                     br(),
                                     strong("9) ocean acidification,"),
                                     p("which is measured by monthly aragonite saturation averaged to yearly for 2017"), 
                                      br(),
                                     strong("10) oceanographic,"),
                                     br(),
                                     strong("11) organic pollution,"),
                                     br(),
                                     strong("12) microplastic pollution,"),
                                     br(),
                                     strong("13) poisons and toxins,"),
                                     br(),
                                     strong("14) salinity changes,"),
                                     br(),
                                     strong("15) sedimentation,"),
                                     br(),
                                     strong("16) sea surface temperature rise,"),
                                     p("which is based on sea surface temperature averaged from 2016-2020"),
                                     br(),
                                     strong("17) storm disturbance,"), 
                                     br(),
                                     strong("18) UV radiation, and"),
                                     p("which is based on the number of extreme events from 2016-2020 scaled from a reference period of 2005-2009"),
                                     br(),
                                     strong("19) wildlife strikes."),
                                     p(), 
                                     br(), 
                                     
                                     p("Each whale is given a vulnerability ranking between 0 and 1 for each of these stressors, with numbers closer to one indicating a higher vulnerability to that stressor."),
                                     
                                ), #end tabset panel 
                                  tabPanel(
                                      h5("Methodology"),
                                      br(), 
                                     p("The initial dataset contained information on many marine mammal species. However, this app was filtered to only focus on commonly-known whale species."),  
                                       
                                     br(), 
                                     
                                     p("Graphs detail how vulnerable individual species are to every stressor, and provide comparisons of vulnerabilty across all species for each stressor. Each whale has a vulnerability score between 0 and 1 for every stressor, wiht numbers closer to 1 indiciating a higher vulnerability to that particular stresor."), 
                                    
                                    br(), 
                                     
                                     p("Maps x x x."),
                                     
                                  ), #end tabsetPanel 
                                     
                                  tabPanel(
                              
                                     h5("Data Sources"),
                                     br(), 
                                       p("Data were collected courtesy of Casey O'Hara, and primarily come from the years 2015-2020."), 
                                     
                                      p("Information about individual whale species classifications was sourced from the IUCN Red List at: https://www.iucnredlist.org.")
                                  ) #end tabset Panel
                                     
                           ) #close MainPanel
                  ) #Close tabPanel
                  ),  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  #MAP ONE - ELERI - Global Species Ranges Map
                  tabPanel("Global Species Ranges",  #tabs up at the top we can select between
                           sidebarLayout( #creates a page that has a sidebar on one side that we can put widgets/explanations on one side, and then a larger panel on the right for graph/map
                             sidebarPanel("",
                                          checkboxGroupInput(
                                            inputId = "pick_species", label = "Choose Whale Species:",
                                            choices = unique(top10_species$species)
                                          )
                             ), #end sidebarPanel
                             mainPanel("Will add interactive map here once I know how to do this.",
                                       imageOutput("ocean_map")) #call your graph or thing from below here, this line of code comes from what you called your plot in output$plot below in the server
                           ) #end sidebar layout
                  ), #end tabPanel("Global Species Ranges")
                  
                  # I am still working on getting the whales data into a mappable form and figuring out how to map it (it took a really long time just to find the whale data I needed and load it into R without R crashing). My R also stopped connecting to my Git for some reason so I’m working in a separate R file on the code and will copy it over to the group repo once I figure that out.
                  
                  #This is my RMD for data wrangling to make a map — I keep getting stuck even loading the data bc R keeps saying that it can’t identify my working directory even though it should be able to and I’ve tried loading the files in using the here() tool in a bunch of different ways 
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  #GRAPHS - HEATHER
                  tabPanel("Individual Species Vulnerability to All Stressors",  
                           sidebarLayout( 
                             sidebarPanel("", 
                                          radioButtons(
                                            inputId = "pick_species", label = "Choose Whale Species:",  
                                            choices = unique(top10_species$common_name) #gives the options for the checkboxes
                             ) #end selectInput
                             ), #end sidebarPanel
                             mainPanel(
                                       plotOutput("species_graph", width = "600px"), 
                                       br(), 
                                       h5("Graph Information"), 
                                       p("This graph shows vulnerability to all 18 stressors for the particular species you select on the left. A species may be more vulnerable to some stressors than others based on their spatial distributions, species characteristics, and severity of the stressor. A number closer to one (a longer bar) indicates a greater vulnerability to that particular stressor for that species.")
                                       ) #end main panel 
                           ) #end sidebar layout
                  ), #end tabPanel("Thing 2")
                  
                  
                  tabPanel("Species Vulnerability to Specific Stressors",
                           sidebarLayout(
                             sidebarPanel("",
                                          selectInput(
                                            inputId = "pick_stressor", label = "Choose Stressor:",
                                            choices = unique(top10_species$stressor)
                                          )
                             ), #end sidebar panel
                             mainPanel(
                                       plotOutput("stressor_graph"), 
                                       br(), 
                                       h5("Graph Information"), 
                                       p("This graph shows vulnerability for all 10 whale species to the particular stressor you select on the left. Based on spatial distributions and species characteristics, certain species are more vulnerable to certain stressors than to other stressors. A number closer to one (a taller bar) indicates a greater vulnerability to that particular stressor for that species.")
                                       ) #end main panel  
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
                           )) #end tabPanel("Thing 4")
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
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
  
  #input for app background 
  #output$app_background <- 
  
  #graph one
  graph_byspecies <- reactive((
    top10_species %>%
      filter(common_name == input$pick_species)
  ))
  
  output$species_graph <- renderPlot(
    ggplot(data = graph_byspecies(), aes(x = stressor, y = vuln)) +
      geom_col(aes(color = stressor, fill = stressor)) +
      geom_text(aes(label = round(vuln, 2)), 
                position = position_stack(vjust = 0.5)) +
      scale_x_discrete(labels = function(x) str_wrap(x, width = 20)) +
      coord_flip() +
      ylim(0,1) +
      labs(x = "Stressor", y = "Vulnerability", title = str_to_title(input$pick_species)) + 
      theme_minimal() +
      theme(legend.position = "none"))

  
  #graph two
  graph_bystressor <- reactive((
    top10_species %>%
      filter(stressor %in% input$pick_stressor)
  ))
  
  output$stressor_graph <- renderPlot({
    #whale_color_vec <- c("blue whale" = "red", "common minke whale" = "blue") #or hex code
    ggplot(data = graph_bystressor(), aes(x = common_name, y = vuln)) +
      geom_col(aes(color = common_name, fill = common_name)) + #leave this in even with vector above 
      #scale_color_manual(values = whale_color_vec) + 
      #scale_fill_manual(values = whale_color_vec) + 
      geom_text(aes(label = round(vuln, 2)), 
                position = position_stack(vjust = 0.5)) +
      scale_x_discrete(labels = function(x)
        stringr::str_wrap(x, width = 10)) +
      ylim(0,1) +
      labs(x = "Species", y = "Vulnerability", title = str_to_sentence(input$pick_stressor)) +
      theme_minimal() +
      theme(legend.position = "none")
    }) 
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  # MAP TWO REACTIVE - MELISSA
  #map_bystressor <- reactive(filename = here("data_mammals", "stressor_maps", input$pickstressor )))
  #filename_r <- raster(map_bystressor) #get error message: "unable to find an inherited method for function ‘raster’ for signature ‘"reactiveExpr"’
  
  stressor_Tmap <- tm_shape() + tm_raster(palette = "Oranges") + tm_layout(legend.outside = TRUE)
  #output$stressor_map <- tmap_leaflet(stressor_Tmap) #tried leaflet
  #now we need to tell user interface where to put the plot we created. go back up to UI and show where you want it to go
  
  
  map_bystressor <- reactive({fname = here('data_mammals', 'stressor_maps', paste0(input$pickstressor, '.tif'))
  
  x <- reactive({###translate input$pickstressor to file name; ### read in file name with rast()})
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  # LEAVE THIS HERE TO CLOSE SERVER PANEL
}

# LEAVE THIS HERE TO RUN THE APPLICATION
shinyApp(ui = ui, server = server)


