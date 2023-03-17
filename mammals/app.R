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
library(terra)
library(sf)
library(raster)
library(tiff)
library(leaflet)
library(rnaturalearth)
library(sp)

# SETUP DATASETS HERE
mammals_info <- read_csv(here("data_mammals", "mammals_info.csv")) 





# for Melissa's map
# Stressor Map
base_map_rast <- rast(here("data_mammals", "spatial", "ocean_area_mol.tif"))

cellid_rast <- rast(base_map_rast ) %>%
  setValues(1:ncell(base_map_rast ))

sst_rast <- rast(here("data_mammals", "stressor_maps", "sst_extremes_2020.tif"))
microplast_rast <- rast(here("data_mammals", "stressor_maps", "microplastics_2015.tif"))
bycatchp_rast <- rast(here("data_mammals", "stressor_maps", "bycatch_pelagic_2017.tif"))
bycatchb_rast <- rast(here("data_mammals", "stressor_maps", "bycatch_benthic_2017.tif"))
habitatloss_rast <- rast(here("data_mammals", "stressor_maps", "benth_str_2020.tif"))
nutrient_rast <- rast(here("data_mammals", "stressor_maps", "nutrient_2020.tif"))
light_rast <- rast(here("data_mammals", "stressor_maps", "light_2018.tif"))
shippingl_rast <- rast(here("data_mammals", "stressor_maps", "shipping_large_2021.tif"))
shippings_rast <- rast(here("data_mammals", "stressor_maps", "shipping_small_2021.tif"))
shippingall_rast <- rast(here("data_mammals", "stressor_maps", "shipping_all_unweighted_2021.tif"))

dt_join <- function(df1, df2, by, type, allow.cartesian = FALSE) {
  ### allow.cartesian for when resulting rows is greater than
  ### nrow(df1) + nrow(df2)
  a <- case_when(type == 'left' ~ c(FALSE, TRUE, FALSE), ### all, all.x, all.y
                 type == 'full' ~ c(TRUE, TRUE, TRUE),
                 type == 'inner' ~ c(FALSE, FALSE, FALSE))
  
  ### if all = FFF, behaves like inner join; if all = TTT,
  ### behaves like full join; if all = FTF, behaves like left_join?
  dt1 <- data.table::data.table(df1, key = by)
  dt2 <- data.table::data.table(df2, key = by)
  dt_full <- merge(dt1, dt2,
                   all = a[1], all.x = a[2], all.y = a[3],
                   allow.cartesian = allow.cartesian)
  return(as.data.frame(dt_full))
}

stressors_df <- data.frame(cell_id  = values(cellid_rast) %>% 
                             as.integer(),
                       sst_extremes  = values(sst_rast) %>%
                         as.numeric(),
                       microplastics  = values(microplast_rast) %>%
                         as.numeric(),
                       pelagic_bycatch  = values(bycatchp_rast) %>%
                         as.numeric(),
                       benthic_bycatch = values(bycatchb_rast)%>%
                         as.numeric(),
                       habitat_loss = values(habitatloss_rast)%>%
                         as.numeric(),
                       nutrient_pollution = values(nutrient_rast)%>%
                         as.numeric(),
                       light_pollution = values(light_rast)%>%
                         as.numeric(),
                      large_shipping_strike = values(shippingl_rast)%>%
                         as.numeric(),
                      small_shipping_strike  = values(shippings_rast)%>%
                         as.numeric(),
                       all_shipping_strikes = values(shippingall_rast) %>%
                        as.numeric())
                      

stressors_df_longer <- stressors_df %>% 
  pivot_longer(cols = sst_extremes:all_shipping_strikes, #returns it so all the stressors are in 1 column
               names_to = "stressor",
               values_to = "intensity"
  ) 

 
 






#for ELERI'S graph 
# Whale Species
fin_range <- read_csv(here("data_mammals", "species_ranges", "iucn_spp_mol_2478.csv"))
blue_range <- read_csv(here("data_mammals", "species_ranges", "iucn_spp_mol_2477.csv"))
sperm_range <- read_csv(here("data_mammals", "species_ranges", "iucn_spp_mol_41755.csv"))
naright_range <- read_csv(here("data_mammals", "species_ranges", "iucn_spp_mol_41712.csv"))
gray_range <- read_csv(here("data_mammals", "species_ranges", "iucn_spp_mol_8097.csv"))
beluga_range <- read_csv(here("data_mammals", "species_ranges", "iucn_spp_mol_6335.csv"))
humpback_range <- read_csv(here("data_mammals", "species_ranges", "iucn_spp_mol_13006.csv"))
orca_range <- read_csv(here("data_mammals", "species_ranges", "iucn_spp_mol_15421.csv"))
minke_range <- read_csv(here("data_mammals", "species_ranges", "iucn_spp_mol_2474.csv"))
pilot_range <- read_csv(here("data_mammals", "species_ranges", "iucn_spp_mol_9249.csv"))

dt_join <- function(df1, df2, by, type, allow.cartesian = FALSE) {
  ### allow.cartesian for when resulting rows is greater than
  ### nrow(df1) + nrow(df2)
  a <- case_when(type == 'left' ~ c(FALSE, TRUE, FALSE), ### all, all.x, all.y
                 type == 'full' ~ c(TRUE, TRUE, TRUE),
                 type == 'inner' ~ c(FALSE, FALSE, FALSE))
  
  ### if all = FFF, behaves like inner join; if all = TTT,
  ### behaves like full join; if all = FTF, behaves like left_join?
  dt1 <- data.table::data.table(df1, key = by)
  dt2 <- data.table::data.table(df2, key = by)
  dt_full <- merge(dt1, dt2,
                   all = a[1], all.x = a[2], all.y = a[3],
                   allow.cartesian = allow.cartesian)
  return(as.data.frame(dt_full))
}

list_whales = list(fin_range, blue_range, sperm_range, naright_range, gray_range, beluga_range, humpback_range, orca_range, minke_range, pilot_range) %>%
  setNames(c("fin whale", "blue whale", "sperm whale", "north Atlantic right whale", "gray whale", "beluga whale", "humpback whale", "orca", "minke whale", "pilot whale"))

whales_df <- list_whales %>%
  bind_rows(.id = "species")
rast_base <- terra::rast(here::here("data_mammals", "spatial", "ocean_area_mol.tif")) %>%
  terra::setValues(1:terra::ncell(.))

#for first graph -- individual species' vulnerabilities to different stressors
top10 <- mammals_info %>%
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

top10_species <- top10 [-c(20, 22, 24, 26, 28, 30, 32, 34, 36, 38, 40, 42, 44, 46, 48, 50, 52, 54, 56, 58, 60, 62, 64, 66, 68, 70, 72, 74, 76, 78, 80, 82, 84, 86, 88, 90, 92, 94, 96, 98, 100, 102, 104, 106, 108, 110, 112, 114, 116, 118, 120, 122, 124, 126, 128, 130, 132, 134, 136, 138, 140, 142, 144, 146, 148, 150, 152, 154, 156, 158, 160, 162, 200, 201, 203, 204, 206, 207, 209, 210, 212, 213, 215, 216, 218, 219, 221, 222, 224, 225, 227, 228, 230, 231, 233, 234, 236, 237, 239, 240, 241, 242, 244, 245, 247, 248, 250, 251, 255, 257, 259, 261, 263, 265, 267, 269, 271, 273, 275, 277, 279, 281, 283, 285, 287, 289, 291, 293, 296, 299, 301, 303, 305, 307, 309, 311, 313, 315, 317, 319, 321, 323),]
# what is oceanographic?


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
                                       highlights differences in species' ranges and their vulnerabilities to climate change and human stressors in an effort to help direct future conservation initiatives."), 
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
                                         p("Many stressors affect whales. This study looks at eighteen of them:"),
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
                                         strong("18) wildlife strikes."),
                                         p("Three datasets, one including small ships, one including large ships, & another with all ships"), 
                                         br(), 
                                         
                                         p("Each whale is given a vulnerability ranking between 0 and 1 for each of these stressors, with numbers closer to one indicating a higher vulnerability to that stressor."),
                                         
                                       ), #end tabset panel 
                                       tabPanel(
                                         h5("Methodology"),
                                         br(), 
                                         p("The initial dataset contained information on many marine mammal species. However, this app was filtered to only focus on commonly-known whale species, with a variety of IUCN classifications."),  
                                         
                                         br(), 
                                         
                                         p("Graphs detail how vulnerable individual species are to every stressor, and provide comparisons of vulnerabilty across all species for each stressor. Each whale has a vulnerability score between 0 and 1 for every stressor, wiht numbers closer to 1 indiciating a higher vulnerability to that particular stresor."), 
                                         
                                         br(), 
                                         
                                         p("Map 1 illustrates an overview of the global habitat range for each selected whale species, indicated in the data by 0 or 1, meaning absence or presence, respectively. Map 2 illustrates the spatial distribution of different oceanic stressors that impact whale species and their habitats, based on an intensity index."),
                                         
                                       ), #end tabsetPanel 
                                       
                                       tabPanel(
                                         
                                         h5("Data Sources"),
                                         br(), 
                                         p("Data source: O'Hara, C. Frazier, M., Valle, M., Butt, N., Kaschner, K., Lkein, C., & Halpern, B. 'Cumulative human impacts on global marine fauna highlight risk to fragile functional diversity of marine ecoystems.' [Unpublished manuscript]"), 
                                         
                                         p("Information about individual whale species classifications was sourced from: IUCN Red List at: IUCN 2023. The IUCN Red List of Threatened Species. Version 2022-2. <https://www.iucnredlist.org")
                                       ) #end tabset Panel
                                       
                                     ) #close MainPanel
                           ) #Close tabPanel
                  ), 
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  #MAP ONE - ELERI - Global Species Ranges Map
                  tabPanel("Global Species Ranges",  #tabs up at the top we can select between
                           sidebarLayout( #creates a page that has a sidebar on one side that we can put widgets/explanations on one side, and then a larger panel on the right for graph/map
                             sidebarPanel("",
                                          selectInput(
                                            inputId = "range_species", label = "Choose Whale Species:",
                                            choices = unique(whales_df$species),
                                            selected = "fin whale"
                                          )
                             ), #end sidebarPanel
                             mainPanel("Whales Species Ranges",
                                       imageOutput("whales_ranges_map"), 
                                       br(),
                                       h5("Map 1 Information"), 
                                       p("This map shows the global spatial ranges (in blue) of each selected whale species.")
                                       ) 
                           ) #end sidebar layout
                  ), #end tabPanel("Global Species Ranges")
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  #MAP TWO - MELISSA Global Stressor Map
                  tabPanel("Global Stressors", 
                           sidebarLayout(
                             sidebarPanel(
                               "Stressor Options",
                               selectInput(
                                 inputId = "choose_stressor", label = "Choose Stressor:",
                                 choices = unique(stressors_df_longer$stressor, 
                                                  selected = "sst_extremes")                                    
                               )                                                      
                             ), #end sidebarPanel
                             mainPanel( "Global Stressors",
                                        imageOutput(outputId = "stressor_Tmap"),
                                        br(),
                                        h5("Map 2 Information"), 
                                        p("This map shows the spatial distribution of global stressors, climate and human caused, that whales may be vulnerable to.
                                        A darker shade of orange indicates higher intensity for the selected stressor.")
                             ) #end main panel 
                           ) #endsidebarLayout
                  ),  #end tabPanel("Global Stressor Map")
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  #GRAPHS - HEATHER
                  tabPanel("Species Vulnerability",  
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
                  
                  
                  tabPanel("Vulnerability by Stressor",
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
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  #LEAVE THIS HERE TO END USER INTERFACE
                ) #end navbarPage()
) #end fluidPage(theme = my_theme)


########SERVER########
server <- function(input, output) {
  
  
  # MAP ONE REACTIVE - ELERI
  whales_ranges_map <- reactive({
    message("In whales ranges map reactive, input$species = ", input$species)
    x <- whales_df %>%
      filter(species == input$range_species) #initially this was input$species but that didn't work either
    
    whales_df1 <- data.frame(cell_id = 1:ncell(rast_base)) %>%
      dt_join(x, by = 'cell_id', type = 'left')
    
    whales_rast <- rast_base %>%
      setValues(whales_df1$species)
  })
  
  
  
  output$whales_ranges_map <- renderPlot({
    message("In whales_ranges_map output, class of whales_ranges_map = ", class(whales_ranges_map()))
    tm_shape(whales_ranges_map(), raster.warp = FALSE, raster.downsample = FALSE) +
      tm_raster(palette = 'Blues', title = "Whale Species Range") + 
      tm_legend(title.size = 5) + 
      tm_shape(ne_countries(type = "countries", scale = "small")) +
      tm_sf(color = 'green', legend.show = FALSE) + 
      tm_layout(legend.show = FALSE)
    
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  # MAP TWO REACTIVE - MELISSA
  map_bystressor <- reactive({
    message("in map stressor reactive, input$pick_stressor=", input$choose_stressor)
    stressor_filtered <- stressors_df_longer %>%
      filter(stressor == input$choose_stressor)
    
    stressordf_2 <- data.frame(cell_id = 1:ncell(cellid_rast)) %>% 
      dt_join(stressor_filtered, by = 'cell_id', type = 'left')   
    
    stressor_rast <- cellid_rast %>%
      setValues(stressordf_2$intensity) 
    
    return(stressor_rast)
  })
  
  
  
  output$stressor_Tmap <- renderPlot({
    tm_shape(map_bystressor(), raster.warp = FALSE, raster.downsample = FALSE) + 
      tm_raster(palette = "Oranges") + 
      tm_layout(legend.show = FALSE, title = str_to_title(input$choose_stressor))
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  # GRAPHS REACTIVE - HEATHER
  
  #input for app background 
  #output$app_background <- 
  
  #graph one
  graph_byspecies <- reactive((
    top10_species %>%
      filter(common_name == input$pick_species)
  ))
  
  output$species_graph <- renderPlot({
    stressor_color_vec <- c("biomass removal" = "#3b638c", 
                            "light pollution" = "#fcc2ae", 
                            "macroplastic entanglement" = "#e37854", 
                            "salinity" = "#a6e5f7", 
                            "habitat loss and degradation" = "#fcf9d7", 
                            "inorganic pollution" = "#65c2bd", 
                            "wildlife strike" = "#d9d1ff", 
                            "marine heat waves" = "#b3664d", 
                            "noise pollution" = "#bccfd6", 
                            "sedimentation" =  "#7695b5", 
                            "storm disturbance" = "#9f99bd", 
                            "microplastic pollution" = "#3fc4fc", 
                            "sea surface temperature rise" = "#3f8c7f", 
                            "nutrient pollution" = "#a7d9c9", 
                            "oceanographic" = "#e6dba1", 
                            "bycatch" = "#adf7ef", 
                            "organic pollution" = "#e39e96", 
                            "poisons and toxins" = "#ffd6d6") #or hex code
    ggplot(data = graph_byspecies(), aes(x = stressor, y = vuln)) +
      geom_col(aes(color = stressor, fill = stressor)) +
      scale_color_manual(values = stressor_color_vec) + 
      scale_fill_manual(values = stressor_color_vec) + 
      geom_text(aes(label = round(vuln, 2)), 
                position = position_stack(vjust = 0.5)) +
      scale_x_discrete(labels = function(x) str_wrap(x, width = 20)) +
      coord_flip() +
      ylim(0,1) +
      labs(x = "Stressor", y = "Vulnerability", title = str_to_title(input$pick_species)) + 
      theme_minimal() + theme(text = element_text(color = 'white')) +
      theme(axis.text.x = element_text(color = "white")) +
      theme(axis.text.y = element_text(color = "white")) +
      theme(legend.position = "none")
  }, 
  bg = "transparent")
  
  
  #graph two
  graph_bystressor <- reactive((
    top10_species %>%
      filter(stressor %in% input$pick_stressor)
  ))
  
  output$stressor_graph <- renderPlot({
    whale_color_vec <- c("blue whale" = "#3b638c", "common minke whale" = "#fcc2ae", "fin whale" = "#e37854", "beluga whale" = "#95e4fc", "gray whale" = "#97c4c2", "north atlantic right whale" = "#deffee", "humpback whale" = "#69a2db", "short-finned pilot whale" = "#bd5b5b", "killer whale" = "#3f8c7f", "sperm whale" = "#ffd6d6") #or hex code
    ggplot(data = graph_bystressor(), aes(x = common_name, y = vuln)) +
      geom_col(aes(color = common_name, fill = common_name)) + #leave this in even with vector above 
      scale_color_manual(values = whale_color_vec) + 
      scale_fill_manual(values = whale_color_vec) + 
      geom_text(aes(label = round(vuln, 2)), 
                position = position_stack(vjust = 0.5)) +
      scale_x_discrete(labels = function(x)
        stringr::str_wrap(x, width = 10)) +
      ylim(0,1) +
      labs(x = "Species", y = "Vulnerability", title = str_to_sentence(input$pick_stressor)) +
      theme_minimal() + theme(text = element_text(color = 'white')) + 
      theme(axis.text.x = element_text(color = "white")) +
      theme(axis.text.y = element_text(color = "white")) +
      theme(legend.position = "none") 
  },
  bg = "transparent") 
  
  
  
  
  
  

  
  
  
  
  
  
  

  
  
  
  
  
  

  
  
  
  
  # LEAVE THIS HERE TO CLOSE SERVER PANEL
}

# LEAVE THIS HERE TO RUN THE APPLICATION
shinyApp(ui = ui, server = server)
