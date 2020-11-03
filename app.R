#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(pdftools)
library(dplyr)
library(stringr)
library(tigris)
library(purrr)
library(tidyr)
library(leaflet)
library(sf)
library(stringi)
library(RColorBrewer)
library(rlang)
library(leafpop)
library(ggplot2)
library(plotly)
library(forcats)
library(shinythemes)
library(shinyjs)

options(tigris_use_cache = TRUE)
rmw <- "[:space:]city|[:space:]township|[:space:]town"
nh <- county_subdivisions("NH", class = "sf") %>% mutate(NAMELSAD = toupper(str_remove(NAMELSAD, rmw)))

# colors to pass to scale_fill_manual
colors <- c("FING" = "#e41a1c", "1+YR" = "#377eb8", "2+YR" = "#4daf4a", "3+YR" = "#984ea3")


# clean up various town names to match with stocking report
nh$NAMELSAD[nh$NAMELSAD == "BEANS PURCHASE"] <- "BEAN'S PURCHASE"
nh$NAMELSAD[nh$NAMELSAD == "WENTWORTH LOCATION"] <- "WENTWORTH'S LOCATION"

# Define UI
ui <- fluidPage(theme = shinytheme("flatly"),
    
                
    useShinyjs(),
    # Application title
    titlePanel("New Hampshire Trout Stocking Visualizer"),
    tabsetPanel(
    tabPanel("Map",    
    sidebarLayout(
        sidebarPanel(
            selectInput("year",
                        "Year",
                        choices = c(2018, 2019),
                        selected = 2019),
            checkboxGroupInput("species", "Include Species:", 
                               choices = c("Eastern Brook Trout" = "EBT",
                                           "Brown Trout" = "BT",
                                           "Rainbow Trout" = "RT",
                                           "Landlocked Salmon" = "LLS"),
                               selected = c("EBT", "BT", "RT")),
            checkboxGroupInput("size", "Include Sizes:", 
                               choices = c("FING", "1+YR", "2+YR", "3+YR"),
                               selected = c("1+YR", "2+YR", "3+YR")),
            actionButton("go", "Generate Map"),
            br(),
            br(),
            p("This app reads the annual stocking report summaries from the NH Fish and Game website and generates visualizations using open-source packages in the R programming language."),
            a(href = "https://github.com/arkraieski/nhtroutmapr", "Check out the source code on Github")
        ),

        
        mainPanel(
           leafletOutput("map", height = 750), br(),
           DT::dataTableOutput("table"))
        
        )
    ),
    tabPanel("Bar Chart",
             sidebarLayout(
                 sidebarPanel(selectInput("year2",
                                          "Year",
                                          choices = c(2018, 2019),
                                          selected = 2019),
                             uiOutput("waterbodySelect")
                              ),
                 mainPanel(plotlyOutput("bar"))
             ))
    )
)

# Define server logic
server <- function(session, input, output) {

    # since year select inputs are necessary on multiple tabs
    # connect the two year inputs with a reactive value 
    # that updates whenever either year input changes
    selected <- reactiveValues(year = 2019)
    observeEvent(input$year, selected$year <- (input$year))
    observeEvent(input$year2, selected$year <- (input$year2))
    observeEvent(selected$year, updateSelectInput(session, "year", selected = selected$year))
    observeEvent(selected$year, updateSelectInput(session, "year2", selected = selected$year))
    
    # after initially generating the map, change the label of the button to make
    # clear to the user that it needs to be clicked again to update the map
    observeEvent(input$go, updateActionButton(session, "go", label = "Update Map"))
    
    # regenerate the map if the user changes year on the 2nd tab
    # so there isn't a mismatch between the year in the year selected
    # and the year mapped when the user goes back to the first tab 
    observeEvent(input$year2, click("go"))
    
    sr_parsed <- reactive({
        stocking_text <- (pdf_text(paste0("https://www.wildlife.state.nh.us/fishing/documents/stocking-full-",
                                          input$year,
                                          ".pdf")) %>%
                              readr::read_lines())[-c(1,2)] # first 2 rows are just the title
        
        if(selected$year == 2018){
            stocking_text <- stocking_text[!str_detect(stocking_text, "1/1/2018 - 12/31/2018")]
            stocking_text <- stocking_text[!str_detect(stocking_text, "FRESHWATER STOCKING SUMMARY BY WATERBODY")]
        }
        
        # remove headers
        dup_headers <- which(str_detect(stocking_text, "WATERBODY|BODY OF WATER"))
        #col_names <- unlist(str_split(str_squish(stocking_text[1]), " "))
        text <- stocking_text[-dup_headers]
        text <- text[text != ""]
        
        # split each line into individual words/values
        # this will allow data to be read right to left
        split_text <- str_split(text, pattern = "[:blank:]")
        line_lengths <- lapply(split_text, length) %>% unlist()
        
        # remove blank lines from text
        split_text <- map(split_text, ~ .[. != ""])
        split_text <- split_text[line_lengths != 1]
        # update line_lengths after trimming out blank lines
        line_lengths <- lapply(split_text, length) %>% unlist()
        
        
        length(split_text) == length(line_lengths)
        dat <- map2(split_text, line_lengths, 
                    function(split_text, line_lengths ){
                        species = split_text[(line_lengths - 2)]
                        size = split_text[(line_lengths - 1)]
                        number = as.integer(str_remove(split_text[line_lengths], ","))
                        data.frame(species, size, number)
                    })
        dat <- do.call(rbind, dat)
        
        
        location <-  unlist(map2(split_text, line_lengths, ~ paste(.x[1:(.y - 3)], collapse = " ")))
        dat$location <-str_trim(location)
        
        # if location is just one word, we know it's a town name
        dat$loc_length <- sapply(gregexpr("[[:alpha:]]+", dat$location), function(x) sum(x > 0))
        dat$town <- vector(mode = "character", length = nrow(dat))
        dat$town[dat$loc_length == 1] <- dat$location[dat$loc_length == 1]
        
        
        
        
        
        # rows with no waterbody will have just the town in the location
        town_detect <- str_detect(text, "^[:blank:]+")
        dat$town[town_detect] <- str_trim(dat$location[town_detect])
        
        dat$town[is.na(dat$town)] <- str_extract(dat$location[is.na(dat$town)], paste(str_trim(nh$NAMELSAD), collapse = "|"))
        
        unique_towns <- str_trim(unique(dat$town[!is.na(dat$town)]))[-1]
        
        dat$town_match <- str_extract(dat$location, 
                                      paste0(base::sort(unique_towns, 
                                                        decreasing = FALSE),
                                             "$", collapse = "|"))
        
        
        dat$town[!is.na(dat$town_match) && dat$town == ""] <- dat$town_match[!is.na(dat$town_match) && dat$town == ""] 
        
        
        dat$town[is.na(dat$town)] <- str_extract(dat$location[is.na(dat$town)], 
                                                 paste0(base::sort(nh$NAMELSAD,
                                                                   decreasing = FALSE),
                                                        "$", collapse = "|"))
        
        
        
        dat <- dat %>% mutate(waterbody = map2_chr(location, town, ~ stri_replace_last(str_trim(.x), 
                                                                                       replacement = "", 
                                                                                       regex = paste0(.y, "$")))) %>% 
            select(waterbody, town, species, size, number)
        
        
        move_modifiers <- function(string){
            if(str_detect(string, ", ")){
                mod <- str_extract(string, ", .+$")
                base <- str_remove(string, mod)
                mod <- str_replace(mod, ", ", " ")
                paste(mod, base)
            }
            else{
                string
            } 
        }
        
        dat$waterbody[str_length(dat$waterbody) == 0] <- NA_character_
        dat <- fill(dat, waterbody) %>% mutate(waterbody = map_chr(waterbody, move_modifiers) %>% str_trim())
        
        # remove extra spaces in waterbody name
        
        if(selected$year == 2019){ dat$waterbody <- dat$waterbody %>% str_replace_all("[:space:]{2,}", " ")
        
        # helps with plotting
        dat$size <- as.factor(dat$size) %>% fct_relevel(c("FING", "1+YR", "2+YR", "3+YR"))}
        
        dat
    })

    
    
    nh_trout <- eventReactive(input$go, {
        trout_towns <- sr_parsed() %>% 
            #mutate(size = as.character(size)) %>%
            filter(species %in% input$species &
                   size %in% input$size) %>%
            group_by(town, species)  %>% 
            summarize(number = sum(number)) %>%
            pivot_wider(names_from = species, values_from = number, values_fill = list(number = 0))
            
        # string to pass to dplyr::mutate() to calculate total
        #total_string <- paste0("Total = ", paste0(names(trout_towns)[-1], collapse = " + "))
        rhs <- paste0(names(trout_towns)[-1], collapse = " + ")
        
        
        trout_towns <- mutate(trout_towns, Total := !!parse_quo(rhs, env = caller_env()))
        left_join(nh, trout_towns, by = c("NAMELSAD" = "town")) %>% rename(Town = NAMELSAD)
    })
    
    # render leaflet map
    output$map <- renderLeaflet({
        validate(
            need(input$species, "Select at least one species"),
            need(input$size, "Select one or more sizes")
        )
        
        pal <- colorNumeric(
            palette = "Reds",
            domain = nh_trout()$Total)
        leaflet(data = nh_trout()) %>%
            addProviderTiles(provider = "OpenTopoMap") %>%
            addPolygons(layerId = nh_trout()$Town, fillColor =  ~ pal(nh_trout()$Total), weight = 2, color = "black", fillOpacity = 0.6,
                        popup = popupTable(nh_trout(), zcol = c("Town", "Total"), feature.id = FALSE, row.numbers = FALSE)) %>%
            addLegend(pal = pal, 
                      values = nh_trout()$Total, 
                      opacity = 0.6,
                      na.label = "unstocked")
    })
    
    output$table <- DT::renderDataTable({
        validate(
            need(input$map_shape_click$id, "Click on a town on the map to get a more detailed table")
        )
        sr_parsed() %>% filter(town == input$map_shape_click$id)
        
    })
    
    
    output$bar <- renderPlotly({
        p <- sr_parsed() %>% 
            filter(waterbody == input$waterbody) %>%
            group_by(species, size) %>%
            summarize(number = sum(number)) %>%
            ggplot(aes(x = species, y = number, fill = size)) + geom_col(position = "stack") +
            theme_minimal() +
            theme(panel.grid.minor = element_blank(),
                  panel.grid.major.x = element_blank(),
                  axis.line = element_line()) +
            scale_fill_manual(values = colors) +
            labs(title = paste(tools::toTitleCase(tolower(input$waterbody)), "Fish Stocked by Species and Size in", selected$year))
        ggplotly(p)
    })
    
    output$waterbodySelect <- renderUI({selectizeInput("waterbody",
                                                       "Waterbody",
                                                       choices = unique(sr_parsed()$waterbody),
                                                       selected = "ANDROSCOGGIN RIVER")
    })
    #observeEvent(input$map_shape_click, {print(input$map_shape_click)})
    #observeEvent(input$go, print(input$go))
    #observeEvent(input$go, print(IQR(nh_trout()$Total, na.rm = TRUE)))
    }

# Run the application 
shinyApp(ui = ui, server = server)
