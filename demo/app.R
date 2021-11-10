library(shiny)
library(shinyjs)
library(igraph)
library(shinydashboard)
library(dplyr)
library(tidyr)
library(visNetwork)
library(DT)
source("suppfunctions.R")

data <- readRDS("mardy_data.rds")
migration_codebook_english <- data[[2]]
data <- data[[1]]

ui <- dashboardPage(
          dashboardHeader(title = "Project Mardy Demo*"), 
          dashboardSidebar(
                    shinyjs::useShinyjs(),
                    selectInput("var",
                                label = "add. variables:",
                                c("all", colnames(data)[5:length(data)]),
                                selected = c("quote", "parties"),
                                multiple = T),
                    radioButtons("unnest",
                                 label = "Unnest actors and claims?",
                                 choices = c("nested", "unnest"),
                                 selected = "unnest"),
                    dateRangeInput("date",
                                   label = "time: yyyy-mm-dd",
                                   start = min(data$cdate),
                                   end = max(data$cdate),
                                   min = min(data$cdate),
                                   max = max(data$cdate),
                                   width = "1000px",
                                   startview = "month"),
                    selectInput("twoslice", "slice",
                                choices = c(0, 1, 2, 3, 4, 5)),
                    sliderInput("degree", "Degree", value = 0, min = 0, max = 100),
                    sidebarMenu(id = "tabs",
                                menuItem("Data", tabName = "data", icon = icon("dashboard")),
                                menuItem("Network", tabName = "network", icon = icon("connectdevelop")))),
          dashboardBody(
                    tabItems(
                              tabItem(tabName = "data",
                                      fluidRow(valueBoxOutput("rows"),
                                               valueBoxOutput("actors"),
                                               valueBoxOutput("claims"),
                                               box(DT::dataTableOutput("table"), width = 12, solidHeader = F))),
                              tabItem(tabName = "network",
                                      fluidRow(valueBoxOutput("meandegree"),
                                               valueBoxOutput("nodes"),
                                               valueBoxOutput("edges"),
                                               box(visNetwork::visNetworkOutput("network_plot", height = "500px", width = "auto"), width = 12, solidHeader = F, status = "info"),
                                               box(selectInput("weight", "Weight", choices = c("all" = "all", "neg" = -1, "pos" = 1, "conf" = 2)), width = 3),
                                               box(selectInput("projection", "Projection", choices = c("affiliation", "actor", "concept")), width = 3),
                                               
                                               box(selectInput("layout", "Layout", choices = c("layout_with_fr", "layout_as_star",
                                                                                               "layout_with_kk", "layout_with_mds", "layout_with_sugiyama"), selected  = "layout_with_fr"), width = 3),
                                               box(selectInput("multi", "show multiplex",choices = c(FALSE,TRUE)), width = 3)
                                      ))
                    ),
         # hr(),
          tags$div("*This software is affiliated with the following publication:", tags$br() , 
                   "Lapesa, G., Blessing, A., Blokker, N., Dayanik, E., Haunss, S., Kuhn, J., Pado, S.: DEbateNet-mig15: Tracing the 2015 Immigration Debate in Germany Over Time. In: Proceedings of LREC. Marseille, France (2020)."),
         tags$br(), "*For further details see: https://github.com/nicoblokker/mardyr2")
)

server <- server <- function(input, output, session){
          tt <- reactiveValues(t = NULL)
          gg <- reactiveValues(g = NULL)
          observe({
                    if(input$tabs == "data"){
                              shinyjs::enable("date")
                              shinyjs::enable("var")
                              shinyjs::enable("unnest")
                              shinyjs::enable("twoslice")
                              shinyjs::reset("projection")
                              shinyjs::reset("degree")
                              shinyjs::reset("weight")
                              shinyjs::disable("degree")
                    }
                    data <- data %>%
                              dplyr::filter(cdate >= min(input$date) & cdate <= max(input$date))
                    if(input$unnest == "unnest"){
                              data <- data %>%
                                        tidyr::separate_rows(claimvalues, sep = ",") %>%
                                        dplyr::mutate(claimvalues = gsub("\\D", "", claimvalues)) %>%
                                        dplyr::filter(!grepl("[1-9]00|999", claimvalues)) %>%
                                        #dplyr::mutate(label = suppressWarnings(lookup_codes(claimvalues))) %>%
                                        as.data.frame()
                    }
                    if(all(input$var != "all")){
                              data <- data %>% dplyr::select(name, claimvalues, cpos, cdate, input$var)
                    }
                    if(input$twoslice != 0 & input$unnest == "unnest"){
                              data <- data %>%
                                        dplyr::distinct(name, claimvalues, cpos, cdate, .keep_all = T) %>%
                                        dplyr::group_by(name, claimvalues, cpos) %>%
                                        dplyr::mutate(cpos = dplyr::n()*as.numeric(cpos)) %>%
                                        dplyr::filter(abs(cpos) >= as.numeric(input$twoslice)) %>%
                                        dplyr::ungroup() %>%
                                        dplyr::distinct(name, claimvalues, cpos, .keep_all = T) %>%
                                        as.data.frame()
                    }
                    tt$t <- data
          })
          output$rows <- renderValueBox({
                    valueBox(paste0(nrow(tt$t[input[["table_rows_all"]],])), "observations", icon = icon("list"),
                             color = "purple")
          })
          output$actors <- renderValueBox({
                    valueBox(paste0(length(unique(tt$t[input[["table_rows_all"]], "name"]))), "actors", icon = icon("list"),
                             color = "yellow")
          })
          output$claims <- renderValueBox({
                    valueBox(paste0(length(unique(tt$t[input[["table_rows_all"]], "claimvalues"]))), "claims", icon = icon("list"),
                             color = "green")
          })
          output$table <- DT::renderDataTable(DT::datatable(tt$t,
                                                            filter = "bot",
                                                            extensions = "Buttons",
                                                            options = list(dom = "Blrtip",
                                                                           buttons = c("csv"),
                                                                           pagelength = 10,
                                                                           lengthMenu = list(c(10, 25, 50, 100, -1), c("10", "25", "50", "100", "all")),
                                                                           searching = T,
                                                                           stateSave = F,
                                                                           search = list(regex = T, caseInsensitive = TRUE))),
                                              server = T)
          observe({
                    if(input$tabs == "network"){
                              shinyjs::disable("date")
                              shinyjs::disable("var")
                              shinyjs::disable("unnest")
                              shinyjs::disable("twoslice")
                              shinyjs::enable("degree")
                              shinyjs::disable("legacy")
                              req(tt$t)
                              data <- tt$t[input[["table_rows_all"]], ]
                              if(input$projection == "affiliation"){
                                        g <- graph_from_data_frame(data[,c("name", "claimvalues", "cpos")], directed = F)
                                        if(input$degree != 0){
                                                  g <- induced_subgraph(g, degree(g) >= input$degree)
                                        }
                                        if(input$twoslice != 0){
                                                  E(g)$label <- as.numeric(E(g)$cpos)
                                                  E(g)$width <- abs(as.numeric(E(g)$cpos))
                                                  E(g)$cpos <- sign(as.numeric(E(g)$cpos))
                                        }
                                        if(input$weight == "all"){
                                                  g <- subgraph.edges(g, which(E(g)$cpos != 0), delete.vertices = F)
                                        } else {
                                                  g <- subgraph.edges(g, which(E(g)$cpos == input$weight))
                                        }
                                        V(g)$type <- V(g)$name %in% data[,1]
                                        if("parties" %in% colnames(data) & vcount(g) > 0){
                                                  V(g)$color <- coloring(g, data = data)
                                        } else {
                                                  V(g)$color <- ifelse(V(g)$type, "skyblue", "salmon")
                                        }
                                        V(g)$shape <- ifelse(V(g)$type, "dot", "square")
                                        E(g)$color <- ifelse(E(g)$cpos < 0, "red", "blue")
                                        gg$g <- g
                              } else if(input$projection == "actor" & input$unnest == "unnest" & length(unique(data$name)) > 1) {
                                        g <- two2one(data[,c("name", "claimvalues", "cpos")])
                                        if(input$degree != 0){
                                                  g <- induced_subgraph(g, degree(g) >= input$degree)
                                        }
                                        if(input$weight == "all"){
                                                  g <- subgraph.edges(g, which(E(g)$cpos != 0), delete.vertices = F)
                                        } else {
                                                  g <- subgraph.edges(g, which(E(g)$cpos == input$weight))
                                        }
                                        if("parties" %in% colnames(data) & vcount(g) > 0){
                                                  V(g)$color <- coloring(g, data = data)
                                        }
                                        gg$g <- g
                                        
                              } else if(input$projection == "concept" & input$unnest == "unnest"  & length(unique(data$claimvalues)) > 1){
                                        g <- two2one(data[,c("claimvalues", "name", "cpos")])
                                        if(input$degree != 0){
                                                  g <- induced_subgraph(g, degree(g) >= input$degree)
                                        }
                                        if(input$weight == "all"){
                                                  g <- subgraph.edges(g, which(E(g)$cpos != 0), delete.vertices = F)
                                        } else {
                                                  g <- subgraph.edges(g, which(E(g)$cpos == input$weight))
                                        }
                                        gg$g <- g
                              }  else {
                                        showModal(modalDialog(
                                                  title = "Warning",
                                                  "Nested projection and projection of singular nodes not implemented.
                                                             Please select multiple actors and reset to 'unnest'.", fade = T))
                              }
                    }
          })
          output$nodes <- renderValueBox({
                    req(gg$g)
                    valueBox(paste0(vcount(gg$g)), "nodes", icon = icon("project-diagram"),
                             color = "purple")
          })
          output$edges <- renderValueBox({
                    req(gg$g)
                    valueBox(paste0(ecount(gg$g)), "edges", icon = icon("connectdevelop"),
                             color = "yellow")
          })
          output$meandegree <- renderValueBox({
                    req(gg$g)
                    valueBox(paste0(round(mean(degree(gg$g)),2)), "mean degree", icon = icon("signal"),
                             color = "green")
          })
          output$network_plot <- visNetwork::renderVisNetwork({
                    req(gg$g)
                    mult <- ifelse(as.logical(input$multi) == TRUE & igraph::vcount(gg$g) < 25, TRUE, FALSE)
                    visNetwork::visIgraph(gg$g) %>%
                              visNetwork::visIgraphLayout(input$layout, smooth = list(enabled = as.logical(mult), type = 'dynamic'))
          })
}

shinyApp(ui = ui, server = server)
