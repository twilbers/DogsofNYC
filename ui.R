library(shinydashboard)

shinyUI(dashboardPage(
  dashboardHeader(title = "NYC Dogs"),
  dashboardSidebar(
    sidebarUserPanel(""),
    sidebarMenu(
      menuItem("Dog Density Map", tabName = "map", icon = icon("map")),
      menuItem("Breed Density Map", tabName = "breed_map", icon = icon("map")),
      menuItem("Borough Diversity", tabName = "borough_stats", icon = icon("bar-chart")),
      menuItem("Dog Names", tabName = "names", icon = icon("paw")),
      menuItem("Bubble", tabName = "bubble", icon = icon("circle")),
      menuItem("How unique is my dog?", tabName = "uniq", icon = icon("question")))
      
    ),
   
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    tabItems(
      
      tabItem(tabName = "map",
              fluidRow(infoBoxOutput("maxBox"),
                       infoBoxOutput("avgBox"),
                       infoBoxOutput("minBox")),
              fluidRow(box(leafletOutput("dog_density"), height = 440),
                       box(plotlyOutput("percapita"), height = 440)),
              #fluidRow(plotlyOutput("bar")),
              fluidRow(plotlyOutput("bar_capita"))
              ),
    
      tabItem(tabName = "breed_map",
              selectizeInput("selectedBreed",
                             "Select Dog Breed",
                             c("All" = "num_dgs", names(shape@data[19:length(shape@data)]))),
              fluidRow(box(leafletOutput("breed_density")),
                        box(plotlyOutput("breed_diversity"))),
              fluidRow(box(plotlyOutput("breed_bar"), width = 12))
              ),
      
      tabItem(tabName = "names",
              selectizeInput("selectedBreed2",
                             "Select Dog Breed",
                             c(names(shape@data[19:length(shape@data)]))),
              fluidRow(box(plotOutput("nameCloud")),
                       box(dataTableOutput("nameTable")))
              ),
      
              #fluidRow(box(includeHTML("./www/bubble_chart.html"))))
      
      tabItem(tabName = "borough_stats",
              selectizeInput("selectedBorough",
                             "Select Borough", c("Brooklyn", "Bronx", "Manhattan", "Queens", "Staten Island")),
              fluidRow(plotlyOutput("top_breeds")),
                       #box(plotlyOutput("density"))),
              #fluidRow(plotlyOutput("all_breeds")),
              fluidRow(dataTableOutput("zipTable"))

              ),
  
      tabItem(tabName = "bubble",
              includeHTML("./www/bubble_chart.html"))
    )
  )
 )
)