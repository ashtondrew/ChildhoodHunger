shinyUI(
    fluidPage(
        wellPanel(
            fluidRow(
                h2("Childhood Poverty and Hunger in the Triangle"),
                h5("These data products are in development as part of a ", a(href="http://ncdata4good.github.io/", target="_blank", "NC Data4Good"), " and ",
                   a(href="http://triangleopendataday.com/", target="_blank", "Triangle Open Data Day"), " project.  For further information regarding the project or underlying data, contact: ", a(href="https://www.linkedin.com/in/zortiz", target="_blank", "Dr, Zeydy Ortiz"), ".  For information about this R script and Shiny app, contact: ", a(href="https://www.linkedin.com/in/ashtondrew", target="_blank", "Dr. Ashton Drew"),".")
            )
        ),
        
        tags$hr(),
        
        fluidRow(
            column(2,
                   selectInput("dataChoice", "What information would you like to view in the maps?",
                               c("Percent Families in Poverty",
                                 "Percent People in Poverty",
                                 "Number of Children",
                                 "Number of Children in Poverty",
                                 "Percent Children in Poverty")
                   ),
                   
                   textOutput("summary"),
                   
                   tags$hr(),
                   selectInput("tableChoice", "What information would you like to view in the table?",
                               c("2005 to 2009 Census Data",
                                 "2010 to 2014 Census Data",
                                 "Change from 2009 to 2014")
                   )
            ),
            column(10,
                   fluidRow(
                       column(6,
                              h4("2005 to 2009 Census Tracts"),
                              leafletOutput("Map1")
                       ),
                       column(6,
                              h4("2010 to 2015 Census Tracts"),
                              leafletOutput("Map2")
                       )
                   )
            )
        ),
        
        fluidRow(
            column(12,
                   hr(),
                   h4("Summary Census Data"),
                   DT::dataTableOutput("CensusTable")
            )
        )
    )
)