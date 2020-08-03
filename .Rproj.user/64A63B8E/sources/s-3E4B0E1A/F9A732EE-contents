#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("HPLC Analysis Aggregator"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            fileInput("zip_file", "Upload Report Archive File",
                      multiple = FALSE,
                      accept = c(".zip")), 
            
            actionButton("unzip", "Unzip Files"),
            h3(), 
            print("--------------"),
            h3(),
            numericInput("PUT_coefficient", label = "PUT coefficient", value = 44.3085),
            numericInput("SPD_coefficient", label = "SPD coefficient", value = 27.4505),
            numericInput("SPM_coefficient", label = "SPM coefficient", value = 22.9089),
            h3(), 
            print("--------------"),
            h3(),
            actionButton("compute","Prepare the aggregate report")
        ),

        # Show a plot of the generated distribution
        mainPanel(
            
            tableOutput("aggregate_table"),
            downloadButton("downloadData", "Download"),
            tableOutput("list_of_xlsx")
        )
    )
))
