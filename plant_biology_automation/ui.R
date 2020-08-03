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
    titlePanel("HPLC report reader"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            fileInput("input_sampool_archive", "Upload Sample-Pool Archive File",
                      multiple = FALSE,
                      accept = c(".zip")), 
            
            actionButton("unzip", "Unzip Files"),
            h3(), 
            print("--------------"),
            h3(),
            
            actionButton("compute","Prepare the excel file")
        ),

        # Show a plot of the generated distribution
        mainPanel(
            tableOutput(outputId = "excel_table"),
            downloadButton("downloadData", "Download"),
            tableOutput(outputId = "list_of_pdfs")
            #plotOutput("distPlot")
        )
    )
))
