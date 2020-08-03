#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(utils)
library(tidyverse)
library(pdftools)
library(tidytext)
library(tabulizer)
library(lubridate)
library(openxlsx)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    global <- reactiveValues(datapath = getwd())
    
    observeEvent(input$unzip,{
        
        utils::unzip(zipfile = input$zip_file$datapath)
        
        # table for information of the user
        output$list_of_xlsx <- renderTable({

            unzipped_path <- global$datapath
            
            # print(str_glue("this is the location of pdf files: {n}",
            #                n = unzipped_path))
            
            print(str_glue("xlsx files: {n}",n = list.files(unzipped_path, pattern = ".xlsx")))
            
            list.files(unzipped_path,  pattern = ".xlsx") %>% 
                as_tibble() %>% 
                set_names("Unzipped Files") %>% 
                mutate(id = row_number(.)) %>% 
                select(id, everything())
        })
        
    })
 
    
    
    # aggregation 
    aggregate_table <- eventReactive(input$compute,{
        #path <- "~/Downloads/batch_files/"
        
        #excel_path <- str_c(path)
        excell_files <- list.files(pattern = ".xlsx")
        
        print(str_glue("excel files list: {n}", n = excell_files ))
        
        excel_tables <- map_dfr(.x = excell_files,
                                .f = ~openxlsx::read.xlsx(xlsxFile = .x)
        ) 
        
        print(str_glue("number of rows: {n}",n = nrow(excel_tables)))

        # new section based on chaotic batches
        all_samples_tbl <- 
            excel_tables %>%
            arrange(name)
        
        all_samples_tbl <- 
            all_samples_tbl %>% 
            separate(col = name , into = c("name","iteration"), remove = F) %>% 
            select(name, iteration, everything()) 
        
        samples_iterations_tbl <- 
            all_samples_tbl %>% 
            pivot_wider(id_cols = name ,
                        names_from = iteration,
                        values_from = c(PUT,HTD,SPD,SPM) ) 
        
        print(str_glue("PUT Coef: {n}",n = input$PUT_coefficient))
        print(str_glue("SPD Coef: {n}",n = input$SPD_coefficient))
        print(str_glue("SPM Coef: {n}",n = input$SPM_coefficient))
        
        
        samples_iterations_computations_tbl <- 
  
            samples_iterations_tbl %>% 
            mutate(grFW_PUT_1 = (input$PUT_coefficient * PUT_1)/HTD_1,
                   grFW_PUT_2 = (input$PUT_coefficient * PUT_2)/HTD_2, 
                   grFW_PUT_3 = (input$PUT_coefficient * PUT_3)/HTD_3
            ) %>% 
            mutate(grFW_SPD_1 = (input$SPD_coefficient * SPD_1)/HTD_1, 
                   grFW_SPD_2 = (input$SPD_coefficient * SPD_2)/HTD_2,
                   grFW_SPD_3 = (input$SPD_coefficient * SPD_3)/HTD_3,
            ) %>% 
            mutate(grFW_SPM_1 = (input$SPM_coefficient * SPM_1)/HTD_1,
                   grFW_SPM_2 = (input$SPM_coefficient * SPM_2)/HTD_2,
                   grFW_SPM_3 = (input$SPM_coefficient * SPM_3)/HTD_3
            ) %>% 
            
            # row-wise Avg of grFW
            mutate(
                avg_grFW_PUT = pmap_dbl(.l = list(grFW_PUT_1, grFW_PUT_2, grFW_PUT_3), 
                                        .f = ~ sum(..1,..2,..3)/3), 
                avg_grFW_SPD = pmap_dbl(.l = list(grFW_SPD_1, grFW_SPD_2, grFW_SPD_3), 
                                        .f = ~ sum(..1,..2,..3)/3),
                avg_grFW_SPM = pmap_dbl(.l = list(grFW_SPM_1, grFW_SPM_2, grFW_SPM_3), 
                                        .f = ~ sum(..1,..2,..3)/3)
            ) %>% 
            
            # SD columns 
            mutate(
                sd_grFW_PUT = pmap_dbl(.l = list(grFW_PUT_1, grFW_PUT_2, grFW_PUT_3), 
                                       .f = ~ sd(c(..1,..2,..3))), 
                sd_grFW_SPD = pmap_dbl(.l = list(grFW_SPD_1, grFW_SPD_2, grFW_SPD_3), 
                                       .f = ~ sd(c(..1,..2,..3))),
                sd_grFW_SPM = pmap_dbl(.l = list(grFW_SPM_1, grFW_SPM_2, grFW_SPM_3), 
                                       .f = ~ sd(c(..1,..2,..3)))
            ) %>% 
            
            # SE columns 
            mutate(
                se_grFW_PUT = (sd_grFW_PUT / 1.73), 
                se_grFW_SPD = (sd_grFW_SPD / 1.73),
                se_grFW_SPM = (sd_grFW_SPM / 1.73)
            )
        
        file.remove(list.files(pattern = ".xlsx"))
        #print("list of files: {n}", n = list.files())
        
        return(samples_iterations_computations_tbl)
        
        
    })
   
    
    #download button
    
    
    output$aggregate_table <- renderTable({
        aggregate_table()
    })
    
    observeEvent(input$compute,{
        output$downloadData <- downloadHandler(
            filename = function() {
                paste("aggregate_table", ".xlsx", sep = "")
            },
            content = function(file) {
                openxlsx::write.xlsx(x = aggregate_table(),
                                     file = file)
            }
        )
        
    })

})
