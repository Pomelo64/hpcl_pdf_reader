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

#path = "~/Downloads/plant_biology/plant_biology/"

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    global <- reactiveValues(datapath = getwd())


# start of unzipping ------------------------------------------------------

    observeEvent(input$unzip,{
        #require(input$input_sampool_archive)
        
        # utils::unzip(zipfile = input$input_sampool_archive$datapath,
        #              exdir = "sampool_data")
        
        utils::unzip(zipfile = input$input_sampool_archive$datapath)
        
        output$list_of_pdfs <- renderTable({

            # unzipped_path <- str_c(global$datapath,"/sampool_data")
            #unzipped_path <- "/sampool_data"
            unzipped_path <- global$datapath
            
            print("this is the location of pdf files")
            print(unzipped_path)
            
            list.files(path = unzipped_path, pattern = ".pdf") %>% 
                as_tibble() %>% 
                set_names("Unzipped Files") %>% 
                mutate(id = row_number(.)) %>% 
                select(id, everything())
        })
        
    })
# start of computation ---------------------------------------------------------------

    excel_table <- eventReactive(input$compute,{
        
        #path = str_c(global$datapath,"/sampool_data/")
        print(list.files())
        path <- ""
        
        
        # read table function
        read_table_from_pdf <- function(path,file_name,area){
            pdf_table <- tabulizer::extract_tables(file = str_c(path,file_name),
                                                   pages = 1 , output = "data.frame",
                                                   guess = FALSE, area = list(area))[[1]]
            
            # to check whether the table is read properly
            default_columns <- c("Peak","Time","Area","Component", 
                                 "Height", "Area.1","Area.Height")
            
            if (!identical(colnames(pdf_table),default_columns)){
                pdf_table <-
                    tabulizer::extract_tables(file = str_c(path,file_name),
                                              pages = 1 , output = "data.frame",
                                              guess = TRUE)[[1]]
            }
            
            return(pdf_table)
            
        }
        
        
        
        # for sample , area = c(426,13,688,322)
        # for pool, area = c(425,14,609,322)
        
        
        # reading pool table
        
        pool_tbl <- read_table_from_pdf(path = path,
                                        file_name = "Pool.pdf",
                                        area =  c(425,14,609,322) )
        
        print(str_glue("pool dimensions: {n}",n = dim(pool_tbl)))
        
        
        
        pool_components_tbl <-
            pool_tbl %>%
            #select(-X, -X.1, -X.2) %>%
            slice(-1) %>%
            set_names(c("peak_num", "time",
                        "area","component_name",
                        "height","area_perc", "area_to_height")) %>%
            as_tibble() %>%
            #filter(time %>% between(8,13) ) %>%
            filter(component_name %in% c("PUT","HTD","SPD","SPM")) %>%
            select(time,component_name) %>%
            mutate(time = time %>% as.numeric)
        
        print(str_glue("pool columns: {n}", n = colnames(pool_components_tbl)))
        
        
        # reading sample tables
        print(str_glue("list of pdf files {n}",n = list.files(pattern = ".pdf")))
        
        
        non_pool_files_vec <-
            !(list.files(pattern = ".pdf") %>%
                  str_detect(pattern = "Pool.pdf"))
        
        print(str_glue("must be many TRUE and one FALSE: {n}",n = non_pool_files_vec))


        sample_files_vec <- list.files(pattern = ".pdf")[non_pool_files_vec]
    
        print(str_glue("list of sample pdf files, {d}",
                       d = sample_files_vec))
        
        sample_tables_list <- map(.x = sample_files_vec ,
                                  .f = read_table_from_pdf,
                                  path = path,
                                  area = c(426,13,688,322))
        # 
        print(str_glue("must be the number of samples: {n}",
              n = length(sample_tables_list)))
    
        # 
        clean_sample <- function(sample_df){
            sample_df %>%
                slice(-1) %>%
                select(Time,Area) %>%
                set_names(c("time","area")) %>%
                mutate(time = time %>% as.numeric,
                       area = area %>% as.numeric)
        }
        # 
        sample_tables_list_cleaned <- map(.x = sample_tables_list ,
                                          .f = clean_sample)

        print(str_glue("how many cleaned samples? {n}",
                       n = length(sample_tables_list_cleaned)))
        # final_table: component tables for each sample-pool pair -------------------------------------------------------------

        find_components <- function(sample_table,pool_table){
            sample_table %>%
                #select(time,area,component_name) %>%
                mutate(id = 1) %>%
                full_join( pool_table %>%
                               mutate(id = 1),
                           by = "id") %>%

                mutate(time_diff = abs(time.y - time.x)) %>%
                arrange(time_diff,desc(area)) %>%

                select(time.x,time.y,area, component_name, time_diff) %>%
                arrange(component_name) %>%

                group_by(component_name) %>%

                slice(1) %>%
                ungroup() %>%

                mutate(component_name = as_factor(component_name)) %>%
                mutate(component_name =  fct_relevel(.f = component_name ,
                                                     c("PUT","HTD","SPD","SPM")) ) %>%
                arrange(component_name) %>%
                ungroup() %>%
                rename(sample_time = time.x ,
                       pool_time = time.y )

        }
        # 
        final_tables <-
            map(.x = sample_tables_list_cleaned,
                .f = find_components ,
                pool_table = pool_components_tbl)

        names(final_tables) <-
            sample_files_vec %>%
            str_remove(pattern = ".pdf") %>%
            str_trim()
       
        # writing component tables to xlsx files ---------------------------------------
        # final_tables_directory <- str_c(path,"component_tables_",
        #                                 lubridate::today(),"/")

        # dir.create(path = final_tables_directory )


        # write_final_tables <- function(path, table, file_name){
        # 
        #     print(path)
        #     print(str_c(path,file_name))
        # 
        #     openxlsx::write.xlsx(x = table , file = str_c(path,file_name) )
        # 
        # }

        # final_tables_vec <-
        #     sample_files_vec %>%
        #     str_trim() %>%
        #     str_replace(pattern = ".pdf", replacement = ".xlsx")
        # str_extract(".+[^pdf]") %>%
        # str_c(".xlsx")
        # 
        # pwalk(.l = list(final_tables , final_tables_vec , final_tables_directory) , 
        #       .f = ~write_final_tables(path = ..3 , table = ..1 , file_name = ..2) )
        # 
        # 
        # writing all component tables to a single excel sheet ----------------------------------------------------------

        add_name <- function(list_of_tables,name){
            list_of_tables[[name]] %>%
                mutate(name = name) %>%
                select(name, everything())
        }

        overall_tbl <- map_dfr(.x = names(final_tables) ,
                               add_name,
                               list_of_tables = final_tables)

        wide_overall_tbl <-
            overall_tbl %>%
            pivot_wider(id_cols = c("name"),
                        names_from = component_name,
                        values_from = area)

        #overall_table_directory <- str_c(path,"overall_table_",
        #                                 lubridate::today(),"/")

        #dir.create(path = overall_table_directory )

        #openxlsx::write.xlsx(x = wide_overall_tbl,
        #                     file = overall_table_directory)
        
        print(dim(wide_overall_tbl))
        
        file.remove(list.files(pattern = ".pdf"))
        print(str_glue("removal done. list of files: {n}", n = list.files()))
        
        return(wide_overall_tbl)
        
        
    })
    
    output$excel_table <- renderTable({
        excel_table()
    })
    
    observeEvent(input$compute,{
        output$downloadData <- downloadHandler(
            filename = function() {
                paste("components_area_table", ".xlsx", sep = "")
            },
            content = function(file) {
                openxlsx::write.xlsx(x = excel_table(),
                                     file = file)
            }
        )
        
    })
    

  

####
})
