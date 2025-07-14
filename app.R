library(shiny)
library(bslib)
library(DT)

DATA_DIR <- "datasets"

ui <- tabsetPanel(
  nav_panel(title="R SCHARE Tools Test",
            card(
              card_header("How to use")
            )
            
  ),
  nav_panel(
    title="SCHARE Search",
    page_sidebar(
  #title = "SCHARE Dataset Search",
      sidebar = sidebar(
        selectInput(
          "dataset_select",
          "Select Dataset:",
          choices = c("Loading..." = ""),
          selected = ""
        ),
        textInput(
          "search_text",
          "Enter search terms:",
          value = "",
          placeholder = "Type words to search for..."
        ),
        actionButton(
          "search_btn",
          "Search",
          class = "btn-primary"
        ),
        br(),
        br(),
        conditionalPanel(
          condition = "output.has_results",
          downloadButton(
            "download_results",
            "Save Results as CSV",
            class = "btn-success"
          ),
          br(),
          br()
        ),
        helpText("Search will look for matches in 'variable' and 'description' columns. Select a specific dataset or 'All Datasets' to search across all files.")
      ),
      card(
        card_header("Search Results"),
        DT::dataTableOutput("results_table")
      ),
      card(
        card_header("Search Summary"),
        verbatimTextOutput("search_summary")
      )
    )
  ),
  nav_panel(title="SCHARE Explore",
            page_sidebar(
              sidebar = sidebar(
                selectInput(
                  "explore_dataset_select",
                  "Select Dataset:",
                  choices = c("Loading..." = ""),
                  selected = ""
                ),
                verbatimTextOutput("explore_data_output"),
                selectInput(
                  "explore_variable_select",
                  "Select Variables:",
                  choices = c("Loading..." = ""),
                  selected = ""
                ),
                actionButton(
                  "show_data_btn",
                  "Show Data",
                  class = "btn-primary"
                ),
                br(),
                br(),
                conditionalPanel(
                  condition = "output.has_stats",
                  downloadButton(
                    "save_data",
                    "Save Dataset as CSV",
                    class = "btn-success"
                  ),
                  br(),
                  br()
                ),
                helpText("Explore ....Select a dataset, select specific variables or 'All Variables' to display summary statistics.")
              ),
              card(
                card_header("Summary Statistics"),
                DT::dataTableOutput("explore_results_table")
              )
            )
    
  ),
  nav_panel(title="SCHARE Visualize",
            page_sidebar(
              sidebar = sidebar(
                selectInput(
                  "plots_dataset_select",
                  "Select Dataset:",
                  choices = c("Loading..." = ""),
                  selected = ""
                ),
                selectInput(
                  "plots_select_x",
                  "Select X:",
                  choices = c("Loading..." = ""),
                  selected = ""
                ),
                selectInput(
                  "plots_select_y",
                  "Select Y:",
                  choices = c("Loading..." = ""),
                  selected = ""
                ),
                selectInput(
                  "plots_select_z",
                  "Select Z:",
                  choices = c("Loading..." = ""),
                  selected = ""
                ),
                actionButton(
                  "show_plots_btn",
                  "Show Plot",
                  class = "btn-primary"
                ),
                conditionalPanel(
                  condition = "output.has_plot",
                  downloadButton(
                    "save_plot",
                    "Download Plot",
                    class = "btn-success"
                  )
                ),
                helpText("Plots ... add helper")
              ),
              card(
                card_header("Plot")
              )
            )
            
  )
  
  
)

server <- function(input, output, session) {
  
  load_file_title_mapping <- function() {
    mapping_file <- file.path(DATA_DIR, "MainTableDatasetsDictionary_3.csv")
    
    if (file.exists(mapping_file)) {
      tryCatch({
        mapping_df <- read.csv(mapping_file, stringsAsFactors = FALSE)
        
        colnames(mapping_df) <- trimws(colnames(mapping_df))
        
        col_names <- tolower(colnames(mapping_df))
        file_col <- which(col_names %in% c("file_name"))
        title_col <- which(col_names %in% c("dataset_title"))
        
        if (length(file_col) > 0 && length(title_col) > 0) {
          file_col_name <- colnames(mapping_df)[file_col[1]]
          title_col_name <- colnames(mapping_df)[title_col[1]]
          
          mapping <- setNames(
            as.character(mapping_df[[title_col_name]]),
            as.character(mapping_df[[file_col_name]])
          )
          
          return(mapping)
        }
      }, error = function(e) {
        message(paste("Could not read dataset title mapping:", e$message))
      })
    }
    
    return(NULL)
  }
  
  get_title_from_filename <- function(filename, mapping) {
    if (is.null(mapping)) {
      return(filename)  
    }
    
    title <- mapping[filename]
    if (is.na(title) || is.null(title)) {
      return(filename)  
    }
    
    return(title)
  }
  
  
  get_filename_from_title <- function(title, mapping) {
    if (is.null(mapping)) {
      return(NULL)
    }
    
    
    filename <- names(mapping)[mapping == title]
    if (length(filename) == 0) {
      return(NULL)
    }
    
    return(filename[1])  
  }
  
  observe({
    file_title_mapping <- load_file_title_mapping()
    
    if (!is.null(file_title_mapping)) {
      unique_titles <- sort(unique(file_title_mapping))
      
      choices <- c("All Datasets" = "all", setNames(unique_titles, unique_titles))
      
      updateSelectInput(
        session,
        "dataset_select",
        choices = choices,
        selected = "all"
      )
    } else {
      updateSelectInput(
        session,
        "dataset_select",
        choices = c("All Datasets" = "all"),
        selected = "all"
      )
    }
  })
  
  observe({
    file_title_mapping <- load_file_title_mapping()
    
    if (!is.null(file_title_mapping)) {
      unique_titles <- sort(unique(file_title_mapping))
      
      choices <-  c("Select Data" = "", setNames(unique_titles, unique_titles))
      
      updateSelectInput(
        session,
        "explore_dataset_select",
        choices = choices,
        selected = ""
      )
    }
  })
  
  observe({
    file_title_mapping <- load_file_title_mapping()
    
    if (!is.null(file_title_mapping)) {
      unique_titles <- sort(unique(file_title_mapping))
      
      choices <-  c("Select Data" = "", setNames(unique_titles, unique_titles))
      
      updateSelectInput(
        session,
        "plots_dataset_select",
        choices = choices,
        selected = ""
      )
    }
  })
  
  # output$explore_data_output <- renderText({
  #   file_title_mapping <- load_file_title_mapping()
  #   if (!input$explore_dataset_select == "") {
  #     selected_dataset <- get_title_from_filename(input$explore_dataset_select, file_title_mapping)
  #     choices <- c(colnames(selected_dataset))
  #     
  #     updateSelectInput(
  #       session,
  #       "explore_variable_select",
  #       choices = choices,
  #       selected = ""
  #     )
  #     
  #   }
  # })
  # 
  search_datasets <- function(search_terms, selected_dataset) {
    if (is.null(search_terms) || trimws(search_terms) == "") {
      return(data.frame())
    }
    
    
    file_title_mapping <- load_file_title_mapping()
    
    search_words <- trimws(strsplit(trimws(search_terms), "\\s+")[[1]])
    
    if (!dir.exists(DATA_DIR)) {
      return(data.frame())
    }
    
    csv_files <- list.files(DATA_DIR, pattern = "\\.csv$", full.names = TRUE)
    
    if (length(csv_files) == 0) {
      return(data.frame())
    }
    
    results_list <- list()
    
    for (file_path in csv_files) {
      file_name <- basename(file_path)
      
      if (file_name == "file_titles.csv") {
        next
      }
      
      if (selected_dataset != "all") {
        target_filename <- get_filename_from_title(selected_dataset, file_title_mapping)
        if (is.null(target_filename) || file_name != target_filename) {
          next
        }
      }
      
      tryCatch({
        df <- read.csv(file_path, stringsAsFactors = FALSE)
        colnames(df) <- trimws(colnames(df))
        
        col_names <- tolower(colnames(df))
        variable_col <- which(col_names %in% c("variable", "variables"))
        description_col <- which(col_names %in% c("description", "descriptions"))
        
        if (length(variable_col) > 0 && length(description_col) > 0) {
          variable_col <- variable_col[1]
          description_col <- description_col[1]
          
          var_col_name <- colnames(df)[variable_col]
          desc_col_name <- colnames(df)[description_col]
          
          df[[var_col_name]] <- as.character(df[[var_col_name]])
          df[[desc_col_name]] <- as.character(df[[desc_col_name]])
          df[[var_col_name]][is.na(df[[var_col_name]])] <- ""
          df[[desc_col_name]][is.na(df[[desc_col_name]])] <- ""
          
          final_mask <- rep(TRUE, nrow(df))
          
          for (word in search_words) {
            escaped_word <- gsub("([.*+?^${}()|\\[\\]\\\\])", "\\\\\\1", word)
            
            word_mask <- (
              grepl(escaped_word, df[[var_col_name]], ignore.case = TRUE, perl = TRUE) |
                grepl(escaped_word, df[[desc_col_name]], ignore.case = TRUE, perl = TRUE)
            )
            
            final_mask <- final_mask & word_mask
          }
          
          matching_rows <- df[final_mask, ]
          
          if (nrow(matching_rows) > 0) {
            data_title <- get_title_from_filename(file_name, file_title_mapping)
            
            result_df <- data.frame(
              Dataset_Title = rep(data_title, nrow(matching_rows)),
              Variable = matching_rows[[var_col_name]],
              Description = matching_rows[[desc_col_name]],
              stringsAsFactors = FALSE
            )
            
            results_list[[length(results_list) + 1]] <- result_df
          }
        }
      }, error = function(e) {
        message(paste("Could not read file:", file_name, "-", e$message))
      })
    }
    
    if (length(results_list) > 0) {
      combined_results <- do.call(rbind, results_list)
      return(combined_results)
    } else {
      return(data.frame())
    }
  }
  
  search_results <- eventReactive(input$search_btn, {
    search_datasets(input$search_text, input$dataset_select)
  })
  
  output$has_results <- reactive({
    if (input$search_btn == 0) {
      return(FALSE)
    }
    results <- search_results()
    return(nrow(results) > 0)
  })
  outputOptions(output, "has_results", suspendWhenHidden = FALSE)
  
  output$results_table <- DT::renderDataTable({
    if (input$search_btn == 0) {
      return(data.frame())
    }
    
    results <- search_results()
    
    if (nrow(results) == 0) {
      return(data.frame(Message = "No results found"))
    }
    
    DT::datatable(
      results,
      options = list(
        pageLength = 25,
        scrollX = TRUE,
        autoWidth = TRUE,
        columnDefs = list(
          list(width = '200px', targets = 0),  
          list(width = '200px', targets = 1), 
          list(width = '400px', targets = 2)   
        )
      ),
      rownames = FALSE,
      class = "cell-border stripe"
    )
  })
  
  output$download_results <- downloadHandler(
    filename = function() {
      search_term <- gsub("[^A-Za-z0-9_-]", "_", input$search_text)
      if (search_term == "" || search_term == "_") {
        search_term <- "all_results"
      }
      dataset_part <- if (input$dataset_select == "all") "all_datasets" else gsub("[^A-Za-z0-9_-]", "_", input$dataset_select)
      paste0("search_results_", dataset_part, "_", search_term, "_", Sys.Date(), ".csv")
    },
    content = function(file) {
      results <- search_results()
      if (nrow(results) > 0) {
        unique_datasets <- length(unique(results$Dataset_Title))
        metadata <- paste0(
          "# Search Results Export\n",
          "# Search Terms: ", input$search_text, "\n",
          "# Export Date: ", Sys.time(), "\n",
          "# Results Found: ", nrow(results), " variable-description pairs", "\n",
          "# Datasets with Matches:", unique_datasets, "\n",
          "# Files Searched: ", length(list.files(DATA_DIR, pattern = "\\.csv$")) - 1, "\n"
        )
        
        writeLines(metadata, file)
        
        write.table(results, file, sep = ",", row.names = FALSE,
                    col.names = TRUE, append = TRUE, quote = TRUE)
      } else {
        
        writeLines("# No results found for the search terms", file)
      }
    }
  )
  
  
  output$search_summary <- renderText({
    if (input$search_btn == 0) {
      return("Enter search terms and click 'Search' to begin")
    }
    
    results <- search_results()
    
    if (nrow(results) == 0) {
      summary_text <- paste(
        "Search Terms:", ifelse(input$search_text == "", "None", input$search_text),
        "\nResults Found: 0",
        "\nData Files Searched:", length(list.files(DATA_DIR, pattern = "\\.csv$")) - 1,  
        "\nStatus: No matches found"
      )
    } else {
      
      unique_datasets <- length(unique(results$Dataset_Title))
      
      summary_text <- paste(
        "Search Terms:", input$search_text,
        "\nResults Found:", nrow(results), "variable-description pairs",
        "\nDatasets with Matches:", unique_datasets,
        "\nTotal Data Files Searched:", length(list.files(DATA_DIR, pattern = "\\.csv$")),
        "\nStatus: Search completed successfully"
      )
    }
    
    return(summary_text)
  })
}

shinyApp(ui = ui, server = server)


