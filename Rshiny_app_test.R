# Environmental Data Analysis RShiny App
# Color scheme: Gold (#f1b61a), Dark Blue (#00517c), White, Black, Green (#31b949)

# Load required libraries
library(shiny)
library(shinydashboard)
library(DT)
library(ggplot2)
library(dplyr)
library(plotly)

# Custom CSS for color scheme
custom_css <- "
  .skin-blue .main-header .logo {
    background-color: #00517c;
  }
  .skin-blue .main-header .navbar {
    background-color: #00517c;
  }
  .skin-blue .main-header .navbar .sidebar-toggle:hover {
    background-color: #004169;
  }
  .skin-blue .main-sidebar {
    background-color: #00517c;
  }
  .skin-blue .main-sidebar .sidebar .sidebar-menu .active a {
    background-color: #004169;
  }
  .box.box-solid.box-primary {
    border: 1px solid #00517c;
  }
  .box.box-solid.box-primary > .box-header {
    background-color: #00517c;
  }
  .btn-primary {
    background-color: #00517c;
    border-color: #004169;
  }
  .btn-success {
    background-color: #31b949;
    border-color: #2ca641;
  }
  .btn-warning {
    background-color: #f1b61a;
    border-color: #e0a80f;
  }
  .nav-tabs-custom > .nav-tabs > li.active {
    border-top-color: #00517c;
  }
  .shiny-input-container {
    margin-bottom: 15px;
  }
  .info-box {
    min-height: 100px;
  }
  .info-box-icon {
    height: 100px;
    line-height: 100px;
  }
  .info-box-content {
    padding-top: 5px;
    padding-bottom: 5px;
  }
"

# UI
ui <- dashboardPage(
  skin = "blue",
  
  # Header
  dashboardHeader(
    title = "Environmental Analysis",
    titleWidth = 300
  ),
  
  # Sidebar
  dashboardSidebar(
    width = 300,
    tags$head(tags$style(HTML(custom_css))),
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Data Import", tabName = "data_import", icon = icon("file-upload")),
      menuItem("Data Exploration", tabName = "data_exploration", icon = icon("search")),
      menuItem("Statistical Analysis", tabName = "statistical_analysis", icon = icon("chart-bar")),
      menuItem("Spatial Analysis", tabName = "spatial_analysis", icon = icon("map")),
      menuItem("Time Series", tabName = "time_series", icon = icon("chart-line")),
      menuItem("Reports", tabName = "reports", icon = icon("file-alt"))
    )
  ),
  
  # Body
  dashboardBody(
    tabItems(
      # Dashboard tab
      tabItem(
        tabName = "dashboard",
        fluidRow(
          box(
            title = "Welcome to Environmental Analysis Tool", 
            status = "primary", 
            solidHeader = TRUE,
            width = 12,
            p("This application allows you to analyze environmental datasets through various methods."),
            p("Use the sidebar to navigate between different analysis modules.")
          )
        ),
        fluidRow(
          valueBox(
            value = "Data Import", 
            subtitle = "Upload and manage your datasets", 
            icon = icon("file-upload"),
            color = "blue",
            width = 4
          ),
          valueBox(
            value = "Analysis", 
            subtitle = "Run statistical and spatial analyses", 
            icon = icon("chart-bar"),
            color = "green",
            width = 4
          ),
          valueBox(
            value = "Reports", 
            subtitle = "Generate downloadable reports", 
            icon = icon("file-alt"),
            color = "yellow",
            width = 4
          )
        )
      ),
      
      # Data Import tab
      tabItem(
        tabName = "data_import",
        fluidRow(
          box(
            title = "Import Data", 
            status = "primary", 
            solidHeader = TRUE,
            collapsible = TRUE,
            width = 12,
            fileInput("file_input", "Choose CSV File", accept = c(".csv", ".tsv")),
            checkboxInput("header", "Header", TRUE),
            radioButtons("sep", "Separator",
                         choices = c(Comma = ",", Semicolon = ";", Tab = "\t"),
                         selected = ","),
            actionButton("load_data", "Load Data", class = "btn-success")
          )
        ),
        fluidRow(
          box(
            title = "Data Preview", 
            status = "primary", 
            solidHeader = TRUE,
            width = 12,
            DTOutput("data_preview")
          )
        )
      ),
      
      # Data Exploration tab
      tabItem(
        tabName = "data_exploration",
        fluidRow(
          box(
            title = "Variable Selection", 
            status = "primary", 
            solidHeader = TRUE,
            width = 4,
            uiOutput("var_selection")
          ),
          tabBox(
            title = "Data Exploration",
            width = 8,
            tabPanel("Summary Statistics", verbatimTextOutput("summary_stats")),
            tabPanel("Data Distribution", plotlyOutput("distribution_plot"))
          )
        ),
        fluidRow(
          box(
            title = "Data Visualization", 
            status = "primary", 
            solidHeader = TRUE,
            width = 12,
            fluidRow(
              column(4, uiOutput("x_var_select")),
              column(4, uiOutput("y_var_select")),
              column(4, uiOutput("color_var_select"))
            ),
            selectInput("plot_type", "Plot Type",
                        choices = c("Scatter Plot", "Box Plot", "Bar Chart", "Histogram", "Density Plot")),
            plotlyOutput("exploration_plot"),
            downloadButton("download_plot", "Download Plot", class = "btn-warning")
          )
        )
      ),
      
      # Statistical Analysis tab
      tabItem(
        tabName = "statistical_analysis",
        fluidRow(
          box(
            title = "Analysis Type", 
            status = "primary", 
            solidHeader = TRUE,
            width = 4,
            selectInput("analysis_type", "Choose Analysis",
                        choices = c("Correlation Analysis", "Regression Analysis", "ANOVA", "Principal Component Analysis")),
            uiOutput("analysis_params")
          ),
          box(
            title = "Analysis Results", 
            status = "primary", 
            solidHeader = TRUE,
            width = 8,
            verbatimTextOutput("analysis_results"),
            plotlyOutput("analysis_plot")
          )
        )
      ),
      
      # Spatial Analysis tab
      tabItem(
        tabName = "spatial_analysis",
        fluidRow(
          box(
            title = "Spatial Parameters", 
            status = "primary", 
            solidHeader = TRUE,
            width = 4,
            uiOutput("spatial_params"),
            selectInput("map_type", "Map Type",
                        choices = c("Point Map", "Choropleth", "Heat Map", "Bubble Map")),
            actionButton("run_spatial", "Run Analysis", class = "btn-success")
          ),
          box(
            title = "Map Visualization", 
            status = "primary", 
            solidHeader = TRUE,
            width = 8,
            uiOutput("map_output")
          )
        )
      ),
      
      # Time Series tab
      tabItem(
        tabName = "time_series",
        fluidRow(
          box(
            title = "Time Series Parameters", 
            status = "primary", 
            solidHeader = TRUE,
            width = 4,
            uiOutput("time_var_select"),
            uiOutput("value_var_select"),
            selectInput("ts_analysis", "Analysis Method",
                        choices = c("Trend Analysis", "Seasonality Analysis", "Forecasting", "Anomaly Detection")),
            actionButton("run_timeseries", "Run Analysis", class = "btn-success")
          ),
          box(
            title = "Time Series Visualization", 
            status = "primary", 
            solidHeader = TRUE,
            width = 8,
            plotlyOutput("timeseries_plot"),
            verbatimTextOutput("timeseries_results")
          )
        )
      ),
      
      # Reports tab
      tabItem(
        tabName = "reports",
        fluidRow(
          box(
            title = "Report Generation", 
            status = "primary", 
            solidHeader = TRUE,
            width = 12,
            checkboxGroupInput("report_sections", "Include Sections:",
                               choices = c("Data Summary", "Exploratory Analysis", "Statistical Results", 
                                           "Spatial Analysis", "Time Series Analysis")),
            radioButtons("report_format", "Report Format",
                         choices = c("PDF", "HTML", "Word"),
                         selected = "HTML"),
            textInput("report_title", "Report Title", value = "Environmental Data Analysis Report"),
            actionButton("generate_report", "Generate Report", class = "btn-warning"),
            downloadButton("download_report", "Download Report", class = "btn-success")
          )
        )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  # Reactive values to store data and analysis results
  rv <- reactiveValues(
    data = NULL,
    analysis_results = NULL,
    spatial_results = NULL,
    timeseries_results = NULL
  )
  
  # Load data when button is clicked
  observeEvent(input$load_data, {
    req(input$file_input)
    
    # Read the file
    rv$data <- read.csv(input$file_input$datapath, 
                        header = input$header,
                        sep = input$sep)
    
    # Display success message
    showNotification("Data loaded successfully!", type = "success")
  })
  
  # Data preview
  output$data_preview <- renderDT({
    req(rv$data)
    datatable(rv$data, options = list(pageLength = 10, scrollX = TRUE))
  })
  
  # Dynamic UI elements based on loaded data
  output$var_selection <- renderUI({
    req(rv$data)
    selectizeInput("selected_vars", "Select Variables for Analysis",
                   choices = names(rv$data),
                   multiple = TRUE,
                   options = list(placeholder = "Select variables"))
  })
  
  output$x_var_select <- renderUI({
    req(rv$data)
    selectInput("x_var", "X Variable",
                choices = names(rv$data))
  })
  
  output$y_var_select <- renderUI({
    req(rv$data)
    selectInput("y_var", "Y Variable",
                choices = names(rv$data))
  })
  
  output$color_var_select <- renderUI({
    req(rv$data)
    selectInput("color_var", "Color By (optional)",
                choices = c("None", names(rv$data)))
  })
  
  # Summary statistics
  output$summary_stats <- renderPrint({
    req(rv$data, input$selected_vars)
    if(length(input$selected_vars) > 0) {
      summary(rv$data[, input$selected_vars, drop = FALSE])
    } else {
      cat("Please select variables to view summary statistics.")
    }
  })
  
  # Distribution plot
  output$distribution_plot <- renderPlotly({
    req(rv$data, input$selected_vars)
    if(length(input$selected_vars) > 0) {
      # Select only numeric variables for histogram display
      numeric_vars <- input$selected_vars[sapply(rv$data[, input$selected_vars, drop = FALSE], is.numeric)]
      
      if(length(numeric_vars) > 0) {
        # Use the first numeric variable for demonstration
        var_to_plot <- numeric_vars[1]
        
        p <- ggplot(rv$data, aes_string(x = var_to_plot)) +
          geom_histogram(fill = "#f1b61a", color = "#00517c", bins = 30) +
          theme_minimal() +
          labs(title = paste("Distribution of", var_to_plot),
               x = var_to_plot,
               y = "Count") +
          theme(
            plot.title = element_text(color = "#00517c", size = 16),
            axis.title = element_text(color = "#00517c"),
            panel.grid = element_line(color = "gray90")
          )
        
        ggplotly(p)
      } else {
        plot_ly() %>% 
          layout(title = "No numeric variables selected")
      }
    } else {
      plot_ly() %>% 
        layout(title = "Please select variables to view distributions")
    }
  })
  
  # Exploration plot
  output$exploration_plot <- renderPlotly({
    req(rv$data, input$x_var, input$y_var, input$plot_type)
    
    # Check if variables exist and are of appropriate type
    if(!input$x_var %in% names(rv$data) || !input$y_var %in% names(rv$data)) {
      return(plot_ly() %>% layout(title = "Variables not found in dataset"))
    }
    
    p <- ggplot(rv$data)
    
    # Apply color if selected and not "None"
    if(input$color_var != "None" && input$color_var %in% names(rv$data)) {
      p <- p + aes_string(color = input$color_var)
    }
    
    # Create different plot types
    if(input$plot_type == "Scatter Plot") {
      p <- p + aes_string(x = input$x_var, y = input$y_var) +
        geom_point(size = 3, alpha = 0.7)
    } else if(input$plot_type == "Box Plot") {
      p <- p + aes_string(x = input$x_var, y = input$y_var) +
        geom_boxplot(fill = "#f1b61a")
    } else if(input$plot_type == "Bar Chart") {
      p <- p + aes_string(x = input$x_var, y = input$y_var) +
        geom_col(fill = "#31b949")
    } else if(input$plot_type == "Histogram") {
      p <- p + aes_string(x = input$x_var) +
        geom_histogram(fill = "#f1b61a", color = "#00517c", bins = 30)
    } else if(input$plot_type == "Density Plot") {
      p <- p + aes_string(x = input$x_var) +
        geom_density(fill = "#31b949", alpha = 0.5)
    }
    
    # Apply theme and labels
    p <- p + theme_minimal() +
      labs(title = paste(input$plot_type, "of", input$x_var, if(input$plot_type != "Histogram" && input$plot_type != "Density Plot") paste("vs", input$y_var) else ""),
           x = input$x_var,
           y = if(input$plot_type != "Histogram" && input$plot_type != "Density Plot") input$y_var else "Frequency") +
      theme(
        plot.title = element_text(color = "#00517c", size = 16),
        axis.title = element_text(color = "#00517c"),
        panel.grid = element_line(color = "gray90")
      )
    
    ggplotly(p)
  })
  
  # Statistical Analysis parameters (dynamic based on analysis type)
  output$analysis_params <- renderUI({
    req(rv$data, input$analysis_type)
    
    if(input$analysis_type == "Correlation Analysis") {
      tagList(
        selectizeInput("corr_vars", "Select Variables",
                       choices = names(rv$data)[sapply(rv$data, is.numeric)],
                       multiple = TRUE),
        selectInput("corr_method", "Correlation Method",
                    choices = c("pearson", "spearman", "kendall")),
        actionButton("run_corr", "Run Correlation", class = "btn-success")
      )
    } else if(input$analysis_type == "Regression Analysis") {
      tagList(
        selectInput("reg_dependent", "Dependent Variable",
                    choices = names(rv$data)[sapply(rv$data, is.numeric)]),
        selectizeInput("reg_independent", "Independent Variables",
                       choices = names(rv$data)[sapply(rv$data, is.numeric)],
                       multiple = TRUE),
        actionButton("run_regression", "Run Regression", class = "btn-success")
      )
    } else if(input$analysis_type == "ANOVA") {
      tagList(
        selectInput("anova_dependent", "Dependent Variable",
                    choices = names(rv$data)[sapply(rv$data, is.numeric)]),
        selectInput("anova_factor", "Factor Variable",
                    choices = names(rv$data)[sapply(rv$data, is.factor) | sapply(rv$data, is.character)]),
        actionButton("run_anova", "Run ANOVA", class = "btn-success")
      )
    } else if(input$analysis_type == "Principal Component Analysis") {
      tagList(
        selectizeInput("pca_vars", "Select Variables",
                       choices = names(rv$data)[sapply(rv$data, is.numeric)],
                       multiple = TRUE),
        numericInput("pca_components", "Number of Components", 
                     value = 2, min = 1, max = 10),
        actionButton("run_pca", "Run PCA", class = "btn-success")
      )
    }
  })
  
  # Run correlation analysis
  observeEvent(input$run_corr, {
    req(rv$data, input$corr_vars, input$corr_method)
    
    if(length(input$corr_vars) >= 2) {
      # Extract numeric variables
      corr_data <- rv$data[, input$corr_vars, drop = FALSE]
      
      # Compute correlation
      corr_matrix <- cor(corr_data, use = "pairwise.complete.obs", 
                         method = input$corr_method)
      
      # Store results
      rv$analysis_results <- list(
        type = "correlation",
        method = input$corr_method,
        matrix = corr_matrix
      )
      
      # Create visualization
      corr_df <- as.data.frame(as.table(corr_matrix))
      names(corr_df) <- c("Var1", "Var2", "Correlation")
      
      p <- ggplot(corr_df, aes(x = Var1, y = Var2, fill = Correlation)) +
        geom_tile() +
        scale_fill_gradient2(low = "#00517c", mid = "white", high = "#f1b61a", 
                             midpoint = 0, limits = c(-1, 1)) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1),
              plot.title = element_text(color = "#00517c")) +
        labs(title = paste("Correlation Matrix (", input$corr_method, ")"),
             x = "", y = "")
      
      output$analysis_plot <- renderPlotly({
        ggplotly(p)
      })
      
      output$analysis_results <- renderPrint({
        print(corr_matrix)
      })
    } else {
      showNotification("Select at least two variables for correlation analysis", type = "warning")
    }
  })
  
  # Run regression analysis
  observeEvent(input$run_regression, {
    req(rv$data, input$reg_dependent, input$reg_independent)
    
    if(length(input$reg_independent) >= 1) {
      # Formulate regression formula
      formula_str <- paste(input$reg_dependent, "~", 
                           paste(input$reg_independent, collapse = " + "))
      formula_obj <- as.formula(formula_str)
      
      # Run regression
      reg_model <- lm(formula_obj, data = rv$data)
      
      # Store results
      rv$analysis_results <- list(
        type = "regression",
        formula = formula_str,
        model = reg_model,
        summary = summary(reg_model)
      )
      
      # Create visualization - actual vs predicted
      predictions <- predict(reg_model)
      pred_df <- data.frame(
        Actual = rv$data[[input$reg_dependent]],
        Predicted = predictions
      )
      
      p <- ggplot(pred_df, aes(x = Actual, y = Predicted)) +
        geom_point(color = "#31b949", alpha = 0.7) +
        geom_abline(intercept = 0, slope = 1, color = "#00517c", 
                    linetype = "dashed", size = 1) +
        theme_minimal() +
        labs(title = "Actual vs Predicted Values",
             x = "Actual Values", 
             y = "Predicted Values") +
        theme(plot.title = element_text(color = "#00517c"))
      
      output$analysis_plot <- renderPlotly({
        ggplotly(p)
      })
      
      output$analysis_results <- renderPrint({
        summary(reg_model)
      })
    } else {
      showNotification("Select at least one independent variable", type = "warning")
    }
  })
  
  # Run ANOVA
  observeEvent(input$run_anova, {
    req(rv$data, input$anova_dependent, input$anova_factor)
    
    # Run ANOVA
    formula_str <- paste(input$anova_dependent, "~", input$anova_factor)
    formula_obj <- as.formula(formula_str)
    
    anova_model <- aov(formula_obj, data = rv$data)
    
    # Store results
    rv$analysis_results <- list(
      type = "anova",
      formula = formula_str,
      model = anova_model,
      summary = summary(anova_model)
    )
    
    # Create visualization - boxplot by group
    p <- ggplot(rv$data, aes_string(x = input$anova_factor, y = input$anova_dependent)) +
      geom_boxplot(fill = "#f1b61a", color = "#00517c") +
      theme_minimal() +
      labs(title = paste("ANOVA:", input$anova_dependent, "by", input$anova_factor),
           x = input$anova_factor, 
           y = input$anova_dependent) +
      theme(plot.title = element_text(color = "#00517c"),
            axis.text.x = element_text(angle = 45, hjust = 1))
    
    output$analysis_plot <- renderPlotly({
      ggplotly(p)
    })
    
    output$analysis_results <- renderPrint({
      summary(anova_model)
    })
  })
  
  # Run PCA
  observeEvent(input$run_pca, {
    req(rv$data, input$pca_vars, input$pca_components)
    
    if(length(input$pca_vars) >= 2) {
      # Extract numeric variables
      pca_data <- rv$data[, input$pca_vars, drop = FALSE]
      
      # Run PCA
      pca_result <- prcomp(pca_data, scale. = TRUE)
      
      # Store results
      rv$analysis_results <- list(
        type = "pca",
        model = pca_result,
        summary = summary(pca_result)
      )
      
      # Create visualization - biplot or scatterplot
      scores <- as.data.frame(pca_result$x)
      
      p <- ggplot(scores, aes(x = PC1, y = PC2)) +
        geom_point(color = "#31b949", alpha = 0.7) +
        theme_minimal() +
        labs(title = "PCA: First Two Principal Components",
             x = paste("PC1 (", round(summary(pca_result)$importance[2, 1] * 100, 1), "%)"),
             y = paste("PC2 (", round(summary(pca_result)$importance[2, 2] * 100, 1), "%)")) +
        theme(plot.title = element_text(color = "#00517c"))
      
      output$analysis_plot <- renderPlotly({
        ggplotly(p)
      })
      
      output$analysis_results <- renderPrint({
        summary(pca_result)
      })
    } else {
      showNotification("Select at least two variables for PCA", type = "warning")
    }
  })
  
  # Time series tab parameters
  output$time_var_select <- renderUI({
    req(rv$data)
    # Look for date/time columns
    date_cols <- names(rv$data)[sapply(rv$data, function(x) inherits(x, "Date") || inherits(x, "POSIXt") || is.character(x))]
    if(length(date_cols) == 0) date_cols <- names(rv$data)
    
    selectInput("time_var", "Time Variable", choices = date_cols)
  })
  
  output$value_var_select <- renderUI({
    req(rv$data)
    # Look for numeric columns
    num_cols <- names(rv$data)[sapply(rv$data, is.numeric)]
    
    selectInput("value_var", "Value Variable", choices = num_cols)
  })
  
  # Run time series analysis
  observeEvent(input$run_timeseries, {
    req(rv$data, input$time_var, input$value_var, input$ts_analysis)
    
    # Basic time series plot
    p <- ggplot(rv$data, aes_string(x = input$time_var, y = input$value_var)) +
      geom_line(color = "#00517c") +
      geom_point(color = "#f1b61a", size = 2) +
      theme_minimal() +
      labs(title = paste("Time Series:", input$value_var, "over time"),
           x = input$time_var, 
           y = input$value_var) +
      theme(plot.title = element_text(color = "#00517c"))
    
    output$timeseries_plot <- renderPlotly({
      ggplotly(p)
    })
    
    # Simple summary (can be expanded with actual time series analysis)
    output$timeseries_results <- renderPrint({
      cat("Time Series Analysis: ", input$ts_analysis, "\n\n")
      cat("Summary Statistics for", input$value_var, ":\n")
      print(summary(rv$data[[input$value_var]]))
    })
  })
  
  # Placeholder for spatial parameters
  output$spatial_params <- renderUI({
    req(rv$data)
    
    # Look for potential lat/lon columns
    lat_cols <- names(rv$data)[grepl("lat|latitude", names(rv$data), ignore.case = TRUE)]
    lon_cols <- names(rv$data)[grepl("lon|longitude", names(rv$data), ignore.case = TRUE)]
    
    tagList(
      if(length(lat_cols) > 0) {
        selectInput("lat_var", "Latitude Variable", 
                    choices = names(rv$data), 
                    selected = lat_cols[1])
      } else {
        selectInput("lat_var", "Latitude Variable", 
                    choices = names(rv$data))
      },
      
      if(length(lon_cols) > 0) {
        selectInput("lon_var", "Longitude Variable", 
                    choices = names(rv$data), 
                    selected = lon_cols[1])
      } else {
        selectInput("lon_var", "Longitude Variable", 
                    choices = names(rv$data))
      },
      
      selectInput("value_var_map", "Value Variable (for color/size)", 
                  choices = names(rv$data)[sapply(rv$data, is.numeric)])
    )
  })
  
  # Placeholder for spatial visualization
  output$map_output <- renderUI({
    HTML("<div class='alert alert-info'>Spatial visualization would appear here. This would typically use packages like leaflet, tmap, or sf for spatial data visualization.</div>")
  })
  
  # Download plot
  output$download_plot <- downloadHandler(
    filename = function() {
      paste("plot-", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      # This is a placeholder. In a real app, you would save the current plot
      ggsave(file, plot = last_plot(), device = "png", width = 10, height = 7)
    }
  )
  
  # Download report
  output$download_report <- downloadHandler(
    filename = function() {
      paste(input$report_title, "-", Sys.Date(), ".", 
            switch(input$report_format, PDF = "pdf", HTML = "html", Word = "docx"), 
            sep = "")
    },
    content = function(file) {
      # This is a placeholder. In a real app, you would generate reports using rmarkdown
      # For example:
      # rmarkdown::render('report_template.Rmd', 
      #                   output_file = file,
      #                   params = list(data = rv$data, 
      #                                 analysis = rv$analysis_results))
    }
  )
}

# Run the application 
shinyApp(ui = ui, server = server)