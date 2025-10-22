# Enhanced Shiny App for OD Data Visualization - Version 1.1.250422
# Set advanced tick customization to on by default
# Interactive OD Data Visualization Script with region highlighting, comprehensive tick customization,
# Michael Baffour Awuah
# Ramsey Lab

# Load required packages
library(shiny)
library(tidyverse)
library(ggpubr)
library(gridExtra)
library(scales)
library(ggrepel)
library(ggprism)
library(svglite)
library(jsonlite)

# Create Shiny app
ui <- fluidPage(
  titlePanel("Optical Density (OD) Time-Series Visualization"),
  
  tags$head(
    tags$script(HTML("
      Shiny.addCustomMessageHandler('updateColorPreview', function(message) {
        var previewElement = document.getElementById(message.id);
        if (previewElement) {
          previewElement.style.backgroundColor = message.color;
        }
      });
    ")),
    tags$style(HTML("
      .panel-section {
        background-color: #f8f9fa;
        padding: 12px;
        border-radius: 5px;
        margin: 10px 0;
        border: 1px solid #e9ecef;
      }
      .panel-title {
        margin-top: 0;
        margin-bottom: 10px;
        font-weight: bold;
        color: #495057;
      }
      .collapsible-section summary {
        cursor: pointer;
        font-weight: bold;
        padding: 5px;
        background-color: #e9ecef;
        border-radius: 3px;
      }
      .collapsible-section details {
        padding-left: 10px;
        margin-bottom: 10px;
      }
      .color-preview {
        display: inline-block;
        width: 24px;
        height: 24px;
        border: 1px solid #ccc;
        border-radius: 4px;
        vertical-align: middle;
        margin-right: 5px;
      }
      .color-input-container {
        display: flex;
        align-items: center;
      }
      .custom-color-container {
        padding: 10px;
        background-color: #f5f5f5;
        border-radius: 5px;
        margin-top: 5px;
      }
      .rgb-slider-container {
        margin-top: 10px;
      }
    "))
  ),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Choose CSV File",
                accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
      
      div(class = "panel-section",
          h4("Save/Load Settings", class = "panel-title"),
          downloadButton("saveSettings", "Save Current Settings"),
          fileInput("loadSettings", "Load Settings File",
                    accept = c("application/json", ".json"))
      ),
      
      tags$details(
        class = "collapsible-section",
        tags$summary("Axis Settings"),
        div(class = "panel-section",
            h4("Axis Scales", class = "panel-title"),
            selectInput("x_scale_type", "X-axis Scale:",
                        choices = c("Linear" = "linear",
                                    "Logarithmic" = "log",
                                    "Square Root" = "sqrt",
                                    "Reverse" = "reverse"),
                        selected = "linear"),
            selectInput("y_scale_type", "Y-axis Scale:",
                        choices = c("Linear" = "linear",
                                    "Logarithmic" = "log",
                                    "Square Root" = "sqrt",
                                    "Reverse" = "reverse"),
                        selected = "log"),
            checkboxInput("use_advanced_ticks", "Use Advanced Tick Customization", value = TRUE),
            conditionalPanel(
              condition = "input.use_advanced_ticks == true && input.y_scale_type == 'log'",
              h5("Advanced Y-axis Log Ticks"),
              numericInput("y_log_min_exponent", "Minimum Exponent:", value = -2),
              numericInput("y_log_max_exponent", "Maximum Exponent:", value = 0)
            ),
            checkboxInput("custom_x_limits", "Custom X-axis limits", value = FALSE),
            conditionalPanel(
              condition = "input.custom_x_limits == true",
              fluidRow(
                column(6, numericInput("x_min", "X-min:", value = 0)),
                column(6, numericInput("x_max", "X-max:", value = 100))
              )
            ),
            checkboxInput("custom_y_limits", "Custom Y-axis limits", value = FALSE),
            conditionalPanel(
              condition = "input.custom_y_limits == true",
              fluidRow(
                column(6, numericInput("y_min", "Y-min:", value = 0.01)),
                column(6, numericInput("y_max", "Y-max:", value = 2))
              )
            ),
            sliderInput("x_expand_left", "X-axis Left Expansion:", 
                        min = 0, max = 0.1, value = 0, step = 0.01),
            sliderInput("x_expand_right", "X-axis Right Expansion:", 
                        min = 0, max = 0.1, value = 0.05, step = 0.01),
            sliderInput("y_expand_bottom", "Y-axis Bottom Expansion:", 
                        min = 0, max = 0.1, value = 0, step = 0.01),
            sliderInput("y_expand_top", "Y-axis Top Expansion:", 
                        min = 0, max = 0.1, value = 0.05, step = 0.01)
        ),
        div(class = "panel-section",
            h4("Axis Labels & Formatting", class = "panel-title"),
            textInput("x_axis_label", "X-axis Label:", value = "Time (minutes)"),
            textInput("y_axis_label", "Y-axis Label:", value = "A550"),
            textInput("plot_title", "Plot Title:", value = ""),
            textInput("plot_subtitle", "Plot Subtitle:", value = ""),
            h5("Gridlines", style = "margin-top: 15px;"),
            checkboxInput("show_major_gridlines", "Show Major Gridlines", value = FALSE),
            checkboxInput("show_minor_gridlines", "Show Minor Gridlines", value = FALSE),
            conditionalPanel(
              condition = "input.show_major_gridlines == true",
              selectInput("major_gridline_color", "Major Gridline Color:",
                          choices = c("Light Gray" = "#E6E6E6",
                                      "Medium Gray" = "#CCCCCC",
                                      "Dark Gray" = "#999999",
                                      "Light Blue" = "#DEEBF7",
                                      "Light Red" = "#FFEAE9",
                                      "Custom" = "custom"),
                          selected = "#E6E6E6"),
              conditionalPanel(
                condition = "input.major_gridline_color == 'custom'",
                textInput("major_gridline_color_custom", "Custom Color (HEX):", value = "#E6E6E6")
              ),
              sliderInput("major_gridline_size", "Major Gridline Width:",
                          min = 0.1, max = 1, value = 0.5, step = 0.1)
            ),
            conditionalPanel(
              condition = "input.show_minor_gridlines == true",
              selectInput("minor_gridline_color", "Minor Gridline Color:",
                          choices = c("Light Gray" = "#F2F2F2",
                                      "Medium Gray" = "#E6E6E6", 
                                      "Dark Gray" = "#CCCCCC",
                                      "Light Blue" = "#EFF6FB",
                                      "Light Red" = "#FFF2F1",
                                      "Custom" = "custom"),
                          selected = "#F2F2F2"),
              conditionalPanel(
                condition = "input.minor_gridline_color == 'custom'",
                textInput("minor_gridline_color_custom", "Custom Color (HEX):", value = "#F2F2F2")
              ),
              sliderInput("minor_gridline_size", "Minor Gridline Width:",
                          min = 0.1, max = 1, value = 0.3, step = 0.1)
            ),
            h5("Font Settings", style = "margin-top: 15px;"),
            selectInput("font_family", "Font Family:",
                        choices = c("Arial" = "sans",
                                    "Times New Roman" = "serif",
                                    "Courier New" = "mono",
                                    "Helvetica" = "helvetica"),
                        selected = "sans"),
            numericInput("title_font_size", "Title Font Size:", value = 20, min = 8, max = 28),
            numericInput("axis_label_font_size", "Axis Label Font Size:", value = 20, min = 8, max = 24),
            numericInput("axis_text_font_size", "Axis Text Font Size:", value = 20, min = 6, max = 20),
            checkboxInput("bold_title", "Bold Title", value = TRUE),
            checkboxInput("italic_axis_labels", "Italic Axis Labels", value = FALSE),
            selectInput("axis_text_angle", "X-axis Text Angle:",
                        choices = c("Horizontal (0°)" = "0",
                                    "Angled (45°)" = "45",
                                    "Vertical (90°)" = "90"),
                        selected = "0")
        )
      ),
      
      tags$details(
        class = "collapsible-section",
        tags$summary("Region Highlighting"),
        div(class = "panel-section",
            h4("Highlight Regions", class = "panel-title"),
            checkboxInput("enable_highlighting", "Enable Region Highlighting", value = FALSE),
            conditionalPanel(
              condition = "input.enable_highlighting == true",
              numericInput("region_count", "Number of Regions to Highlight:", value = 1, min = 1, max = 5),
              uiOutput("region_settings")
            )
        )
      ),
      
      tags$details(
        class = "collapsible-section",
        tags$summary("Time Point Markers"),
        div(class = "panel-section",
            h4("Vertical Line Markers", class = "panel-title"),
            checkboxInput("enable_time_markers", "Enable Time Point Markers", value = FALSE),
            conditionalPanel(
              condition = "input.enable_time_markers == true",
              numericInput("marker_count", "Number of Time Markers:", value = 1, min = 1, max = 5),
              uiOutput("time_marker_settings")
            )
        )
      ),
      
      tags$details(
        class = "collapsible-section",
        tags$summary("Color Palettes"),
        div(class = "panel-section",
            h4("Predefined Color Palettes", class = "panel-title"),
            selectInput("color_palette", "Select Color Palette:",
                        choices = c("Custom" = "custom",
                                    "Viridis" = "viridis",
                                    "Plasma" = "plasma",
                                    "Colorblind-friendly" = "colorblind",
                                    "Publication-ready" = "publication",
                                    "Rainbow" = "rainbow",
                                    "Grayscale" = "gray"),
                        selected = "custom"),
            htmlOutput("palette_preview")
        )
      ),
      
      tags$details(
        class = "collapsible-section",
        tags$summary("Line & Point Settings"),
        div(class = "panel-section",
            h4("Line Options", class = "panel-title"),
            selectInput("line_type", "Line Type:",
                        choices = c("Solid" = "solid",
                                    "Dashed" = "dashed",
                                    "Dotted" = "dotted",
                                    "DotDash" = "dotdash",
                                    "LongDash" = "longdash",
                                    "TwoDash" = "twodash",
                                    "None" = "blank"),
                        selected = "solid"),
            sliderInput("line_thickness", "Line Thickness:",
                        min = 0.1, max = 3, value = 1, step = 0.1)
        ),
        div(class = "panel-section",
            h4("Point Options", class = "panel-title"),
            checkboxInput("show_points", "Show Points", value = TRUE),
            conditionalPanel(
              condition = "input.show_points == true",
              sliderInput("shape_size", "Point Size:",
                          min = 0.5, max = 5, value = 3, step = 0.1),
              sliderInput("point_stroke", "Point Border Thickness:",
                          min = 0, max = 2, value = 0.5, step = 0.1)
            )
        )
      ),
      
      tags$details(
        class = "collapsible-section",
        tags$summary("Label Options"),
        div(class = "panel-section",
            h4("End-of-Line Labels", class = "panel-title"),
            checkboxInput("show_end_labels", "Show Line Labels at End", value = FALSE),
            conditionalPanel(
              condition = "input.show_end_labels == true",
              numericInput("label_font_size", "Label Font Size:", value = 20, min = 3, max = 24),
              checkboxInput("label_bold", "Bold Labels", value = TRUE),
              numericInput("label_offset", "Label Offset (% of x-axis):", value = 3.5, min = 0, max = 10)
            )
        )
      ),
      
      tags$details(
        class = "collapsible-section",
        tags$summary("Error Bar Settings"),
        div(class = "panel-section",
            selectInput("error_type", "Error Bar Type:",
                        choices = c("None" = "none",
                                    "Standard Deviation (SD)" = "sd",
                                    "Standard Error of Mean (SEM)" = "sem",
                                    "95% Confidence Interval" = "ci95"),
                        selected = "sem"),
            conditionalPanel(
              condition = "input.error_type != 'none'",
              selectInput("error_bar_style", "Error Bar Style:",
                          choices = c("T-shaped (whiskers)" = "T",
                                      "Solid lines" = "solid",
                                      "Dashed lines" = "dashed"),
                          selected = "T"),
              selectInput("error_cap_type", "Error Bar Cap Type:",
                          choices = c("Flat" = "flat",
                                      "Round" = "round",
                                      "None" = "none"),
                          selected = "flat"),
              sliderInput("error_bar_width", "Error Bar Width:",
                          min = 0.01, max = 1, value = 1, step = 0.01),
              sliderInput("error_bar_thickness", "Error Bar Thickness:",
                          min = 0.1, max = 2, value = 0.8, step = 0.1),
              numericInput("error_multiplier", "Error Multiplier:", 
                           value = 1, min = 0.1, max = 3, step = 0.1),
              selectInput("error_bar_position", "Error Bar Position:",
                          choices = c("Middle" = "middle",
                                      "Dodge" = "dodge"),
                          selected = "middle"),
              checkboxInput("asymmetric_error", "Allow Asymmetric Errors", value = FALSE)
            )
        )
      ),
      
      tags$details(
        class = "collapsible-section",
        tags$summary("Plot Dimensions & Export"),
        div(class = "panel-section",
            h4("Plot Dimensions", class = "panel-title"),
            fluidRow(
              column(6, numericInput("plot_width", "Width (px):", value = 800, min = 400, max = 2000)),
              column(6, numericInput("plot_height", "Height (px):", value = 600, min = 300, max = 1500))
            ),
            h4("Plot Aspect Ratio", class = "panel-title"),
            checkboxInput("custom_aspect_ratio", "Set Custom Aspect Ratio", value = FALSE),
            conditionalPanel(
              condition = "input.custom_aspect_ratio == true",
              numericInput("aspect_ratio", "Width/Height Ratio:", value = 1, min = 0.5, max = 3, step = 0.1)
            ),
            h4("Export Options", class = "panel-title"),
            selectInput("export_format", "File Format:",
                        choices = c("PDF" = "pdf", 
                                    "PNG" = "png", 
                                    "JPEG" = "jpeg", 
                                    "SVG" = "svg",
                                    "TIFF" = "tiff"),
                        selected = "pdf"),
            fluidRow(
              column(6, numericInput("export_width", "Export Width (in):", value = 10, min = 2, max = 20)),
              column(6, numericInput("export_height", "Export Height (in):", value = 8, min = 2, max = 20))
            ),
            numericInput("export_dpi", "Resolution (DPI):", value = 300, min = 72, max = 1200),
            downloadButton("downloadPlot", "Download Plot")
        )
      ),
      
      tags$details(
        class = "collapsible-section",
        tags$summary("Variable Styling"),
        div(class = "panel-section",
            uiOutput("var_settings")
        )
      )
    ),
    
    mainPanel(
      uiOutput("plot_container")
    )
  )
)

server <- function(input, output, session) {
  # Reactive values to store data and detected properties
  rv <- reactiveValues(
    data = NULL,
    time_col = NULL,
    od_vars = NULL,
    is_long_format = FALSE,
    palette_colors = NULL
  )
  
  # Helper function to detect if data is in long format
  is_long_format <- function(data) {
    potential_id_cols <- c("variable", "Variable", "condition", "Condition", "treatment", "Treatment")
    has_id_col <- any(potential_id_cols %in% colnames(data))
    few_columns <- ncol(data) <= 4
    value_diversity <- sapply(data, function(x) length(unique(x))/nrow(data))
    has_low_diversity_col <- any(value_diversity < 0.3 & value_diversity > 0)
    return(has_id_col || (few_columns && has_low_diversity_col))
  }
  
  # Helper function to detect time column
  detect_time_column <- function(data) {
    time_cols <- grep("time|Time|TIME|t$|T$", colnames(data), ignore.case = TRUE)
    if (length(time_cols) > 0) {
      return(colnames(data)[time_cols[1]])
    }
    numeric_cols <- sapply(data, is.numeric)
    if (any(numeric_cols)) {
      first_numeric <- names(which(numeric_cols))[1]
      if (is.numeric(data[[first_numeric]]) && 
          all(diff(data[[first_numeric]]) >= 0 | is.na(diff(data[[first_numeric]])))) {
        return(first_numeric)
      }
    }
    return(colnames(data)[1])
  }
  
  # Helper function to detect OD columns
  detect_od_columns <- function(data, time_col) {
    cols <- setdiff(colnames(data), time_col)
    if (rv$is_long_format) {
      value_cols <- grep("value|od|OD|absorbance|Absorbance", cols, ignore.case = TRUE)
      if (length(value_cols) > 0) {
        return(cols[value_cols[1]])
      } else if (any(sapply(data[cols], is.numeric))) {
        return(names(which(sapply(data[cols], is.numeric)))[1])
      }
    } else {
      od_cols <- grep("od|OD|absorbance|Absorbance", cols, ignore.case = TRUE)
      if (length(od_cols) > 0) {
        return(cols[od_cols])
      } else {
        return(cols[sapply(data[cols], is.numeric)])
      }
    }
  }
  
  # Function to get palette colors
  get_palette_colors <- function(palette_name, n_colors) {
    switch(palette_name,
           "viridis" = viridis_pal()(n_colors),
           "plasma" = plasma_pal()(n_colors),
           "colorblind" = colorblind_pal()(n_colors),
           "publication" = c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00", "#FFFF33", "#A65628", "#F781BF")[1:min(n_colors, 8)],
           "rainbow" = rainbow(n_colors),
           "gray" = gray.colors(n_colors, start = 0.2, end = 0.8),
           "custom" = NULL)
  }
  
  # Update palette colors when palette selection changes
  observeEvent(input$color_palette, {
    if (input$color_palette != "custom" && !is.null(rv$od_vars)) {
      rv$palette_colors <- get_palette_colors(input$color_palette, length(rv$od_vars))
    }
  })
  
  # Generate palette preview
  output$palette_preview <- renderUI({
    if (input$color_palette == "custom" || is.null(rv$od_vars)) {
      return(NULL)
    }
    colors <- get_palette_colors(input$color_palette, min(8, length(rv$od_vars)))
    color_boxes <- lapply(colors, function(color) {
      div(style = paste0("display: inline-block; width: 30px; height: 20px; background-color: ", 
                         color, "; margin-right: 5px; border: 1px solid #ccc;"))
    })
    div(
      p("Palette Preview:"),
      div(style = "margin-top: 10px;", color_boxes)
    )
  })
  
  # Dynamic UI for region settings
  output$region_settings <- renderUI({
    req(input$region_count, input$enable_highlighting)
    if (!input$enable_highlighting) return(NULL)
    settings_list <- lapply(1:input$region_count, function(i) {
      div(
        style = "border: 1px solid #ddd; padding: 10px; margin-bottom: 10px; border-radius: 5px;",
        h5(paste("Region", i)),
        fluidRow(
          column(6, numericInput(paste0("region_x_min_", i), "X min:", value = 0)),
          column(6, numericInput(paste0("region_x_max_", i), "X max:", value = 60))
        ),
        fluidRow(
          column(6, numericInput(paste0("region_y_min_", i), "Y min:", value = 0.01)),
          column(6, numericInput(paste0("region_y_max_", i), "Y max:", value = 1))
        ),
        selectInput(paste0("region_color_", i), "Fill Color:",
                    choices = c("Light Gray" = "#ebebeb",
                                "Light Blue" = "#e6f3ff",
                                "Light Red" = "#ffebeb",
                                "Light Green" = "#ebffeb",
                                "Light Yellow" = "#ffffeb",
                                "Custom" = "custom"),
                    selected = "#ebebeb"),
        conditionalPanel(
          condition = paste0("input.region_color_", i, " == 'custom'"),
          textInput(paste0("region_color_custom_", i), "Custom Color (HEX):", value = "#ebebeb")
        ),
        sliderInput(paste0("region_alpha_", i), "Transparency:", 
                    min = 0, max = 1, value = 0.3, step = 0.1)
      )
    })
    do.call(tagList, settings_list)
  })
  
  # Dynamic UI for time marker settings
  output$time_marker_settings <- renderUI({
    req(input$marker_count, input$enable_time_markers)
    if (!input$enable_time_markers) return(NULL)
    markers_list <- lapply(1:input$marker_count, function(i) {
      div(
        style = "border: 1px solid #ddd; padding: 10px; margin-bottom: 10px; border-radius: 5px;",
        h5(paste("Time Marker", i)),
        numericInput(paste0("marker_time_", i), "Time Point:", value = 30),
        selectInput(paste0("marker_line_type_", i), "Line Type:",
                    choices = c("Solid" = "solid",
                                "Dashed" = "dashed",
                                "Dotted" = "dotted",
                                "DotDash" = "dotdash",
                                "LongDash" = "longdash",
                                "TwoDash" = "twodash"),
                    selected = "dashed"),
        selectInput(paste0("marker_color_", i), "Line Color:",
                    choices = c("Black" = "#000000",
                                "Red" = "#E41A1C",
                                "Blue" = "#377EB8",
                                "Green" = "#4DAF4A",
                                "Purple" = "#984EA3",
                                "Orange" = "#FF7F00",
                                "Gray" = "#999999",
                                "Custom" = "custom"),
                    selected = "#000000"),
        conditionalPanel(
          condition = paste0("input.marker_color_", i, " == 'custom'"),
          textInput(paste0("marker_color_custom_", i), "Custom Color (HEX):", value = "#000000")
        ),
        sliderInput(paste0("marker_size_", i), "Line Width:", 
                    min = 0.1, max = 2, value = 0.8, step = 0.1),
        checkboxInput(paste0("marker_label_", i), "Add Label", value = FALSE),
        conditionalPanel(
          condition = paste0("input.marker_label_", i, " == true"),
          textInput(paste0("marker_text_", i), "Label Text:", value = paste("t =", 30)),
          numericInput(paste0("marker_label_size_", i), "Label Size:", value = 4, min = 2, max = 8),
          selectInput(paste0("marker_label_position_", i), "Label Position:",
                      choices = c("Top" = "top",
                                  "Bottom" = "bottom",
                                  "Middle" = "middle"),
                      selected = "top"),
          numericInput(paste0("marker_label_hjust_", i), "Horizontal Offset:", 
                       value = 0, min = -2, max = 2, step = 0.1)
        )
      )
    })
    do.call(tagList, markers_list)
  })
  
  # Process uploaded file
  observeEvent(input$file, {
    req(input$file)
    data <- read.csv(input$file$datapath, stringsAsFactors = FALSE)
    rv$is_long_format <- is_long_format(data)
    rv$time_col <- detect_time_column(data)
    
    if (rv$is_long_format) {
      potential_group_cols <- c("variable", "Variable", "condition", "Condition", 
                                "treatment", "Treatment", "sample", "Sample")
      group_col <- potential_group_cols[potential_group_cols %in% colnames(data)][1]
      
      if (is.null(group_col) || length(group_col) == 0) {
        value_diversity <- sapply(data, function(x) length(unique(x))/nrow(data))
        low_diversity_cols <- names(which(value_diversity < 0.3 & value_diversity > 0))
        low_diversity_cols <- setdiff(low_diversity_cols, rv$time_col)
        if (length(low_diversity_cols) > 0) {
          group_col <- low_diversity_cols[1]
        } else {
          cols <- setdiff(colnames(data), rv$time_col)
          group_col <- cols[1]
        }
      }
      
      value_col <- detect_od_columns(data, c(rv$time_col, group_col))
      rv$group_col <- group_col
      rv$value_col <- value_col
      rv$od_vars <- unique(data[[group_col]])
      rv$data <- data
      
      time_range <- range(as.numeric(as.character(data[[rv$time_col]])), na.rm = TRUE)
      updateNumericInput(session, "x_min", value = time_range[1])
      updateNumericInput(session, "x_max", value = time_range[2])
    } else {
      rv$od_vars <- detect_od_columns(data, rv$time_col)
      rv$data <- data
      time_range <- range(as.numeric(as.character(data[[rv$time_col]])), na.rm = TRUE)
      updateNumericInput(session, "x_min", value = time_range[1])
      updateNumericInput(session, "x_max", value = time_range[2])
    }
    
    if (input$color_palette != "custom" && !is.null(rv$od_vars)) {
      rv$palette_colors <- get_palette_colors(input$color_palette, length(rv$od_vars))
    }
    
    output$var_settings <- renderUI({
      req(rv$od_vars)
      shape_options <- c(
        "Circle" = 16, "Square" = 15, "Triangle" = 17, "Diamond" = 18,
        "Upside-down Triangle" = 25, "Cross" = 4, "X" = 8, 
        "Filled Diamond" = 23, "Plus" = 3, "Star" = 10
      )
      
      var_uis <- lapply(seq_along(rv$od_vars), function(i) {
        var_name <- rv$od_vars[i]
        shape_default <- shape_options[1 + (i-1) %% length(shape_options)]
        color_default <- if (!is.null(rv$palette_colors) && input$color_palette != "custom") {
          rv$palette_colors[i]
        } else {
          c("#000000", "#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00", "#FFFF33", "#A65628")[1 + (i-1) %% 8]
        }
        line_type_default <- "solid"
        
        div(
          style = "margin-bottom: 15px; padding: 10px; border: 1px solid #ddd; border-radius: 5px;",
          h4(paste("Settings for", var_name)),
          selectInput(
            inputId = paste0("line_type_", i),
            label = "Line Type:",
            choices = c("Solid" = "solid",
                        "Dashed" = "dashed",
                        "Dotted" = "dotted",
                        "DotDash" = "dotdash",
                        "LongDash" = "longdash",
                        "TwoDash" = "twodash"),
            selected = line_type_default
          ),
          h5("Point Shape Settings", style = "margin-top: 10px;"),
          fluidRow(
            column(6,
                   selectInput(
                     inputId = paste0("shape_", i),
                     label = "Point Shape:",
                     choices = shape_options,
                     selected = shape_default
                   )
            ),
            column(6,
                   checkboxInput(
                     inputId = paste0("shape_filled_", i),
                     label = "Filled Points",
                     value = TRUE
                   )
            )
          ),
          div(class = "panel-section",
              h5("Color Selection", class = "panel-title"),
              selectInput(
                inputId = paste0("color_selector_", i),
                label = "Select Color:",
                choices = c("Black" = "#000000", 
                            "Red" = "#E41A1C", 
                            "Blue" = "#377EB8", 
                            "Green" = "#4DAF4A", 
                            "Purple" = "#984EA3", 
                            "Orange" = "#FF7F00",
                            "Yellow" = "#FFFF33", 
                            "Brown" = "#A65628", 
                            "Pink" = "#F781BF", 
                            "Gray" = "#999999",
                            "Custom" = "custom"),
                selected = color_default
              ),
              conditionalPanel(
                condition = paste0("input.color_selector_", i, " == 'custom'"),
                div(
                  style = "display: flex; align-items: center;",
                  textInput(
                    inputId = paste0("color_", i),
                    label = "HEX Color Code:",
                    value = color_default
                  ),
                  div(
                    id = paste0("color_preview_", i),
                    class = "color-preview",
                    style = paste0("background-color: ", color_default, ";")
                  )
                )
              ),
              checkboxInput(paste0("use_rgb_", i), "Use RGB Values", value = FALSE),
              conditionalPanel(
                condition = paste0("input.use_rgb_", i, " == true"),
                div(class = "rgb-slider-container",
                    sliderInput(paste0("red_", i), "Red:", min = 0, max = 255, value = 0, step = 1),
                    sliderInput(paste0("green_", i), "Green:", min = 0, max = 255, value = 0, step = 1),
                    sliderInput(paste0("blue_", i), "Blue:", min = 0, max = 255, value = 0, step = 1),
                    sliderInput(paste0("alpha_", i), "Alpha:", min = 0, max = 1, value = 1, step = 0.01),
                    actionButton(paste0("apply_rgb_", i), "Apply RGB Values")
                )
              ),
              checkboxInput(paste0("use_hex_", i), "Use HEX Code", value = FALSE),
              conditionalPanel(
                condition = paste0("input.use_hex_", i, " == true"),
                textInput(
                  inputId = paste0("hex_color_", i),
                  label = "HEX Code (e.g., #ff0000 or ff0000):",
                  value = substr(color_default, 1, 7)
                ),
                actionButton(paste0("apply_hex_", i), "Apply HEX Code")
              )
          ),
          textInput(
            inputId = paste0("legend_label_", i),
            label = "Custom Legend Label:",
            value = var_name
          )
        )
      })
      do.call(tagList, var_uis)
    })
  })
  
  # RGB to HEX conversion
  rgb_to_hex <- function(r, g, b, a = 1) {
    r <- min(max(as.integer(r), 0), 255)
    g <- min(max(as.integer(g), 0), 255)
    b <- min(max(as.integer(b), 0), 255)
    a <- min(max(a, 0), 1)
    hex_color <- rgb(r/255, g/255, b/255, a)
    return(hex_color)
  }
  
  # HEX to RGB conversion
  hex_to_rgb <- function(hex) {
    hex <- gsub("^#", "", hex)
    if (nchar(hex) == 6 && grepl("^[0-9A-Fa-f]{6}$", hex)) {
      r <- strtoi(substr(hex, 1, 2), 16)
      g <- strtoi(substr(hex, 3, 4), 16)
      b <- strtoi(substr(hex, 5, 6), 16)
      return(list(r = r, g = g, b = b))
    } else {
      return(list(r = 0, g = 0, b = 0))
    }
  }
  
  # Normalize HEX color
  normalize_hex_color <- function(hex) {
    if (!grepl("^#", hex)) {
      hex <- paste0("#", hex)
    }
    if (grepl("^#[0-9A-Fa-f]{6}$", hex)) {
      return(hex)
    } else {
      return("#000000")
    }
  }
  
  # Handle RGB sliders for each variable
  observe({
    req(rv$od_vars)
    if(is.null(rv$data)) return(NULL)
    
    for (i in seq_along(rv$od_vars)) {
      local({
        i_local <- i
        observeEvent(input[[paste0("color_selector_", i_local)]], {
          color_selection <- input[[paste0("color_selector_", i_local)]]
          if (color_selection != "custom") {
            color <- color_selection
            rgb_values <- col2rgb(color)
            updateSliderInput(session, paste0("red_", i_local), value = rgb_values[1,1])
            updateSliderInput(session, paste0("green_", i_local), value = rgb_values[2,1])
            updateSliderInput(session, paste0("blue_", i_local), value = rgb_values[3,1])
            updateTextInput(session, paste0("color_", i_local), value = color)
            updateTextInput(session, paste0("hex_color_", i_local), value = substr(color, 1, 7))
          }
        })
        
        observeEvent(input[[paste0("color_", i_local)]], {
          if (input[[paste0("color_selector_", i_local)]] == "custom") {
            color <- input[[paste0("color_", i_local)]]
            if (!grepl("^#", color)) {
              color <- paste0("#", color)
              updateTextInput(session, paste0("color_", i_local), value = color)
            }
            if (grepl("^#[0-9A-Fa-f]{6}$", color)) {
              rgb_values <- col2rgb(color)
              updateSliderInput(session, paste0("red_", i_local), value = rgb_values[1,1])
              updateSliderInput(session, paste0("green_", i_local), value = rgb_values[2,1])
              updateSliderInput(session, paste0("blue_", i_local), value = rgb_values[3,1])
              updateTextInput(session, paste0("hex_color_", i_local), value = substr(color, 1, 7))
              session$sendCustomMessage(
                type = 'updateColorPreview',
                list(id = paste0("color_preview_", i_local), color = color)
              )
            }
          }
        })
        
        observeEvent(input[[paste0("apply_rgb_", i_local)]], {
          if (input[[paste0("use_rgb_", i_local)]]) {
            r <- input[[paste0("red_", i_local)]]
            g <- input[[paste0("green_", i_local)]]
            b <- input[[paste0("blue_", i_local)]]
            a <- input[[paste0("alpha_", i_local)]]
            hex_color <- rgb_to_hex(r, g, b, a)
            updateSelectInput(session, paste0("color_selector_", i_local), selected = "custom")
            updateTextInput(session, paste0("color_", i_local), value = hex_color)
            updateTextInput(session, paste0("hex_color_", i_local), value = substr(hex_color, 1, 7))
          }
        })
        
        observeEvent(input[[paste0("apply_hex_", i_local)]], {
          hex_code <- input[[paste0("hex_color_", i_local)]]
          if (!grepl("^#", hex_code)) {
            hex_code <- paste0("#", hex_code)
          }
          if (grepl("^#[0-9A-Fa-f]{6}$", hex_code)) {
            updateTextInput(session, paste0("color_", i_local), value = hex_code)
            rgb_values <- hex_to_rgb(hex_code)
            updateSliderInput(session, paste0("red_", i_local), value = rgb_values$r)
            updateSliderInput(session, paste0("green_", i_local), value = rgb_values$g)
            updateSliderInput(session, paste0("blue_", i_local), value = rgb_values$b)
          }
        })
      })
    }
  })
  
  # Save settings handler
  output$saveSettings <- downloadHandler(
    filename = function() {
      paste("plot_settings_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".json", sep = "")
    },
    content = function(file) {
      settings <- reactiveValuesToList(input)
      write_json(settings, file, pretty = TRUE)
    }
  )
  
  # Load settings handler
  observeEvent(input$loadSettings, {
    req(input$loadSettings)
    settings <- fromJSON(input$loadSettings$datapath)
    
    for (name in names(settings)) {
      if (name %in% names(input)) {
        tryCatch({
          if (is.list(settings[[name]]) || length(settings[[name]]) > 1) {
            updateSelectInput(session, name, selected = settings[[name]])
          } else {
            switch(typeof(settings[[name]]),
                   "double" = updateNumericInput(session, name, value = settings[[name]]),
                   "integer" = updateNumericInput(session, name, value = settings[[name]]),
                   "logical" = updateCheckboxInput(session, name, value = settings[[name]]),
                   "character" = updateTextInput(session, name, value = settings[[name]]),
                   updateSelectInput(session, name, selected = settings[[name]]))
          }
        }, error = function(e) {
          warning(paste("Could not restore setting:", name))
        })
      }
    }
  })
  
  # Prepare data for plotting
  prepare_plot_data <- function() {
    req(rv$data, rv$time_col)
    
    if (rv$is_long_format) {
      req(rv$group_col, rv$value_col)
      rv$data[[rv$time_col]] <- as.numeric(as.character(rv$data[[rv$time_col]]))
      plot_data <- rv$data %>%
        group_by_at(vars(rv$time_col, rv$group_col)) %>%
        summarise(
          mean_value = mean(.data[[rv$value_col]], na.rm = TRUE),
          sd_value = sd(.data[[rv$value_col]], na.rm = TRUE),
          n = n(),
          sem_value = sd_value / sqrt(n),
          ci95_value = qt(0.975, df = max(n-1, 1)) * sem_value,
          .groups = 'drop'
        )
      names(plot_data)[names(plot_data) == rv$group_col] <- "variable"
      names(plot_data)[names(plot_data) == rv$time_col] <- "time"
    } else {
      id_vars <- rv$time_col
      measure_vars <- rv$od_vars
      rv$data[[rv$time_col]] <- as.numeric(as.character(rv$data[[rv$time_col]]))
      melted_data <- rv$data %>%
        pivot_longer(
          cols = all_of(measure_vars),
          names_to = "variable",
          values_to = "value"
        )
      plot_data <- melted_data %>%
        group_by(time = .data[[id_vars]], variable) %>%
        summarise(
          mean_value = mean(value, na.rm = TRUE),
          sd_value = sd(value, na.rm = TRUE),
          n = n(),
          sem_value = sd_value / sqrt(n),
          ci95_value = qt(0.975, df = max(n-1, 1)) * sem_value,
          .groups = 'drop'
        )
    }
    plot_data <- plot_data %>%
      filter(!is.na(time) & !is.na(mean_value) & is.finite(time) & is.finite(mean_value))
    return(plot_data)
  }
  
  # Generate the plot
  generate_plot <- function() {
    req(rv$data, rv$od_vars)
    plot_data <- prepare_plot_data()
    
    shapes <- numeric(length(rv$od_vars))
    colors <- character(length(rv$od_vars))
    line_types <- character(length(rv$od_vars))
    legend_labels <- character(length(rv$od_vars))
    
    for (i in seq_along(rv$od_vars)) {
      shape_input <- input[[paste0("shape_", i)]]
      color_selector <- input[[paste0("color_selector_", i)]]
      color_input <- if (!is.null(color_selector) && color_selector == "custom") {
        input[[paste0("color_", i)]]
      } else {
        color_selector
      }
      line_type_input <- input[[paste0("line_type_", i)]]
      legend_label_input <- input[[paste0("legend_label_", i)]]
      
      shapes[i] <- if (!is.null(shape_input)) as.numeric(shape_input) else 16 + i
      colors[i] <- if (!is.null(color_input)) {
        color_input
      } else if (input$color_palette != "custom" && !is.null(rv$palette_colors)) {
        rv$palette_colors[i]
      } else {
        rainbow(length(rv$od_vars))[i]
      }
      line_types[i] <- if (!is.null(line_type_input)) line_type_input else "solid"
      legend_labels[i] <- if (!is.null(legend_label_input) && legend_label_input != "") {
        legend_label_input
      } else {
        as.character(rv$od_vars[i])
      }
    }
    
    names(shapes) <- rv$od_vars
    names(colors) <- rv$od_vars
    names(line_types) <- rv$od_vars
    names(legend_labels) <- rv$od_vars
    
    plot_data$is_filled <- TRUE
    for (i in seq_along(rv$od_vars)) {
      var_name <- rv$od_vars[i]
      filled_input <- input[[paste0("shape_filled_", i)]]
      if (!is.null(filled_input) && filled_input == FALSE) {
        plot_data$is_filled[plot_data$variable == var_name] <- FALSE
      }
    }
    
    p <- ggplot(plot_data, aes(x = time, y = mean_value, group = variable, 
                               color = variable, shape = variable, 
                               fill = interaction(variable, is_filled),
                               linetype = variable))
    
    if (input$enable_highlighting) {
      for (i in 1:input$region_count) {
        x_min <- input[[paste0("region_x_min_", i)]]
        x_max <- input[[paste0("region_x_max_", i)]]
        y_min <- input[[paste0("region_y_min_", i)]]
        y_max <- input[[paste0("region_y_max_", i)]]
        region_color_select <- input[[paste0("region_color_", i)]]
        color <- if (!is.null(region_color_select) && region_color_select == "custom") {
          input[[paste0("region_color_custom_", i)]]
        } else {
          region_color_select
        }
        alpha <- input[[paste0("region_alpha_", i)]]
        
        if (!is.null(x_min) && !is.null(x_max) && !is.null(y_min) && !is.null(y_max) && 
            !is.null(color) && !is.null(alpha)) {
          p <- p + geom_rect(aes(xmin = x_min, xmax = x_max, ymin = y_min, ymax = y_max), 
                             fill = color, alpha = alpha, inherit.aes = FALSE)
        }
      }
    }
    
    if (input$enable_time_markers) {
      y_range <- if (input$custom_y_limits) {
        c(input$y_min, input$y_max)
      } else {
        y_range <- range(plot_data$mean_value, na.rm = TRUE)
        if (input$y_scale_type == "log") {
          if (input$use_advanced_ticks) {
            y_range <- c(10^input$y_log_min_exponent, 10^(input$y_log_max_exponent + (input$y_log_max_exponent >= 0 ? 1 : 0)))
          } else {
            y_range <- c(max(0.01, min(plot_data$mean_value, na.rm = TRUE) * 0.5), 
                         max(plot_data$mean_value, na.rm = TRUE) * 2)
          }
        } else {
          y_padding <- diff(y_range) * 0.1
          y_range <- c(max(0, y_range[1] - y_padding), y_range[2] + y_padding)
        }
      }
      
      for (i in 1:input$marker_count) {
        time_point <- input[[paste0("marker_time_", i)]]
        line_type <- input[[paste0("marker_line_type_", i)]]
        marker_color_select <- input[[paste0("marker_color_", i)]]
        line_color <- if (!is.null(marker_color_select) && marker_color_select == "custom") {
          input[[paste0("marker_color_custom_", i)]]
        } else {
          marker_color_select
        }
        line_size <- input[[paste0("marker_size_", i)]]
        
        if (!is.null(time_point) && !is.null(line_type) && !is.null(line_color) && !is.null(line_size)) {
          p <- p + geom_vline(xintercept = time_point, 
                              linetype = line_type, 
                              color = line_color, 
                              linewidth = line_size)
          
          if (!is.null(input[[paste0("marker_label_", i)]]) && input[[paste0("marker_label_", i)]]) {
            label_text <- input[[paste0("marker_text_", i)]]
            label_size <- input[[paste0("marker_label_size_", i)]]
            label_position <- input[[paste0("marker_label_position_", i)]]
            
            if (!is.null(label_text) && !is.null(label_size) && !is.null(label_position)) {
              y_pos <- switch(label_position,
                              "top" = y_range[2] * 0.95,
                              "bottom" = y_range[1] * 1.05,
                              "middle" = sqrt(y_range[1] * y_range[2]))
              h_offset <- input[[paste0("marker_label_hjust_", i)]]
              if (is.null(h_offset)) h_offset <- 0
              
              p <- p + annotate("text", 
                                x = time_point + h_offset,
                                y = y_pos,
                                label = label_text,
                                size = label_size,
                                color = line_color)
            }
          }
        }
      }
    }
    
    if (input$line_type != "blank") {
      p <- p + geom_line(linewidth = input$line_thickness)
    }
    
    if (input$show_points) {
      for (i in seq_along(rv$od_vars)) {
        var_name <- rv$od_vars[i]
        filled_input <- input[[paste0("shape_filled_", i)]]
        var_data <- plot_data %>% filter(variable == var_name)
        fill_color <- if (!is.null(filled_input) && filled_input == FALSE) "white" else NA
        
        p <- p + geom_point(
          data = var_data,
          aes(x = time, y = mean_value, color = variable, shape = variable),
          size = input$shape_size, 
          stroke = input$point_stroke,
          fill = fill_color
        )
      }
    }
    
    if (input$show_end_labels) {
      max_time <- max(plot_data$time, na.rm = TRUE)
      offset <- max_time * (input$label_offset / 100)
      end_points <- plot_data %>%
        group_by(variable) %>%
        filter(time == max(time)) %>%
        ungroup()
      
      p <- p + geom_text_repel(
        data = end_points,
        aes(label = variable, color = variable, x = Inf, y = mean_value),
        direction = "y",
        xlim = c(max_time + offset, Inf),
        min.segment.length = Inf,
        hjust = 0,
        size = input$label_font_size,
        fontface = if (input$label_bold) "bold" else "plain"
      )
    }
    
    if (input$error_type != "none") {
      error_value <- switch(input$error_type,
                            "sd" = "sd_value",
                            "sem" = "sem_value",
                            "ci95" = "ci95_value")
      error_mult <- input$error_multiplier
      error_bar_linetype <- if (input$error_bar_style == "dashed") "dashed" else "solid"
      
      pos <- if (input$error_bar_position == "dodge") {
        position_dodge(width = 0.2)
      } else {
        position_identity()
      }
      
      if (input$asymmetric_error) {
        plot_data <- plot_data %>%
          mutate(
            lower_bound = pmax(mean_value - !!sym(error_value) * error_mult, 0),
            upper_bound = mean_value + !!sym(error_value) * error_mult
          )
        error_ymin <- "lower_bound"
        error_ymax <- "upper_bound"
      } else {
        error_ymin <- paste0("mean_value - ", error_value, " * ", error_mult)
        error_ymax <- paste0("mean_value + ", error_value, " * ", error_mult)
      }
      
      if (input$error_bar_style == "T") {
        p <- p + geom_errorbar(
          aes_string(ymin = error_ymin, ymax = error_ymax),
          width = input$error_bar_width,
          linewidth = input$error_bar_thickness,
          linetype = error_bar_linetype,
          position = pos
        )
      } else {
        p <- p + geom_linerange(
          aes_string(ymin = error_ymin, ymax = error_ymax),
          linewidth = input$error_bar_thickness,
          linetype = error_bar_linetype,
          position = pos
        )
      }
    }
    
    p <- p + 
      scale_shape_manual(values = shapes, labels = legend_labels) +
      scale_color_manual(values = colors, labels = legend_labels) +
      scale_linetype_manual(values = line_types, labels = legend_labels)
    
    x_expand <- expansion(
      mult = c(input$x_expand_left, input$x_expand_right),
      add = c(0, 0)
    )
    y_expand <- expansion(
      mult = c(input$y_expand_bottom, input$y_expand_top),
      add = c(0, 0)
    )
    
    x_limits <- if (input$custom_x_limits) c(input$x_min, input$x_max) else NULL
    if (input$x_scale_type == "log") {
      p <- p + scale_x_log10(limits = x_limits, expand = x_expand)
    } else if (input$x_scale_type == "sqrt") {
      p <- p + scale_x_sqrt(limits = x_limits, expand = x_expand)
    } else if (input$x_scale_type == "reverse") {
      p <- p + scale_x_reverse(limits = x_limits, expand = x_expand)
    } else {
      if (input$use_advanced_ticks) {
        max_time <- if (input$custom_x_limits) input$x_max else max(plot_data$time, na.rm = TRUE)
        if (max_time <= 60) {
          major_breaks <- seq(0, ceiling(max_time/10)*10, by = 10)
          minor_breaks <- seq(0, ceiling(max_time/10)*10, by = 5)
        } else if (max_time <= 120) {
          major_breaks <- seq(0, ceiling(max_time/30)*30, by = 30)
          minor_breaks <- seq(0, ceiling(max_time/30)*30, by = 10)
        } else {
          major_breaks <- seq(0, ceiling(max_time/60)*60, by = 60)
          minor_breaks <- seq(0, ceiling(max_time/60)*60, by = 30)
        }
        p <- p + scale_x_continuous(
          limits = x_limits, 
          expand = x_expand,
          breaks = major_breaks,
          minor_breaks = minor_breaks,
          guide = guide_prism_minor()
        )
      } else {
        p <- p + scale_x_continuous(limits = x_limits, expand = x_expand)
      }
    }
    
    y_limits <- if (input$custom_y_limits) c(input$y_min, input$y_max) else NULL
    if (input$y_scale_type == "log") {
      if (input$use_advanced_ticks) {
        min_exp <- input$y_log_min_exponent
        max_exp <- input$y_log_max_exponent
        if (is.null(y_limits)) {
          if (max_exp > 0) {
            max_yax <- 10^(max_exp + 1)
          } else {
            max_yax <- 10^max_exp
          }
          y_limits <- c(10^min_exp, max_yax)
        }
        y_minor <- c(rep(1:9, max_exp - min_exp + 1) * (10^rep(min_exp:max_exp, each=9)))
        if (max_exp >= 0) y_minor <- c(y_minor, 10^(max_exp+1))
        p <- p + scale_y_log10(
          limits = y_limits, 
          expand = y_expand,
          minor_breaks = y_minor,
          guide = guide_prism_minor()
        )
      } else {
        p <- p + scale_y_log10(limits = y_limits, expand = y_expand)
      }
    } else if (input$y_scale_type == "sqrt") {
      p <- p + scale_y_sqrt(limits = y_limits, expand = y_expand)
    } else if (input$y_scale_type == "reverse") {
      p <- p + scale_y_reverse(limits = y_limits, expand = y_expand)
    } else {
      p <- p + scale_y_continuous(limits = y_limits, expand = y_expand)
    }
    
    title_font_face <- if (input$bold_title) "bold" else "plain"
    axis_label_font_face <- if (input$italic_axis_labels) "italic" else "plain"
    
    base_theme <- if (input$use_advanced_ticks) {
      theme_prism(border = TRUE)
    } else {
      theme_pubr()
    }
    
    p <- p + base_theme +
      theme(
        text = element_text(family = input$font_family),
        plot.title = element_text(
          size = input$title_font_size, 
          face = title_font_face,
          hjust = 0.5
        ),
        axis.title = element_text(
          size = input$axis_label_font_size,
          face = axis_label_font_face
        ),
        axis.text = element_text(
          size = input$axis_text_font_size
        ),
        axis.text.x = element_text(
          angle = as.numeric(input$axis_text_angle),
          hjust = if (as.numeric(input$axis_text_angle) > 0) 1 else 0.5,
          vjust = if (as.numeric(input$axis_text_angle) > 0) 1 else 0.5
        ),
        panel.grid.major = if (input$show_major_gridlines) {
          major_color <- if (!is.null(input$major_gridline_color) && input$major_gridline_color == "custom") {
            input$major_gridline_color_custom
          } else {
            input$major_gridline_color
          }
          element_line(
            color = major_color, 
            linewidth = input$major_gridline_size
          )
        } else {
          element_blank()
        },
        panel.grid.minor = if (input$show_minor_gridlines) {
          minor_color <- if (!is.null(input$minor_gridline_color) && input$minor_gridline_color == "custom") {
            input$minor_gridline_color_custom
          } else {
            input$minor_gridline_color
          }
          element_line(
            color = minor_color,
            linewidth = input$minor_gridline_size
          )
        } else {
          element_blank()
        },
        axis.ticks = element_line(color = "black", linewidth = 0.5),
        axis.ticks.length = unit(0.15, "cm"),
        legend.position = "right",
        legend.background = element_rect(fill = "white", color = "gray80"),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5)
      )
    
    if (input$custom_aspect_ratio) {
      p <- p + theme(aspect.ratio = 1/input$aspect_ratio)
    }
    
    p <- p + coord_cartesian(clip = "off")
    
    p <- p + labs(
      x = input$x_axis_label,
      y = input$y_axis_label,
      title = input$plot_title,
      subtitle = if (input$plot_subtitle != "") input$plot_subtitle else NULL
    )
    
    return(p)
  }
  
  output$plot_container <- renderUI({
    plotOutput("od_plot", height = paste0(input$plot_height, "px"), width = paste0(input$plot_width, "px"))
  })
  
  output$od_plot <- renderPlot({
    generate_plot()
  }, res = 96)
  
  output$downloadPlot <- downloadHandler(
    filename = function() {
      paste("od_plot_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".", input$export_format, sep = "")
    },
    content = function(file) {
      p <- generate_plot()
      ggsave(file, plot = p, device = input$export_format, 
             width = input$export_width, height = input$export_height, 
             units = "in", dpi = input$export_dpi)
    }
  )
}

shinyApp(ui, server)