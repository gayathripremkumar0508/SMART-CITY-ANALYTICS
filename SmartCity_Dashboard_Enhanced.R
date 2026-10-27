library(shiny)
library(ggplot2)
library(dplyr)
library(DT)
library(plotly)

# Load the existing cleaned dataset
clean_data <- readRDS("clean_data.rds")

# Prepare columns safely
if ("cluster" %in% names(clean_data)) {
  clean_data$cluster <- factor(clean_data$cluster)
}

# Convert dates if necessary
if ("date_time" %in% names(clean_data)) {
  if (!inherits(clean_data$date_time, "POSIXct") &&
      !inherits(clean_data$date_time, "Date")) {
    clean_data$date_time <- as.POSIXct(
      clean_data$date_time,
      tz = "Asia/Kolkata"
    )
  }
}

# Dropdown choices
category_choices <- sort(unique(as.character(
  clean_data$category_title[
    !is.na(clean_data$category_title) &
      trimws(as.character(clean_data$category_title)) != "" &
      clean_data$category_title != "NULL"
  ]
)))

status_choices <- sort(unique(as.character(
  clean_data$complaint_status_title[
    !is.na(clean_data$complaint_status_title) &
      trimws(as.character(clean_data$complaint_status_title)) != "" &
      clean_data$complaint_status_title != "NULL"
  ]
)))

cluster_choices <- if ("cluster" %in% names(clean_data)) {
  sort(unique(as.character(na.omit(clean_data$cluster))))
} else {
  character(0)
}

# Fields to show in the complaint table, when available
preferred_columns <- c(
  "created_at",
  "date_time",
  "category_title",
  "complaint_status_title",
  "ward_title",
  "cluster",
  "latitude",
  "longitude"
)

display_columns <- intersect(preferred_columns, names(clean_data))

# Dashboard UI
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body {
        background-color: #F7F4FC;
        color: #302047;
        font-family: Arial, sans-serif;
      }

      .container-fluid {
        padding: 22px 28px;
      }

      .main-title {
        color: #493064;
        font-weight: bold;
        margin-bottom: 5px;
      }

      .subtitle {
        color: #786889;
        margin-bottom: 24px;
      }

      .sidebar {
        background: #FFFFFF;
        border: 1px solid #E8DFF4;
        border-radius: 15px;
        padding: 20px;
      }

      .well {
        background-color: #FFFFFF;
        border: 1px solid #E8DFF4;
        border-radius: 14px;
        box-shadow: 0 3px 12px rgba(75, 46, 105, 0.06);
      }

      .kpi-button {
        display: block;
        width: 100%;
        min-height: 142px;
        white-space: normal;
        text-align: left;
        padding: 17px;
        background: #FFFFFF;
        border: 1px solid #E1D4F1;
        border-radius: 15px;
        color: #302047;
        box-shadow: 0 4px 12px rgba(75, 46, 105, 0.06);
        transition: all 0.2s ease;
      }

      .kpi-button:hover {
        background: #F0E8FC;
        border-color: #9871CC;
        transform: translateY(-3px);
        color: #302047;
      }

      .kpi-button:focus {
        background: #EEE5FA;
        color: #302047;
        outline: 2px solid #9871CC;
      }

      .kpi-label {
        display: block;
        font-size: 14px;
        color: #786889;
        margin-bottom: 8px;
      }

      .kpi-number {
        display: block;
        font-size: 30px;
        font-weight: bold;
        color: #493064;
        margin-bottom: 7px;
      }

      .kpi-hint {
        display: block;
        font-size: 12px;
        color: #8054BD;
      }

      .section-panel {
        background: #FFFFFF;
        border: 1px solid #E8DFF4;
        border-radius: 15px;
        padding: 20px;
        margin-top: 20px;
        box-shadow: 0 3px 12px rgba(75, 46, 105, 0.04);
      }

      .section-title {
        color: #493064;
        font-weight: bold;
      }

      .nav-tabs > li > a {
        color: #604783;
      }

      .nav-tabs > li.active > a,
      .nav-tabs > li.active > a:hover {
        color: #493064;
        background: #FFFFFF;
        border-top: 3px solid #8054BD;
      }

      .btn-primary {
        background-color: #7650B8;
        border-color: #7650B8;
      }

      .btn-primary:hover {
        background-color: #604095;
        border-color: #604095;
      }

      .dataTables_wrapper {
        color: #302047;
      }

      .plot-container {
        padding-top: 8px;
      }

      .main-title {
        font-size: 30px;
        letter-spacing: -0.5px;
      }

      .subtitle {
        font-size: 14px;
        line-height: 1.6;
      }

      .sidebar h4 {
        color: #493064;
        font-weight: 700;
        margin-top: 4px;
        margin-bottom: 18px;
      }

      .form-control {
        border-radius: 9px;
        border-color: #DED2EC;
        box-shadow: none;
      }

      .form-control:focus {
        border-color: #9871CC;
        box-shadow: 0 0 0 3px rgba(152, 113, 204, 0.14);
      }

      .btn {
        border-radius: 9px;
        font-weight: 600;
      }

      .nav-tabs {
        border-bottom: 1px solid #E8DFF4;
      }

      .nav-tabs > li > a {
        border-radius: 9px 9px 0 0;
        font-weight: 600;
        padding: 12px 16px;
      }

      .section-panel {
        margin-bottom: 18px;
      }

      .plotly {
        max-width: 100%;
      }

      /* Dark theme: activated by the header toggle */
      body.dark-mode {
        background-color: #171421;
        color: #F1ECF8;
      }
      body.dark-mode .sidebar,
      body.dark-mode .well,
      body.dark-mode .section-panel,
      body.dark-mode .kpi-button {
        background: #242033;
        color: #F1ECF8;
        border-color: #49405D;
      }
      body.dark-mode .main-title,
      body.dark-mode .section-title,
      body.dark-mode .sidebar h4,
      body.dark-mode .kpi-number {
        color: #F1ECF8;
      }
      body.dark-mode .subtitle,
      body.dark-mode .kpi-label,
      body.dark-mode .kpi-hint,
      body.dark-mode .sidebar p {
        color: #C8B9DF !important;
      }
      body.dark-mode .form-control,
      body.dark-mode select,
      body.dark-mode input {
        background-color: #302A42;
        color: #F1ECF8;
        border-color: #55496D;
      }
      body.dark-mode .nav-tabs > li > a {
        color: #D8C8F0;
      }
      body.dark-mode .nav-tabs > li.active > a,
      body.dark-mode .nav-tabs > li.active > a:hover {
        background: #242033;
        color: #FFFFFF;
        border-color: #8054BD;
      }
      body.dark-mode .dataTables_wrapper,
      body.dark-mode table.dataTable,
      body.dark-mode table.dataTable thead th,
      body.dark-mode table.dataTable tbody td {
        color: #F1ECF8 !important;
      }
      body.dark-mode table.dataTable tbody tr,
      body.dark-mode table.dataTable tbody tr.odd,
      body.dark-mode table.dataTable tbody tr.even {
        background-color: #242033 !important;
      }
      body.dark-mode .kpi-button:hover {
        background: #352B4A;
        color: #FFFFFF;
      }
      .insight-item {
        padding: 12px 14px;
        border-radius: 10px;
        background: #F7F3FC;
        border: 1px solid #E8DFF4;
        margin-bottom: 8px;
        min-height: 64px;
      }
      body.dark-mode .insight-item {
        background: #302A42;
        border-color: #49405D;
        color: #F1ECF8;
      }

      @media (max-width: 768px) {
        .container-fluid {
          padding: 12px;
        }

        .main-title {
          font-size: 23px;
        }

        .kpi-button {
          min-height: 125px;
          margin-bottom: 12px;
        }

        .section-panel {
          padding: 12px;
        }
      }
    "))
  ),
  
  titlePanel(
    div(
      h2("SMART CITY | Civic Complaint Analytics",
         class = "main-title"),
      p("Explore complaint patterns, resolution status and geographical clusters",
        class = "subtitle"),
      actionButton(
        "theme_toggle",
        "🌙 Toggle Dark / Light Mode",
        class = "btn-primary"
      ),
      tags$script(HTML("$(document).on('click', '#theme_toggle', function() {
        $('body').toggleClass('dark-mode');
        var dark = $('body').hasClass('dark-mode');
        $(this).text(dark ? '☀️ Switch to Light Mode' : '🌙 Switch to Dark Mode');
      });"))
    )
  ),
  
  sidebarLayout(
    sidebarPanel(
      class = "sidebar",
      h4("Dashboard Filters"),
      
      selectInput(
        "category",
        "Complaint Category",
        choices = c("All Categories", category_choices),
        selected = "All Categories"
      ),
      
      selectInput(
        "status",
        "Complaint Status",
        choices = c("All Statuses", status_choices),
        selected = "All Statuses"
      ),
      
      selectInput(
        "cluster",
        "Geographical Cluster",
        choices = c("All Clusters", cluster_choices),
        selected = "All Clusters"
      ),
      
      actionButton(
        "reset_filters",
        "Reset Filters",
        icon = icon("refresh"),
        class = "btn-primary"
      ),
      
      hr(),
      
      p(
        "Click any KPI card to inspect the corresponding records.",
        style = "color:#786889; font-size:13px;"
      ),
      
      width = 3
    ),
    
    mainPanel(
      h3("City Overview", class = "section-title"),
      
      fluidRow(
        column(
          3,
          actionButton(
            "card_total",
            label = tagList(
              span("TOTAL COMPLAINTS", class = "kpi-label"),
              textOutput("total", inline = TRUE) |>
                tagAppendAttributes(class = "kpi-number"),
              span("Click to view all records →", class = "kpi-hint")
            ),
            class = "kpi-button"
          )
        ),
        
        column(
          3,
          actionButton(
            "card_open",
            label = tagList(
              span("OPEN COMPLAINTS", class = "kpi-label"),
              textOutput("open", inline = TRUE) |>
                tagAppendAttributes(class = "kpi-number"),
              span("Click to view open cases →", class = "kpi-hint")
            ),
            class = "kpi-button"
          )
        ),
        
        column(
          3,
          actionButton(
            "card_resolved",
            label = tagList(
              span("RESOLVED COMPLAINTS", class = "kpi-label"),
              textOutput("resolved", inline = TRUE) |>
                tagAppendAttributes(class = "kpi-number"),
              span("Click to view resolved cases →", class = "kpi-hint")
            ),
            class = "kpi-button"
          )
        ),
        
        column(
          3,
          actionButton(
            "card_clusters",
            label = tagList(
              span("GEO CLUSTERS", class = "kpi-label"),
              textOutput("clusters", inline = TRUE) |>
                tagAppendAttributes(class = "kpi-number"),
              span("Click to explore clusters →", class = "kpi-hint")
            ),
            class = "kpi-button"
          )
        )
      ),
      
      # Automatically generated insights based on the current filters
      div(
        class = "section-panel",
        h3("Key Insights", class = "section-title"),
        p("Insights update automatically when you change the filters.",
          style = "color:#786889;"),
        uiOutput("insights_panel")
      ),
      
      # Clickable cards open this detail section
      conditionalPanel(
        condition = "output.showDetails == true",
        
        div(
          class = "section-panel",
          
          fluidRow(
            column(
              8,
              h3(textOutput("detail_title"), class = "section-title")
            ),
            column(
              4,
              div(
                style = "text-align:right;",
                actionButton(
                  "close_details",
                  "Close Details",
                  icon = icon("times")
                )
              )
            )
          ),
          
          p(
            textOutput("detail_description"),
            style = "color:#786889;"
          ),
          
          fluidRow(
            column(
              12,
              downloadButton(
                "download_data",
                "Download CSV",
                class = "btn-primary"
              )
            )
          ),
          
          br(),
          
          conditionalPanel(
            condition = "output.showClusterSummary == true",
            DTOutput("cluster_summary_table")
          ),
          conditionalPanel(
            condition = "output.showClusterSummary != true",
            DTOutput("complaint_table")
          ),
          conditionalPanel(
            condition = "output.showClusterSummary != true && input.card_clusters > 0",
            actionButton(
              "back_cluster_summary",
              "Back to Cluster Summary",
              icon = icon("arrow-left")
            )
          )
        )
      ),
      
      div(
        class = "section-panel",
        
        tabsetPanel(
          id = "analysis_tabs",
          
          tabPanel(
            "Hotspots",
            br(),
            plotlyOutput("clusterPlot", height = "430px")
          ),
          
          tabPanel(
            "Categories",
            br(),
            plotlyOutput("categoryPlot", height = "430px")
          ),
          
          tabPanel(
            "Monthly Trends",
            br(),
            plotlyOutput("monthlyPlot", height = "430px")
          ),
          
          tabPanel(
            "Complaint Status",
            br(),
            plotlyOutput("statusPlot", height = "430px")
          ),
          
          tabPanel(
            "Top Wards",
            br(),
            plotlyOutput("wardPlot", height = "430px")
          )
        )
      ),
      
      width = 9
    )
  )
)

# Dashboard server
server <- function(input, output, session) {
  
  # Track the selected KPI view and optional selected cluster
  selected_view <- reactiveVal(NULL)
  selected_cluster <- reactiveVal(NULL)
  
  # Apply dropdown filters
  filtered_data <- reactive({
    
    data <- clean_data
    
    if (input$category != "All Categories") {
      data <- data[
        !is.na(data$category_title) &
          data$category_title == input$category,
        ,
        drop = FALSE
      ]
    }
    
    if (input$status != "All Statuses") {
      data <- data[
        !is.na(data$complaint_status_title) &
          data$complaint_status_title == input$status,
        ,
        drop = FALSE
      ]
    }
    
    if (input$cluster != "All Clusters" &&
        "cluster" %in% names(data)) {
      data <- data[
        !is.na(data$cluster) &
          as.character(data$cluster) == input$cluster,
        ,
        drop = FALSE
      ]
    }
    
    data
  })
  
  # The clicked card determines which records to display
  detail_data <- reactive({
    
    data <- filtered_data()
    
    if (identical(selected_view(), "open")) {
      data <- data[
        !is.na(data$complaint_status_title) &
          data$complaint_status_title == "Open",
        ,
        drop = FALSE
      ]
    }
    
    if (identical(selected_view(), "resolved")) {
      data <- data[
        !is.na(data$complaint_status_title) &
          data$complaint_status_title == "Resolved",
        ,
        drop = FALSE
      ]
    }
    
    
    if (identical(selected_view(), "clusters") &&
        !is.null(selected_cluster()) &&
        "cluster" %in% names(data)) {
      data <- data[
        !is.na(data$cluster) &
          as.character(data$cluster) == as.character(selected_cluster()),
        ,
        drop = FALSE
      ]
    }
    
    data
  })
  
  # Click handlers
  observeEvent(input$card_total, {
    selected_view("all")
  })
  
  observeEvent(input$card_open, {
    selected_view("open")
  })
  
  observeEvent(input$card_resolved, {
    selected_view("resolved")
  })
  
  observeEvent(input$card_clusters, {
    selected_cluster(NULL)
    selected_view("clusters")
  })
  
  observeEvent(input$close_details, {
    selected_view(NULL)
    selected_cluster(NULL)
  })
  
  # Reset all filters
  observeEvent(input$reset_filters, {
    updateSelectInput(
      session, "category", selected = "All Categories"
    )
    updateSelectInput(
      session, "status", selected = "All Statuses"
    )
    updateSelectInput(
      session, "cluster", selected = "All Clusters"
    )
    selected_cluster(NULL)
    selected_view(NULL)
  })
  
  # Show/hide the details section
  output$showDetails <- reactive({
    !is.null(selected_view())
  })
  
  outputOptions(
    output, "showDetails", suspendWhenHidden = FALSE
  )
  
  
  output$showClusterSummary <- reactive({
    identical(selected_view(), "clusters") && is.null(selected_cluster())
  })
  outputOptions(
    output, "showClusterSummary", suspendWhenHidden = FALSE
  )
  
  observeEvent(input$back_cluster_summary, {
    selected_cluster(NULL)
  })
  # Card values update with dropdown filters
  output$total <- renderText({
    format(nrow(filtered_data()), big.mark = ",")
  })
  
  output$open <- renderText({
    data <- filtered_data()
    format(
      sum(data$complaint_status_title == "Open", na.rm = TRUE),
      big.mark = ","
    )
  })
  
  output$resolved <- renderText({
    data <- filtered_data()
    format(
      sum(
        data$complaint_status_title == "Resolved",
        na.rm = TRUE
      ),
      big.mark = ","
    )
  })
  
  output$clusters <- renderText({
    data <- filtered_data()
    
    if (!"cluster" %in% names(data)) {
      return("N/A")
    }
    
    format(
      length(unique(na.omit(as.character(data$cluster)))),
      big.mark = ","
    )
  })
  
  # Filter-aware insights for quick interpretation
  output$insights_panel <- renderUI({
    data <- filtered_data()
    if (nrow(data) == 0) {
      return(div(class = "insight-item", "No complaints match the current filters."))
    }
    
    valid_categories <- data %>%
      filter(!is.na(category_title), trimws(as.character(category_title)) != "",
             category_title != "NULL") %>%
      count(category_title, sort = TRUE)
    valid_wards <- data %>%
      filter(!is.na(ward_title), trimws(as.character(ward_title)) != "",
             ward_title != "NULL", ward_title != "Other") %>%
      count(ward_title, sort = TRUE)
    
    total_n <- nrow(data)
    open_n <- sum(data$complaint_status_title == "Open", na.rm = TRUE)
    resolved_n <- sum(data$complaint_status_title == "Resolved", na.rm = TRUE)
    resolution_rate <- if (total_n > 0) round(100 * resolved_n / total_n, 1) else 0
    
    category_text <- if (nrow(valid_categories)) {
      paste0(valid_categories$category_title[1], " (", valid_categories$n[1], " complaints)")
    } else "Not available in this selection"
    ward_text <- if (nrow(valid_wards)) {
      paste0(valid_wards$ward_title[1], " (", valid_wards$n[1], " complaints)")
    } else "Not available in this selection"
    
    fluidRow(
      column(6, div(class = "insight-item",
                    strong("Most frequent category"), br(), category_text)),
      column(6, div(class = "insight-item",
                    strong("Ward with most complaints"), br(), ward_text)),
      column(4, div(class = "insight-item",
                    strong("Open complaints"), br(), format(open_n, big.mark = ","))),
      column(4, div(class = "insight-item",
                    strong("Resolved complaints"), br(), format(resolved_n, big.mark = ","))),
      column(4, div(class = "insight-item",
                    strong("Resolved share of filtered records"), br(), paste0(resolution_rate, "%")))
    )
  })
  
  # Detail section heading
  output$detail_title <- renderText({
    switch(
      selected_view(),
      all = "All Matching Complaints",
      open = "Open Complaints",
      resolved = "Resolved Complaints",
      clusters = if (is.null(selected_cluster())) {
        "Geographical Cluster Summary"
      } else {
        paste("Complaints in Cluster", selected_cluster())
      },
      "Complaint Details"
    )
  })
  
  output$detail_description <- renderText({
    if (identical(selected_view(), "clusters") &&
        is.null(selected_cluster())) {
      return("Select a cluster row below to inspect its individual complaints.")
    }
    data <- detail_data()
    paste(
      "Records displayed:",
      format(nrow(data), big.mark = ","),
      "| Filters are applied to the current selection."
    )
  })
  
  # One row per cluster; select a row to open that cluster's complaints
  output$cluster_summary_table <- renderDT({
    data <- filtered_data()
    validate(
      need("cluster" %in% names(data), "The cluster column is not available."),
      need(nrow(data) > 0, "No complaints match the current filters.")
    )
    
    summary <- data %>%
      filter(!is.na(cluster)) %>%
      mutate(cluster = as.character(cluster)) %>%
      count(cluster, name = "complaint_count") %>%
      arrange(cluster)
    
    validate(
      need(nrow(summary) > 0, "No geographical clusters match the current filters.")
    )
    
    datatable(
      summary,
      rownames = FALSE,
      selection = "single",
      colnames = c("Cluster ID", "Number of Complaints"),
      options = list(pageLength = 10, dom = "tip", ordering = FALSE),
      class = "stripe hover compact"
    )
  })
  
  observeEvent(input$cluster_summary_table_rows_selected, {
    selected_row <- input$cluster_summary_table_rows_selected
    if (length(selected_row) == 1) {
      summary <- filtered_data() %>%
        filter(!is.na(cluster)) %>%
        mutate(cluster = as.character(cluster)) %>%
        count(cluster, name = "complaint_count") %>%
        arrange(cluster)
      if (selected_row <= nrow(summary)) {
        selected_cluster(summary$cluster[selected_row])
      }
    }
  })
  
  # Interactive complaint table
  output$complaint_table <- renderDT({
    
    data <- detail_data()
    
    validate(
      need(
        nrow(data) > 0,
        "No complaints match this selection. Try changing the filters."
      )
    )
    
    # For cluster view, show cluster information first
    cols <- display_columns
    
    if (identical(selected_view(), "clusters") &&
        "cluster" %in% names(data)) {
      cols <- c("cluster", setdiff(cols, "cluster"))
    }
    
    if (length(cols) > 0) {
      data <- data[, cols, drop = FALSE]
    }
    
    datatable(
      data,
      rownames = FALSE,
      filter = "top",
      extensions = "Buttons",
      options = list(
        pageLength = 10,
        lengthMenu = c(5, 10, 25, 50),
        scrollX = TRUE,
        autoWidth = TRUE,
        dom = "Blfrtip",
        buttons = c("copy", "csv"),
        order = list()
      ),
      class = "stripe hover compact"
    )
  })
  
  # Download the currently displayed records
  output$download_data <- downloadHandler(
    filename = function() {
      paste0(
        "smart_city_complaints_",
        Sys.Date(),
        ".csv"
      )
    },
    
    content = function(file) {
      data <- detail_data()
      
      cols <- display_columns
      
      if (length(cols) > 0) {
        data <- data[, cols, drop = FALSE]
      }
      
      write.csv(data, file, row.names = FALSE, na = "")
    }
  )
  
  # Shared chart styling
  chart_theme <- theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(
        colour = "#493064",
        face = "bold",
        size = 15
      ),
      axis.title = element_text(colour = "#493064"),
      axis.text = element_text(colour = "#51445E"),
      panel.grid.major = element_line(colour = "#EDE5F6"),
      panel.grid.minor = element_blank()
    )
  
  # Geographical hotspots
  output$clusterPlot <- renderPlotly({
    
    data <- filtered_data()
    
    validate(
      need(nrow(data) > 0, "No complaints match these filters."),
      need(
        all(c("longitude", "latitude", "cluster") %in% names(data)),
        "Required geographical or cluster columns are missing."
      )
    )
    
    data <- data[
      !is.na(data$longitude) &
        !is.na(data$latitude),
      ,
      drop = FALSE
    ]
    
    validate(
      need(nrow(data) > 0, "No valid coordinates in this selection.")
    )
    
    p <- ggplot(
      data,
      aes(x = longitude, y = latitude, colour = cluster)
    ) +
      geom_point(alpha = 0.65, size = 1.6) +
      scale_colour_brewer(palette = "Set2", na.value = "#9B8AAE") +
      labs(
        title = "Geographical Complaint Clusters",
        x = "Longitude",
        y = "Latitude",
        colour = "Cluster"
      ) +
      chart_theme
    
    ggplotly(p, tooltip = "all") %>%
      config(displaylogo = FALSE, responsive = TRUE)
  })
  
  # Top complaint categories
  output$categoryPlot <- renderPlotly({
    
    data <- filtered_data() %>%
      filter(
        !is.na(category_title),
        trimws(as.character(category_title)) != "",
        category_title != "NULL"
      ) %>%
      count(category_title, sort = TRUE) %>%
      slice_head(n = 10)
    
    validate(
      need(nrow(data) > 0, "No categories match these filters.")
    )
    
    p <- ggplot(data, aes(x = reorder(category_title, n), y = n)) +
      geom_col(fill = "#8964C7", width = 0.72) +
      coord_flip() +
      labs(
        title = "Top Complaint Categories",
        x = "Category",
        y = "Number of Complaints"
      ) +
      chart_theme
    
    ggplotly(p, tooltip = "all") %>%
      config(displaylogo = FALSE, responsive = TRUE)
  })
  
  # Monthly complaint trends
  output$monthlyPlot <- renderPlotly({
    
    data <- filtered_data()
    
    validate(
      need(nrow(data) > 0, "No complaints match these filters."),
      need(
        "date_time" %in% names(data),
        "The date_time column is missing."
      )
    )
    
    dates <- as.Date(data$date_time)
    
    data$month <- format(dates, "%Y-%m")
    
    data <- data[
      !is.na(dates) &
        !is.na(data$month) &
        data$month != "",
      ,
      drop = FALSE
    ]
    
    data <- data %>%
      count(month, name = "total_complaints") %>%
      arrange(month)
    
    validate(
      need(nrow(data) > 0, "No valid dates are available.")
    )
    
    p <- ggplot(
      data,
      aes(x = month, y = total_complaints, group = 1)
    ) +
      geom_line(colour = "#8964C7", linewidth = 1.1) +
      geom_point(colour = "#604095", size = 2.5) +
      labs(
        title = "Monthly Complaint Trends",
        x = "Month",
        y = "Number of Complaints"
      ) +
      chart_theme +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p, tooltip = "all") %>%
      config(displaylogo = FALSE, responsive = TRUE)
  })
  
  # Complaint status distribution
  output$statusPlot <- renderPlotly({
    
    data <- filtered_data() %>%
      filter(
        !is.na(complaint_status_title),
        trimws(as.character(complaint_status_title)) != ""
      ) %>%
      count(complaint_status_title, sort = TRUE)
    
    validate(
      need(nrow(data) > 0, "No status data matches these filters.")
    )
    
    p <- ggplot(
      data,
      aes(x = reorder(complaint_status_title, n), y = n)
    ) +
      geom_col(fill = "#70A995", width = 0.72) +
      coord_flip() +
      labs(
        title = "Complaint Status Distribution",
        x = "Status",
        y = "Number of Complaints"
      ) +
      chart_theme
    
    ggplotly(p, tooltip = "all") %>%
      config(displaylogo = FALSE, responsive = TRUE)
  })
  
  # Top wards
  output$wardPlot <- renderPlotly({
    
    data <- filtered_data() %>%
      filter(
        !is.na(ward_title),
        trimws(as.character(ward_title)) != "",
        ward_title != "NULL",
        ward_title != "Other"
      ) %>%
      count(ward_title, sort = TRUE) %>%
      slice_head(n = 10)
    
    validate(
      need(nrow(data) > 0, "No ward data matches these filters.")
    )
    
    p <- ggplot(data, aes(x = reorder(ward_title, n), y = n)) +
      geom_col(fill = "#A58BC9", width = 0.72) +
      coord_flip() +
      labs(
        title = "Top 10 Wards by Complaint Count",
        x = "Ward",
        y = "Number of Complaints"
      ) +
      chart_theme
    
    ggplotly(p, tooltip = "all") %>%
      config(displaylogo = FALSE, responsive = TRUE)
  })
}

shinyApp(ui = ui, server = server)
