# © 2025 The Johns Hopkins University Applied Physics Laboratory LLC
# Development of this software was sponsored by the U.S. Government under
# contract no. 75D30124C19958


## Set up label tool tips lookup
clsb_ll <- list(
    radius = list(
      l = "Radius for Cluster Computation",
      m = "Indicate the radius of the spatial clustering cylinder, in miles"
    ),
    end_date = list(
      l = "End Date",
      m = "Final/Last date of the test period"
    ),
    test_length = list(
      l = "Max Cluster Days",
      m = "Length in days of the test interval; equivalently, the maximum number of days for the cluster"
    ),
    baseline_length = list(
      l = "Baseline Length",
      m = "Number of days in the baseline interval (min 1, max 365)"
    ),
    minimum_cluster_count = list(
      l = "Minimum Cluster Count",
      m = "Optionally change minimum number of records in clusters, defaults to 2"
    ),
    maximum_cluster_count = list(
      l = "Maximum Cluster Count",
      m = "Optionally change maximum number of records in clusters, defaults to no limit"
    ),
    base_adj_meth = list(
      l = "Baseline Adjustment Method",
      m = "Method used to adjust the baseline counts to avoid divide-by-zero issues in estimating the log(observed/expected).
      Choices include: add a constant (1) to the expected only when otherwise a divide-by-zero would occur; include
      the test interval data in the baseline counts, and no adjustment.
      "
    )
)


clust_sidebar_ui <- function(id) {
  ns <- NS(id)
  

  # basic_acc_panel <- accordion_panel(
  #   value="basic_acc_panel",
  #   title="Basic Options",
  #   radioButtons(
  #     inputId = ns("radius"),
  #     label = labeltt(clsb_ll[["radius"]]),
  #     choices = c(15, 20, 25, 50, 100, 200),
  #     selected = 15,
  #     inline = TRUE
  #   ), 
  #   dateInput(
  #     inputId = ns("end_date"),
  #     label = labeltt(clsb_ll[["end_date"]]),
  #     value = Sys.Date() - 7
  #   ),
  #   sliderInput(
  #     inputId = ns("test_length"),
  #     label = labeltt(clsb_ll[["test_length"]]),
  #     min = 1,
  #     max = 14,
  #     value = 7
  #   ),
  #   numericInput(
  #     inputId = ns("baseline_length"),
  #     label = labeltt(clsb_ll[["baseline_length"]]),
  #     value = 90,
  #     min = 7,
  #     max = 365
  #   )
  # )
  
  # us_matrix_checkbox <- checkboxInput(
  #   inputId = ns("us_matrix"),
  #   label = labeltt(clsb_ll[["us_matrix"]]),
  #   value=F
  # ) 
  spline_selector <- selectInput(
    inputId = ns("spline"),
    label = "Select Spline-based Classifier",
    choices = names(SPLINE_LIBRARY), 
    selected = "Spline-0.01"
  )
  min_clust_count_selector <- numericInput(
    inputId = ns("minimum_cluster_count"),
    label = labeltt(clsb_ll[["minimum_cluster_count"]]),
    value = 2,
    min = 1,
    max = Inf
  )
  max_clust_count_selector <- numericInput(
    inputId = ns("maximum_cluster_count"),
    label = labeltt(clsb_ll[["maximum_cluster_count"]]),
    value = Inf,
    min = 1,
    max = Inf
  )
  base_adj_meth <- selectInput(
    inputId = ns("base_adj_meth"),
    label = labeltt(clsb_ll[["base_adj_meth"]]),
    choices = c("Add One As Needed" = "add_one",
                #"Add One to All Locations" = "add_one_global", 
                "Add Test Data to Baseline" = "add_test",
                "No Adjustment" = "none")
  )
  
  adv_acc_panel <- accordion_panel(
    value = "adv_acc_panel",
    title = "Advanced Options",
    spline_selector,
    min_clust_count_selector,
    max_clust_count_selector,
    base_adj_meth
  )

  tagList(
    card(
      card_header("Clustering Specifications",class = "bg-primary"),
      card_body(
        radioButtons(
          inputId = ns("radius"),
          label = labeltt(clsb_ll[["radius"]]),
          choices = c(15, 20, 25, 50, 100, 200),
          selected = 15,
          inline = TRUE
        ), 
        dateInput(
          inputId = ns("end_date"),
          label = labeltt(clsb_ll[["end_date"]]),
          value = Sys.Date() - 7
        ),
        sliderInput(
          inputId = ns("test_length"),
          label = labeltt(clsb_ll[["test_length"]]),
          min = 1,
          max = 14,
          value = 7
        ),
        numericInput(
          inputId = ns("baseline_length"),
          label = labeltt(clsb_ll[["baseline_length"]]),
          value = 90,
          min = 7,
          max = MAX_DATE_RANGE
        ),
        accordion(
          id = ns("main_accordion"),
          multiple = FALSE,
          open=FALSE,
          #basic_acc_panel,
          adv_acc_panel
        ),
        style = "overflow: visible"
      ),
    class = 'bg-transparent border-0',
    style = "overflow: visible"
    )
  )
}

clust_sidebar_server <- function(id, results, dc, cc) {
  moduleServer(
    id,
    function(input, output, session) {


      ns=session$ns
      
      # Set Cluster Config Global Reactive Values
      observe(cc$spline_lookup <- SPLINE_LIBRARY[[input$spline]])
      observe(cc$spline_value <- input$spline)
      observe(cc$radius <- as.numeric(input$radius))
      observe(cc$end_date <- input$end_date)
      observe(cc$baseline_length <- as.numeric(input$baseline_length))
      observe(cc$test_length <- as.numeric(input$test_length))
      observe(cc$base_adj_meth <- input$base_adj_meth)
      observe(cc$minimum_cluster_count <- as.numeric(input$minimum_cluster_count))
      observe(cc$maximum_cluster_count <- as.numeric(input$maximum_cluster_count))

      # Update the default radius when res changes
      observe({
        updateRadioButtons(
          session=session,
          inputId = "radius",
          selected=ifelse(dc$res == "zip", 15, 50)
        )
      }) |> bindEvent(dc$res)
      
      # Update the end_date, and baseline_length (value, and max) when
      # the result$records (i.e. the input data) changes
      observe({
        updateDateInput(inputId = "end_date", value = results$records[, max(date)])
        updateNumericInput(
          inputId = "baseline_length",
          value=min(c(
            90,
            results$records[, max(date)] - input$test_length + 1 - results$records[, min(date)]
          ))
        )
      }) |> bindEvent(results$records)

    }
  )
}


