# © 2025 The Johns Hopkins University Applied Physics Laboratory LLC
# Development of this software was sponsored by the U.S. Government under
# contract no. 75D30124C19958


## Set up label tool tips lookup
sb_ll <- list(
    res = list(
      l = "Resolution:",
      m = "Choose the resolution of clustering (zip or county)"
    ),
    outcome_annotation = list(
      l = "Optional: Indicated Outcome/Category",
      m = "Provide optional text/description of the outcome/category characterized in local file"
    ),
    state = list(
      l = "State",
      m = "Select a single state (allows zip code or county clustering), or United States (county only)"
    ),
    data_type = list(
      l = HTML("<h6>Data Type</>"),
      m = "Choose between TableBuilder api query vs. Data Details (slower, but can allow\n
      for de-duplication. Note this is only available for single state calls, not national"
    ),
    data_source = list(
      l = HTML("<h6>Data Source</>"),
      m = "Choose between Patient Level or Facility Level; The latter will assign encounters
      to the patient (home) location, while the latter assigns the encounter to the 
      location where the encounter occured (Not available for zip code, or Data Details
      queries.)"
    ),
    local_or_nssp = list(
      l = HTML("<h6>Data Source</>"),
      m = "Load a local csv file with counts by location and date, or construct an NSSP call using the API"
    ),
    local_file_upload = list(
      l = "Upload File",
      m = "Load a csv file; must include columns names 'location', 'date', 'count', in that order"
    ),
    ad_hoc_vs_built=list(
      l = "URL Option:",
      m = "If you have your own custom built URL, choose 'Ad-hoc URL', else choose 'URL Builder' to 
      interacively build an API Call"
    ),
    url_date_change = list(
      l = "Update Custom URL Dates",
      m = "Choose date range to change start and end date in url. Note, the url will
      not update if you choose a start date after an end date"
    ),
    data_load_drange = list(
      l = "Data Date Range",
      m = "Choose a date range for api call. Default is 120 days. We recommend a date
      range sufficient to allow a 7 date test interval preceded by 90 days of baseline"
    ),
    # remove these four later..
    # us_matrix = list(
    #   l = "Pre-Load the US Distance Matrix",
    #   m = "Enabling this option will preload the US Distance Matrix, which takes a couple of seconds, 
    #   but speeds up calculation of subsequent distance matrices. We suggest enabling if you are 
    #   going to examine multiple states in a single session, or do a national level query."
    # ),
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
    )
    
    
)


dl_sidebar_ui <- function(id) {
  ns <- NS(id)
  
  outcome_annotation <- textInput(
    ns("outcome_annotation"),
    label = labeltt(sb_ll[["outcome_annotation"]])
  )
  
  pt_acc_panel <- accordion_panel(
    value = "pt_acc_panel",
    title = "Location and Time",
    radioButtons(
      inputId = ns("res"),
      label = labeltt(sb_ll[["res"]]),
      choices = c("Zip code" = "zip", "County" = "county"),
      selected = character(0), 
      inline=TRUE
    ), 
    selectInput(
      inputId = ns("state"),
      label = labeltt(sb_ll[["state"]]),
      choices = c(sort(c(state.name, "District of Columbia")),"United States"),
      selected = "Alabama"
    ),
    dateRangeInput(
      inputId = ns("data_load_drange"),
      label = labeltt(sb_ll[["data_load_drange"]]),
      end = Sys.Date()-7,
      start = Sys.Date()-126,
      min = Sys.Date()-365,
      max = Sys.Date()
    )
  )

  data_type_button <- tagList(
    radioButtons(
      inputId = ns("data_type"),
      label = labeltt(sb_ll[["data_type"]]),
      choices = c("Table Builder" = "table", "Data Details" = "details"),
      selected = "table",
      inline = TRUE
    ),
    conditionalPanel(
      condition = "input.data_type == 'details'",
      input_switch(id = ns("dedup"), label = "De-duplicate?", value=TRUE),
      ns=ns
    )
  )

  data_source_button <- radioButtons(
    inputId = ns("data_source"),
    label = labeltt(sb_ll[["data_source"]]),
    choices = c("Patient" = "patient", "Facility" = "facility"),
    selected = "patient",
    inline = TRUE
  )
  # us_matrix_checkbox <- checkboxInput(
  #   inputId = ns("us_matrix"),
  #   label = labeltt(sb_ll[["us_matrix"]]),
  #   value=F
  # )
  
  options_card <- card(
    card_body(
      hidden(accordion(
        id = ns("options_accordion"),
        multiple=FALSE, open = FALSE,
        accordion_panel(
          "Advanced Options",
          data_type_button,
          data_source_button
          #us_matrix_checkbox
        )
      )),
    ),
    class = 'bg-transparent border-0'
  )
  
  local_or_nssp_ui <- tagList(
    radioButtons(
      inputId = ns("local_or_nssp"),
      label = labeltt(sb_ll[["local_or_nssp"]]),
      choices = c("Local File" = "local", "NSSP API Call" = "nssp"),
      selected = "local",
      inline = TRUE
    ),
    conditionalPanel(
      condition="input.local_or_nssp == 'nssp'",
      radioButtons(
          inputId = ns("ad_hoc_vs_built"),
          label = labeltt(sb_ll[["ad_hoc_vs_built"]]),
          choices=c("Ad-hoc URL" = "ad_hoc", "URL Builder" = "built"),
          selected="built",
          inline=TRUE
      ), 
      conditionalPanel(
        condition="input.ad_hoc_vs_built == 'ad_hoc'",
        card(
          textAreaInput(ns("custom_url"), "Custom URL",height = "200px"),
          dateRangeInput(
            ns("url_date_change"),
            label = labeltt(sb_ll[["url_date_change"]])
          ),
          full_screen = F,
          class = 'bg-transparent border-0'
        ),
        ns = ns
      ),
      ns=ns
    ),
    hidden(fileInput(
      inputId = ns("local_file_upload"),
      label = labeltt(sb_ll[["local_file_upload"]])
    )),
    hidden(outcome_annotation)
  )
  
  
  tagList(
    local_or_nssp_ui, 
    card(
      card_body(
        hidden(accordion(
          id = ns("main_accordion"),
          h6("Data Specifications"),
          multiple = FALSE,
          pt_acc_panel
        )),
        style = "overflow: visible"
      ),
    class = 'bg-transparent border-0',
    style = "overflow: visible"
    ),
    options_card
  )
}

dl_sidebar_server <- function(id, dc, cc, profile, valid_profile) {
  moduleServer(
    id,
    function(input, output, session) {


      ns=session$ns

      # Set Data Config Global Reactive Values
      observe(dc$USE_NSSP <- input$local_or_nssp=="nssp")
      observe(dc$data_type <- input$data_type)
      observe(dc$data_source <- input$data_source)
      observe(dc$res <- input$res)
      observe(dc$dedup <- input$dedup)
      observe(dc$state <- input$state)
      observe(dc$state2 <- state2())
      observe(dc$data_load_start <- input$data_load_drange[1])
      observe(dc$data_load_end <- input$data_load_drange[2])
      observe(dc$ad_hoc <- input$ad_hoc_vs_built == "ad_hoc")
      observe(dc$custom_url <- input$custom_url)
      observe(dc$source_data <- source_data())
      observe(dc$url_params <- url_params())
      observe(dc$source_data <- source_data())
      observe(dc$synd_summary <- synd_summary())

      # if ALLOW SHINY CREDENTIALS is FALSE, but the
      # profile is still not valid, then hide the api choice
      observe({
        toggleElement(
          "local_or_nssp",
          condition = ALLOW_SHINY_CREDENTIALS || valid_profile()
        )
      }) |> bindEvent(valid_profile())

      # Observe the choice for local vs API. If the latter
      # add the US to state dropdown
      observe({
        if(ALLOW_SHINY_CREDENTIALS == TRUE) {
          if(dc$USE_NSSP && !valid_profile()) {
              credServer("creds", profile, valid_profile)
            }
        }
        # use_nssp changes, reset the states list
        if(dc$USE_NSSP) {
          reset_states()
          updateRadioButtons(inputId = "res",selected="county")
        } else {
          updateRadioButtons(inputId = "res",selected=character(0))
        }
        
      }) |> bindEvent(dc$USE_NSSP)


      # hide/show sidebar elements
      observe({
        hide_show_sidebar_elements(
          use_nssp = dc$USE_NSSP,
          url_builder = input$ad_hoc_vs_built,
          file_uploaded = !is.null(input$local_file_upload$datapath)
        )
        if(dc$USE_NSSP && input$ad_hoc_vs_built == "built") {
          accordion_panel_insert(
            id = "main_accordion",
            panel = create_syndrome_acc_panel(ns,base_vals()$ccdd_cats)
          )
          # reset the full set of choices for states in the dropdown
          reset_states()

        } else {
          accordion_panel_remove("main_accordion", "cat_acc_panel")
        }
      }) |> bindEvent(dc$USE_NSSP, input$ad_hoc_vs_built, input$local_file_upload$datapath, profile())


      reset_states <- function() {
        updateSelectInput(
          session = session,
          inputId = "state",
          choices = c(sort(c(state.name,"District of Columbia")), "United States"),
          selected = "Alabama"
        )
      }


      # Gather the base values.. depending on the use nnsp value
      base_vals <- reactive({
        if(valid_profile()) get_base_vals(TRUE,profile())
      }) |> bindEvent(profile())


      # Update the choices for syndromic categories
      observe({
        req(base_vals())
        req(input$synd_cat)
        # get the set of choices
        sc = list(ccdd=base_vals()$ccdd_cats,
                  synd=base_vals()$syndromes,
                  subsynd=base_vals()$subsyndromes)[[input$synd_cat]]

        updateSelectInput(
          session = session,
          inputId = "synd_drop_menu",
          choices = sc
        )
      })

      # Set the abbreviation based on the state name selector
      state2 <- reactive({
        if(input$state == "United States") return("US")
        if(input$state == "District of Columbia") return("DC")
        state.abb[state.name == input$state]
      })


      use_pre_calc_matrix <- reactiveVal(FALSE)
      observe(use_pre_calc_matrix(TRUE)) |>
        bindEvent(input$us_matrix, ignoreInit = TRUE, once=TRUE)

      #pre_calc_matrix <- reactive(splineClusterDetector::us_distance_matrix()) |>
      pre_calc_matrix <- reactive(us_distance_matrix()) |>
        bindEvent(use_pre_calc_matrix())


      ## Observe the url box
      observe({
        req(dc$custom_url)

        if(input$ad_hoc_vs_built == "ad_hoc") {
          # guess the resolution from the url
          res_guess <- fifelse(
            grepl("zip",dc$custom_url,ignore.case=T),
            "zip",
            "county"
          )

          # update the resolution based on this guess
          updateRadioButtons(session=session, inputId = "res", selected=res_guess )

          # update the states based on this resolution
          states = get_states_from_location(
            unique(extract_locations_from_url(dc$custom_url, res_guess)),
            res_guess
          )
          choices = state.name[which(state.abb %in% states)]
          if("DC" %in% states) {
            choices = sort(c(choices, "District of Columbia"))
          }
          updateSelectInput(
            session = session,
            inputId = "state",
            choices = choices
          )

          # Get the dates in url
          dates = extract_dates_from_url(dc$custom_url)

          # # update the detect date based on enddate in url
          # date_guess = fifelse(
          #   dc$custom_url=="",
          #   Sys.Date()-7,
          #   dates[["end"]]
          # )
          # 
          # #if(is.na(date_guess)) date_guess = dc$end_date
          # if(is.null(date_guess) || is.na(date_guess)) date_guess = Sys.Date()-7
          # updateDateInput(
          #   session=session,
          #   inputId = "end_date",
          #   value = date_guess
          # )


          # What about updating the date input range.. The problem here is
          # that we might enter a reactivity loop. How do we prevent that
          freezeReactiveValue(input, "url_date_change")

          dates = lapply(dates, \(d) if(is.na(d)) NULL else d)
          updateDateRangeInput(
            inputId = "url_date_change",
            start = dates[["start"]],
            end = dates[["end"]]
          )
        }

      }) |> bindEvent(dc$custom_url, input$ad_hoc_vs_built, dc$USE_NSSP)

      # observe the url date change
      observe({

        req(dc$custom_url)

        tryCatch(
          {
            #new_url = Rnssp::change_dates(
            new_url = inject_dates_into_url(
              dc$custom_url,
              start_date = input$url_date_change[1],
              end_date = input$url_date_change[2]
            )

            updateTextAreaInput(
              inputId = "custom_url",value = new_url
            )
          },
          # TODO: this captures the error, but does not alert the user
          # because there is no where to place the error message!!
          error=function(e) stop(paste0(str(e), "Error!!"))
        )

      }) |> bindEvent(input$url_date_change)

      # url_comb is a reactive that holds the url
      url_params <- reactive({

        #start_date <- dc$end_date - cc$test_length + 1 - dc$baseline_length
        
        if(dc$data_type=="details" & dc$data_source == "facility") {
          fields = "HospitalZip"
        } else {
          fields = NULL
        }
        list(
          state = state2(),
          synd_cat = input$synd_cat,
          synd_drop_menu = input$synd_drop_menu,
          start_date = input$data_load_drange[1],
          end_date = input$data_load_drange[2],
          res = dc$res,
          data_type = dc$data_type,
          data_source = dc$data_source,
          fields = fields
        )
      })

      # source_data
      source_data <- reactive({

        # if USE_NSSP, then we simply construct the url (if built), or the user
        # supplied url, but don't yet call api;
        # the call to api  will be triggered only by the ingest data button
        if(dc$USE_NSSP == TRUE && valid_profile()==TRUE) {
          if(input$ad_hoc_vs_built == "built") return(url_params())
          else(return(dc$custom_url))
        }
        # # otherwise, we read a local file (if the path has been provided)
        if(dc$USE_NSSP == FALSE & !is.null(input$local_file_upload$datapath)) {
          d <- fread(input$local_file_upload$datapath)
          setnames(d, new=tolower(names(d)))
          d$location <- as.character(d$location)
          d$date <- as.Date(d$date)
          return(d)
        }
      })

      # Observe for changes in source_data, and if local, limit states
      # and set the date in the time dropdown
      observe({
        req(source_data())
        if(dc$USE_NSSP == F) {
          # get the states
          states = get_states_from_location(
            locations = source_data()[["location"]],
            res = dc$res
          )
          choices = state.name[which(state.abb %in% states)]
          if("DC" %in% states) {
            choices = sort(c(choices, "District of Columbia"))
          }
          updateSelectInput(
            session = session,
            inputId = "state",
            choices = c(choices, "United States")
          )
          req(source_data())
          updateDateRangeInput(
            session = session,
            inputId = "data_load_drange",
            start = source_data()[["date"]] |> min(),
            end = source_data()[["date"]] |> max(),
            min = source_data()[["date"]] |> min(),
            max = source_data()[["date"]] |> max(),
          )
        }
      }) |> bindEvent(source_data(), dc$res)


      synd <- reactive({
        list(
          synd_cat = ifelse(is.null(input$synd_cat) | dc$USE_NSSP == F, "none", input$synd_cat),
          synd_drop_menu = ifelse(is.null(input$synd_drop_menu) | dc$USE_NSSP == F, "none", input$synd_drop_menu)
        )
      })

      synd_summary <- reactive({
        k <- paste0(dc$state, " - ",dc$res, " - ", cc$end_date)
        if(dc$USE_NSSP) {
          k <- paste0(
            k, " - ",
            switch (
                 synd()$synd_cat,
                 "ccdd" = "CCDD Category",
                 "synd" = "Syndrome",
                 "subsynd" = "Sub-Syndrome",
                 "none" = "undefined",
            ), " - ",
            synd()$synd_drop_menu
          )
        } else {
          k <- paste0(k, " - ", input$outcome_annotation)
        }
        return(tools::toTitleCase(k))
      })

    }
  )
}


hide_show_sidebar_elements <- function(use_nssp, url_builder, file_uploaded) {
  
  accordion_panel_close("options_accordion",TRUE)
  
  if(use_nssp == TRUE) {
    
    hideElement("outcome_annotation")
    hideElement("local_file_upload")
    showElement("main_accordion")
    showElement("options_accordion")
    
    if(url_builder == "ad_hoc") {
      hideElement("data_type")
      hideElement("data_source")
    } else {
      showElement("data_type")
      showElement("data_source")
    }
  } else {
    showElement("local_file_upload")
    hideElement("data_type")
    hideElement("data_source")

    if(file_uploaded) {
      showElement("main_accordion")
      showElement("options_accordion")
      showElement("outcome_annotation")
    } else {
      hideElement("main_accordion")
      hideElement("options_accordion")
      hideElement("outcome_annotation")
    }
  }
}

# Function to create the syndrome panel
create_syndrome_acc_panel <- function(ns, cats) {
  
  accordion_panel(
    value = "cat_acc_panel",
    title = "Syndromic Category",
    radioButtons(
      inputId = ns("synd_cat"),
      label = "Syndromic Category:",
      choices = c(
        "CCDD" = "ccdd",
        "Syndrome" = "synd",
        "Sub-Syndrome" = "subsynd"
      )
    ),
    selectInput(
      inputId = ns("synd_drop_menu"),
      label="Select Type",
      choices = cats
    )
  )
}
