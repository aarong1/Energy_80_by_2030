#load libraries
library(shiny)
library(shinydashboard)
library(readxl)
library(dplyr)
library(tidyr)
library(plotly)
library(janitor)
library(stringr)


#clean data
classify_type <- function(x) {
  if (is.na(x) || trimws(x) == "") return(NA_character_)
  
  x <- gsub('"', '', x)
  x <- gsub("[\r\n]+", "/", x)  # line breaks -> "/"
  x <- gsub("\\s*/\\s*", "/", x)
  x <- trimws(x)
  xl <- tolower(x)
  
  parts <- unlist(strsplit(xl, "/", fixed = TRUE))
  parts <- trimws(parts)
  
  mapped <- character(0)
  for (p in parts) {
    if (grepl("new build", p)) mapped <- c(mapped, "New build")
    else if (grepl("uprate|upgrade|modify", p)) mapped <- c(mapped, "Upgrade/Modify")
    else if (grepl("refurbish|replace", p)) mapped <- c(mapped, "Refurbish/Replace")
  }
  
  mapped <- unique(mapped)
  
  if (length(mapped) == 0) return(x)          
  if (length(mapped) == 1) return(mapped[1])   
  return("Combination")                       
}

classify_type_vec <- Vectorize(classify_type)

#load data
data1 <- suppressWarnings(read_excel("./data/TDPNI_network_development_45_whole_projects_north_west_clean.xlsx")) %>%
  clean_names() %>%
  mutate(
    ecd_clean = ifelse(ecd == "TBC", NA, gsub("-.*", "", ecd)),
    ecd_clean = as.numeric(ecd_clean),
    type = classify_type_vec(type))

data2 <- suppressWarnings(read_excel("./data/TDPNI_network_development_45_whole_projects_south_east_clean.xlsx")) %>%
  clean_names() %>%
  mutate(
    ecd_clean = ifelse(ecd == "TBC", NA, gsub("-.*", "", ecd)),
    ecd_clean = as.numeric(ecd_clean),
    type = classify_type_vec(type))

data3 <- suppressWarnings(read_excel("./data/TDPNI_network_development_45_whole_projects_both_area_clean.xlsx")) %>%
  clean_names() %>%
  mutate(
    ecd_clean = ifelse(ecd == "TBC", NA, gsub("-.*", "", ecd)),
    ecd_clean = as.numeric(ecd_clean),
    type = classify_type_vec(type))

data4 <- suppressWarnings(read_excel("./data/TDPNI_network_development_45_projects_changes_clean.xlsx")) %>%
  clean_names() %>%
  mutate(change = tolower(change)) %>%
  mutate(change_category = case_when(
    str_detect(change, "new") ~ "New Project",
    str_detect(change, "rename") ~ "Renamed",
    str_detect(change, "hold") ~ "On Hold",
    str_detect(change, "remove") ~ "Removed",
    TRUE ~ "Other"))

data5 <- suppressWarnings(read_excel("./data/TDPNI_network_development_45_new_projects_clean.xlsx")) %>%
  clean_names()

data6 <- suppressWarnings(read_excel("./data/TDPNI_asset_replacement_rp6_clean.xlsx")) %>%
  clean_names() %>%
  mutate(
    ecd_clean = ifelse(ecd == "TBC", NA, gsub("-.*", "", ecd)),
    ecd_clean = as.numeric(ecd_clean),
    type = classify_type_vec(type))

data7 <- suppressWarnings(read_excel("./data/TDPNI_asset_replacement_rp7_clean.xlsx")) %>%
  clean_names() %>%
  mutate(
    ecd_clean = ifelse(ecd == "TBC", NA, gsub("-.*", "", ecd)),
    ecd_clean = as.numeric(ecd_clean),
    type = classify_type_vec(type))

data8 <- suppressWarnings(read_excel("./data/TDPNI_asset_replacement_d5_clean.xlsx")) %>%
  clean_names() %>%
  mutate(
    ecd_clean = ifelse(ecd == "TBC", NA, gsub("-.*", "", ecd)),
    ecd_clean = as.numeric(ecd_clean),
    type = classify_type_vec(type))


driver_cols <- c("new_connection", "security_of_supply", "res_integration", "market_integration")
need_cols   <- c("inter_regional_power_flows", "local_constraints", "connection", "inter_connection", "asset_condition")


drivers_long1 <- data1 %>%
  pivot_longer(cols = all_of(driver_cols), names_to = "driver", values_to = "has_driver") %>%
  filter(has_driver == "✓")

needs_long1 <- data1 %>%
  pivot_longer(cols = all_of(need_cols), names_to = "need", values_to = "has_need") %>%
  filter(has_need == "✓")

drivers_long2 <- data2 %>%
  pivot_longer(cols = all_of(driver_cols), names_to = "driver", values_to = "has_driver") %>%
  filter(has_driver == "✓")

needs_long2 <- data2 %>%
  pivot_longer(cols = all_of(need_cols), names_to = "need", values_to = "has_need") %>%
  filter(has_need == "✓")

drivers_long3 <- data3 %>%
  pivot_longer(cols = all_of(driver_cols), names_to = "driver", values_to = "has_driver") %>%
  filter(has_driver == "✓")

needs_long3 <- data3 %>%
  pivot_longer(cols = all_of(need_cols), names_to = "need", values_to = "has_need") %>%
  filter(has_need == "✓")




transmission_ui <- function(id) {
  ns <- NS(id)
  
  navlistPanel(header = div(class = 'mb-5'),
    id      = ns("tabs"),   
    widths  = c(3, 9),      
    fluid = TRUE,
    
    tabPanel(
      title = "Network Development — North & West",
      value = ns("north"),
      transmission_north_ui(ns("north"))
    ),
    
    tabPanel("Network Development — South & East", value = ns("south"), transmission_south_ui(ns("south"))),
    tabPanel("Network Development — Both Areas",   value = ns("both"),  transmission_both_ui(ns("both"))),
    
    tabPanel(
      title = "Project Changes",
      value = ns("changes"),
      transmission_changes_ui(ns("changes"))
    ),
    
    tabPanel("Asset Replacement — RP6", value = ns("rp6"),
             transmission_rp6_ui(ns("rp6"))),
    
    tabPanel("Asset Replacement — RP7", value = ns("rp7"),
             transmission_rp7_ui(ns("rp7"))),
    
    tabPanel("Asset Replacement — D5", value = ns("d5"),
             transmission_d5_ui(ns("d5")))
    
    
  )
}


transmission_server <- function(id, state) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    session$onFlushed(function() {
      updateTabsetPanel(session, inputId = "tabs", selected = ns("north"))
    }, once = TRUE)
    
    observe({
      message("tabs input = ", paste0(input$tabs, collapse = ""))
    })
    
    transmission_north_server("north")
    transmission_south_server("south")
    transmission_both_server("both")
    transmission_changes_server("changes")
    transmission_rp6_server("rp6")  
    transmission_rp7_server("rp7")
    transmission_d5_server("d5") 
  })
}





transmission_north_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      box(width = 3,
          selectInput(ns("category_nw"), "Categorize by:",
                      choices = c("type", "stage_part", "driver", "need"),
                      selected = "type"),
          selectInput(ns("cost_metric_nw"), "Cost Metric:",
                      choices = c("Total Project Cost" = "total_m",
                                  "TDPNI Period Cost" = "tdpni_total_m"),
                      selected = "total_m")
      ),
      box(width = 9,
          plotlyOutput(ns("costPlot_nw")),
          DT::dataTableOutput(ns("costTable_nw")),
          actionButton(ns("makePrioritized_nw"), "Create prioritized table", icon = icon("check")),
          actionButton(ns("resetPrioritized_nw"), "Reset", icon = icon("undo")),
          br(), br(),
          DT::dataTableOutput(ns("prioritizedTable_nw")),
          plotlyOutput(ns("ecdPlot_nw"))
      )
    )
  )
}

transmission_south_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      box(width = 3,
          selectInput(ns("category_se"), "Categorize by:",
                      choices = c("type", "stage_part", "driver", "need"),
                      selected = "type"),
          selectInput(ns("cost_metric_se"), "Cost Metric:",
                      choices = c("Total Project Cost" = "total_m",
                                  "TDPNI Period Cost" = "tdpni_total_m"),
                      selected = "total_m")
      ),
      box(width = 9,
          plotlyOutput(ns("costPlot_se")),
          DT::dataTableOutput(ns("costTable_se")),
          actionButton(ns("makePrioritized_se"), "Create prioritized table", icon = icon("check")),
          actionButton(ns("resetPrioritized_se"), "Reset", icon = icon("undo")),
          br(), br(),
          DT::dataTableOutput(ns("prioritizedTable_se")),
          plotlyOutput(ns("ecdPlot_se"))
      )
    )
  )
}

transmission_both_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      box(width = 3,
          selectInput(ns("category_both"), "Categorize by:",
                      choices = c("type", "stage_part", "driver", "need"),
                      selected = "type"),
          selectInput(ns("cost_metric_both"), "Cost Metric:",
                      choices = c("Total Project Cost" = "total_m",
                                  "TDPNI Period Cost" = "tdpni_total_m"),
                      selected = "total_m")
      ),
      box(width = 9,
          plotlyOutput(ns("costPlot_both")),
          DT::dataTableOutput(ns("costTable_both")),
          actionButton(ns("makePrioritized_both"), "Create prioritized table", icon = icon("check")),
          actionButton(ns("resetPrioritized_both"), "Reset", icon = icon("undo")),
          br(), br(),
          DT::dataTableOutput(ns("prioritizedTable_both")),
          plotlyOutput(ns("ecdPlot_both"))
      )
    )
  )
}

transmission_changes_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      box(width = 12,
          h4("Project Changes Since TDPNI 2023–2032"),
          plotlyOutput(ns("changePlot")),
          br(),
          DT::dataTableOutput(ns("changeTable"))
      )
    )
  )
}

transmission_rp6_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      box(width = 3,
          selectInput(ns("category_rp6"), "Categorize by:",
                      choices = c("type", "planning_area"),
                      selected = "type")
      ),
      box(width = 9,
          
          plotlyOutput(ns("costPlot_rp6")),
          DT::dataTableOutput(ns("costTable_rp6")),
          actionButton(ns("makePrioritized_rp6"), "Create prioritized table", icon = icon("check")),
          actionButton(ns("resetPrioritized_rp6"), "Reset", icon = icon("undo")),
          br(), br(),
          DT::dataTableOutput(ns("prioritizedTable_rp6")),
          plotlyOutput(ns("ecdPlot_rp6"))
      )
    )
  )
}

transmission_rp7_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      box(width = 3,
          selectInput(ns("category_rp7"), "Categorize by:",
                      choices = c("type", "planning_area"),
                      selected = "type")
      ),
      box(width = 9,
          plotlyOutput(ns("costPlot_rp7")),
          DT::dataTableOutput(ns("costTable_rp7")),
          actionButton(ns("makePrioritized_rp7"), "Create prioritized table", icon = icon("check")),
          actionButton(ns("resetPrioritized_rp7"), "Reset", icon = icon("undo")),
          br(), br(),
          DT::dataTableOutput(ns("prioritizedTable_rp7")),
          plotlyOutput(ns("ecdPlot_rp7"))
      )
    )
  )
}


transmission_d5_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      box(width = 3,
          selectInput(ns("category_d5"), "Categorize by:",
                      choices = c("type", "planning_area"),
                      selected = "type")
      ),
      box(width = 9,
          plotlyOutput(ns("costPlot_d5")),
          DT::dataTableOutput(ns("costTable_d5")),
          actionButton(ns("makePrioritized_d5"), "Create prioritized table", icon = icon("check")),
          actionButton(ns("resetPrioritized_d5"), "Reset", icon = icon("undo")),
          br(), br(),
          DT::dataTableOutput(ns("prioritizedTable_d5")),
          plotlyOutput(ns("ecdPlot_d5"))
      )
    )
  )
}

transmission_north_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    reactive_cost_data_nw <- reactive({
      cost_col <- input$cost_metric_nw
      cat      <- input$category_nw
      
      if (cat == "driver") {
        drivers_long1 %>%
          dplyr::group_by(driver) %>%
          dplyr::summarise(total_cost = sum(as.numeric(.data[[cost_col]]), na.rm = TRUE), .groups = "drop") %>%
          dplyr::rename(category = driver)
        
      } else if (cat == "need") {
        needs_long1 %>%
          dplyr::group_by(need) %>%
          dplyr::summarise(total_cost = sum(as.numeric(.data[[cost_col]]), na.rm = TRUE), .groups = "drop") %>%
          dplyr::rename(category = need)
        
      } else {
        validate(need(cat %in% names(data1), paste0("Column '", cat, "' not found in data1")))
        col_sym <- rlang::sym(cat)
        data1 %>%
          dplyr::group_by(.data[[cat]]) %>%
          dplyr::summarise(total_cost = sum(as.numeric(.data[[cost_col]]), na.rm = TRUE), .groups = "drop") %>%
          dplyr::rename(category = !!col_sym)
      }
    })
    
    reactive_cost_table_nw <- reactive({
      cost_col <- input$cost_metric_nw
      cat      <- input$category_nw
      
      df <- if (cat == "driver") {
        drivers_long1 %>%
          dplyr::select(driver, project_title, dplyr::all_of(cost_col)) %>%
          dplyr::rename(category = driver, project = project_title, cost = !!rlang::sym(cost_col))
        
      } else if (cat == "need") {
        needs_long1 %>%
          dplyr::select(need, project_title, dplyr::all_of(cost_col)) %>%
          dplyr::rename(category = need, project = project_title, cost = !!rlang::sym(cost_col))
        
      } else {
        validate(need(cat %in% names(data1), paste0("Column '", cat, "' not found in data1")))
        cat_sym <- rlang::sym(cat)
        data1 %>%
          dplyr::select(!!cat_sym, project_title, dplyr::all_of(cost_col)) %>%
          dplyr::rename(category = !!cat_sym, project = project_title, cost = !!rlang::sym(cost_col))
      }
      
      df$prioritize <- sprintf(
        "<input type='checkbox' class='prio' value=\"%s\">",
        htmltools::htmlEscape(df$project)
      )
      df
    })
    
    output$costPlot_nw <- renderPlotly({
      cost_col <- input$cost_metric_nw
      cat      <- input$category_nw
      
      df <- switch(cat, "driver" = drivers_long1,
                   "need"   = needs_long1,
                   data1)
      
      
      x_map <- if (cat == "driver") ~driver
      else if (cat == "need") ~need
      else as.formula(paste0("~`", cat, "`"))
      
      plot_ly(
        data  = df,
        x     = x_map,
        y     = ~as.numeric(get(cost_col)),
        color = ~project_title,
        text  = ~project_title,
        type  = "bar"
      )%>%
        layout(barmode = "stack",
               title   = paste("Cost by", cat, "(North & West)"),
               yaxis   = list(title = "Cost (£m)"),
               xaxis   = list(title = "Category"))
    })
    
    output$costTable_nw <- DT::renderDataTable({
      df <- reactive_cost_table_nw()
      df$category <- factor(df$category, levels = unique(df$category))
      df <- df %>% dplyr::arrange(category, dplyr::desc(as.numeric(cost)))
      
      DT::datatable(
        df,
        escape   = FALSE,
        rownames = FALSE,
        options  = list(pageLength = 10),
        callback = DT::JS(sprintf("
          table.on('change', 'input.prio', function() {
            var selected = [];
            table.$('input.prio:checked').each(function(){
              selected.push($(this).val());
            });
            Shiny.setInputValue('%s', selected, {priority: 'event'});
          });

          Shiny.addCustomMessageHandler('%s', function(message) {
            table.$('input.prio').prop('checked', false);
            Shiny.setInputValue('%s', [], {priority: 'event'});
          });
        ", session$ns("prio_selected_nw"),
                                  session$ns("resetCheckboxes_nw"),
                                  session$ns("prio_selected_nw")))
      )
    })
    
    observeEvent(input$makePrioritized_nw, {
      sel <- if (is.null(input$prio_selected_nw)) character(0) else input$prio_selected_nw
      
      prioritized <- reactive_cost_table_nw() %>%
        dplyr::filter(project %in% sel) %>%
        dplyr::select(category, project, cost)
      
      prioritized$category <- factor(
        prioritized$category,
        levels = unique(reactive_cost_table_nw()$category)
      )
      
      prioritized <- prioritized %>% dplyr::arrange(category, dplyr::desc(as.numeric(cost)))
      
      output$prioritizedTable_nw <- DT::renderDataTable({
        DT::datatable(prioritized, rownames = FALSE, options = list(pageLength = 10))
      })
    })
    
    observeEvent(input$resetPrioritized_nw, {
      session$sendCustomMessage(session$ns("resetCheckboxes_nw"), "reset")
      output$prioritizedTable_nw <- DT::renderDataTable({
        DT::datatable(
          data.frame(category = character(), project = character(), cost = numeric()),
          rownames = FALSE,
          options = list(pageLength = 10)
        )
      })
    })
    
    observeEvent(input$category_nw, {
      session$sendCustomMessage(session$ns("resetCheckboxes_nw"), "reset")
      output$prioritizedTable_nw <- DT::renderDataTable({
        DT::datatable(
          data.frame(category = character(), project = character(), cost = numeric()),
          rownames = FALSE,
          options = list(pageLength = 10)
        )
      })
    })
    
    output$ecdPlot_nw <- renderPlotly({
      cat <- input$category_nw
      
      df_src <- switch(cat, "driver" = drivers_long1,
                       "need"   = needs_long1,
                       data1)
      
      cat_col_exists <- cat %in% names(df_src)
      
      df_ecd <- df_src %>%
        dplyr::filter(!is.na(ecd_clean)) %>%
        dplyr::group_by(project_title) %>%
        dplyr::summarise(
          ecd_clean    = ecd_clean[1],
          category_val = if (cat_col_exists) .data[[cat]][1] else NA_character_,
          .groups      = "drop"
        ) %>%
        dplyr::arrange(ecd_clean)
      
      validate(need(nrow(df_ecd) > 0, "No projects with a valid ECD to display."))
      
      min_year <- 2026
      
      df_ecd <- df_ecd %>%
        dplyr::mutate(
          hover_txt = if (cat_col_exists) {
            paste0("<b>", project_title, "</b><br>ECD: ", ecd_clean, "<br>", cat, ": ", category_val)
          } else {
            paste0("<b>", project_title, "</b><br>ECD: ", ecd_clean)
          }
        )
      
      color_map <- if (cat_col_exists) ~category_val else ~project_title
      
      plot_ly(df_ecd) %>%
        add_segments(
          x = min_year, xend = ~ecd_clean,
          y = ~project_title, yend = ~project_title,
          line = list(color = "rgba(150,150,150,0.6)", width = 3),
          hoverinfo = "none"
        ) %>%
        add_markers(
          x = ~ecd_clean, y = ~project_title,
          color = color_map,
          marker = list(size = 10),
          text   = ~hover_txt,
          hovertemplate = "%{text}<extra></extra>"
        ) %>%
        layout(
          title = "Estimated Completion Date (ECD) — North & West",
          xaxis = list(title = "ECD (Year)"),
          yaxis = list(
            title = "Project",
            categoryorder = "array",
            categoryarray = df_ecd$project_title
          )
        )
    })

    
  })
}


transmission_south_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    reactive_cost_table_se <- reactive({
      cost_col <- input$cost_metric_se
      cat      <- input$category_se
      
      df <- if (cat == "driver") {
        drivers_long2 %>%
          dplyr::select(driver, project_title, dplyr::all_of(cost_col)) %>%
          dplyr::rename(category = driver, project = project_title, cost = !!rlang::sym(cost_col))
      } else if (cat == "need") {
        needs_long2 %>%
          dplyr::select(need, project_title, dplyr::all_of(cost_col)) %>%
          dplyr::rename(category = need, project = project_title, cost = !!rlang::sym(cost_col))
      } else {
        validate(need(cat %in% names(data2), paste0("Column '", cat, "' not found in South & East dataset")))
        cat_sym <- rlang::sym(cat)
        data2 %>%
          dplyr::select(!!cat_sym, project_title, dplyr::all_of(cost_col)) %>%
          dplyr::rename(category = !!cat_sym, project = project_title, cost = !!rlang::sym(cost_col))
      }
      
      df$prioritize <- sprintf(
        "<input type='checkbox' class='prio' value=\"%s\">",
        htmltools::htmlEscape(df$project)
      )
      df
    })
    
    output$costPlot_se <- renderPlotly({
      cost_col <- input$cost_metric_se
      cat      <- input$category_se
      
      df <- switch(cat,
                   "driver" = drivers_long2,
                   "need"   = needs_long2,
                   data2)
      
      x_map <- if (cat == "driver") ~driver
      else if (cat == "need") ~need
      else as.formula(paste0("~`", cat, "`"))
      
      plot_ly(
        data  = df,
        x     = x_map,
        y     = ~as.numeric(get(cost_col)),
        color = ~project_title,
        text  = ~project_title,
        type  = "bar"
      ) %>%
        layout(
          barmode = "stack",
          title   = paste("Cost by", cat, "(South & East)"),
          yaxis   = list(title = "Cost (£m)"),
          xaxis   = list(title = "Category")
        )
    })
    
    output$costTable_se <- DT::renderDataTable({
      df <- reactive_cost_table_se()
      df$category <- factor(df$category, levels = unique(df$category))
      df <- df %>% dplyr::arrange(category, dplyr::desc(as.numeric(cost)))
      
      DT::datatable(
        df,
        escape   = FALSE,
        rownames = FALSE,
        options  = list(pageLength = 10),
        callback = DT::JS(sprintf("
          table.on('change', 'input.prio', function() {
            var selected = [];
            table.$('input.prio:checked').each(function(){
              selected.push($(this).val());
            });
            Shiny.setInputValue('%s', selected, {priority: 'event'});
          });
          Shiny.addCustomMessageHandler('%s', function(message) {
            table.$('input.prio').prop('checked', false);
            Shiny.setInputValue('%s', [], {priority: 'event'});
          });
        ",
                                  session$ns("prio_selected_se"),
                                  session$ns("resetCheckboxes_se"),
                                  session$ns("prio_selected_se")
        ))
      )
    })
    
    observeEvent(input$makePrioritized_se, {
      sel <- input$prio_selected_se %||% character(0)
      prioritized <- reactive_cost_table_se() %>%
        dplyr::filter(project %in% sel) %>%
        dplyr::select(category, project, cost)
      
      prioritized$category <- factor(
        prioritized$category,
        levels = unique(reactive_cost_table_se()$category)
      )
      prioritized <- prioritized %>% dplyr::arrange(category, dplyr::desc(as.numeric(cost)))
      
      output$prioritizedTable_se <- DT::renderDataTable({
        DT::datatable(prioritized, rownames = FALSE, options = list(pageLength = 10))
      })
    })
    
    observeEvent(input$resetPrioritized_se, {
      session$sendCustomMessage(session$ns("resetCheckboxes_se"), "reset")
      output$prioritizedTable_se <- DT::renderDataTable({
        DT::datatable(
          data.frame(category = character(), project = character(), cost = numeric()),
          rownames = FALSE,
          options = list(pageLength = 10)
        )
      })
    })
    
    observeEvent(list(input$category_se, input$cost_metric_se), {
      session$sendCustomMessage(session$ns("resetCheckboxes_se"), "reset")
      output$prioritizedTable_se <- DT::renderDataTable({
        DT::datatable(
          data.frame(category = character(), project = character(), cost = numeric()),
          rownames = FALSE,
          options = list(pageLength = 10)
        )
      })
    })
    
    output$ecdPlot_se <- renderPlotly({
      cat <- input$category_se
      
      df_src <- switch(cat, "driver" = drivers_long2,
                       "need"   = needs_long2,
                       data2)
      
      cat_col_exists <- cat %in% names(df_src)
      
      df_ecd <- df_src %>%
        dplyr::filter(!is.na(ecd_clean)) %>%
        dplyr::group_by(project_title) %>%
        dplyr::summarise(
          ecd_clean    = ecd_clean[1],
          category_val = if (cat_col_exists) .data[[cat]][1] else NA_character_,
          .groups      = "drop"
        ) %>%
        dplyr::arrange(ecd_clean)
      
      validate(need(nrow(df_ecd) > 0, "No projects with a valid ECD to display."))
      
      min_year <- 2026
      
      df_ecd <- df_ecd %>%
        dplyr::mutate(
          hover_txt = if (cat_col_exists) {
            paste0("<b>", project_title, "</b><br>",
                   "ECD: ", ecd_clean, "<br>",
                   cat, ": ", category_val)
          } else {
            paste0("<b>", project_title, "</b><br>",
                   "ECD: ", ecd_clean)
          }
        )
      
      color_map <- if (cat_col_exists) ~category_val else ~project_title
      
      plot_ly(df_ecd) %>%
        add_segments(x = min_year, xend = ~ecd_clean,
                     y = ~project_title, yend = ~project_title,
                     line = list(color = 'rgba(150,150,150,0.6)', width = 3),
                     hoverinfo = "none") %>%
        add_markers(x = ~ecd_clean, y = ~project_title,
                    color = color_map,
                    marker = list(size = 10),
                    text = ~hover_txt,
                    hovertemplate = "%{text}<extra></extra>") %>%
        layout(
          title = "Project completion (ECD) — South & East",
          xaxis = list(title = "ECD (Year)"),
          yaxis = list(
            title = "Project",
            categoryorder = "array",
            categoryarray = df_ecd$project_title
          )
        )
    })
  })
}

transmission_both_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    reactive_cost_table_both <- reactive({
      cost_col <- input$cost_metric_both
      cat      <- input$category_both
      
      df <- if (cat == "driver") {
        drivers_long3 %>%
          dplyr::select(driver, project_title, dplyr::all_of(cost_col)) %>%
          dplyr::rename(category = driver,
                        project  = project_title,
                        cost     = !!rlang::sym(cost_col))
      } else if (cat == "need") {
        needs_long3 %>%
          dplyr::select(need, project_title, dplyr::all_of(cost_col)) %>%
          dplyr::rename(category = need,
                        project  = project_title,
                        cost     = !!rlang::sym(cost_col))
      } else {
        validate(need(cat %in% names(data3),
                      paste0("Column '", cat, "' not found in Both Areas dataset")))
        cat_sym <- rlang::sym(cat)
        data3 %>%
          dplyr::select(!!cat_sym, project_title, dplyr::all_of(cost_col)) %>%
          dplyr::rename(category = !!cat_sym,
                        project  = project_title,
                        cost     = !!rlang::sym(cost_col))
      }
      
      df$prioritize <- sprintf(
        "<input type='checkbox' class='prio' value=\"%s\">",
        htmltools::htmlEscape(df$project)
      )
      df
    })
    
    output$costPlot_both <- renderPlotly({
      cost_col <- input$cost_metric_both
      cat      <- input$category_both
      
      df <- switch(cat,
                   "driver" = drivers_long3,
                   "need"   = needs_long3,
                   data3)
      
      x_map <- if (cat == "driver") ~driver
      else if (cat == "need") ~need
      else as.formula(paste0("~`", cat, "`"))
      
      plot_ly(
        data  = df,
        x     = x_map,
        y     = ~as.numeric(get(cost_col)),
        color = ~project_title,
        text  = ~project_title,
        type  = "bar"
      ) %>%
        layout(
          barmode = "stack",
          title   = paste("Cost by", cat, "(Both Areas)"),
          yaxis   = list(title = "Cost (£m)"),
          xaxis   = list(title = "Category")
        )
    })
    
    output$costTable_both <- DT::renderDataTable({
      df <- reactive_cost_table_both()
      df$category <- factor(df$category, levels = unique(df$category))
      df <- df %>% dplyr::arrange(category, dplyr::desc(as.numeric(cost)))
      
      DT::datatable(
        df,
        escape   = FALSE,
        rownames = FALSE,
        options  = list(pageLength = 10),
        callback = DT::JS(sprintf("
          // Capture checkbox changes and send selected project values to Shiny
          table.on('change', 'input.prio', function() {
            var selected = [];
            table.$('input.prio:checked').each(function(){
              selected.push($(this).val());
            });
            Shiny.setInputValue('%s', selected, {priority: 'event'});
          });

          // Namespaced reset for this tab
          Shiny.addCustomMessageHandler('%s', function(message) {
            table.$('input.prio').prop('checked', false);
            Shiny.setInputValue('%s', [], {priority: 'event'});
          });
        ",
                                  session$ns("prio_selected_both"),
                                  session$ns("resetCheckboxes_both"),
                                  session$ns("prio_selected_both")
        ))
      )
    })
    
    observeEvent(input$makePrioritized_both, {
      sel <- input$prio_selected_both %||% character(0)
      prioritized <- reactive_cost_table_both() %>%
        dplyr::filter(project %in% sel) %>%
        dplyr::select(category, project, cost)
      
      prioritized$category <- factor(
        prioritized$category,
        levels = unique(reactive_cost_table_both()$category)
      )
      prioritized <- prioritized %>% dplyr::arrange(category, dplyr::desc(as.numeric(cost)))
      
      output$prioritizedTable_both <- DT::renderDataTable({
        DT::datatable(prioritized, rownames = FALSE, options = list(pageLength = 10))
      })
    })
    
    observeEvent(input$resetPrioritized_both, {
      session$sendCustomMessage(session$ns("resetCheckboxes_both"), "reset")
      output$prioritizedTable_both <- DT::renderDataTable({
        DT::datatable(
          data.frame(category = character(), project = character(), cost = numeric()),
          rownames = FALSE,
          options  = list(pageLength = 10)
        )
      })
    })
    
    observeEvent(list(input$category_both, input$cost_metric_both), {
      session$sendCustomMessage(session$ns("resetCheckboxes_both"), "reset")
      output$prioritizedTable_both <- DT::renderDataTable({
        DT::datatable(
          data.frame(category = character(), project = character(), cost = numeric()),
          rownames = FALSE,
          options  = list(pageLength = 10)
        )
      })
    })
    
    output$ecdPlot_both <- renderPlotly({
      cat <- input$category_both
      
      df_src <- switch(cat, "driver" = drivers_long3,
                       "need"   = needs_long3,
                       data3)
      
      cat_col_exists <- cat %in% names(df_src)
      
      df_ecd <- df_src %>%
        dplyr::filter(!is.na(ecd_clean)) %>%
        dplyr::group_by(project_title) %>%
        dplyr::summarise(
          ecd_clean    = ecd_clean[1],
          category_val = if (cat_col_exists) .data[[cat]][1] else NA_character_,
          .groups      = "drop"
        ) %>%
        dplyr::arrange(ecd_clean)
      
      validate(need(nrow(df_ecd) > 0, "No projects with a valid ECD to display."))
      
      min_year <- 2026
      
      df_ecd <- df_ecd %>%
        dplyr::mutate(
          hover_txt = if (cat_col_exists) {
            paste0("<b>", project_title, "</b><br>",
                   "ECD: ", ecd_clean, "<br>",
                   cat, ": ", category_val)
          } else {
            paste0("<b>", project_title, "</b><br>",
                   "ECD: ", ecd_clean)
          }
        )
      
      color_map <- if (cat_col_exists) ~category_val else ~project_title
      
      plot_ly(df_ecd) %>%
        add_segments(x = min_year, xend = ~ecd_clean,
                     y = ~project_title, yend = ~project_title,
                     line = list(color = 'rgba(150,150,150,0.6)', width = 3),
                     hoverinfo = "none") %>%
        add_markers(x = ~ecd_clean, y = ~project_title,
                    color = color_map,
                    marker = list(size = 10),
                    text = ~hover_txt,
                    hovertemplate = "%{text}<extra></extra>") %>%
        layout(
          title = "Project completion (ECD) — Both Areas",
          xaxis = list(title = "ECD (Year)"),
          yaxis = list(
            title = "Project",
            categoryorder = "array",
            categoryarray = df_ecd$project_title
          )
        )
    })
    
  })
}

transmission_changes_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    change_summary <- reactive({
      validate(need(exists("data4"), "Changes dataset (data4) is not loaded."))
      validate(need(nrow(data4) > 0, "No change records found in data4."))
      
      data4 %>%
        dplyr::group_by(change_category) %>%
        dplyr::summarise(count = dplyr::n(), .groups = "drop") %>%
        dplyr::arrange(dplyr::desc(count))
    })
    
    output$changePlot <- renderPlotly({
      df <- change_summary()
      
      plot_ly(
        data = df,
        x    = ~change_category,
        y    = ~count,
        type = "bar",
        text = ~count,
        textposition = "outside"
      ) %>%
        layout(
          title  = "Project Changes Since TDPNI 2023–2032",
          xaxis  = list(title = "Change Category"),
          yaxis  = list(title = "Number of Projects"),
          margin = list(t = 60)
        )
    })
    outputOptions(output, "changePlot", suspendWhenHidden = FALSE)
    
    output$changeTable <- DT::renderDataTable({
      validate(need(exists("data4"), "Changes dataset (data4) is not loaded."))
      validate(need(nrow(data4) > 0, "No change records found in data4."))
      
      preferred_cols <- c("project_title", "change", "change_category")
      cols           <- intersect(preferred_cols, names(data4))
      df_out         <- if (length(cols) > 0) data4[, cols, drop = FALSE] else data4
      
      order_col <- max(0L, ncol(df_out) - 1L)
      
      DT::datatable(
        df_out,
        rownames = FALSE,
        options  = list(
          pageLength = 10,
          order      = list(list(order_col, "asc"))
        )
      )
    })
    outputOptions(output, "changeTable", suspendWhenHidden = FALSE)
  })
}


transmission_rp6_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    observeEvent(TRUE, {
      validate(need(exists("data6"), "RP6 dataset (data6) is not loaded."))
      validate(need(nrow(data6) > 0, "RP6 dataset (data6) is empty."))
    }, once = TRUE)
    
    reactive_cost_table_rp6 <- reactive({
      category_col <- input$category_rp6
      validate(need(category_col %in% names(data6),
                    paste0("Column '", category_col, "' not found in RP6 dataset")))
      
      df <- data6 %>%
        dplyr::select(category = .data[[category_col]], project = project_title)
      
      df$prioritize <- sprintf(
        "<input type='checkbox' class='prio' value=\"%s\">",
        htmltools::htmlEscape(df$project)
      )
      df
    })
    
    output$costPlot_rp6 <- renderPlotly({
      category_col <- input$category_rp6
      validate(need(category_col %in% names(data6),
                    paste0("Column '", category_col, "' not found in RP6 dataset")))
      
      df_counts <- data6 %>%
        dplyr::count(category = .data[[category_col]], name = "n")
      
      plot_ly(
        data = df_counts,
        x    = ~category,     
        y    = ~n,
        type = "bar",
        text = ~n,
        textposition = "outside"
      ) %>%
        layout(
          title = paste("Number of projects by", category_col, "(RP6)"),
          yaxis = list(title = "Projects (count)"),
          xaxis = list(title = "Category")
        )
    })
    
    output$costTable_rp6 <- DT::renderDataTable({
      df <- reactive_cost_table_rp6()
      df$category <- factor(df$category, levels = unique(df$category))
      df <- df %>% dplyr::arrange(category, project)
      
      DT::datatable(
        df,
        escape   = FALSE,
        rownames = FALSE,
        options  = list(pageLength = 10),
        callback = DT::JS(sprintf("
          table.on('change', 'input.prio', function() {
            var selected = [];
            table.$('input.prio:checked').each(function(){
              selected.push($(this).val());
            });
            Shiny.setInputValue('%s', selected, {priority: 'event'});
          });

          Shiny.addCustomMessageHandler('%s', function(message) {
            table.$('input.prio').prop('checked', false);
            Shiny.setInputValue('%s', [], {priority: 'event'});
          });
        ",
                                  session$ns("prio_selected_rp6"),
                                  session$ns("resetCheckboxes_rp6"),
                                  session$ns("prio_selected_rp6")
        ))
      )
    })
    
    observeEvent(input$makePrioritized_rp6, {
      sel <- input$prio_selected_rp6 %||% character(0)
      prioritized <- reactive_cost_table_rp6() %>%
        dplyr::filter(project %in% sel) %>%
        dplyr::select(category, project)
      
      prioritized$category <- factor(
        prioritized$category,
        levels = unique(reactive_cost_table_rp6()$category)
      )
      prioritized <- prioritized %>% dplyr::arrange(category, project)
      
      output$prioritizedTable_rp6 <- DT::renderDataTable({
        DT::datatable(prioritized, rownames = FALSE, options = list(pageLength = 10))
      })
    })
    
    observeEvent(input$resetPrioritized_rp6, {
      session$sendCustomMessage(session$ns("resetCheckboxes_rp6"), "reset")
      output$prioritizedTable_rp6 <- DT::renderDataTable({
        DT::datatable(
          data.frame(category = character(), project = character()),
          rownames = FALSE,
          options  = list(pageLength = 10)
        )
      })
    })
    
    observeEvent(input$category_rp6, {
      session$sendCustomMessage(session$ns("resetCheckboxes_rp6"), "reset")
      output$prioritizedTable_rp6 <- DT::renderDataTable({
        DT::datatable(
          data.frame(category = character(), project = character()),
          rownames = FALSE,
          options  = list(pageLength = 10)
        )
      })
    })
    
    output$ecdPlot_rp6 <- renderPlotly({
      cat <- input$category_rp6
      validate(need(cat %in% names(data6),
                    paste0("Column '", cat, "' not found in RP6 dataset")))
      
      cat_col_exists <- cat %in% names(data6)
      
      df_ecd <- data6 %>%
        dplyr::filter(!is.na(ecd_clean)) %>%
        dplyr::group_by(project_title) %>%
        dplyr::summarise(
          ecd_clean    = ecd_clean[1],
          category_val = if (cat_col_exists) .data[[cat]][1] else NA_character_,
          .groups      = "drop"
        ) %>%
        dplyr::arrange(ecd_clean)
      
      validate(need(nrow(df_ecd) > 0, "No projects with a valid ECD to display."))
      
      min_year <- 2026
      
      df_ecd <- df_ecd %>%
        dplyr::mutate(
          hover_txt = if (cat_col_exists) {
            paste0("<b>", project_title, "</b><br>",
                   "ECD: ", ecd_clean, "<br>",
                   cat, ": ", category_val)
          } else {
            paste0("<b>", project_title, "</b><br>",
                   "ECD: ", ecd_clean)
          }
        )
      
      color_map <- if (cat_col_exists) ~category_val else ~project_title
      
      plot_ly(df_ecd) %>%
        add_segments(
          x = min_year, xend = ~ecd_clean,
          y = ~project_title, yend = ~project_title,
          line = list(color = 'rgba(150,150,150,0.6)', width = 3),
          hoverinfo = "none"
        ) %>%
        add_markers(
          x = ~ecd_clean, y = ~project_title,
          color = color_map,
          marker = list(size = 10),
          text   = ~hover_txt,
          hovertemplate = "%{text}<extra></extra>"
        ) %>%
        layout(
          title = "Project completion (ECD) — RP6",
          xaxis = list(title = "ECD (Year)"),
          yaxis = list(
            title = "Project",
            categoryorder = "array",
            categoryarray = df_ecd$project_title
          )
        )
    })
  })
}

transmission_rp7_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    observeEvent(TRUE, {
      validate(need(exists("data7"), "RP7 dataset (data7) is not loaded."))
      validate(need(nrow(data7) > 0, "RP7 dataset (data7) is empty."))
    }, once = TRUE)
    
    reactive_cost_table_rp7 <- reactive({
      category_col <- input$category_rp7
      validate(need(category_col %in% names(data7),
                    paste0("Column '", category_col, "' not found in RP7 dataset")))
      
      df <- data7 %>%
        dplyr::select(category = .data[[category_col]], project = project_title)
      
      df$prioritize <- sprintf(
        "<input type='checkbox' class='prio' value=\"%s\">",
        htmltools::htmlEscape(df$project)
      )
      df
    })
    
    output$costPlot_rp7 <- renderPlotly({
      category_col <- input$category_rp7
      validate(need(category_col %in% names(data7),
                    paste0("Column '", category_col, "' not found in RP7 dataset")))
      
      df_counts <- data7 %>%
        dplyr::count(category = .data[[category_col]], name = "n")
      
      plot_ly(
        data = df_counts,
        x    = ~category,  # renamed to a static column
        y    = ~n,
        type = "bar",
        text = ~n,
        textposition = "outside"
      ) %>%
        layout(
          title = paste("Number of projects by", category_col, "— RP7"),
          yaxis = list(title = "Projects (count)"),
          xaxis = list(title = "Category")
        )
    })
    
    output$costTable_rp7 <- DT::renderDataTable({
      df <- reactive_cost_table_rp7()
      df$category <- factor(df$category, levels = unique(df$category))
      df <- df %>% dplyr::arrange(category, project)
      
      DT::datatable(
        df,
        escape   = FALSE,
        rownames = FALSE,
        options  = list(pageLength = 10),
        callback = DT::JS(sprintf("
          table.on('change', 'input.prio', function() {
            var selected = [];
            table.$('input.prio:checked').each(function(){
              selected.push($(this).val());
            });
            Shiny.setInputValue('%s', selected, {priority: 'event'});
          });

          Shiny.addCustomMessageHandler('%s', function(message) {
            table.$('input.prio').prop('checked', false);
            Shiny.setInputValue('%s', [], {priority: 'event'});
          });
        ",
                                  session$ns("prio_selected_rp7"),
                                  session$ns("resetCheckboxes_rp7"),
                                  session$ns("prio_selected_rp7")
        ))
      )
    })
    
    observeEvent(input$makePrioritized_rp7, {
      sel <- input$prio_selected_rp7 %||% character(0)
      prioritized <- reactive_cost_table_rp7() %>%
        dplyr::filter(project %in% sel) %>%
        dplyr::select(category, project)
      
      prioritized$category <- factor(
        prioritized$category,
        levels = unique(reactive_cost_table_rp7()$category)
      )
      prioritized <- prioritized %>% dplyr::arrange(category, project)
      
      output$prioritizedTable_rp7 <- DT::renderDataTable({
        DT::datatable(prioritized, rownames = FALSE, options = list(pageLength = 10))
      })
    })
    
    observeEvent(input$resetPrioritized_rp7, {
      session$sendCustomMessage(session$ns("resetCheckboxes_rp7"), "reset")
      output$prioritizedTable_rp7 <- DT::renderDataTable({
        DT::datatable(
          data.frame(category = character(), project = character()),
          rownames = FALSE,
          options  = list(pageLength = 10)
        )
      })
    })
    
    observeEvent(input$category_rp7, {
      session$sendCustomMessage(session$ns("resetCheckboxes_rp7"), "reset")
      output$prioritizedTable_rp7 <- DT::renderDataTable({
        DT::datatable(
          data.frame(category = character(), project = character()),
          rownames = FALSE,
          options  = list(pageLength = 10)
        )
      })
    })
    
    output$ecdPlot_rp7 <- renderPlotly({
      cat <- input$category_rp7
      validate(need(cat %in% names(data7),
                    paste0("Column '", cat, "' not found in RP7 dataset")))
      
      cat_col_exists <- cat %in% names(data7)
      
      df_ecd <- data7 %>%
        dplyr::filter(!is.na(ecd_clean)) %>%
        dplyr::group_by(project_title) %>%
        dplyr::summarise(
          ecd_clean    = ecd_clean[1],
          category_val = if (cat_col_exists) .data[[cat]][1] else NA_character_,
          .groups      = "drop"
        ) %>%
        dplyr::arrange(ecd_clean)
      
      validate(need(nrow(df_ecd) > 0, "No projects with a valid ECD to display."))
      
      min_year <- 2026
      
      df_ecd <- df_ecd %>%
        dplyr::mutate(
          hover_txt = if (cat_col_exists) {
            paste0("<b>", project_title, "</b><br>",
                   "ECD: ", ecd_clean, "<br>",
                   cat, ": ", category_val)
          } else {
            paste0("<b>", project_title, "</b><br>",
                   "ECD: ", ecd_clean)
          }
        )
      
      color_map <- if (cat_col_exists) ~category_val else ~project_title
      
      plot_ly(df_ecd) %>%
        add_segments(
          x = min_year, xend = ~ecd_clean,
          y = ~project_title, yend = ~project_title,
          line = list(color = 'rgba(150,150,150,0.6)', width = 3),
          hoverinfo = "none"
        ) %>%
        add_markers(
          x = ~ecd_clean, y = ~project_title,
          color = color_map,
          marker = list(size = 10),
          text   = ~hover_txt,
          hovertemplate = "%{text}<extra></extra>"
        ) %>%
        layout(
          title = "Project completion (ECD) — RP7",
          xaxis = list(title = "ECD (Year)"),
          yaxis = list(
            title = "Project",
            categoryorder = "array",
            categoryarray = df_ecd$project_title
          )
        )
    })
    
   
  })
}

transmission_d5_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    observeEvent(TRUE, {
      validate(need(exists("data8"), "D5 dataset (data8) is not loaded."))
      validate(need(nrow(data8) > 0, "D5 dataset (data8) is empty."))
    }, once = TRUE)
    
    reactive_cost_table_d5 <- reactive({
      category_col <- input$category_d5
      validate(need(category_col %in% names(data8),
                    paste0("Column '", category_col, "' not found in D5 dataset")))
      
      df <- data8 %>%
        dplyr::select(category = .data[[category_col]], project = project_title)
      
      df$prioritize <- sprintf(
        "<input type='checkbox' class='prio' value=\"%s\">",
        htmltools::htmlEscape(df$project)
      )
      df
    })
    
    output$costPlot_d5 <- renderPlotly({
      category_col <- input$category_d5
      validate(need(category_col %in% names(data8),
                    paste0("Column '", category_col, "' not found in D5 dataset")))
      
      df_counts <- data8 %>%
        dplyr::count(category = .data[[category_col]], name = "n")
      
      plot_ly(
        data = df_counts,
        x    = ~category, 
        y    = ~n,
        type = "bar",
        text = ~n,
        textposition = "outside"
      ) %>%
        layout(
          title = paste("Number of projects by", category_col, "— D5"),
          yaxis = list(title = "Projects (count)"),
          xaxis = list(title = "Category")
        )
    })
    
    output$costTable_d5 <- DT::renderDataTable({
      df <- reactive_cost_table_d5()
      df$category <- factor(df$category, levels = unique(df$category))
      df <- df %>% dplyr::arrange(category, project)
      
      DT::datatable(
        df,
        escape   = FALSE,
        rownames = FALSE,
        options  = list(pageLength = 10),
        callback = DT::JS(sprintf("
          table.on('change', 'input.prio', function() {
            var selected = [];
            table.$('input.prio:checked').each(function(){
              selected.push($(this).val());
            });
            Shiny.setInputValue('%s', selected, {priority: 'event'});
          });

          Shiny.addCustomMessageHandler('%s', function(message) {
            table.$('input.prio').prop('checked', false);
            Shiny.setInputValue('%s', [], {priority: 'event'});
          });
        ",
                                  session$ns("prio_selected_d5"),
                                  session$ns("resetCheckboxes_d5"),
                                  session$ns("prio_selected_d5")
        ))
      )
    })
    
    observeEvent(input$makePrioritized_d5, {
      sel <- input$prio_selected_d5 %||% character(0)
      prioritized <- reactive_cost_table_d5() %>%
        dplyr::filter(project %in% sel) %>%
        dplyr::select(category, project)
      
      prioritized$category <- factor(
        prioritized$category,
        levels = unique(reactive_cost_table_d5()$category)
      )
      prioritized <- prioritized %>% dplyr::arrange(category, project)
      
      output$prioritizedTable_d5 <- DT::renderDataTable({
        DT::datatable(prioritized, rownames = FALSE, options = list(pageLength = 10))
      })
    })
    
    observeEvent(input$resetPrioritized_d5, {
      session$sendCustomMessage(session$ns("resetCheckboxes_d5"), "reset")
      output$prioritizedTable_d5 <- DT::renderDataTable({
        DT::datatable(
          data.frame(category = character(), project = character()),
          rownames = FALSE,
          options  = list(pageLength = 10)
        )
      })
    })
    
    observeEvent(input$category_d5, {
      session$sendCustomMessage(session$ns("resetCheckboxes_d5"), "reset")
      output$prioritizedTable_d5 <- DT::renderDataTable({
        DT::datatable(
          data.frame(category = character(), project = character()),
          rownames = FALSE,
          options  = list(pageLength = 10)
        )
      })
    })
    
    output$ecdPlot_d5 <- renderPlotly({
      cat <- input$category_d5
      validate(need(cat %in% names(data8),
                    paste0("Column '", cat, "' not found in D5 dataset")))
      
      cat_col_exists <- cat %in% names(data8)
      
      df_ecd <- data8 %>%
        dplyr::filter(!is.na(ecd_clean)) %>%
        dplyr::group_by(project_title) %>%
        dplyr::summarise(
          ecd_clean    = ecd_clean[1],
          category_val = if (cat_col_exists) .data[[cat]][1] else NA_character_,
          .groups      = "drop"
        ) %>%
        dplyr::arrange(ecd_clean)
      
      validate(need(nrow(df_ecd) > 0, "No projects with a valid ECD to display."))
      
      min_year <- 2026
      
      df_ecd <- df_ecd %>%
        dplyr::mutate(
          hover_txt = if (cat_col_exists) {
            paste0("<b>", project_title, "</b><br>",
                   "ECD: ", ecd_clean, "<br>",
                   cat, ": ", category_val)
          } else {
            paste0("<b>", project_title, "</b><br>",
                   "ECD: ", ecd_clean)
          }
        )
      
      color_map <- if (cat_col_exists) ~category_val else ~project_title
      
      plot_ly(df_ecd) %>%
        add_segments(
          x = min_year, xend = ~ecd_clean,
          y = ~project_title, yend = ~project_title,
          line = list(color = 'rgba(150,150,150,0.6)', width = 3),
          hoverinfo = "none"
        ) %>%
        add_markers(
          x = ~ecd_clean, y = ~project_title,
          color = color_map,
          marker = list(size = 10),
          text   = ~hover_txt,
          hovertemplate = "%{text}<extra></extra>"
        ) %>%
        layout(
          title = "Project completion (ECD) — D5",
          xaxis = list(title = "ECD (Year)"),
          yaxis = list(
            title = "Project",
            categoryorder = "array",
            categoryarray = df_ecd$project_title
          )
        )
    })
  })
}
