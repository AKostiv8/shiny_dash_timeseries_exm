page_2_page <- tagList(

  # HDN Biting pattern (B1-B3)
  material_side_nav_tab_content(
    side_nav_tab_id = "tab1_p2",
    div(class = "container-custom",
        tags$h2("Predicted Forecast Next N-Days"),
        material_card(
          tags$h3(""),
          material_card(
            material_row(
              material_column(width = 4,
                              material_dropdown(
                                input_id = "film_code_tickets__input",
                                label    = "Film Code",
                                choices  = c(
                                  indicators_sales
                                ),
                                selected = 1554
                              ),
              ),
              material_column(width = 4,
                              material_slider(
                                input_id      = "slider_horizon_tickets__input",
                                label         = "Forecast Horizon (N)",
                                min_value     = 1,
                                max_value     = 90,
                                initial_value = 30,
                                step_size     = 1
                              )
              ),
              material_column(width = 4,
                              material_slider(
                                input_id      = "slider_prediction_interal_tickets__input",
                                label         = "Prediction Interval",
                                min_value     = 0,
                                max_value     = 1,
                                initial_value = 0.9,
                                step_size     = 0.01
                              )
              )
            )
          ),

          material_row(
            material_column(width = 7,
                            material_card(
                              tags$h3(""),
                              plotlyOutput('forecast_tickets_plot')
                            )
            ),
            material_column(width = 5,
                            material_card(
                              tags$h3(""),
                              reactableOutput("tickets_info_table")
                            )
            )
          )
        )

    )
  ),

  # HDN (Survival B4)
  material_side_nav_tab_content(
    side_nav_tab_id = "tab2_p2",
    div(class = "container-custom",
        tags$h2("Test Forecast Sales"),
        material_card(
          tags$h3(""),
          plotlyOutput("forecast_tickets_test_plot")
        )

    )
  ),

  # LAB_Mosq_pools
  material_side_nav_tab_content(
    side_nav_tab_id = "tab3_p2",
    div(class = "container-custom",
        tags$h2("Test Accurancy Sales"),
        material_card(
          tags$h3(""),
          reactableOutput("reactable_tickets_test")
        )

    )
  ),

  # HDN (Pools_Lab linkage)
  material_side_nav_tab_content(
    side_nav_tab_id = "tab4_p2",
    div(class = "container-custom",
        tags$h2("HDN (Pools_Lab linkage)"),
        material_card(
          tags$h3(""),
          # reactableOutput("HDN_Pools_Lab_linkage_table")
        )

    )
  )


)


############ SERVER ###############
page_2_page_server <- function(input, output, session) {

  forecast_tbl_tickets_reactive <- reactive({

    best_sales_models_tbl %>%
      filter(film_code %in% input$film_code_tickets__input) %>%
      modeltime_nested_forecast(
        h = input$slider_horizon_tickets__input,
        conf_interval = input$slider_prediction_interal_tickets__input,
        control = control_nested_forecast(
          verbose   = TRUE,
          allow_par = FALSE
        )
      )

  })

  output$forecast_tickets_plot <- renderPlotly({

    req(forecast_tbl_tickets_reactive())

    facet_col <- 1

    forecast_tbl_tickets_reactive() %>%
      group_by(film_code) %>%
      plot_modeltime_forecast(
        .facet_ncol = facet_col,
        .legend_max_width = 26,
        .plotly_slider = TRUE
      ) %>% ggplotly(source = 'C')
  })

  output$tickets_info_table <- renderReactable({
    selected_data_tickets <- event_data("plotly_click", source = 'C')

    if(is.null(selected_data_tickets)) {
      date <- get_min_data(input$film_code_tickets__input)
    } else {
      # Change to date retrieve from graph `selected_data_sales`
      print(selected_data_tickets)
      print(selected_data_tickets$x)
      date <- selected_data_tickets$x
    }

    reactable(
      create_custom_output_table(input$film_code_tickets__input, date),
      groupBy = "cinema_code",
      resizable = TRUE,
      # defaultPageSize = 2,
      defaultColDef = colDef(
        minWidth = 320,
        headerStyle = list(background = "#f7f7f8")
      ),
      columns = list(
        `cinema_code` = colDef(minWidth = 100),  # overrides the default
        `indicator` = colDef(minWidth = 120),  # overrides the default
        `value` = colDef(
          minWidth = 150,
          cell = function(value, index) {
            sparkline(get_full_data_cinema(input$film_code_tickets__input)$value[[index]], type = "box")
          }
        )  # overrides the default
      ),
      bordered = TRUE,
      highlight = TRUE
    )


  })

  output$forecast_tickets_test_plot <- renderPlotly({

    req(forecast_tbl_tickets_reactive())
    req(input$film_code_tickets__input)

    facet_col <- 1

    best_tickets_models_tbl %>%
      filter(film_code %in% input$film_code_tickets__input) %>%
      extract_nested_test_forecast(.id_subset = input$film_code_tickets__input) %>%
      group_by(film_code) %>%
      plot_modeltime_forecast(
        .facet_ncol = facet_col,
        .legend_max_width = 26,
        .plotly_slider = TRUE
      ) %>% ggplotly(source = 'D')


  })

  output$reactable_tickets_test <- renderReactable({
    best_tickets_models_tbl %>%
      filter(film_code %in% input$film_code_tickets__input) %>%
      extract_nested_test_accuracy() %>%
      filter(film_code %in% input$film_code_tickets__input) %>%
      table_modeltime_accuracy()
  })

}
