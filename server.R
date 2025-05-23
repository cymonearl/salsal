server <- function(input, output, session) {
  
  # --- Reactive Data Filtering ---
  filtered_data <- reactive({
    req(input$selected_regions, input$selected_price_level, input$selected_year_range)
    # Ensure price_data is loaded
    validate(need(exists("price_data") && nrow(price_data) > 0, "Price data is not loaded or is empty."))
    
    price_data %>%
      filter(
        Region %in% input$selected_regions,
        Price_Level == input$selected_price_level,
        Year >= input$selected_year_range[1] & Year <= input$selected_year_range[2]
      )
  })
  
  # --- Map Warning UI ---
  output$mapWarningUI <- renderUI({
    if (is.null(regions_sf)) {
      return(tags$div(class = "alert alert-danger", 
                      HTML(paste("<b>Map Data Error:</b> GeoJSON file ('", geojson_path, 
                                 "') not loaded or processed correctly. Map cannot be displayed. ",
                                 "Please check console warnings for details."))))
    }
    # Additional checks for the join column if regions_sf is not NULL
    if (!region_col_for_join %in% names(regions_sf)) {
      return(tags$div(class = "alert alert-warning", 
                      HTML(paste("<b>Map Configuration Warning:</b> The expected column for joining ('", region_col_for_join, 
                                 "') was not found in the GeoJSON data after processing. This might be due to an incorrect 'region_col_in_geojson' setting or mapping issues in global.R. ",
                                 "Map may not display correctly."))))
    }
    return(NULL)
  })
  
  # --- 1. Overview & Map Tab ---
  output$choroplethMap <- renderLeaflet({
    req(regions_sf, region_col_for_join %in% names(regions_sf), input$map_year, input$selected_price_level) 
    
    map_data_for_year <- price_data %>%
      filter(Year == input$map_year, Price_Level == input$selected_price_level)
    
    if (nrow(map_data_for_year) == 0) {
      leaflet() %>% addTiles() %>% 
        setView(lng = 121.7740, lat = 12.8797, zoom = 5) %>% 
        addControl(paste("No price data available for", input$selected_price_level, "in", input$map_year), position = "topright")
      return()
    }
    
    map_sf <- regions_sf %>%
      left_join(map_data_for_year, by = setNames("Region", region_col_for_join))
    
    if (nrow(map_sf) == 0 || all(is.na(map_sf$Price))) {
      leaflet(data = regions_sf) %>% addTiles() %>% 
        addPolygons(fillColor = "lightgrey", weight = 1, color = "white", fillOpacity = 0.5,
                    label = ~sprintf("<strong>%s</strong><br/>No data for selected criteria", regions_sf[[region_col_in_geojson]])) %>%
        setView(lng = 121.7740, lat = 12.8797, zoom = 5) %>%
        addControl(paste("No data to display on map for", input$selected_price_level, "in", input$map_year, "after join."), position = "topright")
      return()
    }
    
    pal <- colorNumeric(palette = "YlOrRd", domain = map_sf$Price, na.color = "#bdbdbd") # na.color handles color of NA areas
    
    leaflet(data = map_sf) %>%
      addTiles(options = providerTileOptions(minZoom = 5, maxZoom = 10)) %>%
      addPolygons(
        fillColor = ~pal(Price),
        weight = 1,
        opacity = 1,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.7,
        highlightOptions = highlightOptions(
          weight = 3,
          color = "#666",
          dashArray = "",
          fillOpacity = 0.9,
          bringToFront = TRUE
        ),
        label = ~sprintf(
          "<strong>%s</strong> (%s)<br/>%s: %s<br/>Price: %s<br/>Year: %s",
          map_sf[[region_col_for_join]], map_sf[[region_col_in_geojson]], 
          "Price Level", Price_Level, 
          ifelse(is.na(Price), "No data", scales::dollar(Price, accuracy = 0.01)), 
          Year
        ) %>% lapply(htmltools::HTML),
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto"
        )
      ) %>%
      addLegend(
        pal = pal, 
        values = ~Price, 
        opacity = 0.7, 
        title = paste(input$selected_price_level, "Price"),
        position = "bottomright",
        # Remove na.label from labelFormat. labelFormat is for numeric labels.
        labFormat = labelFormat(prefix = "$") 
      ) %>%
      setView(lng = 121.7740, lat = 12.8797, zoom = 5) 
  })
  
  output$timeSeriesPlot <- renderPlotly({
    df_plot <- filtered_data()
    validate(need(nrow(df_plot) > 0, "No data to display for selected filters."))
    
    p <- ggplot(df_plot, aes(x = Year, y = Price, color = Region, group = Region,
                             text = paste("Region:", Region, "<br>",
                                          "Year:", Year, "<br>",
                                          "Price Level:", Price_Level, "<br>",
                                          "Price:", scales::dollar(Price, accuracy = 0.01)))) +
      geom_line(alpha = 0.8) +
      geom_point(alpha = 0.8, size = 1.5) +
      labs(title = paste("Time Series of", input$selected_price_level, "Prices"),
           x = "Year", y = "Price (PHP)", color = "Region") +
      theme_minimal(base_size = 11) +
      theme(legend.position = "top", plot.title = element_text(hjust = 0.5)) +
      scale_y_continuous(labels = scales::dollar_format(accuracy = 0.01))
    
    ggplotly(p, tooltip = "text")
  })
  
  # --- 2. Data Exploration Tab ---
  output$summaryTable <- renderDT({
    df_summary <- filtered_data()
    validate(need(nrow(df_summary) > 0, "No data to summarize for selected filters."))
    
    summary_df <- df_summary %>%
      group_by(Region, Price_Level) %>%
      summarise(
        N = n(),
        Mean = round(mean(Price, na.rm = TRUE), 2),
        Median = round(median(Price, na.rm = TRUE), 2),
        SD = round(sd(Price, na.rm = TRUE), 2),
        Min = round(min(Price, na.rm = TRUE), 2),
        Max = round(max(Price, na.rm = TRUE), 2),
        .groups = 'drop'
      )
    
    datatable(summary_df, options = list(pageLength = 10, scrollX = TRUE, autoWidth = TRUE), rownames = FALSE,
              caption = htmltools::tags$caption(style = 'caption-side: top; text-align: center;', 
                                                paste('Summary Statistics for', input$selected_price_level)))
  })
  
  output$distributionPlot <- renderPlotly({
    df_dist <- filtered_data()
    validate(need(nrow(df_dist) > 0, "No data for distribution plot based on selected filters."))
    
    p <- ggplot(df_dist, aes(x = Price, fill = Region,
                             text = paste("Region:", Region, "<br>",
                                          "Price:", scales::dollar(Price, accuracy = 0.01)))) +
      geom_histogram(alpha = 0.7, position = "identity", bins = max(15, nrow(df_dist)/length(unique(df_dist$Region))/5)) + 
      facet_wrap(~Region, scales = "free_y", ncol = min(3, length(unique(df_dist$Region)))) + 
      labs(title = paste("Distribution of", input$selected_price_level, "Prices"),
           x = "Price (PHP)", y = "Frequency") +
      theme_minimal(base_size = 10) +
      scale_x_continuous(labels = scales::dollar_format(accuracy = 0.01)) +
      theme(legend.position = "none", plot.title = element_text(hjust = 0.5))
    
    ggplotly(p, tooltip = "text")
  })
  
  output$correlationMatrix <- renderPlot({
    plot(1, type="n", xlab="", ylab="", main="Correlation Matrix (Placeholder)")
    text(1, 1, "Feature To Be Implemented\n(Requires data restructuring)", cex=1.2)
  })
  
  # --- 3. Forecasting Tab ---
  forecast_data_reactive <- reactive({
    req(input$forecast_region, input$selected_price_level)
    validate(need(exists("price_data") && nrow(price_data) > 0, "Price data not loaded."))
    
    price_data %>%
      filter(Region == input$forecast_region, Price_Level == input$selected_price_level) %>%
      arrange(Year) %>%
      select(Year, Price)
  })
  
  output$forecastTitle <- renderText({
    paste("ARIMA Forecast for", input$selected_price_level, "in", input$forecast_region)
  })
  
  arima_model_and_forecast <- reactive({
    df_ts_forecast <- forecast_data_reactive()
    validate(
      need(nrow(df_ts_forecast) > 3, paste("Not enough data points for", input$forecast_region, "(need > 3).")),
      need(sum(!is.na(df_ts_forecast$Price)) > 3, paste("Not enough non-NA data points for", input$forecast_region, "to forecast."))
    )
    
    price_ts <- ts(df_ts_forecast$Price, start = min(df_ts_forecast$Year), frequency = 1) 
    
    fit <- tryCatch(auto.arima(price_ts), error = function(e) NULL)
    validate(need(!is.null(fit), "ARIMA model could not be fitted. Data might be constant or have too few distinct values."))
    
    fc <- forecast(fit, h = input$forecast_horizon)
    list(fit = fit, forecast = fc, original_ts = price_ts, region = input$forecast_region, level = input$selected_price_level)
  })
  
  output$forecastPlot <- renderPlotly({
    model_output <- arima_model_and_forecast()
    req(model_output)
    
    # autoplot from forecast package creates a ggplot object
    p_forecast <- autoplot(model_output$forecast) +
      labs(title = paste("Forecast for", model_output$level, "in", model_output$region),
           x = "Year", y = "Price (PHP)") +
      theme_minimal(base_size = 11) +
      scale_y_continuous(labels = scales::dollar_format(accuracy = 0.01)) +
      theme(plot.title = element_text(hjust = 0.5))
    
    ggplotly(p_forecast)
  })
  
  output$accuracyMetrics <- renderPrint({
    model_output <- arima_model_and_forecast()
    req(model_output)
    accuracy(model_output$forecast) 
  })
  
  output$residualDiagnosticsPlot <- renderPlot({
    model_output <- arima_model_and_forecast()
    req(model_output)
    checkresiduals(model_output$fit, plot = TRUE) # This already creates a plot
  })
  
  # --- 4. Statistical Tests Tab ---
  output$anovaResults <- renderPrint({
    df_anova <- filtered_data()
    num_regions <- length(unique(df_anova$Region))
    
    validate(
      need(nrow(df_anova) > 0, "No data for ANOVA based on selected filters."),
      need(num_regions > 1, "Please select at least two regions for ANOVA comparison.")
    )
    
    # Further check: each group (region) must have more than one observation for ANOVA
    region_counts <- table(df_anova$Region)
    validate(need(all(region_counts > 1), 
                  paste0("ANOVA requires at least two observations per region. Problematic regions: ", 
                         paste(names(region_counts[region_counts <= 1]), collapse=", "))))
    # Also, ensure there is variance within groups (ANOVA will fail if all values in a group are the same)
    # This is harder to pre-validate simply, tryCatch will handle it.
    
    anova_data <- df_anova %>% mutate(Region = factor(Region))
    
    tryCatch({
      aov_model <- aov(Price ~ Region, data = anova_data)
      summary_aov <- summary(aov_model)
      
      # Add interpretation
      p_value <- summary_aov[[1]][["Pr(>F)"]][1]
      interpretation <- if (p_value < 0.05) {
        paste0("The p-value (", round(p_value, 4), ") is less than 0.05, suggesting a statistically significant difference in mean ", 
               input$selected_price_level, " prices across the selected regions.")
      } else {
        paste0("The p-value (", round(p_value, 4), ") is not less than 0.05, suggesting no statistically significant difference in mean ", 
               input$selected_price_level, " prices across the selected regions.")
      }
      
      print(summary_aov)
      cat("\nInterpretation:\n", interpretation, "\n")
      
    }, error = function(e) {
      paste("Error in ANOVA computation:", e$message, 
            "\nThis can happen if there's insufficient data, no variance within groups, or other data issues.")
    })
  })
  
  # --- Download Handlers ---
  last_ggplot <- reactiveVal() # To store the ggplot object for download
  
  observe({
    current_tab <- input$main_tabs
    p <- NULL # Initialize plot object
    
    if (current_tab == "Overview & Map") {
      df_plot_dl <- filtered_data()
      if(nrow(df_plot_dl) > 0){
        p <- ggplot(df_plot_dl, aes(x = Year, y = Price, color = Region, group = Region)) +
          geom_line(alpha = 0.8) + geom_point(alpha = 0.8, size = 1.5) +
          labs(title = paste("Time Series of", input$selected_price_level, "Prices"),
               x = "Year", y = "Price (PHP)", color = "Region") +
          theme_minimal(base_size = 11) + theme(legend.position = "top", plot.title = element_text(hjust = 0.5)) +
          scale_y_continuous(labels = scales::dollar_format(accuracy = 0.01))
      }
    } else if (current_tab == "Data Exploration" && !is.null(input$distributionPlot_clicked)) { # Assuming plotly click events
      # This part is tricky as plotlyOutput doesn't directly give back ggplot object easily
      # For simplicity, let's make it download the distribution plot only if explicitly built as ggplot first
      # Currently, distributionPlot is directly plotly, so this won't trigger for it.
      # If distributionPlot was ggplot then ggplotly, this would work.
    } else if (current_tab == "Forecasting") {
      model_output_dl <- tryCatch(arima_model_and_forecast(), error = function(e) NULL)
      if (!is.null(model_output_dl) && !is.null(model_output_dl$forecast)) {
        p <- autoplot(model_output_dl$forecast) +
          labs(title = paste("Forecast for", model_output_dl$level, "in", model_output_dl$region),
               x = "Year", y = "Price (PHP)") +
          theme_minimal(base_size = 11) + scale_y_continuous(labels = scales::dollar_format(accuracy = 0.01)) +
          theme(plot.title = element_text(hjust = 0.5))
      }
    }
    last_ggplot(p)
  })
  
  output$downloadPlot <- downloadHandler(
    filename = function() {
      active_tab_name <- gsub("[^A-Za-z0-9_]", "", gsub(" ", "_", tolower(input$main_tabs)))
      paste0(active_tab_name, "_plot_", Sys.Date(), ".png")
    },
    content = function(file) {
      plot_to_save <- last_ggplot()
      if (!is.null(plot_to_save) && inherits(plot_to_save, "ggplot")) {
        ggsave(file, plot = plot_to_save, device = "png", width = 10, height = 6, dpi = 300)
      } else {
        # Fallback for non-ggplot (e.g. base R plots or if no plot stored)
        png(file, width=800, height=600)
        plot(1, type="n", xlab="", ylab="", main="No ggplot object available for this view / Plot not downloadable in this format.")
        text(1,1, "Selected plot is not available for PNG download in this manner.", cex=0.9)
        dev.off()
        shiny::showNotification("Selected plot might not be a standard ggplot object or is not available for download.", type = "warning", duration=5)
      }
    }
  )
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste0("filtered_price_data_", input$selected_price_level, "_", Sys.Date(), ".csv")
    },
    content = function(file) {
      df_to_download <- filtered_data()
      validate(need(nrow(df_to_download) > 0, "No data to download for selected filters."))
      write.csv(df_to_download, file, row.names = FALSE)
    }
  )
}