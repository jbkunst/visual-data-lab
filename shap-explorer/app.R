# packages ----------------------------------------------------------------
library(shiny)
library(bslib)
library(markdown)
library(rpart)
library(randomForest)
library(xgboost)
library(highcharter)

# data --------------------------------------------------------------------
app_dir <- if (file.exists("shap-credit.rds")) "." else "shap-explorer"

shap_data <- readRDS(file.path(app_dir, "shap-credit.rds"))
shap_data$models$xgboost$fit <- xgboost::xgb.load.raw(shap_data$models$xgboost$fit)

predictors <- shap_data$predictors
models <- shap_data$models
model_labels <- vapply(models, `[[`, character(1), "label")

# Derive the lightweight app views from the shared SHAP data.
shap_data$predictor_data <- shap_data$test[, predictors, drop = FALSE]
shap_data$background <- shap_data$predictor_data
initial_scores <- shap_data$predictions[shap_data$predictions$model == "logistic", ]
initial_distance <- abs(initial_scores$score - shap_data$baseline[["logistic"]])
initial_candidates <- initial_scores$row_id[
  initial_distance >= stats::median(initial_distance)
]
set.seed(2026L)
initial_row <- initial_candidates[[sample.int(length(initial_candidates), 1L)]]
shap_data$control_meta <- stats::setNames(
  lapply(predictors, function(variable) {
    values <- shap_data$predictor_data[[variable]]
    list(
      min = min(values), max = max(values),
      value = as.numeric(values[[initial_row]])
    )
  }),
  predictors
)
shap_data$portfolio_pd <- split(shap_data$predictions$score, shap_data$predictions$model)

profile_values <- do.call(
  rbind,
  lapply(predictors, function(variable) data.frame(
    row_id = shap_data$test$row_id,
    variable,
    value = shap_data$test[[variable]]
  ))
)
shap_data$dependence_data <- merge(shap_data$shap_values, profile_values,
  by = c("row_id", "variable"), sort = FALSE)

predict_model <- function(model, newdata) {
  prediction <- switch(
    model$type,
    logistic = stats::predict(model$fit, newdata = newdata, type = "response"),
    tree = stats::predict(model$fit, newdata = newdata, type = "prob")[, "1"],
    random_forest = stats::predict(model$fit, newdata = newdata, type = "prob")[, "1"],
    xgboost = stats::predict(model$fit,
      xgboost::xgb.DMatrix(as.matrix(newdata), nthread = 1))
  )

  pmin(pmax(as.numeric(prediction), 0), 1)
}

source(file.path(app_dir, "local_shap.R"), local = TRUE)

profile_labels <- c(
  seniority = "Seniority", time = "Loan term", age = "Age",
  expenses = "Expenses", income = "Income", assets = "Assets",
  debt = "Debt", amount = "Loan amount", price = "Price"
)

slider_importance <- aggregate(abs(shap) ~ variable, shap_data$shap_values, mean)
slider_variables <- slider_importance$variable[order(slider_importance[[2L]], decreasing = TRUE)]
slider_variables <- as.character(slider_variables)

make_slider <- function(variable) {
  meta <- shap_data$control_meta[[variable]]

  sliderInput(variable, tags$small(profile_labels[[variable]]),
    min = meta$min, max = meta$max, value = meta$value, ticks = FALSE)
}

# theme -------------------------------------------------------------------
apptheme <- bs_theme()

sidebar <- purrr::partial(sidebar, width = 300)
card <- purrr::partial(card, full_screen = TRUE,
  wrapper = purrr::partial(card_body, padding = 12))

options(
  highcharter.theme = hc_theme(
    chart = list(style = list(fontFamily = "system-ui")),
    legend = list(itemStyle = list(fontWeight = "normal")),
    colors = unname(bs_get_variables(
      apptheme,
      c("primary", "danger", "warning", "success", "info", "secondary")
    )),
    tooltip = list(valueDecimals = 3, shared = TRUE,
      style = list(fontWeight = "normal")),
    xAxis = list(
      gridLineWidth = 1,
      labels = list(style = list(fontWeight = "normal", textOutline = "none")),
      title = list(style = list(fontWeight = "normal"))
    ),
    yAxis = list(
      labels = list(style = list(fontWeight = "normal", textOutline = "none")),
      title = list(style = list(fontWeight = "normal"))
    )
  )
)

# ui ----------------------------------------------------------------------
ui <- page_fillable(
  theme = apptheme, padding = 0,
  layout_sidebar(
    fillable = TRUE,
    padding = "0.75rem",
    sidebar = sidebar(
      title = "SHAP Explorer",
      withMathJax(),
      radioButtons("model", tags$small("Model"),
        choices = stats::setNames(names(models), model_labels),
        selected = "logistic"),
      actionButton("random_profile", "Random case", class = "btn-primary btn-sm w-100"),
      tags$small(
        "Selects one observation from the test sample.",
        class = "text-muted fst-italic"
      ),
      accordion(
        open = FALSE,
        accordion_panel(
          "Client profile",
          lapply(slider_variables, make_slider)
        ),
        accordion_panel("How it works",
          tags$small(htmltools::includeMarkdown("readme.md")))
      ),
      tags$small(htmltools::includeMarkdown("credits.md"))
    ),
    layout_columns(
      col_widths = c(6, 6, 12), row_heights = c(3, 2),
      card(
        card_header("Local SHAP contributions"),
        highchartOutput("shap_plot", height = "100%")
      ),
      layout_columns(
        col_widths = 12, row_heights = c(1, 1),
        card(card_header("Portfolio PD distribution"),
          highchartOutput("pd_plot", height = "100%")),
        card(card_header(uiOutput("dependence_title")),
          highchartOutput("dependence_plot", height = "100%"))
      ),
      card(card_header("From background mean to predicted PD"),
        highchartOutput("waterfall_plot", height = "100%"))
    )
  )
)

# server ------------------------------------------------------------------
server <- function(input, output, session) {
  current_profile <- reactive({
    values <- lapply(predictors, function(variable) input[[variable]])
    names(values) <- predictors
    as.data.frame(values, check.names = FALSE)
  })

  active_variable <- reactiveVal("income")
  previous_profile <- reactiveVal(NULL)

  observeEvent(current_profile(), {
    current <- unlist(current_profile()[1, predictors], use.names = TRUE)
    previous <- previous_profile()

    if (!is.null(previous)) {
      changed <- predictors[current[predictors] != previous[predictors]]

      if (length(changed) == 1L) {
        active_variable(changed[[1]])
      }
    }

    previous_profile(current)
  }, ignoreInit = FALSE)

  observeEvent(input$random_profile, {
    different <- Reduce(
      `|`,
      lapply(predictors, function(variable)
        shap_data$predictor_data[[variable]] != current_profile()[[variable]])
    )
    candidates <- which(different)
    selected <- if (length(candidates)) sample(candidates, 1L) else
      sample.int(nrow(shap_data$predictor_data), 1L)
    row <- shap_data$predictor_data[selected, predictors, drop = FALSE]

    for (variable in predictors) {
      updateSliderInput(session, variable, value = as.numeric(row[[variable]][[1]]))
    }
  })

  analysis_request <- reactive({
    list(
      model_name = input$model,
      profile = current_profile(),
      active_variable = active_variable()
    )
  }) |>
    debounce(250)

  profile_analysis <- reactive({
    request <- analysis_request()
    model <- models[[request$model_name]]
    matching_rows <- which(Reduce(
      `&`,
      lapply(predictors, function(variable)
        shap_data$predictor_data[[variable]] == request$profile[[variable]])
    ))

    if (length(matching_rows)) {
      row_id <- shap_data$test$row_id[[matching_rows[[1L]]]]
      stored_shap <- shap_data$shap_values[
        shap_data$shap_values$model == request$model_name &
          shap_data$shap_values$row_id == row_id,
        , drop = FALSE
      ]
      predicted_pd <- shap_data$predictions$score[
        shap_data$predictions$model == request$model_name &
          shap_data$predictions$row_id == row_id
      ][[1L]]
      shap_values <- stats::setNames(stored_shap$shap, stored_shap$variable)
      baseline <- shap_data$baseline[[request$model_name]]
    } else {
      predicted_pd <- predict_model(model, request$profile)[[1]]
      shap_values <- local_shap_trace_optimized(model, request$profile,
        shap_data$background, seed = 2026L) |>
        summarize_shap()
      baseline <- shap_data$baseline[[request$model_name]]
    }

    list(
      model_name = request$model_name,
      profile = request$profile,
      active_variable = request$active_variable,
      predicted_pd = predicted_pd,
      baseline = baseline,
      shap_values = shap_values
    )
  })

  chart_data <- reactive({
    analysis <- profile_analysis()
    values <- 100 * analysis$shap_values[slider_variables]
    pd <- shap_data$portfolio_pd[[analysis$model_name]]
    pd_histogram <- hist(pd, breaks = 28, plot = FALSE)
    variable <- analysis$active_variable
    dependence <- shap_data$dependence_data
    dependence <- dependence[
      dependence$model == analysis$model_name & dependence$variable == variable,
      , drop = FALSE
    ]
    trend <- if (nrow(dependence) > 5) {
      stats::lowess(dependence$value, dependence$shap, f = 0.6)
    } else {
      list(x = numeric(), y = numeric())
    }
    contribution_points <- unname(Map(
      function(variable, value) list(
        name = unname(profile_labels[[variable]]),
        y = unname(value),
        color = if (value >= 0) "#d95f59" else "#4c91d9"
      ),
      slider_variables,
      values
    ))
    active_position <- match(variable, slider_variables)
    shap_band <- list(list(
      from = active_position - 1.5, to = active_position - 0.5,
      color = "rgba(13, 110, 253, 0.08)"
    ))
    waterfall_band <- list(list(
      from = active_position - 0.5, to = active_position + 0.5,
      color = "rgba(13, 110, 253, 0.08)"
    ))

    list(
      contributions = contribution_points,
      shap_band = shap_band,
      waterfall = unname(c(
        list(list(
          name = "Background<br/>mean",
          y = 100 * unname(analysis$baseline),
          color = "#e0e0e0"
        )),
        contribution_points,
        list(list(name = "Predicted<br/>PD", isSum = TRUE, color = "#34495e"))
      )),
      waterfall_band = waterfall_band,
      portfolio = list(
        values = pd,
        histogram = list_parse2(data.frame(x = pd_histogram$mids, y = pd_histogram$counts)),
        line = list_parse2(data.frame(
          x = rep(analysis$predicted_pd, 2),
          y = c(0, max(pd_histogram$counts) * 1.05)
        )),
        ymax = max(pd_histogram$counts) * 1.1
      ),
      dependence = list(
        x_title = unname(profile_labels[[variable]]),
        points = list_parse2(data.frame(x = dependence$value, y = dependence$shap)),
        trend = list_parse2(data.frame(x = trend$x, y = trend$y)),
        active = list(list(
          x = unname(analysis$profile[[variable]]),
          y = unname(analysis$shap_values[[variable]])
        ))
      )
    )
  })

  output$shap_plot <- renderHighchart({
    chart <- isolate(chart_data())

    highchart() |>
      hc_chart(type = "bar") |>
      hc_xAxis(
        categories = unname(profile_labels[slider_variables]), reversed = TRUE,
        plotBands = chart$shap_band
      ) |>
      hc_yAxis(
        title = list(text = "Contribution to predicted PD (pp)"),
        plotLines = list(list(value = 0, color = "#dee2e6", width = 1, zIndex = 3))
      ) |>
      hc_legend(enabled = FALSE) |>
      hc_tooltip(pointFormat = "{point.y:.1f} pp") |>
      hc_plotOptions(series = list(
        animation = list(duration = 300),
        dataLabels = list(
          enabled = TRUE,
          style = list(fontWeight = "normal", textOutline = "none"),
          formatter = JS("function () { return (this.y >= 0 ? '+' : '') + Highcharts.numberFormat(this.y, 1) + ' pp'; }")
        )
      )) |>
      hc_add_series(id = "shap", name = "SHAP contribution",
        data = chart$contributions)
  })

  output$waterfall_plot <- renderHighchart({
    chart <- isolate(chart_data())

    highchart() |>
      hc_add_dependency("modules/waterfall.js") |>
      hc_chart(type = "waterfall") |>
      hc_xAxis(type = "category", plotBands = chart$waterfall_band) |>
      hc_yAxis(title = list(text = "Probability of default (%)")) |>
      hc_legend(enabled = FALSE) |>
      hc_tooltip(pointFormat = "{point.y:.1f}") |>
      hc_plotOptions(series = list(
        animation = list(duration = 300),
        borderWidth = 0,
        dataLabels = list(
          enabled = TRUE,
          inside = FALSE,
          useHTML = TRUE,
          style = list(
            color = "#495057", fontWeight = "normal", textOutline = "none"
          ),
          formatter = JS(paste(
            "function () {",
            "  if (this.point.isSum || this.point.index === 0) {",
            "    const value = Highcharts.numberFormat(this.y, 1) + '%';",
            "    return '<span style=\"font-size: 13px; font-weight: 600\">' + value + '</span>';",
            "  }",
            "  return (this.y >= 0 ? '+' : '') + Highcharts.numberFormat(this.y, 1) + ' pp';",
            "}"
          ))
        )
      )) |>
      hc_add_series(id = "waterfall", name = "PD", data = chart$waterfall)
  })

  chart_context <- reactiveVal(list(model = NULL, variable = NULL))

  observeEvent(profile_analysis(), {
    analysis <- profile_analysis()
    data <- chart_data()
    previous <- chart_context()

    highchartProxy("shap_plot") |>
      hcpxy_update(xAxis = list(plotBands = data$shap_band)) |>
      hcpxy_update_series(id = "shap", data = data$contributions)
    highchartProxy("waterfall_plot") |>
      hcpxy_update(xAxis = list(plotBands = data$waterfall_band)) |>
      hcpxy_update_series(id = "waterfall", data = data$waterfall)
    highchartProxy("pd_plot") |>
      hcpxy_update_series(id = "pd_line", data = data$portfolio$line)

    if (!identical(previous$model, analysis$model_name)) {
      highchartProxy("pd_plot") |>
        hcpxy_update(yAxis = list(max = data$portfolio$ymax)) |>
        hcpxy_update_series(id = "portfolio", data = data$portfolio$histogram)
    }

    dependence_proxy <- highchartProxy("dependence_plot")
    if (!identical(previous$model, analysis$model_name) ||
        !identical(previous$variable, analysis$active_variable)) {
      dependence_proxy |>
        hcpxy_update(xAxis = list(title = list(text = data$dependence$x_title))) |>
        hcpxy_update_series(id = "dependence", data = data$dependence$points) |>
        hcpxy_update_series(id = "trend", data = data$dependence$trend)
    }
    dependence_proxy |>
      hcpxy_update_series(id = "active", data = data$dependence$active)

    chart_context(list(
      model = analysis$model_name,
      variable = analysis$active_variable
    ))
  }, ignoreInit = TRUE)

  output$pd_plot <- renderHighchart({
    data <- isolate(chart_data())$portfolio

    hchart(data$values, breaks = 28, id = "portfolio", name = "Clients",
      color = "#e0e0e0") |>
      hc_xAxis(title = list(text = "Predicted probability of default")) |>
      hc_yAxis(title = list(text = "Clients"), min = 0, max = data$ymax) |>
      hc_legend(enabled = FALSE) |>
      hc_tooltip(valueDecimals = 3) |>
      hc_plotOptions(column = list(borderWidth = 0, groupPadding = 0, pointPadding = 0)) |>
      hc_add_series(
        id = "pd_line", name = "Predicted PD", type = "line",
        data = data$line, color = "#34495e", dashStyle = "ShortDash",
        lineWidth = 2, marker = list(enabled = FALSE),
        dataLabels = list(
          enabled = TRUE, align = "left", x = 5, y = 4,
          crop = FALSE, overflow = "allow",
          style = list(
            color = "#495057", fontSize = "13px",
            fontWeight = "600", textOutline = "none"
          ),
          formatter = JS(paste(
            "function () {",
            "  if (this.point.index !== 1) return null;",
            "  return Highcharts.numberFormat(100 * this.x, 1) + '%';",
            "}"
          ))
        )
      )
  })

  output$dependence_title <- renderUI({
    variable <- profile_analysis()$active_variable
    paste("SHAP dependence ·", profile_labels[[variable]])
  })

  output$dependence_plot <- renderHighchart({
    data <- isolate(chart_data())$dependence

    highchart() |>
      hc_chart(type = "scatter") |>
      hc_xAxis(title = list(text = data$x_title)) |>
      hc_yAxis(title = list(text = "SHAP contribution to PD")) |>
      hc_legend(enabled = FALSE) |>
      hc_tooltip(pointFormat = "x: {point.x:.2f}<br/>SHAP: {point.y:.3f}") |>
      hc_plotOptions(scatter = list(marker = list(symbol = "circle"))) |>
      hc_add_series(
        id = "dependence", name = "Test sample", data = data$points,
        color = "rgba(224, 224, 224, 0.65)", zIndex = 1,
        marker = list(radius = 3, symbol = "circle")
      ) |>
      hc_add_series(
        id = "trend", name = "Trend", type = "line", data = data$trend,
        color = "#6c757d", lineWidth = 2, zIndex = 2,
        marker = list(enabled = FALSE),
        enableMouseTracking = FALSE
      ) |>
      hc_add_series(
        id = "active", name = "Current profile", data = data$active,
        color = "#34495e", zIndex = 3,
        marker = list(
          radius = 6, symbol = "circle", lineColor = "white", lineWidth = 2
        )
      )
  })
}

shinyApp(ui, server)
