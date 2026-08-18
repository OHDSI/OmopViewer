survivalPicker <- function(nm, id, label, multiple = TRUE, selected = NULL, choices = NULL) {
  choices <- choices %||% paste0("choices$", nm, "_", id)
  selected <- selected %||% paste0("selected$", nm, "_", id)
  glue::glue(
    'shinyWidgets::pickerInput(
      inputId = "{nm}_{id}",
      label = "{label}",
      choices = {choices},
      selected = {selected},
      multiple = {toupper(as.character(multiple))},
      options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3")
    )'
  ) |>
    as.character()
}

survivalConditional <- function(condition, ...) {
  code <- c(...) |>
    purrr::compact() |>
    paste0(collapse = ",\n")
  if (nchar(code) == 0) {
    return(character())
  }
  glue::glue(
    'shiny::conditionalPanel(
      condition = "{condition}",
      {code}
    )'
  ) |>
    as.character()
}

survivalMaybeConditional <- function(condition = NULL, ...) {
  code <- c(...) |>
    purrr::compact()
  if (length(code) == 0) {
    return(character())
  }
  if (is.null(condition)) {
    return(paste0(code, collapse = ",\n"))
  }
  survivalConditional(condition, code)
}

survivalDownloadHeader <- function(outputId, type) {
  if (type == "table") {
    filters <- c(
      survivalPicker(
        nm = outputId,
        id = "format",
        label = "Format",
        multiple = FALSE,
        choices = 'c("docx", "png", "pdf", "html")',
        selected = '"docx"'
      ),
      glue::glue('shiny::downloadButton(outputId = "{outputId}_download", label = "Download table")')
    )
  } else {
    filters <- c(
      glue::glue('shiny::numericInput(inputId = "{outputId}_width", label = "Width", value = 15)'),
      glue::glue('shiny::numericInput(inputId = "{outputId}_height", label = "Height", value = 15)'),
      survivalPicker(
        nm = outputId,
        id = "units",
        label = "Units",
        multiple = FALSE,
        choices = 'c("px", "cm", "inch")',
        selected = '"cm"'
      ),
      glue::glue('shiny::numericInput(inputId = "{outputId}_dpi", label = "DPI", value = 300)'),
      glue::glue('shiny::downloadButton(outputId = "{outputId}_download", label = "Download plot")')
    )
  }

  glue::glue(
    'bslib::card_header(
      bslib::popover(
        shiny::icon("download"),
        {paste0(filters, collapse = ",\n")}
      ),
      class = "text-end"
    )'
  ) |>
    as.character()
}

writeSurvivalUiPanel <- function(x, nm, updateButtons) {
  hasAttrition <- "table_attrition" %in% names(x$content)
  hasNonAttrition <- any(names(x$content) %in% c("table_survival", "table_events", "plot_survival"))
  nonAttritionCondition <- if (hasAttrition) {
    paste0("input.", nm, "_active_tab != 'Table Attrition'")
  } else {
    NULL
  }
  updateButton <- updateButtonUi(updateButtons, nm) |>
    sub(pattern = ",\\s*$", replacement = "")

  leftFilters <- c(
    updateButton,
    survivalPicker(nm, "cdm_name", "CDM name"),
    survivalPicker(nm, "target_cohort", "Target cohort"),
    if (hasNonAttrition) {
      survivalMaybeConditional(
        condition = nonAttritionCondition,
        survivalPicker(nm, "sex", "Sex"),
        survivalPicker(nm, "age_group", "Age group")
      )
    },
    if (hasAttrition) {
      survivalConditional(
        condition = paste0("input.", nm, "_active_tab == 'Table Attrition'"),
        survivalPicker(nm, "reason", "Reason")
      )
    },
    survivalPicker(nm, "variable_level", "Variable level"),
    survivalPicker(nm, "analysis_type", "Analysis type", multiple = FALSE),
    if (hasNonAttrition) {
      survivalMaybeConditional(
        condition = nonAttritionCondition,
        survivalPicker(nm, "censor_on_cohort_exit", "Censor on cohort exit")
      )
    },
    survivalPicker(nm, "competing_outcome", "Competing outcome"),
    if (hasNonAttrition) {
      survivalMaybeConditional(
        condition = nonAttritionCondition,
        survivalPicker(nm, "follow_up_days", "Follow up days"),
        survivalPicker(nm, "minimum_survival_days", "Minimum survival days")
      )
    },
    survivalPicker(nm, "outcome", "Outcome"),
    if (hasNonAttrition) {
      survivalMaybeConditional(
        condition = nonAttritionCondition,
        survivalPicker(nm, "outcome_date_variable", "Outcome date variable"),
        survivalPicker(nm, "outcome_washout", "Outcome washout"),
        survivalPicker(nm, "restricted_mean_follow_up", "Restricted mean follow up")
      )
    }
  ) |>
    purrr::compact() |>
    paste0(collapse = ",\n")

  outputPanels <- survivalContentUi(x$content, nm)

  c(
    "bslib::nav_panel(",
    c(
      paste0("title = ", cast(x$title)),
      writeIcon(x$icon),
      glue::glue(
        'bslib::layout_sidebar(
          sidebar = bslib::sidebar(
            {leftFilters},
            position = "left"
          ),
          bslib::navset_card_tab(
            id = "{nm}_active_tab",
            {outputPanels}
          )
        )'
      ) |>
        as.character()
    ) |>
      paste0(collapse = ",\n"),
    ")"
  ) |>
    paste0(collapse = "\n")
}

survivalContentUi <- function(content, nm) {
  purrr::imap_chr(content, \(cont, id) {
    outputId <- cont$output_id
    download <- switch(cont$output_type,
      "gt" = survivalDownloadHeader(outputId, "table"),
      "ui" = survivalDownloadHeader(outputId, "plot"),
      ""
    )
    body <- switch(id,
      table_survival = glue::glue(
        'bslib::layout_sidebar(
          sidebar = bslib::sidebar(
            {survivalPicker(outputId, "time_scale", "Time Scale", multiple = FALSE, choices = \'c("days", "months", "years")\', selected = \'"days"\')},
            position = "right"
          ),
          gt::gt_output("{outputId}") |>
            shinycssloaders::withSpinner()
        )'
      ),
      table_events = glue::glue(
        'gt::gt_output("{outputId}") |>
          shinycssloaders::withSpinner()'
      ),
      table_attrition = glue::glue(
        'gt::gt_output("{outputId}") |>
          shinycssloaders::withSpinner()'
      ),
      plot_survival = glue::glue(
        'bslib::layout_sidebar(
          sidebar = bslib::sidebar(
            shinyWidgets::materialSwitch(
              inputId = "{outputId}_interactive",
              label = "Interactive",
              value = TRUE
            ),
            shinyWidgets::materialSwitch(
              inputId = "{outputId}_cumulative_failure",
              label = "Cumulative failure",
              value = FALSE
            ),
            shinyWidgets::materialSwitch(
              inputId = "{outputId}_log_log",
              label = "Log-Log plot",
              value = FALSE
            ),
            {survivalPicker(outputId, "time_scale", "Time Scale", multiple = FALSE, choices = \'c("days", "months", "years")\', selected = \'"days"\')},
            {survivalPicker(outputId, "facet", "Facet", choices = survivalPlotAestheticChoices(), selected = "character()")},
            {survivalPicker(outputId, "colour", "Colour", choices = survivalPlotAestheticChoices(), selected = "character()")},
            position = "right"
          ),
          shiny::uiOutput("{outputId}") |>
            shinycssloaders::withSpinner()
        )'
      )
    ) |>
      as.character()

    glue::glue(
      'bslib::nav_panel(
        title = "{cont$title}",
        bslib::card(
          full_screen = TRUE,
          {download},
          {body}
        )
      )'
    ) |>
      as.character()
  }) |>
    paste0(collapse = ",\n")
}

survivalPlotAestheticChoices <- function() {
  paste0(
    'c("variable", "cdm_name", "target_cohort", "sex", "age_group", "time", ',
    '"analysis_type", "censor_on_cohort_exit", "competing_outcome", "eventgap", ',
    '"follow_up_days", "minimum_survival_days", "outcome", ',
    '"outcome_date_variable", "outcome_washout", "restricted_mean_follow_up")'
  )
}

survivalTemplate <- function(x, ...) {
  replacements <- list(...)
  for (nm in names(replacements)) {
    x <- stringr::str_replace_all(
      string = x,
      pattern = stringr::fixed(paste0("<", nm, ">")),
      replacement = replacements[[nm]]
    )
  }
  x
}

writeSurvivalServer <- function(x, nm, data, updateButtons) {
  c(
    "",
    paste0("# ", nm, " -----"),
    writeUpdateDataMessage(nm = nm, filters = x$filters, updateButtons = updateButtons),
    survivalServerCore(nm, data, updateButtons),
    survivalContentServer(x$content)
  ) |>
    paste0(collapse = "\n")
}

survivalServerCore <- function(nm, data, updateButtons) {
  survivalTemplate(
    '
    appliedSurvivalInputs <- shiny::reactiveVal(NULL)

    readSurvivalInputs <- function() {
      list(
        cdm_name = input$<nm>_cdm_name,
        target_cohort = input$<nm>_target_cohort,
        sex = input$<nm>_sex,
        age_group = input$<nm>_age_group,
        reason = input$<nm>_reason,
        variable_level = input$<nm>_variable_level,
        analysis_type = input$<nm>_analysis_type,
        censor_on_cohort_exit = input$<nm>_censor_on_cohort_exit,
        competing_outcome = input$<nm>_competing_outcome,
        follow_up_days = input$<nm>_follow_up_days,
        minimum_survival_days = input$<nm>_minimum_survival_days,
        outcome = input$<nm>_outcome,
        outcome_date_variable = input$<nm>_outcome_date_variable,
        outcome_washout = input$<nm>_outcome_washout,
        restricted_mean_follow_up = input$<nm>_restricted_mean_follow_up
      )
    }

    selectedNonOverall <- function(x) {
      x <- x[!is.na(x)]
      setdiff(x, "overall")
    }

    selectedAesthetics <- function(x) {
      varying <- character()

      if (identical(x$analysis_type, "competing_risk")) {
        varying <- c(varying, "variable")
      }
      if (length(selectedNonOverall(x$sex)) > 1) {
        varying <- c(varying, "sex")
      }
      if (length(selectedNonOverall(x$age_group)) > 1) {
        varying <- c(varying, "age_group")
      }
      if (length(selectedNonOverall(x$outcome)) > 1) {
        varying <- c(varying, "outcome")
      }
      if (length(selectedNonOverall(x$competing_outcome)) > 1) {
        varying <- c(varying, "competing_outcome")
      }

      varying <- unique(varying)

      list(
        colour = varying[seq_len(min(2, length(varying)))],
        facet = if (length(varying) > 2) varying[-seq_len(2)] else character()
      )
    }

    applySurvivalInputs <- function() {
      applied <- readSurvivalInputs()
      shiny::validate(
        shiny::need(
          length(applied$analysis_type) == 1,
          "Choose exactly one analysis type."
        )
      )

      appliedSurvivalInputs(applied)

      aesthetics <- selectedAesthetics(applied)
      shinyWidgets::updatePickerInput(
        session = session,
        inputId = "<nm>_plot_survival_colour",
        selected = aesthetics$colour
      )
      shinyWidgets::updatePickerInput(
        session = session,
        inputId = "<nm>_plot_survival_facet",
        selected = aesthetics$facet
      )
      shinyWidgets::updateMaterialSwitch(
        session = session,
        inputId = "<nm>_plot_survival_cumulative_failure",
        value = identical(applied$analysis_type, "competing_risk")
      )
    }

    shiny::observeEvent(input$<nm>_analysis_type, {
      isCompetingRisk <- identical(input$<nm>_analysis_type, "competing_risk")

      shinyWidgets::updatePickerInput(
        session = session,
        inputId = "<nm>_competing_outcome",
        selected = if (isCompetingRisk) "death_cohort" else "none"
      )
      shinyWidgets::updatePickerInput(
        session = session,
        inputId = "<nm>_variable_level",
        selected = if (isCompetingRisk) c("progression", "death_cohort") else "death_cohort"
      )
      shinyWidgets::updatePickerInput(
        session = session,
        inputId = "<nm>_outcome",
        selected = if (isCompetingRisk) "progression" else "death_cohort"
      )
    }, ignoreInit = FALSE)

    <apply_inputs_observer>

    ## get <nm> data
    getSurvivalData <- function(resultType,
                                useStrata = FALSE,
                                useReason = FALSE,
                                useFollowUpSettings = TRUE) {
      applied <- appliedSurvivalInputs()
      shiny::req(applied)

      result <- <data>[["<nm>"]] |>
        dplyr::filter(
          .data$cdm_name %in% applied$cdm_name,
          .data$variable_level %in% applied$variable_level
        ) |>
        omopgenerics::filterGroup(.data$target_cohort %in% applied$target_cohort) |>
        omopgenerics::filterSettings(
          .data$result_type == resultType,
          .data$analysis_type %in% applied$analysis_type,
          .data$competing_outcome %in% applied$competing_outcome,
          .data$outcome %in% applied$outcome
        )

      if (useStrata) {
        result <- result |>
          omopgenerics::filterStrata(
            .data$sex %in% applied$sex,
            .data$age_group %in% applied$age_group
          )
      }

      if (useReason) {
        result <- result |>
          omopgenerics::filterStrata(.data$reason %in% applied$reason)
      }

      if (useFollowUpSettings) {
        result <- result |>
          omopgenerics::filterSettings(
            .data$censor_on_cohort_exit %in% applied$censor_on_cohort_exit,
            .data$follow_up_days %in% applied$follow_up_days,
            .data$minimum_survival_days %in% applied$minimum_survival_days,
            .data$outcome_date_variable %in% applied$outcome_date_variable,
            .data$outcome_washout %in% applied$outcome_washout,
            .data$restricted_mean_follow_up %in% applied$restricted_mean_follow_up
          )
      }

      result
    }

    emptyTable <- function(message = "No data available for the selected filters.") {
      gt::gt(tibble::tibble(Message = message))
    }

    safeTable <- function(result, tableCall) {
      if (nrow(result) == 0) {
        return(emptyTable())
      }

      tryCatch(
        tableCall(result),
        error = function(e) emptyTable(conditionMessage(e))
      )
    }',
    nm = nm,
    data = data,
    apply_inputs_observer = survivalApplyInputsObserver(nm, updateButtons)
  )
}

survivalApplyInputsObserver <- function(nm, updateButtons) {
  if (updateButtons) {
    survivalTemplate(
      "shiny::observeEvent(input$update_<nm>, {
        applySurvivalInputs()
      }, ignoreInit = TRUE)",
      nm = nm
    )
  } else {
    "shiny::observe({
      applySurvivalInputs()
    })"
  }
}

survivalContentServer <- function(content) {
  purrr::imap_chr(content, \(cont, id) {
    switch(id,
      table_survival = survivalTableSummaryServer(cont$output_id, cont$reactive_function),
      table_events = survivalTableEventsServer(cont$output_id, cont$reactive_function),
      table_attrition = survivalTableAttritionServer(cont$output_id, cont$reactive_function),
      plot_survival = survivalPlotServer(cont$output_id, cont$reactive_function)
    )
  }) |>
    paste0(collapse = "\n")
}

survivalTableSummaryServer <- function(outputId, reactiveFunction) {
  survivalTemplate(
    '<reactiveFunction> <- shiny::reactive({
      result <- getSurvivalData("survival_summary", useStrata = TRUE)
      safeTable(result, function(x) {
        CohortSurvival::tableSurvival(
          x,
          timeScale = input$<outputId>_time_scale,
          type = "gt"
        )
      })
    })
    output$<outputId> <- gt::render_gt({
      <reactiveFunction>()
    })
    output$<outputId>_download <- shiny::downloadHandler(
      filename = paste0("table_survival_summary.", input$<outputId>_format),
      content = function(file) {
        gt::gtsave(<reactiveFunction>(), file)
      }
    )',
    outputId = outputId,
    reactiveFunction = reactiveFunction
  )
}

survivalTableEventsServer <- function(outputId, reactiveFunction) {
  survivalTemplate(
    '<reactiveFunction> <- shiny::reactive({
      result <- getSurvivalData("survival_events", useStrata = TRUE)
      safeTable(result, function(x) {
        CohortSurvival::tableSurvivalEvents(
          x,
          type = "gt"
        )
      })
    })
    output$<outputId> <- gt::render_gt({
      <reactiveFunction>()
    })
    output$<outputId>_download <- shiny::downloadHandler(
      filename = paste0("table_survival_events.", input$<outputId>_format),
      content = function(file) {
        gt::gtsave(<reactiveFunction>(), file)
      }
    )',
    outputId = outputId,
    reactiveFunction = reactiveFunction
  )
}

survivalTableAttritionServer <- function(outputId, reactiveFunction) {
  survivalTemplate(
    '<reactiveFunction> <- shiny::reactive({
      result <- getSurvivalData("survival_attrition", useReason = TRUE, useFollowUpSettings = FALSE)
      safeTable(result, function(x) {
        CohortSurvival::tableSurvivalAttrition(
          x,
          type = "gt"
        )
      })
    })
    output$<outputId> <- gt::render_gt({
      <reactiveFunction>()
    })
    output$<outputId>_download <- shiny::downloadHandler(
      filename = paste0("table_survival_attrition.", input$<outputId>_format),
      content = function(file) {
        gt::gtsave(<reactiveFunction>(), file)
      }
    )',
    outputId = outputId,
    reactiveFunction = reactiveFunction
  )
}

survivalPlotServer <- function(outputId, reactiveFunction) {
  survivalTemplate(
    '<reactiveFunction> <- shiny::reactive({
      applied <- appliedSurvivalInputs()
      shiny::req(applied)
      isCompetingRisk <- identical(applied$analysis_type, "competing_risk")
      result <- getSurvivalData("survival_estimates", useStrata = TRUE)

      shiny::validate(
        shiny::need(
          nrow(result) > 0,
          "No survival estimates available for the selected filters."
        )
      )

      result |>
        CohortSurvival::plotSurvival(
          facet = input$<outputId>_facet,
          colour = input$<outputId>_colour,
          cumulativeFailure = isCompetingRisk || input$<outputId>_cumulative_failure,
          logLog = input$<outputId>_log_log,
          timeScale = input$<outputId>_time_scale
        )
    })
    output$<outputId> <- shiny::renderUI({
      x <- <reactiveFunction>()
      renderInteractivePlot(x, input$<outputId>_interactive)
    })
    output$<outputId>_download <- shiny::downloadHandler(
      filename = "plot_survival.png",
      content = function(file) {
        ggplot2::ggsave(
          filename = file,
          plot = <reactiveFunction>(),
          width = as.numeric(input$<outputId>_width),
          height = as.numeric(input$<outputId>_height),
          units = input$<outputId>_units,
          dpi = as.numeric(input$<outputId>_dpi)
        )
      }
    )',
    outputId = outputId,
    reactiveFunction = reactiveFunction
  )
}
