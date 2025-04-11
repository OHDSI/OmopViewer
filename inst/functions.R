backgroundCard <- function(fileName) {
  # read file
  content <- readLines(fileName)

  # extract yaml metadata
  # Find the positions of the YAML delimiters (----- or ---)
  yamlStart <- grep("^---|^-----", content)[1]
  yamlEnd <- grep("^---|^-----", content)[2]

  if (any(is.na(c(yamlStart, yamlEnd)))) {
    metadata <- NULL
  } else {
    # identify YAML block
    id <- (yamlStart + 1):(yamlEnd - 1)
    # Parse the YAML content
    metadata <- yaml::yaml.load(paste(content[id], collapse = "\n"))
    # eliminate yaml part from content
    content <- content[-(yamlStart:yamlEnd)]
  }

  tmpFile <- tempfile(fileext = ".md")
  writeLines(text = content, con = tmpFile)

  # metadata referring to keys
  backgroundKeywords <- list(
    header = "bslib::card_header",
    footer = "bslib::card_footer"
  )
  keys <- names(backgroundKeywords) |>
    rlang::set_names() |>
    purrr::map(\(x) {
      if (x %in% names(metadata)) {
        paste0(backgroundKeywords[[x]], "(metadata[[x]])") |>
          rlang::parse_expr() |>
          rlang::eval_tidy()
      } else {
        NULL
      }
    }) |>
    purrr::compact()

  arguments <- c(
    # metadata referring to arguments of card
    metadata[names(metadata) %in% names(formals(bslib::card))],
    # content
    list(
      keys$header,
      bslib::card_body(shiny::HTML(markdown::markdownToHTML(
        file = tmpFile, fragment.only = TRUE
      ))),
      keys$footer
    ) |>
      purrr::compact()
  )

  unlink(tmpFile)

  do.call(bslib::card, arguments)
}
summaryCdmName <- function(data) {
  x <- data |>
    purrr::map(\(x) {
      x |>
        dplyr::group_by(.data$cdm_name) |>
        dplyr::summarise(number_rows = dplyr::n(), .groups = "drop")
    }) |>
    dplyr::bind_rows() |>
    dplyr::group_by(.data$cdm_name) |>
    dplyr::summarise(
      number_rows = as.integer(sum(.data$number_rows)),
      .groups = "drop"
    ) |>
    dplyr::mutate(label = paste0(.data$cdm_name, " (", .data$number_rows, ")")) |>
    dplyr::pull("label") |>
    rlang::set_names() |>
    as.list()
  list("<b>CDM names</b>" = x)
}
summaryPackages <- function(data) {
  x <- data |>
    purrr::map(\(x) {
      x |>
        omopgenerics::addSettings(
          settingsColumn = c("package_name", "package_version")
        ) |>
        dplyr::group_by(.data$package_name, .data$package_version) |>
        dplyr::summarise(number_rows = dplyr::n(), .groups = "drop") |>
        dplyr::right_join(
          omopgenerics::settings(x) |>
            dplyr::select(c("package_name", "package_version")) |>
            dplyr::distinct(),
          by = c("package_name", "package_version")
        ) |>
        dplyr::mutate(number_rows = dplyr::coalesce(.data$number_rows, 0))
    }) |>
    dplyr::bind_rows() |>
    dplyr::group_by(.data$package_name, .data$package_version) |>
    dplyr::summarise(
      number_rows = as.integer(sum(.data$number_rows)),
      .groups = "drop"
    ) |>
    dplyr::group_by(.data$package_name) |>
    dplyr::group_split() |>
    as.list()
  lab <- "<b>"
  names(x) <- x |>
    purrr::map_chr(\(x) {
      if (nrow(x) > 1) {
        lab <<- "<b style='color:red'>"
        paste0("<b style='color:red'>", unique(x$package_name), " (Multiple versions!) </b>")
      } else {
        paste0(
          x$package_name, " (version = ", x$package_version,
          "; number records = ", x$number_rows,")"
        )
      }
    })
  x <- x |>
    purrr::map(\(x) {
      if (nrow(x) > 1) {
        paste0(
          "version = ", x$package_version, "; number records = ",
          x$number_rows
        ) |>
          rlang::set_names() |>
          as.list()
      } else {
        x$package_name
      }
    })
  list(x) |>
    rlang::set_names(nm = paste0(lab, "Packages versions</b>"))
}
summaryMinCellCount <- function(data) {
  x <- data |>
    purrr::map(\(x) {
      x |>
        omopgenerics::addSettings(settingsColumn = "min_cell_count") |>
        dplyr::group_by(.data$min_cell_count) |>
        dplyr::summarise(number_rows = dplyr::n(), .groups = "drop") |>
        dplyr::right_join(
          omopgenerics::settings(x) |>
            dplyr::select("min_cell_count") |>
            dplyr::distinct(),
          by = "min_cell_count"
        ) |>
        dplyr::mutate(number_rows = dplyr::coalesce(.data$number_rows, 0))
    }) |>
    dplyr::bind_rows() |>
    dplyr::group_by(.data$min_cell_count) |>
    dplyr::summarise(
      number_rows = as.integer(sum(.data$number_rows)),
      .groups = "drop"
    ) |>
    dplyr::mutate(min_cell_count = as.integer(.data$min_cell_count)) |>
    dplyr::arrange(.data$min_cell_count) |>
    dplyr::mutate(
      label = dplyr::if_else(
        .data$min_cell_count == 0L,
        "<b style='color:red'>Not censored</b>",
        paste0("Min cell count = ", .data$min_cell_count)
      ),
      label = paste0(.data$label, " (", .data$number_rows, ")")
    ) |>
    dplyr::pull("label") |>
    rlang::set_names() |>
    as.list()
  lab <- ifelse(any(grepl("Not censored", unlist(x))), "<b style='color:red'>", "<b>")
  list(x) |>
    rlang::set_names(nm = paste0(lab, "Min Cell Count Suppression</b>"))
}
summaryPanels <- function(data) {
  x <- data |>
    purrr::map(\(x) {
      if (nrow(x) == 0) {
        res <- omopgenerics::settings(x) |>
          dplyr::select(!c(
            "result_id", "package_name", "package_version", "group", "strata",
            "additional", "min_cell_count"
          )) |>
          dplyr::relocate("result_type") |>
          as.list() |>
          purrr::map(\(x) sort(unique(x)))
      } else {
        sets <- c("result_type", omopgenerics::settingsColumns(x))
        res <- x |>
          omopgenerics::addSettings(settingsColumn = sets) |>
          dplyr::relocate(dplyr::all_of(sets)) |>
          omopgenerics::splitAll() |>
          dplyr::select(!c(
            "variable_name", "variable_level", "estimate_name",
            "estimate_type", "estimate_value", "result_id"
          )) |>
          as.list() |>
          purrr::map(\(values) {
            values <- as.list(table(values))
            paste0(names(values), " (number rows = ", values, ")") |>
              rlang::set_names() |>
              as.list()
          })
      }
      res
    })
  list(x) |>
    rlang::set_names(nm = "<b>Panels</b>")
}
simpleTable <- function(result,
                        header = character(),
                        group = character(),
                        hide = character()) {
  # initial checks
  if (length(header) == 0) header <- character()
  if (length(group) == 0) group <- NULL
  if (length(hide) == 0) hide <- character()

  if (nrow(result) == 0) {
    return(gt::gt(dplyr::tibble()))
  }

  result <- result |>
    omopgenerics::addSettings() |>
    omopgenerics::splitAll() |>
    dplyr::select(-"result_id")

  # format estimate column
  formatEstimates <- c(
    "N (%)" = "<count> (<percentage>%)",
    "N" = "<count>",
    "median [Q25 - Q75]" = "<median> [<q25> - <q75>]",
    "mean (SD)" = "<mean> (<sd>)",
    "[Q25 - Q75]" = "[<q25> - <q75>]",
    "range" = "[<min> <max>]",
    "[Q05 - Q95]" = "[<q05> - <q95>]"
  )
  result <- result |>
    visOmopResults::formatEstimateValue(
      decimals = c(integer = 0, numeric = 1, percentage = 0)
    ) |>
    visOmopResults::formatEstimateName(estimateName = formatEstimates) |>
    suppressMessages() |>
    visOmopResults::formatHeader(header = header) |>
    dplyr::select(!dplyr::any_of(c("estimate_type", hide)))
  if (length(group) > 1) {
    id <- paste0(group, collapse = "; ")
    result <- result |>
      tidyr::unite(col = !!id, dplyr::all_of(group), sep = "; ", remove = TRUE)
    group <- id
  }
  result <- result |>
    visOmopResults::formatTable(groupColumn = group)
  return(result)
}
prepareResult <- function(result, resultList) {
  resultList |>
    purrr::map(\(x) {
      if (is.numeric(x)) {
        resultId <- x
        resultType <- NULL
      } else if (is.character(x)) {
        resultId <- NULL
        resultType <- x
      } else {
        resultId <- x$result_id
        resultType <- x$result_type
      }
      filterResult(result, resultId = resultId, resultType = resultType)
    })
}
tidyDT <- function(x,
                   columns,
                   pivotEstimates) {
  groupColumns <- omopgenerics::groupColumns(x)
  strataColumns <- omopgenerics::strataColumns(x)
  additionalColumns <- omopgenerics::additionalColumns(x)
  settingsColumns <- omopgenerics::settingsColumns(x)

  # split and add settings
  x <- x |>
    omopgenerics::splitAll() |>
    omopgenerics::addSettings()

  # remove density
  x <- x |>
    dplyr::filter(!.data$estimate_name %in% c("density_x", "density_y"))

  # estimate columns
  if (pivotEstimates) {
    estCols <- unique(x$estimate_name)
    x <- x |>
      omopgenerics::pivotEstimates()
  } else {
    estCols <- c("estimate_name", "estimate_type", "estimate_value")
  }

  # order columns
  cols <- list(
    "CDM name" = "cdm_name", "Group" = groupColumns, "Strata" = strataColumns,
    "Additional" = additionalColumns, "Settings" = settingsColumns,
    "Variable" = c("variable_name", "variable_level")
  ) |>
    purrr::map(\(x) x[x %in% columns]) |>
    purrr::compact()
  cols[["Estimates"]] <- estCols
  x <- x |>
    dplyr::select(dplyr::all_of(unname(unlist(cols))))

  # prepare the header
  container <- shiny::tags$table(
    class = "display",
    shiny::tags$thead(
      purrr::imap(cols, \(x, nm) shiny::tags$th(colspan = length(x), nm)) |>
        shiny::tags$tr(),
      shiny::tags$tr(purrr::map(unlist(cols), shiny::tags$th))
    )
  )

  # create DT table
  DT::datatable(
    data = x,
    filter = "top",
    container = container,
    rownames = FALSE,
    options = list(searching = FALSE)
  )
}
filterResult <- function(result, resultId = NULL, resultType = NULL) {
  resultId <- unique(resultId)
  resultType <- unique(resultType)
  if (is.null(resultType)) {
    if (is.null(resultId)) {
      res <- omopgenerics::emptySummarisedResult()
    } else {
      res <- result |>
        omopgenerics::filterSettings(.data$result_id %in% .env$resultId)
    }
  } else {
    if (is.null(resultId)) {
      res <- result |>
        omopgenerics::filterSettings(.data$result_type %in% .env$resultType)
    } else {
      res <- result |>
        omopgenerics::filterSettings(
          .data$result_id %in% .env$resultId &
            .data$result_type %in% .env$resultType
        )
    }
  }
  return(res)
}
getValues <- function(result, resultList) {
  resultList |>
    purrr::imap(\(x, nm) {
      res <- result |>
        filterResult(resultId = x$result_id, resultType = x$result_type)
      values <- res |>
        dplyr::select(!c("estimate_type", "estimate_value")) |>
        dplyr::distinct() |>
        omopgenerics::splitAll() |>
        dplyr::select(!"result_id") |>
        as.list() |>
        purrr::map(\(x) sort(unique(x)))
      valuesSettings <- omopgenerics::settings(res) |>
        dplyr::select(!dplyr::any_of(c(
          "result_id", "result_type", "package_name", "package_version",
          "group", "strata", "additional", "min_cell_count"
        ))) |>
        as.list() |>
        purrr::map(\(x) sort(unique(x[!is.na(x)]))) |>
        purrr::compact()
      values <- c(values, valuesSettings)
      names(values) <- paste0(nm, "_", names(values))
      values
    }) |>
    purrr::flatten()
}
tableTopLargeScaleCharacteristics <- function(result,
                                              topConcepts = 10) {
  # check input
  result <- omopgenerics::validateResultArgument(result) |>
    omopgenerics::filterSettings(.data$result_type == "summarise_large_scale_characteristics")
  topConcepts <- as.integer(topConcepts)
  omopgenerics::assertNumeric(topConcepts, integerish = TRUE, length = 1)

  # create table
  x <- result |>
    omopgenerics::tidy() |>
    dplyr::group_by(dplyr::across(!c(
      "variable_name", "concept_id", "count", "percentage"
    ))) |>
    dplyr::arrange(dplyr::desc(.data$percentage)) |>
    dplyr::slice_head(n = topConcepts) |>
    dplyr::mutate("top" = dplyr::row_number()) |>
    dplyr::ungroup() |>
    dplyr::mutate(estimate_value = sprintf(
      "%s (%s) <br> %i (%.1f%%)", .data$variable_name, .data$concept_id,
      .data$count, .data$percentage
    )) |>
    dplyr::select(!c("variable_name", "concept_id", "count", "percentage")) |>
    dplyr::rename(window = "variable_level") |>
    dplyr::mutate(estimate_name = "counts", estimate_type = "character")
  header <- x |>
    dplyr::select(!dplyr::starts_with(c("estimate", "top"))) |>
    as.list() |>
    purrr::map(unique) |>
    purrr::keep(\(x) length(x) > 1) |>
    names()
  hide <- colnames(x)[!colnames(x) %in% c("top", "estimate_value", header)]

  # final table
  x |>
    visOmopResults::visTable(header = header, type = "gt", hide = hide) |>
    gt::fmt_markdown()
}
tableLargeScaleCharacteristics <- function(result,
                                           compareBy = NULL,
                                           hide = c("type"),
                                           smdReference = NULL,
                                           type = "reactable") {
  # initial checks
  result <- result |>
    omopgenerics::validateResultArgument() |>
    omopgenerics::filterSettings(.data$result_type == "summarise_large_scale_characteristics") |>
    dplyr::filter(.data$estimate_name == "percentage")

  strataCols <- omopgenerics::strataColumns(result)
  choic <- c("cdm_name", "cohort_name", strataCols, "variable_level", "type")
  hide <- hide %||% character()

  omopgenerics::assertChoice(type, choices = c("DT", "reactable"))
  omopgenerics::assertChoice(compareBy, choices = choic, length = 1, null = TRUE)
  omopgenerics::assertChoice(hide, choices = choic)

  hide <- hide[!hide %in% compareBy]
  result <- omopgenerics::tidy(result) |>
    dplyr::select(!dplyr::all_of(hide))

  if (length(compareBy) == 0) {
    opts <- "percentage"
    smdReference <- NULL
  } else {
    opts <- unique(result[[compareBy]])
    omopgenerics::assertChoice(smdReference, choices = opts, length = 1, null = TRUE)
  }

  # pivot
  if (!is.null(compareBy)) {
    result <- result |>
      tidyr::pivot_wider(
        names_from = dplyr::all_of(compareBy),
        values_fill = 0,
        values_from = "percentage"
      )
  }

  result <- result |>
    dplyr::select(dplyr::any_of(c(
      "cdm_name", "cohort_name", strataCols, "type",
      "window" = "variable_level", "concept_name" = "variable_name",
      "concept_id", opts
    )))

  if (length(smdReference) > 0) {
    cols <- character()
    for (col in opts) {
      if (col == smdReference) {
        ref <- rlang::set_names(smdReference, paste0(smdReference, " (ref)"))
      } else {
        result <- result |>
          dplyr::mutate(!!paste0(col, " SMD") := qSmd(.data[[smdReference]], .data[[col]]))
        cols <- c(cols, col, paste0(col, " SMD"))
      }
    }
    result <- result |>
      dplyr::relocate(dplyr::all_of(c(ref, cols)), .after = "concept_id")
  }

  if (type == "DT") {
    out <- DT::datatable(result)
  } else {
    out <- reactable::reactable(result)
  }
  out
}
qSmd <- function(ref, comp) {
  dplyr::if_else(
    ref == 0 & comp == 0,
    0,
    round(suppressWarnings((ref - comp)/sqrt((ref * (100 - ref) + comp * (100 - comp)) / 2)), 4)
  )
}
plotComparedLargeScaleCharacteristics <- function(result,
                                                  colour,
                                                  reference,
                                                  facet) {
  # initial checks
  result <- omopgenerics::validateResultArgument(result) |>
    omopgenerics::filterSettings(.data$result_type == "summarise_large_scale_characteristics") |>
    dplyr::filter(.data$estimate_name == "percentage")
  strataCols <- omopgenerics::strataColumns(result)

  choic <- c("cdm_name", "cohort_name", strataCols, "variable_level", "type")
  omopgenerics::assertChoice(colour, choices = choic, length = 1)
  result <- omopgenerics::tidy(result) |>
    dplyr::rename(concept_name = "variable_name")
  opts <- unique(result[[colour]])

  if (length(opts) < 2) {
    cli::cli_inform(c(x = "No multiple values for {.var {colour}} to compare."))
    p <- ggplot2::ggplot() +
      ggplot2::annotate(
        geom = "text", x = 0.5, y = 0.5, size = 6, hjust = 0.5,
        label = paste0("No multiple values to compare for: ", colour),
      ) +
      ggplot2::theme_void() +
      ggplot2::xlim(0, 1) + ylim(0, 1)
  } else {
    omopgenerics::assertChoice(reference, choices = opts, length = 1, null = TRUE)

    # prepare reference
    ref <- result |>
      dplyr::filter(.data[[colour]] == .env$reference) |>
      dplyr::rename(reference_percentage = "percentage") |>
      dplyr::select(!dplyr::all_of(colour)) |>
      dplyr::cross_join(dplyr::tibble(!!colour := opts[opts != reference]))
    join <- colnames(ref)[colnames(ref) != "reference_percentage"]
    result <- result |>
      dplyr::filter(.data[[colour]] != .env$reference) |>
      dplyr::full_join(ref, by = join) |>
      dplyr::mutate(dplyr::across(
        c("percentage", "reference_percentage"), \(x) dplyr::coalesce(x, 0)
      ))

    label <- c(choic[choic != colour], "concept_name", "concept_id")
    colourLab <- paste0(
      "Comparator (",
      colour |>
        stringr::str_replace_all(pattern = "_", replacement = " ") |>
        stringr::str_to_sentence(),
      ")"
    )

    p <- visOmopResults::scatterPlot(
      result = result, x= "reference_percentage", y = "percentage",
      point = TRUE, line = FALSE, ribbon = FALSE, ymin = NULL, ymax = NULL,
      facet = facet, colour = colour, group = NULL, label = label
    ) +
      ggplot2::geom_line(
        mapping = ggplot2::aes(x = x, y = y),
        data = dplyr::tibble(x = c(0, 100), y = c(0, 100)),
        color = "black",
        linetype = "dashed",
        inherit.aes = FALSE
      ) +
      ggplot2::ylab("Comparator (%)") +
      ggplot2::xlab("Reference (%)") +
      ggplot2::labs(colour = colourLab, fill = colourLab)
  }
  plotly::ggplotly(p)
}
