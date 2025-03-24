filterData <- function(result,
                       prefix,
                       input) {
  result <- result[[prefix]]

  if (nrow(result) == 0) {
    return(omopgenerics::emptySummarisedResult())
  }

  if (length(input) == 0) inputs <- character() else inputs <- names(input)

  # subset to inputs of interest
  inputs <- inputs[startsWith(inputs, prefix)]

  # filter settings
  set <- omopgenerics::settings(result)
  setPrefix <- paste0(c(prefix, "settings_"), collapse = "_")
  toFilter <- inputs[startsWith(inputs, setPrefix)]
  nms <- substr(toFilter, nchar(setPrefix) + 1, nchar(toFilter))
  for (nm in nms) {
    if (nm %in% colnames(set)) {
      set <- set |>
        dplyr::filter(as.character(.data[[nm]]) %in% input[[paste0(setPrefix, nm)]])
    }
  }
  result <- result |>
    dplyr::filter(.data$result_id %in% set$result_id)

  if (nrow(result) == 0) {
    return(omopgenerics::emptySummarisedResult())
  }

  # filter grouping
  cols <- c(
    "cdm_name", "group_name", "group_level", "strata_name", "strata_level",
    "additional_name", "additional_level"
  )
  groupCols <- visOmopResults::groupColumns(result)
  strataCols <- visOmopResults::strataColumns(result)
  additionalCols <- visOmopResults::additionalColumns(result)
  group <- result |>
    dplyr::select(dplyr::all_of(cols)) |>
    dplyr::distinct() |>
    visOmopResults::splitAll()
  groupPrefix <- paste0(c(prefix, "grouping_"), collapse = "_")
  toFilter <- inputs[startsWith(inputs, groupPrefix)]
  nms <- substr(toFilter, nchar(groupPrefix) + 1, nchar(toFilter))
  for (nm in nms) {
    if (nm %in% colnames(group)) {
      group <- group |>
        dplyr::filter(.data[[nm]] %in% input[[paste0(groupPrefix, nm)]])
    }
  }
  result <- result |>
    dplyr::inner_join(
      group |>
        visOmopResults::uniteGroup(cols = groupCols) |>
        visOmopResults::uniteStrata(cols = strataCols) |>
        visOmopResults::uniteAdditional(cols = additionalCols),
      by = cols
    )

  # filter variables and estimates
  nms <- c("variable_name", "estimate_name")
  nms <- nms[paste0(prefix, "_", nms) %in% inputs]
  for (nm in nms) {
    result <- result |>
      dplyr::filter(.data[[nm]] %in% input[[paste0(prefix, "_", nm)]])
  }

  # return a summarised_result
  result <- result |>
    omopgenerics::newSummarisedResult(settings = set)

  return(result)
}
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
summaryCard <- function(result) {
  nPanels <- length(result)

  # bind everything back
  result <- result |>
    purrr::compact() |>
    omopgenerics::bind() |>
    suppressMessages()
  if (is.null(result)) {
    result <- omopgenerics::emptySummarisedResult()
  }
  sets <- omopgenerics::settings(result)

  # result overview
  nResult <- format(nrow(result), big.mark = ",")
  nSets <- format(nrow(sets), big.mark = ",")
  nResultType <- format(length(unique(sets$result_type)), big.mark = ",")
  cdmNames <- unique(result$cdm_name)
  nCdm <- format(length(cdmNames), big.mark = ",")
  cdms <- if (length(cdmNames) > 0) {
    paste0(": ", paste0("*", cdmNames, "*", collapse = ", "))
  } else {
    ""
  }
  overview <- c(
    "### Result overview",
    "- Results contain **{nResult}** rows with **{nSets}** different result_id." |>
      glue::glue(),
    "- Results contain **{nPanels}** panels with **{nResultType}** diferent result_type." |>
      glue::glue(),
    "- Results contain data from **{nCdm}** different cdm objects{cdms}." |>
      glue::glue()
  )

  # packages versions
  packageVersions <- sets |>
    dplyr::group_by(.data$package_name, .data$package_version) |>
    dplyr::summarise(result_ids = dplyr::n(), .groups = "drop") |>
    dplyr::group_by(.data$package_name) |>
    dplyr::mutate(
      n = dplyr::n_distinct(.data$package_version),
      group = paste(
        dplyr::if_else(.data$n > 1, "Inconsistent", "Consistent"),
        "package versions"
      )
    ) |>
    dplyr::ungroup() |>
    dplyr::arrange(
      dplyr::desc(.data$n), .data$package_name, .data$package_version
    ) |>
    dplyr::mutate(
      message = paste0(
        "**", .data$package_name, "** ", .data$package_version, " in ",
        .data$result_ids, " result id(s)."
      ),
      message = dplyr::if_else(
        .data$n > 1,
        paste0('- <span style="color:red">', .data$message, "</span>"),
        paste0('- <span style="color:green">', .data$message, "</span>"),
      )
    ) |>
    dplyr::group_by(.data$group) |>
    dplyr::group_split() |>
    purrr::map(\(x) c(unique(x$group), x$message)) |>
    purrr::flatten_chr()

  # result suppression
  resultSuppression <- sets |>
    dplyr::select("result_id", "min_cell_count") |>
    dplyr::mutate(min_cell_count = dplyr::if_else(
      as.integer(.data$min_cell_count) <= 1L, "0", .data$min_cell_count
    )) |>
    dplyr::group_by(.data$min_cell_count) |>
    dplyr::tally() |>
    dplyr::arrange(.data$min_cell_count) |>
    dplyr::mutate(
      message = paste0("**", .data$n, "** ", dplyr::if_else(
        .data$min_cell_count == "0",
        "not suppressed results",
        paste0("results suppressed at minCellCount = `", .data$min_cell_count, "`.")
      )),
      message = dplyr::if_else(
        .data$min_cell_count == "0",
        paste0('- <span style="color:red">', .data$message, "</span>"),
        paste0('- <span style="color:green">', .data$message, "</span>"),
      )
    ) |>
    dplyr::pull("message")

  bslib::card(
    bslib::card_header("Results summary"),
    shiny::markdown(c(
      overview, "", " ### Package versions", packageVersions, "",
      "### Result suppression", resultSuppression, "", "### Explore settings"
    )),
    DT::datatable(sets, options = list(scrollX = TRUE), filter = "top", rownames = FALSE)
  )
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
    visOmopResults::formatEstimateName(estimateNameFormat = formatEstimates) |>
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
  # split and add settings
  x <- x |>
    omopgenerics::splitAll() |>
    omopgenerics::addSettings()

  # estimate columns
  if (pivotEstimates) {
    estCols <- unique(x$estimate_name)
    x <- x |>
      omopgenerics::pivotEstimates()
  } else {
    estCols <- c("estimate_name", "estimate_type", "estimate_value")
  }

  # order columns
  groupColumns <- omopgenerics::groupColumns(x)
  strataColumns <- omopgenerics::strataColumns(x)
  additionalColumns <- omopgenerics::additionalColumns(x)
  settingsColumns <- omopgenerics::settingsColumns(x)
  cols <- list(
    'CDM name' = "cdm_name", 'Group' = groupColumns, 'Strata' = strataColumns,
    'Additional' = additionalColumns, 'Settings' = settingsColumns
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
          .data$result_id %in% .env$resultId |
            .data$result_type %in% .env$resultType
        )
    }
  }
  return(res)
}
populateChoices <- function(panelDetails, result) {
  choices <- list()
  for (nm1 in names(panelDetails)) {
    res <- filterResult(
      result = result,
      resultId = panelDetails[[nm1]]$result_id,
      resultType = panelDetails[[nm1]]$result_type
    ) |>
      omopgenerics::splitAll() |>
      omopgenerics::addSettings() |>
      dplyr::select(!c("result_id", "estimate_type", "estimate_value")) |>
      as.list() |>
      purrr::map(unique)
    filters <- panelDetails[[nm1]]$filters
    for (nm2 in names(filters)) {
      if (identical(filters[[nm2]]$choices, "choices$")) {
        choices[[paste0(nm1, "_", nm2)]] <- res[[filters[[nm2]]$column]]
      }
    }
  }
  return(choices)
}
populateSelected <- function(panelDetails, result) {
  selected <- list()
  for (nm1 in names(panelDetails)) {
    res <- filterResult(
      result = result,
      resultId = panelDetails[[nm1]]$result_id,
      resultType = panelDetails[[nm1]]$result_type
    ) |>
      omopgenerics::splitAll() |>
      omopgenerics::addSettings() |>
      dplyr::select(!c("result_id", "estimate_type", "estimate_value")) |>
      as.list() |>
      purrr::map(unique)
    filters <- panelDetails[[nm1]]$filters
    for (nm2 in names(filters)) {
      if (identical(filters[[nm2]]$selected, "selected$")) {
        selected[[paste0(nm1, "_", nm2)]] <- res[[filters[[nm2]]$column]]
      }
    }
  }
  return(selected)
}
