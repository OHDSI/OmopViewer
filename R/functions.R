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
      result |>
        dplyr::filter(.data$result_id %in% .env$x) |>
        omopgenerics::newSummarisedResult(
          settings = omopgenerics::settings(result) |>
            dplyr::filter(.data$result_id %in% .env$x)
        )
    })
}
defaultFilterValues <- function(result, resultList) {
  resultList |>
    purrr::imap(\(x, nm) {
      sOpts <- omopgenerics::settings(result) |>
        dplyr::filter(.data$result_id %in% .env$x) |>
        dplyr::select(!dplyr::any_of(c(
          "result_id", "result_type", "package_name", "package_version",
          "strata", "group", "additional", "min_cell_count"
        ))) |>
        as.list() |>
        purrr::map(\(x) {
          x <- unique(x)
          x[!is.na(x)]
        }) |>
        purrr::compact()
      names(sOpts) <- glue::glue("settings_{names(sOpts)}")
      res <- result |>
        dplyr::filter(.data$result_id %in% .env$x)

      # omopgenerics 0.4.1 should fix this
      attr(res, "settings") <- attr(res, "settings") |>
        dplyr::filter(.data$result_id %in% .env$x)

      gOpts <- res |>
        dplyr::select(c(
          "cdm_name", "group_name", "group_level", "strata_name",
          "strata_level", "additional_name", "additional_level"
        )) |>
        dplyr::distinct() |>
        visOmopResults::splitAll() |>
        as.list() |>
        purrr::map(unique)
      names(gOpts) <- glue::glue("grouping_{names(gOpts)}")
      veOpts <- res |>
        dplyr::select("variable_name", "estimate_name") |>
        as.list() |>
        purrr::map(unique)
      res <- c(sOpts, gOpts, veOpts) |>
        purrr::compact()
      tidyColumns <- names(res)
      id <- startsWith(tidyColumns, "settings_") | startsWith(tidyColumns, "grouping_")
      tidyColumns[id] <- substr(tidyColumns[id], 10, nchar(tidyColumns[id]))
      res$tidy_columns <- tidyColumns
      names(res) <- glue::glue("{nm}_{names(res)}")
      return(res)
    }) |>
    purrr::flatten()
}
